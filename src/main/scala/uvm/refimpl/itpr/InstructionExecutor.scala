package uvm.refimpl.itpr

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm._
import uvm.comminsts._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables._
import uvm.types._
import uvm.refimpl.nat.NativeCallResult

/**
 * Part of the InterpreterThread. It isolates the GIANT case-matching function.
 */
trait InstructionExecutor extends InterpreterActions with CommInstExecutor {
  import InterpreterThread.logger

  protected def mutator: Mutator
  implicit protected def microVM: MicroVM
  implicit protected def memorySupport: MemorySupport

  /** Interpret the current instruction. */
  protected def interpretCurrentInstruction(): Unit = try {
    logger.debug(ctx + "Executing instruction...")

    curInst match {
      case i @ InstBinOp(op, opndTy, op1, op2, excClause) => {
        def doScalar(scalarTy: Type, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          scalarTy match {
            case TypeInt(l)   => br.asIntRaw = PrimOpHelpers.intBinOp(op, l, b1.asIntRaw, b2.asIntRaw, ctx)
            case TypeFloat()  => br.asFloat = PrimOpHelpers.floatBinOp(op, b1.asFloat, b2.asFloat, ctx)
            case TypeDouble() => br.asDouble = PrimOpHelpers.doubleBinOp(op, b1.asDouble, b2.asDouble, ctx)
            case _            => throw new UvmRuntimeException(ctx + "BinOp not suitable for type %s".format(scalarTy))
          }
        }

        try {
          opndTy match {
            case TypeVector(scalarTy, sz) => {
              for (((b1, b2), br) <- ((op1.asSeq zip op2.asSeq) zip results(0).asSeq)) {
                doScalar(scalarTy, b1, b2, br)
              }
            }
            case scalarTy => doScalar(scalarTy, op1, op2, results(0))
          }
          continueNormally()
        } catch {
          case e: UvmDivisionByZeroException => excClause match {
            case None => throw e
            case Some(ec) => {
              branchTo(ec.exc)
            }
          }
        }
      }

      case i @ InstCmp(op, opndTy, op1, op2) => {
        def doScalar(scalarTy: Type, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          br.asBoolean = scalarTy match {
            case TypeInt(l)      => PrimOpHelpers.intCmp(op, l, b1.asIntRaw, b2.asIntRaw, ctx)
            case TypeFloat()     => PrimOpHelpers.floatCmp(op, b1.asFloat, b2.asFloat, ctx)
            case TypeDouble()    => PrimOpHelpers.doubleCmp(op, b1.asDouble, b2.asDouble, ctx)
            case TypeRef(_)      => PrimOpHelpers.refCmp(op, b1.asRef, b2.asRef, ctx)
            case TypeFuncRef(_)  => PrimOpHelpers.objCmp(op, b1.asFunc, b2.asFunc, "funcref", ctx)
            case TypeStackRef()  => PrimOpHelpers.objCmp(op, b1.asStack, b2.asStack, "stackref", ctx)
            case TypeThreadRef() => PrimOpHelpers.objCmp(op, b1.asThread, b2.asThread, "threadref", ctx)
            case TypeIRef(_) => {
              val (op1b, op1o) = b1.asIRef
              val (op2b, op2o) = b2.asIRef
              PrimOpHelpers.irefCmp(op, op1b, op1o, op2b, op2o, ctx)
            }
            case _ => throw new UvmRuntimeException(ctx + "Comparison not suitable for type %s".format(opndTy))
          }
        }

        opndTy match {
          case TypeVector(scalarTy, sz) => {
            for (((b1, b2), br) <- ((op1.asSeq zip op2.asSeq) zip results(0).asSeq)) {
              doScalar(scalarTy, b1, b2, br)
            }
          }
          case scalarTy => doScalar(scalarTy, op1, op2, results(0))
        }

        continueNormally()
      }

      case i @ InstConv(op, fromTy, toTy, opnd) => {
        def doScalar(scalarFromTy: Type, scalarToTy: Type, bOpnd: ValueBox, br: ValueBox): Unit = {
          def iToI(): Unit = (scalarFromTy, scalarToTy) match {
            case (TypeInt(fl), TypeInt(tl)) => {
              val od = bOpnd.asIntRaw
              val result = op match {
                case ConvOptr.TRUNC => OpHelper.trunc(od, tl)
                case ConvOptr.ZEXT  => OpHelper.zext(od, fl, tl)
                case ConvOptr.SEXT  => OpHelper.sext(od, fl, tl)
              }
              br.asIntRaw = result
            }
            case _ => throw new UvmRuntimeException(ctx + "Expect integer source and dest type. Found %s and %s".format(scalarFromTy, scalarToTy))
          }

          def fpToI(signed: Boolean): Unit = {
            val tl = scalarToTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer dest type. Found %s".format(scalarToTy))
            }
            val result = scalarFromTy match {
              case TypeFloat()  => OpHelper.floatToI(bOpnd.asFloat, tl, signed)
              case TypeDouble() => OpHelper.doubleToI(bOpnd.asDouble, tl, signed)
              case _            => throw new UvmRuntimeException(ctx + "Expect FP source type. Found %s.".format(scalarFromTy))
            }
            br.asIntRaw = result
          }

          def iToFP(signed: Boolean): Unit = {
            val fl = scalarFromTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer source type. Found %s".format(scalarFromTy))
            }
            val od = bOpnd.asIntRaw
            val extended = if (signed) OpHelper.prepareSigned(od, fl) else OpHelper.prepareUnsigned(od, fl)
            scalarToTy match {
              case TypeFloat()  => br.asFloat = extended.toFloat
              case TypeDouble() => br.asDouble = extended.toDouble
              case _            => throw new UvmRuntimeException(ctx + "Expect FP dest type. Found %s.".format(scalarToTy))
            }
          }

          def bitcast(): Unit = (scalarFromTy, scalarToTy) match {
            case (TypeInt(32), TypeFloat())  => br.asFloat = java.lang.Float.intBitsToFloat(bOpnd.asIntRaw.intValue)
            case (TypeInt(64), TypeDouble()) => br.asDouble = java.lang.Double.longBitsToDouble(bOpnd.asIntRaw.longValue)
            case (TypeFloat(), TypeInt(32))  => br.asInt32 = java.lang.Float.floatToRawIntBits(bOpnd.asFloat)
            case (TypeDouble(), TypeInt(64)) => br.asInt64 = java.lang.Double.doubleToRawLongBits(bOpnd.asDouble)
            case _ => throw new UvmRuntimeException(ctx +
              "BITCAST can only convert between int and FP types of the same size. Found %s and %s.".format(scalarFromTy, scalarToTy))
          }

          def refcast(): Unit = (scalarFromTy, scalarToTy) match {
            case (TypeRef(_), TypeRef(_))         => br.copyFrom(bOpnd)
            case (TypeIRef(_), TypeIRef(_))       => br.copyFrom(bOpnd)
            case (TypeFuncRef(_), TypeFuncRef(_)) => br.copyFrom(bOpnd)
            case _ => throw new UvmRuntimeException(ctx +
              "REFCAST can only convert between two types both of which are ref, iref, or func. Found %s and %s.".format(scalarFromTy, scalarToTy))
          }

          def ptrcast(): Unit = {
            (scalarFromTy, scalarToTy) match {
              case (TypeInt(_), TypeInt(_)) => throw new UvmRuntimeException(ctx +
                "PTRCAST cannot convert between two int types. Found %s and %s.".format(scalarFromTy, scalarToTy))
              case _ =>
            }
            val srcAddr: Word = scalarFromTy match {
              case TypeInt(n)             => bOpnd.getUInt(n).longValue // truncates
              case _: AbstractPointerType => bOpnd.asPtr
            }
            scalarToTy match {
              case TypeInt(n)             => br.setInt(srcAddr, Math.min(n, 64))
              case _: AbstractPointerType => br.asPtr = srcAddr
            }
          }

          op match {
            case ConvOptr.TRUNC   => iToI()
            case ConvOptr.ZEXT    => iToI()
            case ConvOptr.SEXT    => iToI()
            case ConvOptr.FPTRUNC => br.asFloat = bOpnd.asDouble.toFloat
            case ConvOptr.FPEXT   => br.asDouble = bOpnd.asFloat.toDouble
            case ConvOptr.FPTOUI  => fpToI(signed = false)
            case ConvOptr.FPTOSI  => fpToI(signed = true)
            case ConvOptr.UITOFP  => iToFP(signed = false)
            case ConvOptr.SITOFP  => iToFP(signed = true)
            case ConvOptr.BITCAST => bitcast()
            case ConvOptr.REFCAST => refcast()
            case ConvOptr.PTRCAST => ptrcast()
          }
        }

        (fromTy, toTy) match {
          case (TypeVector(scalarFromTy, sz), TypeVector(scalarToTy, sz2)) => {
            if (sz != sz2) throw new UvmRefImplException(ctx + "The source and dest vector types must have the same length")

            for ((bOpnd, br) <- (opnd.asSeq zip results(0).asSeq)) {
              doScalar(scalarFromTy, scalarToTy, bOpnd, br)
            }
          }
          case _ => doScalar(fromTy, toTy, opnd, results(0))
        }

        incPC()
      }

      case i @ InstSelect(condTy, opndTy, cond, ifTrue, ifFalse) => {
        def doScalar(bCond: ValueBox, bTrue: ValueBox, bFalse: ValueBox, br: ValueBox): Unit = {
          if (bCond.asBoolean) {
            br.copyFrom(bTrue)
          } else {
            br.copyFrom(bFalse)
          }
        }

        condTy match {
          case TypeVector(TypeInt(1), sz) => {
            for ((((bCond, bTrue), bFalse), br) <- (cond.asSeq zip ifTrue.asSeq zip ifFalse.asSeq zip results(0).asSeq)) {
              doScalar(bCond, bTrue, bFalse, br)
            }
          }
          case TypeInt(1) => {
            doScalar(cond, ifTrue, ifFalse, results(0))
          }
          case _ => throw new UvmRefImplException(ctx + "Condition must be either int<1> or a vector of int<1>. Found %s".format(condTy))
        }

        continueNormally()
      }

      case i @ InstBranch(dest) => {
        branchTo(dest)
      }

      case i @ InstBranch2(cond, ifTrue, ifFalse) => {
        val cv = boxOf(cond).asInstanceOf[BoxInt].value
        val dest = if (cv == 1) ifTrue else ifFalse
        branchTo(dest)
      }

      case i @ InstSwitch(opndTy, opnd, defDest, cases) => {
        opndTy match {
          case TypeInt(l) => {
            val ov = boxOf(opnd).asInstanceOf[BoxInt].value
            val dest = cases.find(pair => boxOf(pair._1).asInstanceOf[BoxInt].value == ov).map(_._2).getOrElse(defDest)
            branchTo(dest)
          }
          case _ => throw new UvmRefImplException(ctx + "Operand type must be integer. %s found.".format(opndTy))
        }
      }

      case i @ InstCall(sig, callee, argList, excClause, keepAlives) => {
        val calleeFunc = callee.asFunc.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val argBoxes = argList.map(boxOf)

        val shouldIncrementPC = curStack.callMu(calleeFunc, argBoxes)
        if (shouldIncrementPC) {
          throw new UvmRefImplException(ctx + "Should not continue normally immediately after calling")
        }
      }

      case i @ InstTailCall(sig, callee, argList) => {
        val calleeFunc = callee.asFunc.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val argBoxes = argList.map(boxOf)

        val shouldIncrementPC = curStack.tailCallMu(calleeFunc, argBoxes)
        if (shouldIncrementPC) {
          throw new UvmRefImplException(ctx + "Should not continue normally immediately after tail-calling")
        }
      }

      case i @ InstRet(funcVer, retVals) => {
        val rvbs = retVals.map(boxOf)
        val shouldIncrementPC = curStack.retFromMu(rvbs)
        if (shouldIncrementPC) {
          continueNormally()
        }
      }

      case i @ InstThrow(excVal) => {
        val exc = excVal.asRef
        curStack.popMuFrameForThrow()
        catchException(exc)
      }

      case i @ InstExtractValue(strTy, index, opnd) => {
        results(0) copyFrom opnd.asSeq(index)
        continueNormally()
      }

      case i @ InstInsertValue(strTy, index, opnd, newVal) => {
        val nvb = boxOf(newVal)
        for (((ofb, ifb), ind) <- (opnd.asSeq zip results(0).asSeq).zipWithIndex) {
          if (ind == index) {
            ifb.copyFrom(nvb)
          } else {
            ifb.copyFrom(ofb)
          }
        }
        continueNormally()
      }

      case i @ InstExtractElement(vecTy, indTy, opnd, index) => {
        val ind = index.getUInt(indTy.length).toLong

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        results(0) copyFrom opnd.asSeq(ind.toInt)
        continueNormally()
      }

      case i @ InstInsertElement(vecTy, indTy, opnd, index, newVal) => {
        val ind = index.getUInt(indTy.length).toLong

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        val indInt = ind.intValue
        val nvb = boxOf(newVal)

        for (((oeb, ieb), ind2) <- (opnd.asSeq zip results(0).asSeq).zipWithIndex) {
          if (ind2 == indInt) {
            ieb.copyFrom(nvb)
          } else {
            ieb.copyFrom(oeb)
          }
        }
        continueNormally()
      }

      case i @ InstShuffleVector(vecTy, maskTy, vec1, vec2, mask) => {
        val vecLen = vecTy.len.toInt
        val maskIntLen = maskTy.elemTy.asInstanceOf[TypeInt].length
        val v1s = vec1.asSeq
        val v2s = vec2.asSeq
        val ms = mask.asSeq
        val is = results(0).asSeq

        for (((meb, ieb), ind) <- (ms zip is).zipWithIndex) {
          val me = meb.getUInt(maskIntLen).toInt
          if (me < vecLen) {
            ieb.copyFrom(v1s(me))
          } else if (vecLen <= me && me < vecLen * 2) {
            ieb.copyFrom(v2s(me - vecLen))
          } else {
            throw new UvmRuntimeException(ctx + "Index %d as the %d-th element of mask is out of range. Vector type: %s".format(me, ind, vecTy))
          }
        }
        continueNormally()
      }

      case i @ InstNew(allocTy, excClause) => {
        handleOutOfMemory(excClause) {
          val addr = mutator.newScalar(allocTy)
          results(0).asRef = addr
          continueNormally()
        }
      }

      case i @ InstNewHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val len = length.getUInt(lenTy.length).longValue
          val addr = mutator.newHybrid(allocTy, len)
          results(0).asRef = addr
          continueNormally()
        }
      }

      case i @ InstAlloca(allocTy, excClause) => {
        handleOutOfMemory(excClause) {
          val addr = mutator.allocaScalar(curStack.stackMemory, allocTy)
          results(0).asIRef = (0L, addr)
          continueNormally()
        }
      }

      case i @ InstAllocaHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val len = length.getUInt(lenTy.length).longValue
          val addr = mutator.allocaHybrid(curStack.stackMemory, allocTy, len)
          results(0).asIRef = (0L, addr)
          continueNormally()
        }
      }

      case i @ InstGetIRef(referentTy, opnd) => {
        results(0).asIRef = (opnd.asRef, 0L)
        continueNormally()
      }

      case i @ InstGetFieldIRef(ptr, referentTy, index, opnd) => {
        val addrIncr = TypeSizes.fieldOffsetOf(referentTy, index)

        incrementBoxIRefOrPointer(ptr, opnd, results(0), addrIncr)
        continueNormally()
      }

      case i @ InstGetElemIRef(ptr, referentTy, indTy, opnd, index) => {
        val ind = index.getSInt(indTy.length).longValue
        val addrIncr = TypeSizes.elemOffsetOf(referentTy, ind)

        incrementBoxIRefOrPointer(ptr, opnd, results(0), addrIncr)
        continueNormally()
      }

      case i @ InstShiftIRef(ptr, referentTy, offTy, opnd, offset) => {
        val off = offset.getSInt(offTy.length).longValue
        val addrIncr = TypeSizes.shiftOffsetOf(referentTy, off)

        incrementBoxIRefOrPointer(ptr, opnd, results(0), addrIncr)
        continueNormally()
      }

      case i @ InstGetVarPartIRef(ptr, referentTy, opnd) => {
        val addrIncr = TypeSizes.varPartOffsetOf(referentTy)

        incrementBoxIRefOrPointer(ptr, opnd, results(0), addrIncr)
        continueNormally()
      }

      case i @ InstLoad(ptr, ord, referentTy, loc, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val ib = resultBox(0)

        val addr = addressOf(ptr, loc)
        if (addr == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.load(ptr, uty, addr, ib)
          continueNormally()
        }
      }

      case i @ InstStore(ptr, ord, referentTy, loc, newVal, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val nvb = boxOf(newVal)

        val addr = addressOf(ptr, loc)
        if (addr == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.store(ptr, uty, addr, nvb)
          continueNormally()
        }
      }

      case i @ InstCmpXchg(ptr, weak, ordSucc, ordFail, referentTy, loc, expected, desired, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val eb = boxOf(expected)
        val db = boxOf(desired)
        val br = resultBox(0)
        val bs = resultBox(1)

        val addr = addressOf(ptr, loc)
        if (addr == 0L) {
          nullRefError(excClause)
        } else {
          val succ = MemoryOperations.cmpXchg(ptr, uty, addr, eb, db, br)
          bs.asBoolean = succ
          continueNormally()
        }
      }

      case i @ InstAtomicRMW(ptr, ord, op, referentTy, loc, opnd, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val ob = boxOf(opnd)
        val ib = resultBox(0)

        val addr = addressOf(ptr, loc)
        if (addr == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.atomicRMW(ptr, uty, op, addr, ob, ib)
          continueNormally()
        }
      }

      case i @ InstFence(ord) => {
        // No-op in this interpreter
        continueNormally()
      }

      case i @ InstTrap(retTy, excClause, keepAlives) => {
        doTrap(retTy, 0)
      }

      case i @ InstWatchPoint(wpID, retTy, dis, ena, exc, keepAlives) => {
        val isEnabled = microVM.trapManager.isWatchPointEnabled(wpID)

        if (isEnabled) {
          doTrap(retTy, wpID)
        } else {
          branchTo(dis)
        }
      }

      case i @ InstWPBranch(wpID, dis, ena) => {
        val isEnabled = microVM.trapManager.isWatchPointEnabled(wpID)
        if (isEnabled) {
          branchTo(ena)
        } else {
          branchTo(dis)
        }
      }

      case i @ InstCCall(callConv, funcTy, sig, callee, argList, excClause, keepAlives) => {
        if (callConv != Flag("#DEFAULT")) {
          throw new UvmRefImplException(ctx + "Currently only support the #DEFAULT callConv. %s found.".format(callConv.name))
        }

        val addr = callee.asPtr

        val argBoxes = argList.map(boxOf)

        val shouldIncrementPC = curStack.callNative(sig, addr, argBoxes)
        if (shouldIncrementPC) {
          continueNormally()
        }
      }

      case i @ InstNewThread(stack, newStackAction, excClause) => {
        val newStack = stack.asStack.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to bind to a NULL stack.")
        }

        val newThread = newStackAction match {
          case PassValues(argTys, args) => {
            val argBoxes = args.map(boxOf)
            microVM.threadStackManager.newThread(newStack, HowToResume.PassValues(argBoxes))
          }
          case ThrowExc(exc) => {
            val excAddr = exc.asRef
            microVM.threadStackManager.newThread(newStack, HowToResume.ThrowExc(excAddr))
          }
        }
        results(0).asThread = Some(newThread)

        continueNormally()
      }

      case i @ InstSwapStack(swappee, curStackAction, newStackAction, excClause, keepAlives) => {
        val oldStack = curStack
        val newStack = swappee.asStack.getOrElse {
          throw new UvmRuntimeException(ctx + "Swappee must not be NULL.")
        }

        def handleOldStack() = curStackAction match {
          case RetWith(retTys) => {
            unbindRetWith(retTys)
          }
          case KillOld() => {
            unbindAndKillStack()
          }
        }

        newStackAction match {
          case PassValues(argTys, args) => {
            val argBoxes = args.map(boxOf)
            handleOldStack()
            rebindPassValues(newStack, argBoxes)
          }
          case ThrowExc(exc) => {
            val excBox = boxOf(exc) // need to get the box before the stack is destroyed.
            handleOldStack()
            rebindThrowExc(newStack, excBox.asRef)
          }
        }
      }

      case i: InstCommInst => interpretCurrentCommonInstruction()

      case i => {
        throw new UvmRefImplException("Unimplemented instruction %s".format(i.getClass.getName))
      }
    }
  } catch {
    case e: Exception => {
      logger.error(ctx + "Exception thrown while interpreting instruction.")
      throw e
    }
  }
}