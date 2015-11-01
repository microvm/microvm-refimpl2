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
        def doInt(l: Int, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxInt].value
          val op2v = b2.asInstanceOf[BoxInt].value

          val result = PrimOpHelpers.intBinOp(op, l, op1v, op2v, ctx)

          val iBox = br.asInstanceOf[BoxInt]
          iBox.value = result
        }

        def doFloat(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxFloat].value
          val op2v = b2.asInstanceOf[BoxFloat].value

          val result = PrimOpHelpers.floatBinOp(op, op1v, op2v, ctx)

          val iBox = br.asInstanceOf[BoxFloat]
          iBox.value = result
        }

        def doDouble(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxDouble].value
          val op2v = b2.asInstanceOf[BoxDouble].value

          val result = PrimOpHelpers.doubleBinOp(op, op1v, op2v, ctx)

          val iBox = br.asInstanceOf[BoxDouble]
          iBox.value = result
        }

        def doScalar(scalarTy: Type, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          scalarTy match {
            case TypeInt(l)   => doInt(l, b1, b2, br)
            case TypeFloat()  => doFloat(b1, b2, br)
            case TypeDouble() => doDouble(b1, b2, br)
            case _            => throw new UvmRuntimeException(ctx + "BinOp not suitable for type %s".format(opndTy))
          }
        }

        try {
          opndTy match {
            case TypeVector(scalarTy, sz) => {
              val op1Bs = boxOf(op1).asInstanceOf[BoxSeq].values
              val op2Bs = boxOf(op2).asInstanceOf[BoxSeq].values
              val rBs = resultBox(0).asInstanceOf[BoxSeq].values

              for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
                doScalar(scalarTy, b1, b2, br)
              }
            }
            case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), resultBox(0))
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
        def doInt(l: Int, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxInt].value
          val op2v = b2.asInstanceOf[BoxInt].value

          val result = PrimOpHelpers.intCmp(op, l, op1v, op2v, ctx)
          writeBooleanResult(result, br)
        }

        def doFloat(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxFloat].value
          val op2v = b2.asInstanceOf[BoxFloat].value

          val result = PrimOpHelpers.floatCmp(op, op1v, op2v, ctx)
          writeBooleanResult(result, br)
        }

        def doDouble(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxDouble].value
          val op2v = b2.asInstanceOf[BoxDouble].value

          val result = PrimOpHelpers.doubleCmp(op, op1v, op2v, ctx)
          writeBooleanResult(result, br)
        }

        def doRef(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxRef].objRef
          val op2v = b2.asInstanceOf[BoxRef].objRef

          val result = op match {
            case CmpOptr.EQ => op1v == op2v
            case CmpOptr.NE => op1v != op2v
            case _          => throw new UvmRuntimeException(ctx + "Comparison %s not suitable for reference type %s".format(op, opndTy))
          }
          writeBooleanResult(result, br)
        }

        def doIRef(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxIRef].oo
          val op2v = b2.asInstanceOf[BoxIRef].oo

          val result = op match {
            case CmpOptr.EQ => op1v == op2v
            case CmpOptr.NE => op1v != op2v
            case _          => throw new UvmRuntimeException(ctx + "Comparison %s not suitable for internal reference type %s".format(op, opndTy))
          }
          writeBooleanResult(result, br)
        }

        def doFunc(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxFunc].func
          val op2v = b2.asInstanceOf[BoxFunc].func

          val result = op match {
            case CmpOptr.EQ => op1v == op2v
            case CmpOptr.NE => op1v != op2v
            case _          => throw new UvmRuntimeException(ctx + "Comparison %s not suitable for function type %s".format(op, opndTy))
          }
          writeBooleanResult(result, br)
        }

        def doStack(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxStack].stack
          val op2v = b2.asInstanceOf[BoxStack].stack

          val result = op match {
            case CmpOptr.EQ => op1v == op2v
            case CmpOptr.NE => op1v != op2v
            case _          => throw new UvmRuntimeException(ctx + "Comparison %s not suitable for stack type %s".format(op, opndTy))
          }
          writeBooleanResult(result, br)
        }

        def doScalar(scalarTy: Type, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          scalarTy match {
            case TypeInt(l)     => doInt(l, b1, b2, br)
            case TypeFloat()    => doFloat(b1, b2, br)
            case TypeDouble()   => doDouble(b1, b2, br)
            case TypeRef(_)     => doRef(b1, b2, br)
            case TypeIRef(_)    => doIRef(b1, b2, br)
            case TypeFuncRef(_) => doFunc(b1, b2, br)
            case TypeStackRef() => doStack(b1, b2, br)
            case _              => throw new UvmRuntimeException(ctx + "Comparison not suitable for type %s".format(opndTy))
          }
        }

        opndTy match {
          case TypeVector(scalarTy, sz) => {
            val op1Bs = boxOf(op1).asInstanceOf[BoxSeq].values
            val op2Bs = boxOf(op2).asInstanceOf[BoxSeq].values
            val rBs = resultBox(0).asInstanceOf[BoxSeq].values

            for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
              doScalar(scalarTy, b1, b2, br)
            }
          }
          case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), resultBox(0))
        }

        continueNormally()
      }

      case i @ InstConv(op, fromTy, toTy, opnd) => {
        def doScalar(scalarFromTy: Type, scalarToTy: Type, bOpnd: ValueBox, br: ValueBox): Unit = {
          def iToI(): Unit = (scalarFromTy, scalarToTy) match {
            case (TypeInt(fl), TypeInt(tl)) => {
              val od = bOpnd.asInstanceOf[BoxInt].value
              val result = op match {
                case ConvOptr.TRUNC => OpHelper.trunc(od, tl)
                case ConvOptr.ZEXT  => OpHelper.zext(od, fl, tl)
                case ConvOptr.SEXT  => OpHelper.sext(od, fl, tl)
              }
              br.asInstanceOf[BoxInt].value = result
            }
            case _ => throw new UvmRuntimeException(ctx + "Expect integer source and dest type. Found %s and %s".format(scalarFromTy, scalarToTy))
          }

          def fpToI(signed: Boolean): Unit = {
            val tl = scalarToTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer dest type. Found %s".format(scalarToTy))
            }
            val result = scalarFromTy match {
              case TypeFloat()  => OpHelper.floatToI(bOpnd.asInstanceOf[BoxFloat].value, tl, signed)
              case TypeDouble() => OpHelper.doubleToI(bOpnd.asInstanceOf[BoxDouble].value, tl, signed)
              case _            => throw new UvmRuntimeException(ctx + "Expect FP source type. Found %s.".format(scalarFromTy))
            }
            br.asInstanceOf[BoxInt].value = result
          }

          def iToFP(signed: Boolean): Unit = {
            val fl = scalarFromTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer source type. Found %s".format(scalarFromTy))
            }
            val od = bOpnd.asInstanceOf[BoxInt].value
            val extended = if (signed) OpHelper.prepareSigned(od, fl) else OpHelper.prepareUnsigned(od, fl)
            scalarToTy match {
              case TypeFloat() => {
                val result = extended.toFloat
                br.asInstanceOf[BoxFloat].value = result
              }
              case TypeDouble() => {
                val result = extended.toDouble
                br.asInstanceOf[BoxDouble].value = result
              }
              case _ => throw new UvmRuntimeException(ctx + "Expect FP dest type. Found %s.".format(scalarToTy))
            }
          }

          def bitcast(): Unit = (scalarFromTy, scalarToTy) match {
            case (TypeInt(32), TypeFloat()) => {
              val result = java.lang.Float.intBitsToFloat(bOpnd.asInstanceOf[BoxInt].value.intValue)
              br.asInstanceOf[BoxFloat].value = result
            }
            case (TypeInt(64), TypeDouble()) => {
              val result = java.lang.Double.longBitsToDouble(bOpnd.asInstanceOf[BoxInt].value.longValue)
              br.asInstanceOf[BoxDouble].value = result
            }
            case (TypeFloat(), TypeInt(32)) => {
              val result = java.lang.Float.floatToRawIntBits(bOpnd.asInstanceOf[BoxFloat].value)
              br.asInstanceOf[BoxInt].value = result
            }
            case (TypeDouble(), TypeInt(64)) => {
              val result = java.lang.Double.doubleToRawLongBits(bOpnd.asInstanceOf[BoxDouble].value)
              br.asInstanceOf[BoxInt].value = result
            }
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
              case TypeInt(n)             => bOpnd.asInstanceOf[BoxInt].value.longValue // truncates
              case _: AbstractPointerType => bOpnd.asInstanceOf[BoxPointer].addr
            }
            scalarToTy match {
              case TypeInt(n)             => br.asInstanceOf[BoxInt].value = OpHelper.trunc(BigInt(srcAddr), Math.min(n, 64))
              case _: AbstractPointerType => br.asInstanceOf[BoxPointer].addr = srcAddr
            }
          }

          op match {
            case ConvOptr.TRUNC => iToI()
            case ConvOptr.ZEXT  => iToI()
            case ConvOptr.SEXT  => iToI()
            case ConvOptr.FPTRUNC => {
              val od = bOpnd.asInstanceOf[BoxDouble].value
              val result = od.toFloat
              br.asInstanceOf[BoxFloat].value = result
            }
            case ConvOptr.FPEXT => {
              val od = bOpnd.asInstanceOf[BoxFloat].value
              val result = od.toDouble
              br.asInstanceOf[BoxDouble].value = result
            }
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

            val bOpnds = boxOf(opnd).asInstanceOf[BoxSeq].values
            val rBs = resultBox(0).asInstanceOf[BoxSeq].values

            for ((bOpnd, br) <- (bOpnds zip rBs)) {
              doScalar(scalarFromTy, scalarToTy, bOpnd, br)
            }
          }
          case _ => doScalar(fromTy, toTy, boxOf(opnd), resultBox(0))
        }

        incPC()
      }

      case i @ InstSelect(condTy, opndTy, cond, ifTrue, ifFalse) => {
        def doScalar(bCond: ValueBox, bTrue: ValueBox, bFalse: ValueBox, br: ValueBox): Unit = {
          val c = bCond.asInstanceOf[BoxInt].value

          if (c == 1) {
            br.copyFrom(bTrue)
          } else {
            br.copyFrom(bFalse)
          }
        }

        condTy match {
          case TypeVector(TypeInt(1), sz) => {
            val bConds = boxOf(cond).asInstanceOf[BoxSeq].values
            val bTrues = boxOf(ifTrue).asInstanceOf[BoxSeq].values
            val bFalses = boxOf(ifFalse).asInstanceOf[BoxSeq].values
            val bResults = resultBox(0).asInstanceOf[BoxSeq].values

            for ((((bCond, bTrue), bFalse), br) <- bConds.zip(bTrues).zip(bFalses).zip(bResults)) {
              doScalar(bCond, bTrue, bFalse, br)
            }
          }
          case TypeInt(1) => {
            doScalar(boxOf(cond), boxOf(ifTrue), boxOf(ifFalse), resultBox(0))
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
        val calleeFunc = boxOf(callee).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val argBoxes = argList.map(boxOf)

        val shouldIncrementPC = curStack.callMu(calleeFunc, argBoxes)
        if (shouldIncrementPC) {
          // Impossible because the callee is always fresh. Added here for consistency.
          continueNormally()
        }
      }

      case i @ InstTailCall(sig, callee, argList) => {
        val calleeFunc = boxOf(callee).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val argBoxes = argList.map(boxOf)

        val shouldIncrementPC = curStack.tailCallMu(calleeFunc, argBoxes)
        if (shouldIncrementPC) {
          // Impossible because the callee is always fresh. Added here for consistency.
          continueNormally()
        }
      }

      case i @ InstRet(funcVer, retVals) => {
        val rvbs = retVals.map(boxOf)
        val shouldIncrementPC = curStack.retFromMu(rvbs)
        if (shouldIncrementPC) {
          // This should always be executed, unless Mu is extended again so it is possible to push
          // a frame on a beginning frame.
          continueNormally()
        }
      }

      case i @ InstThrow(excVal) => {
        val exc = boxOf(excVal).asInstanceOf[BoxRef].objRef
        curStack.popFrame()
        catchException(exc)
      }

      case i @ InstExtractValue(strTy, index, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxSeq]
        val fb = ob.values(index)
        val ib = resultBox(0)
        ib.copyFrom(fb)
        continueNormally()
      }

      case i @ InstInsertValue(strTy, index, opnd, newVal) => {
        val ob = boxOf(opnd).asInstanceOf[BoxSeq]
        val nvb = boxOf(newVal)
        val ib = resultBox(0).asInstanceOf[BoxSeq]
        for (((ofb, ifb), ind) <- (ob.values zip ib.values).zipWithIndex) {
          if (ind == index) {
            ifb.copyFrom(nvb)
          } else {
            ifb.copyFrom(ofb)
          }
        }
        continueNormally()
      }

      case i @ InstExtractElement(vecTy, indTy, opnd, index) => {
        val ob = boxOf(opnd).asInstanceOf[BoxSeq]
        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareUnsigned(indb.value, indTy.length)

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        val eb = ob.values(ind.intValue())
        val ib = resultBox(0)
        ib.copyFrom(eb)
        continueNormally()
      }

      case i @ InstInsertElement(vecTy, indTy, opnd, index, newVal) => {
        val ob = boxOf(opnd).asInstanceOf[BoxSeq]

        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareUnsigned(indb.value, indTy.length)

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        val indInt = ind.intValue

        val nvb = boxOf(newVal)
        val ib = resultBox(0).asInstanceOf[BoxSeq]

        for (((oeb, ieb), ind2) <- (ob.values zip ib.values).zipWithIndex) {
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
        val vb1 = boxOf(vec1).asInstanceOf[BoxSeq]
        val vb2 = boxOf(vec2).asInstanceOf[BoxSeq]
        val mb = boxOf(mask).asInstanceOf[BoxSeq]
        val ib = resultBox(0).asInstanceOf[BoxSeq]

        for (((meb, ieb), ind) <- (mb.values zip ib.values).zipWithIndex) {
          val me = OpHelper.prepareUnsigned(meb.asInstanceOf[BoxInt].value, maskIntLen)
          if (me < vecLen) {
            ieb.copyFrom(vb1.values(me.intValue))
          } else if (vecLen <= me && me < vecLen * 2) {
            ieb.copyFrom(vb2.values(me.intValue - vecLen))
          } else {
            throw new UvmRuntimeException(ctx + "Index %d as the %d-th element of mask is out of range. Vector type: %s".format(me, ind, vecTy))
          }
        }
        continueNormally()
      }

      case i @ InstNew(allocTy, excClause) => {
        handleOutOfMemory(excClause) {
          val addr = mutator.newScalar(allocTy)
          val ib = resultBox(0).asInstanceOf[BoxRef]
          ib.objRef = addr
          continueNormally()
        }
      }

      case i @ InstNewHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val lb = boxOf(length).asInstanceOf[BoxInt]
          val len = OpHelper.prepareUnsigned(lb.value, lenTy.length)
          val addr = mutator.newHybrid(allocTy, len.longValue)
          val ib = resultBox(0).asInstanceOf[BoxRef]
          ib.objRef = addr
          continueNormally()
        }
      }

      case i @ InstAlloca(allocTy, excClause) => {
        handleOutOfMemory(excClause) {
          val addr = mutator.allocaScalar(curStack.stackMemory, allocTy)
          val ib = resultBox(0).asInstanceOf[BoxIRef]
          ib.objRef = 0L
          ib.offset = addr
          continueNormally()
        }
      }

      case i @ InstAllocaHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val lb = boxOf(length).asInstanceOf[BoxInt]
          val len = OpHelper.prepareUnsigned(lb.value, lenTy.length)
          val addr = mutator.allocaHybrid(curStack.stackMemory, allocTy, len.longValue)
          val ib = resultBox(0).asInstanceOf[BoxIRef]
          ib.objRef = 0L
          ib.offset = addr
          continueNormally()
        }
      }

      case i @ InstGetIRef(referentTy, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxRef]
        val ib = resultBox(0).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = 0L
        continueNormally()
      }

      case i @ InstGetFieldIRef(ptr, referentTy, index, opnd) => {
        val addrIncr = TypeSizes.fieldOffsetOf(referentTy, index)

        incrementBoxIRefOrPointer(ptr, opnd, i.results(0), addrIncr)
        continueNormally()
      }

      case i @ InstGetElemIRef(ptr, referentTy, indTy, opnd, index) => {
        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareSigned(indb.value, indTy.length)
        val addrIncr = TypeSizes.elemOffsetOf(referentTy, ind.longValue())

        incrementBoxIRefOrPointer(ptr, opnd, i.results(0), addrIncr)
        continueNormally()
      }

      case i @ InstShiftIRef(ptr, referentTy, offTy, opnd, offset) => {
        val offb = boxOf(offset).asInstanceOf[BoxInt]
        val off = OpHelper.prepareSigned(offb.value, offTy.length)
        val addrIncr = TypeSizes.shiftOffsetOf(referentTy, off.longValue())

        incrementBoxIRefOrPointer(ptr, opnd, i.results(0), addrIncr)
        continueNormally()
      }

      case i @ InstGetVarPartIRef(ptr, referentTy, opnd) => {
        val addrIncr = TypeSizes.varPartOffsetOf(referentTy)

        incrementBoxIRefOrPointer(ptr, opnd, i.results(0), addrIncr)
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
          writeBooleanResult(succ, bs)
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

        val addr = boxOf(callee).asInstanceOf[BoxPointer].addr

        val argBoxes = argList.map(boxOf)
        val retBox = resultBox(0)

        val shouldIncrementPC = curStack.callNative(sig, addr, argBoxes)
        if (shouldIncrementPC) {
          continueNormally()
        }
      }

      case i @ InstNewThread(stack, newStackAction, excClause) => {
        val newStack = boxOf(stack).asInstanceOf[BoxStack].stack.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to bind to a NULL stack.")
        }
        
        val newThread = newStackAction match {
          case PassValues(argTys, args) => {
            val argBoxes = args.map(boxOf)
            microVM.threadStackManager.newThread(newStack, HowToResume.PassValues(argBoxes))
          }
          case ThrowExc(exc) => {
            val excBox = boxOf(exc)
            val excAddr = excBox.asInstanceOf[BoxRef].objRef
            microVM.threadStackManager.newThread(newStack, HowToResume.ThrowExc(excAddr))
          }
        }
        resultBox(0).asInstanceOf[BoxThread].thread = Some(newThread)
        
        continueNormally()
      }

      case i @ InstSwapStack(swappee, curStackAction, newStackAction, excClause, keepAlives) => {
        val oldStack = curStack
        val newStack = boxOf(swappee).asInstanceOf[BoxStack].stack.getOrElse {
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
            val excBox = boxOf(exc)
            handleOldStack()
            rebindThrowExc(newStack, excBox)
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