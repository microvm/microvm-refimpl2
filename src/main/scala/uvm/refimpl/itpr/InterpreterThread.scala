package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.comminsts._
import uvm.refimpl._
import uvm.refimpl.mem._
import TypeSizes.Word
import scala.annotation.tailrec
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object InterpreterThread {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class InterpreterThread(val id: Int, microVM: MicroVM, initialStack: InterpreterStack, val mutator: Mutator) {
  import InterpreterThread._

  var stack: Option[InterpreterStack] = Some(initialStack)

  var isRunning: Boolean = true

  def step(): Unit = {
    interpretCurrentInstruction()
  }

  def threadExit(): Unit = {
    curStack.state = StackState.Dead
    isRunning = false
  }

  def curStack = stack.get
  def top = curStack.top
  def curBB = top.curBB
  def curInst = top.curInst

  def incPC(): Unit = top.incPC()
  def jump(bb: BasicBlock, ix: Int): Unit = top.jump(bb, ix)

  def boxOf(v: SSAVariable): ValueBox = v match {
    case g: GlobalVariable => microVM.constantPool.getGlobalVarBox(g)
    case l: LocalVariable  => top.boxes(l)
  }

  def ctx = stack match {
    case None => "(Thred not bound to stack): "
    case Some(_) => {
      val ix = top.curInstIndex
      if (ix >= curBB.insts.size) {
        "FuncVer %s, BasicBlock %s, Instruction exceeds the basic block (error)".format(top.funcVer.repr, curBB.repr)
      } else {
        "FuncVer %s, BasicBlock %s, Instruction %s (%s): ".format(top.funcVer.repr, curBB.repr, curInst.repr, curInst.getClass.getName)
      }
    }
  }

  private def interpretCurrentInstruction(): Unit = try {
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
              val op1Bs = boxOf(op1).asInstanceOf[BoxVector].values
              val op2Bs = boxOf(op2).asInstanceOf[BoxVector].values
              val rBs = boxOf(i).asInstanceOf[BoxVector].values

              for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
                doScalar(scalarTy, b1, b2, br)
              }
            }
            case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), boxOf(i))
          }
          continueNormally()
        } catch {
          case e: UvmDivisionByZeroException => excClause match {
            case None => throw e
            case Some(ec) => {
              branchAndMovePC(ec.exc)
            }
          }
        }
      }

      case i @ InstCmp(op, opndTy, op1, op2) => {
        def writeBooleanResult(result: Boolean, br: ValueBox): Unit = {
          br.asInstanceOf[BoxInt].value = if (result) 1 else 0
        }

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

        def doScalar(scalarTy: Type, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          scalarTy match {
            case TypeInt(l)   => doInt(l, b1, b2, br)
            case TypeFloat()  => doFloat(b1, b2, br)
            case TypeDouble() => doDouble(b1, b2, br)
            case _            => throw new UvmRuntimeException(ctx + "Comparison not suitable for type %s".format(opndTy))
          }
        }

        opndTy match {
          case TypeVector(scalarTy, sz) => {
            val op1Bs = boxOf(op1).asInstanceOf[BoxVector].values
            val op2Bs = boxOf(op2).asInstanceOf[BoxVector].values
            val rBs = boxOf(i).asInstanceOf[BoxVector].values

            for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
              doScalar(scalarTy, b1, b2, br)
            }
          }
          case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), boxOf(i))
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
            case (TypeFunc(_), TypeFunc(_))   => br.copyFrom(bOpnd)
            case (TypeThread(), TypeThread()) => br.copyFrom(bOpnd)
            case (TypeStack(), TypeStack())   => br.copyFrom(bOpnd)
            case _ => throw new UvmRuntimeException(ctx +
              "REFCAST can only convert between two types both of which are func, thread or stack. Found %s and %s.".format(scalarFromTy, scalarToTy))
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
          }
        }

        (fromTy, toTy) match {
          case (TypeVector(scalarFromTy, sz), TypeVector(scalarToTy, sz2)) => {
            if (sz != sz2) throw new UvmRefImplException(ctx + "The source and dest vector types must have the same length")

            val bOpnds = boxOf(opnd).asInstanceOf[BoxVector].values
            val rBs = boxOf(i).asInstanceOf[BoxVector].values

            for ((bOpnd, br) <- (bOpnds zip rBs)) {
              doScalar(scalarFromTy, scalarToTy, bOpnd, br)
            }
          }
          case _ => doScalar(fromTy, toTy, boxOf(opnd), boxOf(i))
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
            val bConds = boxOf(cond).asInstanceOf[BoxVector].values
            val bTrues = boxOf(ifTrue).asInstanceOf[BoxVector].values
            val bFalses = boxOf(ifFalse).asInstanceOf[BoxVector].values
            val bResults = boxOf(i).asInstanceOf[BoxVector].values

            for ((((bCond, bTrue), bFalse), br) <- bConds.zip(bTrues).zip(bFalses).zip(bResults)) {
              doScalar(bCond, bTrue, bFalse, br)
            }
          }
          case TypeInt(1) => {
            doScalar(boxOf(cond), boxOf(ifTrue), boxOf(ifFalse), boxOf(i))
          }
          case _ => throw new UvmRefImplException(ctx + "Condition must be either int<1> or a vector of int<1>. Found %s".format(condTy))
        }

        continueNormally()
      }

      case i @ InstBranch(dest) => {
        branchAndMovePC(dest)
      }

      case i @ InstBranch2(cond, ifTrue, ifFalse) => {
        val cv = boxOf(cond).asInstanceOf[BoxInt].value
        val dest = if (cv == 1) ifTrue else ifFalse
        branchAndMovePC(dest)
      }

      case i @ InstSwitch(opndTy, opnd, defDest, cases) => {
        opndTy match {
          case TypeInt(l) => {
            val ov = boxOf(opnd).asInstanceOf[BoxInt].value
            val dest = cases.find(pair => boxOf(pair._1).asInstanceOf[BoxInt].value == ov).map(_._2).getOrElse(defDest)
            branchAndMovePC(dest)
          }
          case _ => throw new UvmRefImplException(ctx + "Operand type must be integer. %s found.".format(opndTy))
        }
      }

      case i @ InstPhi(_, _) => throw new UvmRefImplException(ctx + "PHI instructions reached in normal execution, " +
        "but PHI must only appear in the beginning of basic blocks and not in the entry block.")

      case i @ InstCall(sig, callee, argList, excClause, keepAlives) => {
        val calleeFunc = boxOf(callee).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val funcVer = getFuncDefOrTriggerCallback(calleeFunc)

        val argBoxes = argList.map(boxOf)

        curStack.pushFrame(funcVer, argBoxes)
      }

      case i @ InstTailCall(sig, callee, argList) => {
        val calleeFunc = boxOf(callee).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Callee must not be NULL")
        }

        val funcVer = getFuncDefOrTriggerCallback(calleeFunc)

        val argBoxes = argList.map(boxOf)

        curStack.replaceTop(funcVer, argBoxes)
      }

      case i @ InstRet(retTy, retVal) => {
        val rvb = boxOf(retVal)
        curStack.popFrame()
        val newCurInst = curInst // in the parent frame of the RET
        boxOf(newCurInst).copyFrom(rvb)
        continueNormally()
      }

      case i @ InstRetVoid() => {
        curStack.popFrame()
        continueNormally()
      }

      case i @ InstThrow(excVal) => {
        val exc = boxOf(excVal).asInstanceOf[BoxRef].objRef
        curStack.popFrame()
        catchException(exc)
      }

      case i @ InstLandingPad() => throw new UvmRefImplException(ctx + "LANDINGPAD instructions reached in normal execution, " +
        "but LANDINGPAD must only appear in the beginning of basic blocks and not in the entry block.")

      case i @ InstExtractValue(strTy, index, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxStruct]
        val fb = ob.values(index)
        val ib = boxOf(i)
        ib.copyFrom(fb)
        continueNormally()
      }

      case i @ InstInsertValue(strTy, index, opnd, newVal) => {
        val ob = boxOf(opnd).asInstanceOf[BoxStruct]
        val nvb = boxOf(newVal)
        val ib = boxOf(i).asInstanceOf[BoxStruct]
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
        val ob = boxOf(opnd).asInstanceOf[BoxVector]
        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareUnsigned(indb.value, indTy.length)

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        val eb = ob.values(ind.intValue())
        val ib = boxOf(i)
        ib.copyFrom(eb)
        continueNormally()
      }

      case i @ InstInsertElement(vecTy, indTy, opnd, index, newVal) => {
        val ob = boxOf(opnd).asInstanceOf[BoxVector]

        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareUnsigned(indb.value, indTy.length)

        if (ind > vecTy.len) {
          throw new UvmRuntimeException(ctx + "Index %d out of range. Vector type: %s".format(ind, vecTy))
        }

        val indInt = ind.intValue

        val nvb = boxOf(newVal)
        val ib = boxOf(i).asInstanceOf[BoxVector]

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
        val vb1 = boxOf(vec1).asInstanceOf[BoxVector]
        val vb2 = boxOf(vec2).asInstanceOf[BoxVector]
        val mb = boxOf(mask).asInstanceOf[BoxVector]
        val ib = boxOf(i).asInstanceOf[BoxVector]

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
          val ib = boxOf(i).asInstanceOf[BoxRef]
          ib.objRef = addr
          continueNormally()
        }
      }

      case i @ InstNewHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val lb = boxOf(length).asInstanceOf[BoxInt]
          val len = OpHelper.prepareUnsigned(lb.value, lenTy.length)
          val addr = mutator.newHybrid(allocTy, len.longValue)
          val ib = boxOf(i).asInstanceOf[BoxRef]
          ib.objRef = addr
          continueNormally()
        }
      }

      case i @ InstAlloca(allocTy, excClause) => {
        handleOutOfMemory(excClause) {
          val addr = mutator.allocaScalar(curStack.stackMemory, allocTy)
          val ib = boxOf(i).asInstanceOf[BoxIRef]
          ib.objRef = addr
          ib.offset = 0L
          continueNormally()
        }
      }

      case i @ InstAllocaHybrid(allocTy, lenTy, length, excClause) => {
        handleOutOfMemory(excClause) {
          val lb = boxOf(length).asInstanceOf[BoxInt]
          val len = OpHelper.prepareUnsigned(lb.value, lenTy.length)
          val addr = mutator.allocaHybrid(curStack.stackMemory, allocTy, len.longValue)
          val ib = boxOf(i).asInstanceOf[BoxIRef]
          ib.objRef = addr
          ib.offset = 0L
          continueNormally()
        }
      }

      case i @ InstGetIRef(referentTy, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxRef]
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = 0L
        continueNormally()
      }

      case i @ InstGetFieldIRef(referentTy, index, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxIRef]
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = ob.offset + TypeSizes.fieldOffsetOf(referentTy, index)
        continueNormally()
      }

      case i @ InstGetElemIRef(referentTy, indTy, opnd, index) => {
        val ob = boxOf(opnd).asInstanceOf[BoxIRef]
        val indb = boxOf(index).asInstanceOf[BoxInt]
        val ind = OpHelper.prepareSigned(indb.value, indTy.length)
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = ob.offset + TypeSizes.elemOffsetOf(referentTy, ind.longValue())
        continueNormally()
      }

      case i @ InstShiftIRef(referentTy, offTy, opnd, offset) => {
        val ob = boxOf(opnd).asInstanceOf[BoxIRef]
        val offb = boxOf(offset).asInstanceOf[BoxInt]
        val off = OpHelper.prepareSigned(offb.value, offTy.length)
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = ob.offset + TypeSizes.shiftOffsetOf(referentTy, off.longValue())
        continueNormally()
      }

      case i @ InstGetFixedPartIRef(referentTy, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxIRef]
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = ob.offset
        continueNormally()
      }

      case i @ InstGetVarPartIRef(referentTy, opnd) => {
        val ob = boxOf(opnd).asInstanceOf[BoxIRef]
        val ib = boxOf(i).asInstanceOf[BoxIRef]
        ib.objRef = ob.objRef
        ib.offset = ob.offset + TypeSizes.varPartOffsetOf(referentTy)
        continueNormally()
      }

      case i @ InstLoad(ord, referentTy, loc, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val lb = boxOf(loc).asInstanceOf[BoxIRef]
        val ib = boxOf(i)

        val la = lb.objRef + lb.offset
        if (la == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.load(uty, la, ib, microVM)
          continueNormally()
        }
      }

      case i @ InstStore(ord, referentTy, loc, newVal, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val lb = boxOf(loc).asInstanceOf[BoxIRef]
        val nvb = boxOf(newVal)
        val ib = boxOf(i)

        val la = lb.objRef + lb.offset
        if (la == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.store(uty, la, nvb, ib, microVM)
          continueNormally()
        }
      }

      case i @ InstCmpXchg(weak, ordSucc, ordFail, referentTy, loc, expected, desired, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val lb = boxOf(loc).asInstanceOf[BoxIRef]
        val eb = boxOf(expected)
        val db = boxOf(desired)
        val ib = boxOf(i)

        val la = lb.objRef + lb.offset
        if (la == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.cmpXchg(uty, la, eb, db, ib, microVM)
          continueNormally()
        }
      }

      case i @ InstAtomicRMW(ord, op, referentTy, loc, opnd, excClause) => {
        val uty = InternalTypePool.unmarkedOf(referentTy)
        val lb = boxOf(loc).asInstanceOf[BoxIRef]
        val ob = boxOf(opnd)
        val ib = boxOf(i)

        val la = lb.objRef + lb.offset
        if (la == 0L) {
          nullRefError(excClause)
        } else {
          MemoryOperations.atomicRMW(uty, op, la, ob, ib, microVM)
          continueNormally()
        }
      }

      case i @ InstFence(ord) => {
        // No-op in this interpreter
        continueNormally()
      }

      case i @ InstTrap(retTy, excClause, keepAlives) => {
        val ca = microVM.newClientAgent()

        val hThread = ca.putThread(Some(this))
        val hStack = ca.putStack(Some(curStack))

        unbind(retTy)

        val res = microVM.trapManager.trapHandler.handleTrap(ca, hThread, hStack, 0)

        res match {
          case TrapExit() => {
            isRunning = false
          }
          case TrapRebindPassValue(newStack, value) => {
            rebindPassValue(newStack.vb.asInstanceOf[BoxStack].stack, value.vb)
          }
          case TrapRebindPassVoid(newStack) => {
            rebindPassVoid(newStack.vb.asInstanceOf[BoxStack].stack)
          }
          case TrapRebindThrowExc(newStack, exc) => {
            rebindThrowExc(newStack.vb.asInstanceOf[BoxStack].stack, exc.vb)
          }
        }

        ca.close()
      }

      case i @ InstWatchPoint(wpID, retTy, dis, ena, exc, keepAlives) => {
        val isEnabled = microVM.trapManager.isWatchPointEnabled(wpID)

        if (isEnabled) {
          val ca = microVM.newClientAgent()

          val hThread = ca.putThread(Some(this))
          val hStack = ca.putStack(Some(curStack))

          unbind(retTy)

          val res = microVM.trapManager.trapHandler.handleTrap(ca, hThread, hStack, wpID)

          res match {
            case TrapExit() => {
              isRunning = false
            }
            case TrapRebindPassValue(newStack, value) => {
              rebindPassValue(newStack.vb.asInstanceOf[BoxStack].stack, value.vb)
            }
            case TrapRebindPassVoid(newStack) => {
              rebindPassVoid(newStack.vb.asInstanceOf[BoxStack].stack)
            }
            case TrapRebindThrowExc(newStack, exc) => {
              rebindThrowExc(newStack.vb.asInstanceOf[BoxStack].stack, exc.vb)
            }
          }

          ca.close()
        } else {
          branchAndMovePC(dis)
        }
      }
      
      case i @ InstNewStack(sig, callee, argList, excClause) => {
        val calleeFunc = boxOf(callee).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Stack-bottom function must not be NULL")
        }

        val funcVer = getFuncDefOrTriggerCallback(calleeFunc)

        val argBoxes = argList.map(boxOf)
        
        val ib = boxOf(i).asInstanceOf[BoxStack]

        handleOutOfMemory(excClause) {
          val sta = microVM.threadStackManager.newStack(funcVer, argBoxes, mutator)
          ib.stack = Some(sta)
          continueNormally()
        }
      }
      
      case i @ InstSwapStack(swappee, curStackAction, newStackAction, excClause, keepAlives) => {
        
      }
      
      // Indentation guide: Insert more instructions (after TRAP) here.

      case i @ InstCommInst(ci, typeList, argList, excClause, keepAlives) => {
        def theCI(name: String): CommInst = CommInsts(name)
        ci.name.get match {
          case "@uvm.thread_exit" => {
            threadExit()
          }

          // Insert more CommInsts here.

          case ciName => {
            throw new UvmRefImplException("Unimplemented common instruction %s".format(ciName))
          }

        }
      }

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

  def branchAndMovePC(dest: BasicBlock, excAddr: Word = 0L): Unit = {
    val curBB = this.curBB
    var cont = true
    var i = 0
    while (cont) {
      dest.insts(i) match {
        case phi @ InstPhi(opndTy, cases) => {
          val caseVal = cases.find(_._1 == curBB).map(_._2).getOrElse {
            throw new UvmRuntimeException(s"Phi node ${phi.repr} does not include the case for source basic block ${curBB.repr}")
          }
          val vb = boxOf(caseVal)
          val db = boxOf(phi)
          db.copyFrom(vb)
          i += 1
        }
        case lp: InstLandingPad => {
          val db = boxOf(lp).asInstanceOf[BoxRef]
          db.objRef = excAddr
          i += 1
        }
        case _ => cont = false
      }
    }
    jump(dest, i)
  }

  def continueNormally(): Unit = {
    curInst match {
      case wp: InstWatchPoint => {
        branchAndMovePC(wp.ena)
        // NOTE: WatchPoint only "continue normally" when the current stack is rebound with value or void.
        // This includes executing a watch point. In any case, this watch point must have been enabled. If the watch
        // point is disabled during the course the stack is unbound, this watch point should still continue from the
        // destination determined WHEN THIS INSTRUCTION IS EXECUTED.
      }
      case h: HasExcClause => h.excClause match {
        case None => incPC()
        case Some(ec) => {
          branchAndMovePC(ec.nor)
        }
      }
      case _ => incPC()
    }
  }

  def unbind(readyType: Type): Unit = {
    curStack.state = StackState.Ready(readyType)
    stack = None
  }

  def rebind(newStack: Option[InterpreterStack]): Unit = {
    if (newStack == None) throw new UvmRuntimeException(ctx + "Rebinding to NULL stack. This does not make sense.")
    stack = newStack
  }

  def rebindPassValue(newStack: Option[InterpreterStack], value: ValueBox): Unit = {
    rebind(newStack)
    try {
      boxOf(curInst).copyFrom(value)
    } catch {
      case e: Exception => {
        throw new UvmRuntimeException(ctx + "Error during rebinding while assigning the value passed to a stack " +
          "to the instruction waiting for rebinding. This is usually caused by the mismatching between the type of " +
          "READY<T> and the actual value type. The passed value box is a %s.".format(value.getClass.getName), e)
      }
    }

    continueNormally()
  }

  def rebindPassVoid(newStack: Option[InterpreterStack]): Unit = {
    rebind(newStack)

    continueNormally()
  }

  def rebindThrowExc(newStack: Option[InterpreterStack], exc: ValueBox): Unit = {
    rebind(newStack)

    val excObjRef = exc.asInstanceOf[BoxRef].objRef

    catchException(excObjRef)
  }

  /**
   * Attempt to catch exception in the current frame. Will repeatedly unwind the stack until the exception can be
   * handled. Stack underflow is an undefined behaviour.
   */
  def catchException(exc: Word): Unit = {
    @tailrec
    def unwindUntilCatchable(f: InterpreterFrame): (InterpreterFrame, BasicBlock) = {
      maybeFindExceptionHandler(f.curInst) match {
        case Some(bb) => (f, bb)
        case None => f.prev match {
          case None       => throw new UvmRuntimeException(ctx + "Exception is thrown out of the bottom frame.")
          case Some(prev) => unwindUntilCatchable(prev)
        }
      }
    }

    val s = curStack
    val f = s.top
    val (newFrame, newBB) = unwindUntilCatchable(f)
    s.top = newFrame

    branchAndMovePC(newBB, exc)
  }

  /**
   * Test if the current frame with i as the current instruction can catch an exception that unwinds the stack.
   *
   * @return Return Some(h) if i can catch the exception and h is the basic block for the exception. Return None if i
   * cannot catch exceptions.
   *
   * @throw Throw UvmRefimplException if a frame stops at an unexpected instruction. Normally the top frame can be
   * executing TRAP, WATCHPOINT, SWAPSTACK or CALL and all other frames must be executing CALL.
   */
  def maybeFindExceptionHandler(inst: Instruction): Option[BasicBlock] = {
    inst match {
      case i: InstCall       => i.excClause.map(_.exc)
      case i: InstTrap       => i.excClause.map(_.exc)
      case i: InstWatchPoint => i.exc
      case i: InstSwapStack  => i.excClause.map(_.exc)
      case _ => {
        throw new UvmRefImplException(ctx + "Instruction %s (%s) is in a stack frame when an exception is thrown.".format(inst.repr, inst.getClass.getName))
      }
    }
  }

  @tailrec
  private def getFuncDefOrTriggerCallback(f: Function): FuncVer = {
    f.versions.headOption match {
      case Some(v) => v
      case None =>
        logger.debug(ctx + "Function %s is undefined. Trigger undefined function event.".format(f.repr))
        microVM.trapManager.undefinedFunctionHandler.handleUndefinedFunction(f.id)
        getFuncDefOrTriggerCallback(f)
    }
  }

  private def branchToExcDestOr(excClause: Option[ExcClause])(f: => Unit): Unit = {
    excClause match {
      case None                      => f
      case Some(ExcClause(_, excBB)) => branchAndMovePC(excBB, 0L)
    }
  }

  private def handleOutOfMemory(excClause: Option[ExcClause])(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e: UvmOutOfMemoryException => {
        branchToExcDestOr(excClause) {
          throw new UvmRuntimeException(ctx + "Out of memory and there is no handler.", e)
        }
      }
    }
  }

  private def nullRefError(excClause: Option[ExcClause]): Unit = {
    branchToExcDestOr(excClause) {
      throw new UvmRuntimeException(ctx + "Accessing null reference.")
    }
  }
}
