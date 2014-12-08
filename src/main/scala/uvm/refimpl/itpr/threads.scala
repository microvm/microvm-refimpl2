package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.mem._
import TypeSizes.Word

class InterpreterThread(val id: Int, microVM: MicroVM, initialStack: InterpreterStack, val mutator: Mutator) {
  var stack: Option[InterpreterStack] = Some(initialStack)

  var isRunning: Boolean = true

  def step(): Unit = {
    interpretCurrentInstruction()
  }

  def sta = stack.get
  def top = sta.top
  def curBB = top.curBB
  def inst = top.curInst

  def incPC(): Unit = top.incPC()
  def jump(bb: BasicBlock, ix: Int): Unit = top.jump(bb, ix)

  def branchAndMovePC(dest: BasicBlock, excAddr: Word = 0L): Unit = {
    val curBB = this.curBB
    var cont = true
    var i = 0
    while (cont) {
      dest.insts(i) match {
        case phi @ InstPhi(opndTy, cases) => {
          val caseVal = cases.find({ case (bb, _) => bb == dest }).map(_._2).getOrElse {
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

  def continueNormally(excClause: Option[ExcClause]): Unit = {
    excClause match {
      case None => incPC()
      case Some(ec) => {
        branchAndMovePC(ec.nor)
      }
    }
  }

  def boxOf(v: SSAVariable): ValueBox = v match {
    case g: GlobalVariable => microVM.constantPool.getGlobalVarBox(g)
    case l: LocalVariable  => top.boxes(l)
  }

  def ctx = "FuncVer %s, BasicBlock %s, Instruction %s (%s): ".format(top.funcVer, curBB, inst)

  private def interpretCurrentInstruction(): Unit = {
    inst match {
      case i @ InstBinOp(op, opndTy, op1, op2, excClause) => {
        def doInt(l: Int, b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxInt].value
          val op2v = b2.asInstanceOf[BoxInt].value

          val result = PrimOpHelpers.intBinOp(op, l, op1v, op2v, ctx)

          val iBox = boxOf(i).asInstanceOf[BoxInt]
          iBox.value = result
        }

        def doFloat(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxFloat].value
          val op2v = b2.asInstanceOf[BoxFloat].value

          val result = PrimOpHelpers.floatBinOp(op, op1v, op2v, ctx)

          val iBox = boxOf(i).asInstanceOf[BoxFloat]
          iBox.value = result
        }

        def doDouble(b1: ValueBox, b2: ValueBox, br: ValueBox): Unit = {
          val op1v = b1.asInstanceOf[BoxDouble].value
          val op2v = b2.asInstanceOf[BoxDouble].value

          val result = PrimOpHelpers.doubleBinOp(op, op1v, op2v, ctx)

          val iBox = boxOf(i).asInstanceOf[BoxDouble]
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
              val op2Bs = boxOf(op1).asInstanceOf[BoxVector].values
              val rBs = boxOf(i).asInstanceOf[BoxVector].values

              for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
                doScalar(scalarTy, b1, b2, br)
              }
            }
            case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), boxOf(i))
          }
          continueNormally(excClause)
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
            val op2Bs = boxOf(op1).asInstanceOf[BoxVector].values
            val rBs = boxOf(i).asInstanceOf[BoxVector].values

            for (((b1, b2), br) <- ((op1Bs zip op2Bs) zip rBs)) {
              doScalar(scalarTy, b1, b2, br)
            }
          }
          case scalarTy => doScalar(scalarTy, boxOf(op1), boxOf(op2), boxOf(i))
        }

        incPC()
      }

      case i @ InstConv(op, fromTy, toTy, opnd) => {
        def doScalar(bOpnd: ValueBox, br: ValueBox): Unit = {
          def iToI(): Unit = (fromTy, toTy) match {
            case (TypeInt(fl), TypeInt(tl)) => {
              val od = bOpnd.asInstanceOf[BoxInt].value
              val result = op match {
                case ConvOptr.TRUNC => OpHelper.trunc(od, tl)
                case ConvOptr.ZEXT  => OpHelper.zext(od, fl, tl)
                case ConvOptr.SEXT  => OpHelper.sext(od, fl, tl)
              }
              br.asInstanceOf[BoxInt].value = result
            }
            case _ => throw new UvmRuntimeException(ctx + "Expect integer source and dest type. Found %s and %s".format(fromTy, toTy))
          }

          def fpToI(signed: Boolean): Unit = {
            val tl = toTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer dest type. Found %s".format(toTy))
            }
            val result = fromTy match {
              case TypeFloat()  => OpHelper.floatToI(bOpnd.asInstanceOf[BoxFloat].value, tl, signed)
              case TypeDouble() => OpHelper.doubleToI(bOpnd.asInstanceOf[BoxDouble].value, tl, signed)
              case _            => throw new UvmRuntimeException(ctx + "Expect FP source type. Found %s.".format(fromTy))
            }
            br.asInstanceOf[BoxInt].value = result
          }

          def iToFP(signed: Boolean): Unit = {
            val fl = fromTy match {
              case TypeInt(l) => l
              case _          => throw new UvmRuntimeException(ctx + "Expect integer source type. Found %s".format(fromTy))
            }
            val od = bOpnd.asInstanceOf[BoxInt].value
            val extended = if (signed) OpHelper.prepareSigned(od, fl) else OpHelper.prepareUnsigned(od, fl)
            toTy match {
              case TypeFloat() => {
                val result = extended.toFloat
                br.asInstanceOf[BoxFloat].value = result
              }
              case TypeDouble() => {
                val result = extended.toDouble
                br.asInstanceOf[BoxDouble].value = result
              }
              case _ => throw new UvmRuntimeException(ctx + "Expect FP dest type. Found %s.".format(toTy))
            }
          }

          def bitcast(): Unit = (fromTy, toTy) match {
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
              "BITCAST can only convert between int and FP types of the same size. Found %s and %s.".format(fromTy, toTy))
          }

          def refcast(): Unit = (fromTy, toTy) match {
            case (TypeFunc(_), TypeFunc(_))   => br.copyFrom(bOpnd)
            case (TypeThread(), TypeThread()) => br.copyFrom(bOpnd)
            case (TypeStack(), TypeStack())   => br.copyFrom(bOpnd)
            case _ => throw new UvmRuntimeException(ctx +
              "REFCAST can only convert between two types both of which are func, thread or stack. Found %s and %s.".format(fromTy, toTy))
          }

          op match {
            case ConvOptr.TRUNC => iToI()
            case ConvOptr.ZEXT  => iToI()
            case ConvOptr.SEXT  => iToI()
            case ConvOptr.FPTRUNC => {
              val od = opnd.asInstanceOf[BoxDouble].value
              val result = od.toFloat
              boxOf(i).asInstanceOf[BoxFloat].value = result
            }
            case ConvOptr.FPEXT => {
              val od = opnd.asInstanceOf[BoxFloat].value
              val result = od.toDouble
              boxOf(i).asInstanceOf[BoxDouble].value = result
            }
            case ConvOptr.FPTOUI  => fpToI(signed = false)
            case ConvOptr.FPTOSI  => fpToI(signed = true)
            case ConvOptr.UITOFP  => iToFP(signed = false)
            case ConvOptr.SITOFP  => iToFP(signed = true)
            case ConvOptr.BITCAST => bitcast()
            case ConvOptr.REFCAST => refcast()
          }
        }
      }

      // Indentation guide: Insert more instructions here.
    }
  }
}
