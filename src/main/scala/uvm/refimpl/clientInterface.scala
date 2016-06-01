package uvm.refimpl

import uvm._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes._
import uvm.ssavariables._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.MemoryOrder._
import uvm.types._

object MuValue {
  def apply(ty: Type, vb: ValueBox): MuValue = (ty, vb) match {
    case (t: TypeInt, v: BoxInt)                    => MuIntValue(t, v)
    case (t: TypeFloat, v: BoxFloat)                => MuFloatValue(t, v)
    case (t: TypeDouble, v: BoxDouble)              => MuDoubleValue(t, v)
    case (t: TypeRef, v: BoxRef)                    => MuRefValue(t, v)
    case (t: TypeIRef, v: BoxIRef)                  => MuIRefValue(t, v)
    case (t: TypeStruct, v: BoxSeq)                 => MuStructValue(t, v)
    case (t: TypeArray, v: BoxSeq)                  => MuArrayValue(t, v)
    case (t: TypeVector, v: BoxSeq)                 => MuVectorValue(t, v)
    case (t: TypeFuncRef, v: BoxFunc)               => MuFuncRefValue(t, v)
    case (t: TypeThreadRef, v: BoxThread)           => MuThreadRefValue(t, v)
    case (t: TypeStackRef, v: BoxStack)             => MuStackRefValue(t, v)
    case (t: TypeTagRef64, v: BoxTagRef64)          => MuTagRef64Value(t, v)
    case (t: TypeUPtr, v: BoxPointer)               => MuUPtrValue(t, v)
    case (t: TypeUFuncPtr, v: BoxPointer)           => MuUFPValue(t, v)
    case (t: TypeFrameCursorRef, v: BoxFrameCursor) => MuFCRefValue(t, v)
    case (t, v) => {
      throw new IllegalArgumentException("Improper type-box pair: %s,%s".format(t.toString, vb.getClass.getSimpleName))
    }
  }
}

/**
 * A handle to a Mu value, held by a MuCtx. In the Scala API, Handles are immutable and cannot be copied.
 * Only use the handle in the MuCtx it is defined.
 */
abstract class MuValue {
  def ty: Type
  def vb: ValueBox

  def showTy: String = "%s: %s".format(this.getClass.getSimpleName, ty)
}

abstract class MuSeqValue extends MuValue {
  def ty: AbstractSeqType
  def vb: BoxSeq
}

abstract class MuGenRefValue extends MuValue {
  def ty: AbstractGenRefType
  def isNull: Boolean
}

abstract class MuSpecialEntityRefValue extends MuGenRefValue {
  def vb: ObjectBox[_]
  def isNull: Boolean = vb.obj.isEmpty
}

case class MuIntValue(ty: TypeInt, vb: BoxInt) extends MuValue
case class MuFloatValue(ty: TypeFloat, vb: BoxFloat) extends MuValue
case class MuDoubleValue(ty: TypeDouble, vb: BoxDouble) extends MuValue
case class MuRefValue(ty: TypeRef, vb: BoxRef) extends MuGenRefValue {
  override def isNull: Boolean = vb.objRef == 0L
}
case class MuIRefValue(ty: TypeIRef, vb: BoxIRef) extends MuGenRefValue {
  override def isNull: Boolean = vb.objRef + vb.offset == 0L
}
case class MuStructValue(ty: TypeStruct, vb: BoxSeq) extends MuValue
case class MuArrayValue(ty: TypeArray, vb: BoxSeq) extends MuSeqValue
case class MuVectorValue(ty: TypeVector, vb: BoxSeq) extends MuSeqValue
case class MuFuncRefValue(ty: TypeFuncRef, vb: BoxFunc) extends MuSpecialEntityRefValue
case class MuThreadRefValue(ty: TypeThreadRef, vb: BoxThread) extends MuSpecialEntityRefValue
case class MuStackRefValue(ty: TypeStackRef, vb: BoxStack) extends MuSpecialEntityRefValue
case class MuTagRef64Value(ty: TypeTagRef64, vb: BoxTagRef64) extends MuValue
case class MuUPtrValue(ty: TypeUPtr, vb: BoxPointer) extends MuValue
case class MuUFPValue(ty: TypeUFuncPtr, vb: BoxPointer) extends MuValue
case class MuFCRefValue(ty: TypeFrameCursorRef, vb: BoxFrameCursor) extends MuSpecialEntityRefValue

abstract class MuIRNode extends MuSpecialEntityRefValue {
  def ty: TypeIRNodeRef
  def vb: BoxIRNode
  def node: IRNode = vb.node.get
}

case class MuBundleNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuIRNode {
  def bundle: TrantientBundle = node.asInstanceOf[BundleNode].bundle
}

abstract class MuChildNode extends MuIRNode {
  type T <: Identified
  def obj: T = node.asInstanceOf[ChildNode[T]].obj
}

abstract class MuVarNode extends MuChildNode { override type T <: SSAVariable }
abstract class MuGlobalVarNode extends MuVarNode { override type T <: GlobalVariable }
abstract class MuLocalVarNode extends MuVarNode { override type T <: LocalVariable }

case class MuTypeNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuChildNode
case class MuFuncSigNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuChildNode
case class MuConstNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuGlobalVarNode
case class MuGlobalNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuGlobalVarNode
case class MuFuncNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuGlobalVarNode
case class MuExpFuncNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuGlobalVarNode
case class MuFuncVerNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuIRNode
case class MuBBNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuIRNode
case class MuNorParamNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuLocalVarNode
case class MuExcParamNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuLocalVarNode
case class MuInstResNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuLocalVarNode
case class MuInstNode(ty: TypeIRNodeRef, vb: BoxIRNode) extends MuIRNode

abstract class TrapHandlerResult
object TrapHandlerResult {
  case class ThreadExit() extends TrapHandlerResult
  case class Rebind(newStack: MuStackRefValue, htr: HowToResume) extends TrapHandlerResult
}

abstract class HowToResume
object HowToResume {
  case class PassValues(values: Seq[MuValue]) extends HowToResume
  case class ThrowExc(exc: MuRefValue) extends HowToResume
}

trait TrapHandler {
  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult
}

trait UndefinedFunctionHandler {
  def handleUndefinedFunction(functionID: Int): Unit
}
