package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.refimpl._
import uvm.refimpl.mem.TypeSizes.Word

abstract class ValueBox

abstract class HasObjRef extends ValueBox {
  def hasObjRef(): Boolean
  def getObjRef(): Word
  def setObjRef(newObjRef: Word): Unit
}

abstract class ObjectBox[T](obj: Option[T]) extends ValueBox

case class BoxInt(var value: BigInt) extends ValueBox
case class BoxFloat(var value: Float) extends ValueBox
case class BoxDouble(var value: Double) extends ValueBox
case class BoxVector(var values: Seq[ValueBox]) extends ValueBox
case class BoxRef(var objRef: Word) extends HasObjRef {
  def hasObjRef() = true
  def getObjRef() = objRef
  def setObjRef(newObjRef: Word): Unit = { objRef = newObjRef }
}
case class BoxIRef(var objRef: Word, var offset: Word) extends HasObjRef {
  def hasObjRef() = objRef != 0
  def getObjRef() = objRef
  def setObjRef(newObjRef: Word): Unit = { objRef = newObjRef }
}
case class BoxStruct(var values: Seq[ValueBox]) extends ValueBox
case class BoxVoid() extends ValueBox
case class BoxFunc(var func: Option[Function]) extends ObjectBox[Function](func)
case class BoxThread(var thread: Option[InterpreterThread]) extends ObjectBox[InterpreterThread](thread)
case class BoxStack(var stack: Option[InterpreterStack]) extends ObjectBox[InterpreterStack](stack)
case class BoxTagRef64(var raw: Long) extends HasObjRef {
  def hasObjRef() = OpHelper.tr64IsRef(raw)
  def getObjRef() = OpHelper.tr64ToRef(raw)
  def setObjRef(newObjRef: Word) = {
    val oldTag = OpHelper.tr64ToTag(raw)
    raw = OpHelper.refToTr64(newObjRef, oldTag)
  }
}

object ValueBox {

  def makeBoxForType(ty: Type): ValueBox = ty match {
    case _: TypeInt => BoxInt(0)
    case _: TypeFloat => BoxFloat(0.0f)
    case _: TypeDouble => BoxDouble(0.0d)
    case TypeVector(elemTy, len) => BoxVector(Seq.fill(4)(makeBoxForType(elemTy)))
    case _: TypeRef => BoxRef(0L)
    case _: TypeIRef => BoxIRef(0L, 0L)
    case _: TypeWeakRef => throw new UvmRefImplException("weakref cannot be an SSA variable type")
    case TypeStruct(fieldTys) => BoxStruct(fieldTys.map(makeBoxForType))
    case _: TypeArray => throw new UvmRefImplException("array cannot be an SSA variable type")
    case _: TypeHybrid => throw new UvmRefImplException("hybrid cannot be an SSA variable type")
    case _: TypeVoid => BoxVoid()
    case _: TypeFunc => BoxFunc(None)
    case _: TypeStack => BoxStack(None)
    case _: TypeThread => BoxThread(None)
    case _: TypeTagRef64 => BoxTagRef64(0L)
  }

}