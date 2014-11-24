package uvm.refimpl.itpr

import uvm._
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

