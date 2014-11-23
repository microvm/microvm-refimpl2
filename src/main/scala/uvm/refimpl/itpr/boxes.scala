package uvm.refimpl.itpr

import uvm._
import uvm.refimpl.mem.TypeSizes.Word

abstract class ValueBox

abstract class ObjectBox[T](obj: Option[T]) extends ValueBox

case class BoxInt(value: BigInt) extends ValueBox
case class BoxFloat(value: Float) extends ValueBox
case class BoxDouble(value: Double) extends ValueBox
case class BoxVector(values: Seq[ValueBox]) extends ValueBox
case class BoxRef(addr: Word) extends ValueBox
case class BoxIRef(base: Word, offset: Word) extends ValueBox
case class BoxStruct(values: Seq[ValueBox]) extends ValueBox
case class BoxVoid() extends ValueBox
case class BoxFunc(func: Option[Function]) extends ObjectBox[Function](func)
case class BoxThread(thread: Option[InterpreterThread]) extends ObjectBox[InterpreterThread](thread)
case class BoxStack(stack: Option[InterpreterStack]) extends ObjectBox[InterpreterStack](stack)
case class BoxTagRef64(raw: Long) extends ValueBox {

}

