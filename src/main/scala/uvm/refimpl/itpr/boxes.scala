package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.refimpl._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ir.irbuilder.IRNode

abstract class ValueBox {
  def copyFrom(other: ValueBox): Unit
}

abstract class HasObjRef extends ValueBox {
  def hasObjRef(): Boolean
  def getObjRef(): Word
  def setObjRef(newObjRef: Word): Unit
}

abstract class ObjectBox[T] extends ValueBox {
  def obj: Option[T]
  def obj_=(o: Option[T]): Unit

  def copyFrom(other: ValueBox): Unit = { this.obj = other.asInstanceOf[ObjectBox[T]].obj }
}

case class BoxInt(var value: BigInt) extends ValueBox {
  def copyFrom(other: ValueBox): Unit = { this.value = other.asInstanceOf[BoxInt].value }
}
case class BoxFloat(var value: Float) extends ValueBox {
  def copyFrom(other: ValueBox): Unit = { this.value = other.asInstanceOf[BoxFloat].value }
}
case class BoxDouble(var value: Double) extends ValueBox {
  def copyFrom(other: ValueBox): Unit = { this.value = other.asInstanceOf[BoxDouble].value }
}
case class BoxSeq(var values: Seq[ValueBox]) extends ValueBox {
  def copyFrom(other: ValueBox): Unit = { for ((t, o) <- this.values.zip(other.asInstanceOf[BoxSeq].values)) t.copyFrom(o) }
}
case class BoxRef(var objRef: Word) extends HasObjRef {
  def copyFrom(other: ValueBox): Unit = { this.objRef = other.asInstanceOf[BoxRef].objRef }
  def hasObjRef() = true
  def getObjRef() = objRef
  def setObjRef(newObjRef: Word): Unit = { objRef = newObjRef }
}
case class BoxIRef(var objRef: Word, var offset: Word) extends HasObjRef {
  def copyFrom(other: ValueBox): Unit = {
    val that = other.asInstanceOf[BoxIRef]
    this.objRef = that.objRef; this.offset = that.offset
  }
  def hasObjRef() = objRef != 0
  def getObjRef() = objRef
  def setObjRef(newObjRef: Word): Unit = { objRef = newObjRef }
  
  // Helper to get and set the objRef and offset at the same time
  def oo: (Word, Word) = (objRef, offset)
  def oo_=(newVal: (Word, Word)): Unit = { objRef = newVal._1; offset = newVal._2 }
}
case class BoxFunc(var func: Option[Function]) extends ObjectBox[Function] {
  def obj = func
  def obj_=(other: Option[Function]): Unit = { func = other }
}
case class BoxThread(var thread: Option[InterpreterThread]) extends ObjectBox[InterpreterThread] {
  def obj = thread
  def obj_=(other: Option[InterpreterThread]): Unit = { thread = other }
}
case class BoxStack(var stack: Option[InterpreterStack]) extends ObjectBox[InterpreterStack] {
  def obj = stack
  def obj_=(other: Option[InterpreterStack]): Unit = { stack = other }
}
case class BoxFrameCursor(var cursor: Option[FrameCursor]) extends ObjectBox[FrameCursor] {
  def obj = cursor
  def obj_=(other: Option[FrameCursor]): Unit = { cursor = other }
}
case class BoxIRNode(var node: Option[IRNode]) extends ObjectBox[IRNode] {
  def obj = node
  def obj_=(other: Option[IRNode]): Unit = { node = other }
}
case class BoxTagRef64(var raw: Long) extends HasObjRef {
  def copyFrom(other: ValueBox): Unit = { this.raw = other.asInstanceOf[BoxTagRef64].raw }
  def hasObjRef() = OpHelper.tr64IsRef(raw)
  def getObjRef() = OpHelper.tr64ToRef(raw)
  def setObjRef(newObjRef: Word) = {
    val oldTag = OpHelper.tr64ToTag(raw)
    raw = OpHelper.refToTr64(newObjRef, oldTag)
  }
}
case class BoxPointer(var addr: Word) extends ValueBox {
  def copyFrom(other: ValueBox): Unit = {
    val that = other.asInstanceOf[BoxPointer]
    this.addr = that.addr
  }
}

object ValueBox {

  def makeBoxForType(ty: Type): ValueBox = ty match {
    case _: TypeInt => BoxInt(0)
    case _: TypeFloat => BoxFloat(0.0f)
    case _: TypeDouble => BoxDouble(0.0d)
    case TypeVector(elemTy, len) => BoxSeq(Seq.fill(len.toInt)(makeBoxForType(elemTy)))
    case _: TypeRef => BoxRef(0L)
    case _: TypeIRef => BoxIRef(0L, 0L)
    case _: TypeWeakRef => throw new UvmRefImplException("weakref cannot be an SSA variable type")
    case TypeStruct(fieldTys) => BoxSeq(fieldTys.map(makeBoxForType))
    case TypeArray(elemTy, len) => BoxSeq(Seq.fill(len.toInt)(makeBoxForType(elemTy)))
    case _: TypeHybrid => throw new UvmRefImplException("hybrid cannot be an SSA variable type")
    case _: TypeVoid => throw new UvmRefImplException("void cannot be an SSA variable type")
    case _: TypeFuncRef => BoxFunc(None)
    case _: TypeStackRef => BoxStack(None)
    case _: TypeThreadRef => BoxThread(None)
    case _: TypeFrameCursorRef => BoxFrameCursor(None)
    case _: TypeTagRef64 => BoxTagRef64(0L)
    case _: TypeUPtr => BoxPointer(0L)
    case _: TypeUFuncPtr => BoxPointer(0L)
  }

}