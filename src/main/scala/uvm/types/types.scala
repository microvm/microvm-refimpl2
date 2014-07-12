package uvm.types

import uvm._

abstract class Type extends IdentifiedSettable {
  override final def toString: String = Type.prettyPrint(this)
}

case class TypeInt(var length: Int) extends Type
case class TypeFloat() extends Type
case class TypeDouble() extends Type
case class TypeRef(var ty: Type) extends Type
case class TypeIRef(var ty: Type) extends Type
case class TypeWeakRef(var ty: Type) extends Type
case class TypeStruct(var fieldTy: Seq[Type]) extends Type
case class TypeArray(var elemTy: Type, var len: Long) extends Type
case class TypeHybrid(var fixedPart: Type, var varPart: Type) extends Type
case class TypeVoid extends Type
case class TypeFunc(var sig: FuncSig) extends Type
case class TypeThread() extends Type
case class TypeStack() extends Type
case class TypeTagRef64 extends Type

object Type {
  def prettyPrint(ty: Type): String = ty match {
    case TypeInt(length) => "int<%d>".format(length)
    case TypeFloat() => "float"
    case TypeDouble() => "double"
    case TypeRef(ty) => "ref<%s>".format(ty.repr)
    case TypeIRef(ty) => "iref<%s>".format(ty.repr)
    case TypeWeakRef(ty) => "weakref<%s>".format(ty.repr)
    case TypeStruct(fieldTy) => "struct<%s>".format(fieldTy.map(_.repr).mkString(" "))
    case TypeArray(elemTy, len) => "array<%s %d>".format(elemTy.repr, len)
    case TypeHybrid(fixedPart, varPart) => "hybrid<%s %s>".format(fixedPart.repr, varPart.repr)
    case TypeVoid() => "void"
    case TypeFunc(sig) => "func<%s>".format(FuncSig.prettyPrint(sig))
    case TypeThread() => "thread"
    case TypeStack() => "stack"
    case TypeTagRef64() => "tagref64"
    case _ => "unknown type " + ty.getClass.getName
  }
}

object CommonTypes {
  val I1 = TypeInt(1)
  val I8 = TypeInt(8)
  val I16 = TypeInt(16)
  val I32 = TypeInt(32)
  val I64 = TypeInt(64)
  val IWORD = TypeInt(64)
  val FLOAT = TypeFloat()
  val DOUBLE = TypeDouble()
  val VOID = TypeVoid()
  val REFVOID = TypeRef(VOID)
  val THREAD = TypeThread()
  val STACK = TypeStack()
}