package uvm.types

import uvm._

abstract class Type extends IdentifiedSettable {
  override final def toString: String = Type.prettyPrint(this)
}

abstract class FPType extends Type

abstract class AbstractRefType extends Type {
  def ty: Type
}

abstract class AbstractStructType extends Type {
  def fieldTys: Seq[Type]
}

abstract class AbstractSeqType extends Type {
  def elemTy: Type
  def len: Long
}

abstract class AbstractPointerType extends Type

case class TypeInt(var length: Int) extends Type
case class TypeFloat() extends FPType
case class TypeDouble() extends FPType
case class TypeRef(var ty: Type) extends AbstractRefType
case class TypeIRef(var ty: Type) extends AbstractRefType
case class TypeWeakRef(var ty: Type) extends AbstractRefType
case class TypeStruct(var fieldTys: Seq[Type]) extends Type
case class TypeArray(var elemTy: Type, var len: Long) extends AbstractSeqType
case class TypeHybrid(var fieldTys: Seq[Type], var varTy: Type) extends Type
case class TypeVoid() extends Type
case class TypeFuncRef(var sig: FuncSig) extends Type
case class TypeThreadRef() extends Type
case class TypeStackRef() extends Type
case class TypeTagRef64() extends Type
case class TypeVector(var elemTy: Type, var len: Long) extends AbstractSeqType
case class TypeUPtr(var ty: Type) extends AbstractPointerType
case class TypeUFuncPtr(var sig: FuncSig) extends AbstractPointerType

object Type {
  def prettyPrint(ty: Type): String = ty match {
    case TypeInt(length)                => "int<%d>".format(length)
    case TypeFloat()                    => "float"
    case TypeDouble()                   => "double"
    case TypeRef(ty)                    => "ref<%s>".format(ty.repr)
    case TypeIRef(ty)                   => "iref<%s>".format(ty.repr)
    case TypeWeakRef(ty)                => "weakref<%s>".format(ty.repr)
    case TypeStruct(fieldTy)            => "struct<%s>".format(fieldTy.map(_.repr).mkString(" "))
    case TypeArray(elemTy, len)         => "array<%s %d>".format(elemTy.repr, len)
    case TypeHybrid(fixedPart, varPart) => "hybrid<%s %s>".format(fixedPart.repr, varPart.repr)
    case TypeVoid()                     => "void"
    case TypeFuncRef(sig)               => "funcref<%s>".format(sig.repr)
    case TypeThreadRef()                => "threadref"
    case TypeStackRef()                 => "stackref"
    case TypeTagRef64()                 => "tagref64"
    case TypeVector(elemTy, len)        => "vector<%s %d>".format(elemTy.repr, len)
    case TypeUPtr(ty)                   => "uptr<%s>".format(ty.repr)
    case TypeUFuncPtr(sig)              => "ufuncptr<%s>".format(sig.repr)
    case _                              => "unknown type " + ty.getClass.getName
  }
}
