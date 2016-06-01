package uvm.types

import uvm._

abstract class Type extends IdentifiedSettable {
  override final def toString: String = Type.prettyPrint(this)
}

abstract class FPType extends Type

abstract class AbstractGenRefType extends Type

abstract class AbstractRefType extends AbstractGenRefType {
  def ty: Type
}

abstract class AbstractObjRefType extends AbstractRefType

abstract class AbstractCompositeType extends Type

abstract class AbstractStructType extends AbstractCompositeType {
  def fieldTys: Seq[Type]
}

abstract class AbstractSeqType extends AbstractCompositeType {
  def elemTy: Type
  def len: Long
}

abstract class AbstractPointerType extends Type

case class TypeInt(var length: Int) extends Type
case class TypeFloat() extends FPType
case class TypeDouble() extends FPType
case class TypeRef(var ty: Type) extends AbstractObjRefType
case class TypeIRef(var ty: Type) extends AbstractRefType
case class TypeWeakRef(var ty: Type) extends AbstractObjRefType
case class TypeStruct(var fieldTys: Seq[Type]) extends AbstractStructType
case class TypeArray(var elemTy: Type, var len: Long) extends AbstractSeqType
case class TypeHybrid(var fieldTys: Seq[Type], var varTy: Type) extends AbstractStructType
case class TypeVoid() extends Type
case class TypeFuncRef(var sig: FuncSig) extends AbstractGenRefType
case class TypeThreadRef() extends AbstractGenRefType
case class TypeStackRef() extends AbstractGenRefType
case class TypeTagRef64() extends Type
case class TypeVector(var elemTy: Type, var len: Long) extends AbstractSeqType
case class TypeUPtr(var ty: Type) extends AbstractPointerType
case class TypeUFuncPtr(var sig: FuncSig) extends AbstractPointerType
case class TypeFrameCursorRef() extends AbstractGenRefType
case class TypeIRNodeRef() extends AbstractGenRefType

object Type {
  def prettyPrint(ty: Type): String = ty match {
    case TypeInt(length)               => "int<%d>".format(length)
    case TypeFloat()                   => "float"
    case TypeDouble()                  => "double"
    case TypeRef(ty)                   => "ref<%s>".format(ty.repr)
    case TypeIRef(ty)                  => "iref<%s>".format(ty.repr)
    case TypeWeakRef(ty)               => "weakref<%s>".format(ty.repr)
    case TypeStruct(fieldTys)          => "struct<%s>".format(fieldTys.map(_.repr).mkString(" "))
    case TypeArray(elemTy, len)        => "array<%s %d>".format(elemTy.repr, len)
    case TypeHybrid(fieldTys, varPart) => "hybrid<%s %s>".format(fieldTys.map(_.repr).mkString(" "), varPart.repr)
    case TypeVoid()                    => "void"
    case TypeFuncRef(sig)              => "funcref<%s>".format(sig.repr)
    case TypeThreadRef()               => "threadref"
    case TypeStackRef()                => "stackref"
    case TypeTagRef64()                => "tagref64"
    case TypeVector(elemTy, len)       => "vector<%s %d>".format(elemTy.repr, len)
    case TypeUPtr(ty)                  => "uptr<%s>".format(ty.repr)
    case TypeUFuncPtr(sig)             => "ufuncptr<%s>".format(sig.repr)
    case TypeFrameCursorRef()          => "framecursorref"
    case TypeIRNodeRef()               => "irnoderef"
    case _                             => "unknown type " + ty.getClass.getName
  }
}
