package uvm.clientsupport.text

sealed abstract class TypeCtor(str: => String) {
  override lazy val toString: String = str
}
object TypeCtor {
  def Void = Singletons.Void; type Void = Singletons.Void.type
  case class Int(bits: scala.Int) extends TypeCtor(s"int<$bits>")
  def Float = Singletons.Float; type Float = Singletons.Float.type
  def Double = Singletons.Double; type Double = Singletons.Double.type
  def ThreadRef = Singletons.ThreadRef; type ThreadRef = Singletons.ThreadRef.type
  def StackRef = Singletons.StackRef; type StackRef = Singletons.StackRef.type
  def FrameCursorRef = Singletons.FrameCursorRef; type FrameCursorRef = Singletons.FrameCursorRef.type
  case class Ref(ty: TypeCtor) extends TypeCtor(s"ref<$ty>")
  case class IRef(ty: TypeCtor) extends TypeCtor(s"iref<$ty>")
  case class WeakRef(ty: TypeCtor) extends TypeCtor(s"weakref<$ty>")
  case class FuncRef(sig: FuncSigName) extends TypeCtor(s"funcref<$sig>")
  case class UPtr(ty: TypeCtor) extends TypeCtor(s"uptr<$ty>")
  case class UFuncPtr(sig: FuncSigName) extends TypeCtor(s"ufuncptr<$sig>")
  def TagRef64 = Singletons.TagRef64; type TagRef64 = Singletons.TagRef64.type
  case class Array(ty: TypeCtor, len: scala.Long) extends TypeCtor(s"array<$ty $len>")
  case class Vector(ty: TypeCtor, len: scala.Int) extends TypeCtor(s"vector<$ty $len>")
  case class Hybrid(fixedTy: TypeCtor, varTy: TypeCtor) extends TypeCtor(s"hybrid<$fixedTy $varTy>")
  case class Struct(types: IList[TypeCtor]) extends TypeCtor(s"struct<${types mkString " "}>")

  object Singletons {
    case object Void extends TypeCtor("void")
    case object Float extends TypeCtor("float")
    case object Double extends TypeCtor("double")
    case object ThreadRef extends TypeCtor("threadref")
    case object StackRef extends TypeCtor("stackref")
    case object FrameCursorRef extends TypeCtor("framecursorref")
    case object TagRef64 extends TypeCtor("tagref64")
  }
}

sealed trait MuName {
  def name: String
}
sealed trait GlobalName extends MuName {
  override lazy val toString = "@" + name
}
sealed trait LocalName extends MuName {
  override lazy val toString = "%" + name
}
sealed trait VarName extends MuName
case class TypeName(name: String) extends TypeCtor("@" + name) with GlobalName
case class FuncSigName(name: String) extends GlobalName
case class FuncVerName(name: String) extends GlobalName
case class GlobalVarName(name: String) extends GlobalName with VarName
case class LocalVarName(name: String) extends LocalName with VarName
case class LabelName(name: String) extends LocalName

final class Flag(cName: String) {
  val name = cName.toUpperCase.replaceAll("[^A-Z_]", "")
  override def toString = "#" + name
}
