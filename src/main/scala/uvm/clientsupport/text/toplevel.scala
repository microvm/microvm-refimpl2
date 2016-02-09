package uvm.clientsupport.text

sealed abstract class Const(str: => String) {
  def ty: TypeName
  override def toString = str
}
object Const {
  case class Null(ty: TypeName) extends Const("NULL")
  case class Int(ty: TypeName, value: scala.Long) extends Const(value.toString)
  case class Float(ty: TypeName, value: scala.Float)
    extends Const(s"bitsf(0x${Integer.toHexString(java.lang.Float.floatToRawIntBits(value))})")
  case class Double(ty: TypeName, value: scala.Double)
    extends Const(s"bitsd(0x${java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))})")
  case class Pointer(ty: TypeName, addr: scala.Long) extends Const(addr.toString)
  case class List(ty: TypeName, fields: IList[GlobalVarName]) extends Const(s"{$fields}")
}

case class FuncSig(paramTy: IList[TypeName], retTy: IList[TypeName]) {
  override def toString = s"($paramTy) -> ($retTy)"
}

case class PreFuncVer(
  func: GlobalVarName,
  sig: FuncSigName,
  params: IList[LocalVarName],
  bbs: IList[PreBasicBlock]
) {
  override lazy val toString = "(" + params + ") {\n" + (bbs mkString "\n") + "\n}"
}

case class PostFuncVer(func: GlobalVarName, sig: FuncSigName, bbs: IList[PostBasicBlock]) {
  override lazy val toString = "{\n" + (bbs mkString "\n") + "\n}"
}

case class Expose(func: GlobalVarName, callConv: Flag, cookie: GlobalVarName) {
  override def toString = s"$func $callConv $cookie"
}

trait Context {
  def resolve(typeName: TypeName): Option[TypeCtor]
  def resolve(funcSigName: FuncSigName): Option[FuncSig]
  def typeDef(ctor: TypeCtor): TypeName
  def funcSig(args: IList[TypeName], ret: IList[TypeName]): FuncSigName
  def constDef(c: Const): GlobalVarName
}