package uvm.ir.textinput

import scala.util.parsing.combinator.RegexParsers

object UvmIRParser extends RegexParsers {
  import UvmIRAST._

  implicit class ParserExtension[T](that: Parser[T]) {
    def ~<[U](q: => Parser[U]): Parser[T] = {
      lazy val p = q // lazy argument
      (for (a <- that; b <- p) yield a).named("~<")
    }
  }

  implicit class ParserMap2[T1, T2](that: Parser[T1 ~ T2]) {
    def ^^^^[U](f: (T1, T2) => U): Parser[U] = that ^^ { case t1 ~ t2 => f(t1, t2) }
  }

  implicit class ParserMap3[T1, T2, T3](that: Parser[T1 ~ T2 ~ T3]) {
    def ^^^^[U](f: (T1, T2, T3) => U): Parser[U] = that ^^ { case t1 ~ t2 ~ t3 => f(t1, t2, t3) }
  }
  implicit class ParserMap4[T1, T2, T3, T4](that: Parser[T1 ~ T2 ~ T3 ~ T4]) {
    def ^^^^[U](f: (T1, T2, T3, T4) => U): Parser[U] = that ^^ { case t1 ~ t2 ~ t3 ~ t4 => f(t1, t2, t3, t4) }
  }
  implicit class ParserMap5[T1, T2, T3, T4, T5](that: Parser[T1 ~ T2 ~ T3 ~ T4 ~ T5]) {
    def ^^^^[U](f: (T1, T2, T3, T4, T5) => U): Parser[U] = that ^^ { case t1 ~ t2 ~ t3 ~ t4 ~ t5 => f(t1, t2, t3, t4, t5) }
  }
  implicit class ParserMap6[T1, T2, T3, T4, T5, T6](that: Parser[T1 ~ T2 ~ T3 ~ T4 ~ T5 ~ T6]) {
    def ^^^^[U](f: (T1, T2, T3, T4, T5, T6) => U): Parser[U] = that ^^ { case t1 ~ t2 ~ t3 ~ t4 ~ t5 ~ t6 => f(t1, t2, t3, t4, t5, t6) }
  }

  def ang[T](q: => Parser[T]): Parser[T] = { lazy val p = q; "<" ~> p <~ ">" }

  def paren[T](q: => Parser[T]): Parser[T] = { lazy val p = q; "(" ~> p <~ ")" }

  def brace[T](q: => Parser[T]): Parser[T] = { lazy val p = q; "{" ~> p <~ "}" }

  def ir: Parser[IR] = rep(topLevel) ^^ IR

  def topLevel: Parser[TopLevel] = typeDef | funcSigDef | constDef | globalDataDef | funcDecl | funcDef

  def typeDef: Parser[TypeDef] = ".typedef" ~> gid ~< "=" ~ typeCons ^^^^ TypeDef
  def funcSigDef: Parser[FuncSigDef] = ".funcsig" ~> gid ~< "=" ~ funcSigCons ^^^^ FuncSigDef
  def constDef: Parser[ConstDef] = ".const" ~> gid ~ ang(typeExpr) ~< "=" ~ constCons ^^^^ ConstDef
  def globalDataDef: Parser[GlobalDataDef] = ".global" ~> gid ~ ang(typeExpr) ^^^^ GlobalDataDef
  def funcDecl: Parser[FuncDecl] = ".funcdecl" ~> gid ~ ang(funcSigExpr) ^^^^ FuncDecl
  def funcDef: Parser[FuncDef] = ".funcdef" ~> gid ~ ang(funcSigExpr) ~ paren(rep(lid)) ~< "=" ~ funcBodyDef ^^^^ FuncDef

  def typeExpr: Parser[TypeExpr] = gid ^^ TypeRef | typeCons
  def typeCons: Parser[TypeCons] =
    "int" ~> ang(intLit.asInt) ^^ IntCons |
      "float" ^^^ FloatCons |
      "double" ^^^ DoubleCons |
      "ref" ~> ang(typeExpr) ^^ RefCons |
      "iref" ~> ang(typeExpr) ^^ IRefCons |
      "weakref" ~> ang(typeExpr) ^^ WeakRefCons |
      "struct" ~> ang(rep(typeExpr)) ^^ StructCons |
      "array" ~> ang(typeExpr ~ intLit.asLong) ^^^^ ArrayCons |
      "void" ^^^ VoidCons |
      "func" ~> ang(funcSigExpr) ^^ FuncCons |
      "thread" ^^^ ThreadCons |
      "stack" ^^^ StackCons |
      "tagref64" ^^^ TagRef64Cons

  def funcSigExpr: Parser[FuncSigExpr] = gid ^^ FuncSigRef | funcSigCons
  def funcSigCons: Parser[FuncSigCons] = typeExpr ~ paren(rep(typeExpr)) ^^^^ FuncSigCons

  def constExpr: Parser[ConstExpr] = gid ^^ ConstRef | constCons
  def constCons: Parser[ConstCons] =
    intLit ^^ IntConstCons |
      floatLit ^^ FloatConstCons |
      doubleLit ^^ DoubleConstCons |
      "NULL" ^^^ NullConstCons

  def funcBodyDef: Parser[FuncBodyDef] = brace(entryBlock ~ rep(regularBlock)) ^^^^ FuncBodyDef

  def entryBlock: Parser[BasicBlockDef] = opt(label) ~ rep1(inst) ^^^^ BasicBlockDef
  def regularBlock: Parser[BasicBlockDef] = (label ^^ { Some(_) }) ~ rep1(inst) ^^^^ BasicBlockDef

  def label: Parser[LID] = lid <~ ":"

  def inst: Parser[InstDef] = opt(lid <~ "=") ~ instCons ^^^^ InstDef

  def t = typeExpr
  def s = funcSigExpr
  def v = valueExpr
  def l = lid
  def g = gid

  def instCons: Parser[InstCons] =
    binOptr ~ ang(t) ~ v ~ v ^^^^ BinOpCons |
      cmpOptr ~ ang(t) ~ v ~ v ^^^^ CmpCons |
      convOptr ~ ang(t ~ t) ~ v ^^ { case a ~ (b ~ c) ~ d => ConvCons(a, b, c, d) } |
      "SELECT" ~> ang(t) ~ v ~ v ~ v ^^^^ SelectCons |
      "BRANCH" ~> l ^^ BranchCons |
      "BRANCH2" ~> v ~ l ~ l ^^^^ Branch2Cons |
      "SWITCH" ~> ang(t) ~ v ~ l ~ vbMap ^^^^ SwitchCons |
      "PHI" ~> ang(t) ~ bvMap ^^^^ PhiCons |
      "CALL" ~> ang(s) ~ v ~ args ~ maybeKeepAlive ^^^^ CallCons |
      "INVOKE" ~> ang(s) ~ v ~ args ~ l ~ l ~ maybeKeepAlive ^^^^ InvokeCons |
      "TAILCALL" ~> ang(s) ~ v ~ args ^^^^ TailCallCons |
      "RET" ~> ang(t) ~ v ^^^^ RetCons |
      "RETVOID" ^^^ RetVoidCons |
      "THROW" ~> v ^^ ThrowCons |
      "LANDINGPAD" ^^^ LandingpadCons |
      "EXTRACTVALUE" ~> ang(t ~ intLit.asInt) ~ v ^^^^ ExtractValueCons |
      "INSERTVALUE" ~> ang(t ~ intLit.asInt) ~ v ~ v ^^^^ InsertValueCons |
      "NEW" ~> ang(t) ^^ NewCons |
      "NEWHYBRID" ~> ang(t) ~ v ^^^^ NewHybridCons |
      "ALLOCA" ~> ang(t) ^^ AllocaCons |
      "ALLOCAHYBRID" ~> ang(t) ~ v ^^^^ AllocaHybridCons |
      "GETIREF" ~> ang(t) ~ v ^^^^ GetIRefCons |
      "GETFIELDIREF" ~> ang(t ~ intLit.asInt) ~ v ^^^^ GetFieldIRefCons |
      "GETELEMIREF" ~> ang(t) ~ v ~ v ^^^^ GetElemIRefCons |
      "GETFIXEDPARTIREF" ~> ang(t) ~ v ^^^^ GetFixedPartIRefCons |
      "GETVARPARTIREF" ~> ang(t) ~ v ^^^^ GetVarPartIRefCons |
      "LOAD" ~> maybeMemOrd ~ ang(t) ~ v ^^^^ LoadCons |
      "STORE" ~> maybeMemOrd ~ ang(t) ~ v ~ v ^^^^ StoreCons |
      "CMPXCHG" ~> memOrd ~ memOrd ~ ang(t) ~ v ~ v ~ v ^^^^ CmpXchgCons |
      "ATOMICRMW" ~> memOrd ~ atomicRMWOp ~ ang(t) ~ v ~ v ^^^^ AtomicRMWCons |
      "FENCE" ~> memOrd ^^ FenceCons |
      "TRAP" ~> ang(t) ~ l ~ l ~ keepAlive ^^^^ TrapCons |
      "WATCHPOINT" ~> intLit.asInt ~ ang(t) ~ l ~ l ~ l ~ keepAlive ^^^^ WatchpointCons |
      "CCALL" ~> callConv ~ ang(s) ~ v ~ args ^^^^ CCallCons |
      "NEWSTACK" ~> ang(s) ~ v ~ args ^^^^ NewStackCons |
      "ICALL" ~> g ~ args ~ maybeKeepAlive ^^^^ ICallCons |
      "IINVOKE" ~> g ~ args ~ l ~ l ~ maybeKeepAlive ^^^^ IInvokeCons

  def vbMap: Parser[Seq[(ValueExpr, LID)]] = rep(valueExpr ~< ":" ~ lid ~< ";" ^^ { case a ~ b => (a, b) })
  def bvMap: Parser[Seq[(LID, ValueExpr)]] = rep(lid ~< ":" ~ valueExpr ~< ";" ^^ { case a ~ b => (a, b) })

  def args: Parser[Seq[ValueExpr]] = paren(rep(v))
  def keepAlive: Parser[Seq[ValueExpr]] = "KEEPALIVE" ~> args
  def maybeKeepAlive: Parser[Seq[ValueExpr]] = opt(keepAlive) ^^ { _.getOrElse(Nil) }

  def valueExpr: Parser[ValueExpr] = id ^^ RefValue | constCons ^^ InlineValue

  def binOptr: Parser[String] = "ADD|SUB|MUL|UDIV|SDIV|UREM|SREM|SHL|LSHR|ASHR|AND|OR|XOR".r
  def cmpOptr: Parser[String] = "EQ|NE|ULT|ULE|UGT|UGE|SLT|SLE|SGT|SGE|FTRUE|FFALSE|FEQ|FNE|FORD|FOEQ|FONE|FOLT|FOLE|FOGT|FOGE|FUNO|FUEQ|FUNE|FULT|FULE|FUGT|FUGE".r
  def convOptr: Parser[String] = "TRUNC|ZEXT|SEXT|FPTRUNC|FPEXT|FPTOUI|FPTOSI|UPTOFP|SITPFO|BITCAST|REFCAST|IREFCAST|FUNCCAST".r
  def memOrd: Parser[String] = "NOT_ATOMIC|UNORDERED|MONOTONIC|ACQUIRE|RELEASE|ACQ_REL|SEQ_CST".r
  def atomicRMWOp: Parser[String] = "XCHG|ADD|SUB|AND|NAND|OR|XOR|MIN|MAX|UMIN|UMAX".r
  def callConv: Parser[String] = "DEFAULT".r

  def maybeMemOrd: Parser[String] = opt(memOrd) ^^ { _.getOrElse("NOT_ATOMIC") }

  def id: Parser[ID] = lid | gid
  def lid: Parser[LID] = """%[a-zA-Z0-9-_.]+""".r ^^ LID
  def gid: Parser[GID] = """@[a-zA-Z0-9-_.]+""".r ^^ GID

  def sign: Parser[Boolean] = opt("+" ^^^ false | "-" ^^^ true) ^^ { _.getOrElse(false) }

  def parseInt(radix: Int)(res: Boolean ~ String): BigInt = res match {
    case neg ~ ns =>
      val num = BigInt(ns, radix)
      if (neg) -num else num
  }

  implicit class BigIntParserConv(p: Parser[BigInt]) {
    def asInt: Parser[Int] = p ^^ { _.intValue }
    def asLong: Parser[Long] = p ^^ { _.longValue }
  }

  def intLit: Parser[BigInt] =
    sign ~ """[1-9][0-9]*""" ^^ parseInt(10) |
      sign ~< "0" ~ """[0-9]*""" ^^ parseInt(8) |
      sign ~< "0x" ~ """[0-9a-fA-F]*""" ^^ parseInt(16)

  def floatLit: Parser[Float] =
    """[+-]?[0-9]+\.[0-9]+(e[+-]?[0-9]+)?""".r <~ "f" ^^ { _.toFloat } |
      "nan" <~ "f" ^^^ Float.NaN |
      "inf" <~ "f" ^^^ Float.PositiveInfinity |
      "-inf" <~ "f" ^^^ Float.NegativeInfinity |
      "bitsf" ~> paren(intLit.asInt) ^^ { java.lang.Float.intBitsToFloat(_) }

  def doubleLit: Parser[Double] =
    """[+-]?[0-9]+\.[0-9]+(e[+-]?[0-9]+)?""".r <~ "d" ^^ { _.toDouble } |
      "nan" <~ "d" ^^^ Double.NaN |
      "inf" <~ "d" ^^^ Double.PositiveInfinity |
      "-inf" <~ "d" ^^^ Double.NegativeInfinity |
      "bitsd" ~> paren(intLit.asLong) ^^ { java.lang.Double.longBitsToDouble(_) }

  def apply(input: java.lang.CharSequence): IR = handleNoSuccess(parseAll(ir, input))
  def apply(input: java.io.Reader): IR = handleNoSuccess(parseAll(ir, input))

  private def handleNoSuccess(pr: ParseResult[IR]): IR = pr match {
    case Success(irNode, _) => irNode
    case NoSuccess(msg, _) => throw new TextIRParsingException(msg)
  }
}

object UvmIRAST {

  case class IR(topLevels: Seq[TopLevel])

  abstract class TopLevel
  case class TypeDef(name: GID, cons: TypeCons) extends TopLevel

  abstract class TypeExpr
  case class TypeRef(id: GID) extends TypeExpr

  abstract class TypeCons extends TypeExpr
  case class IntCons(length: Int) extends TypeCons
  case object FloatCons extends TypeCons
  case object DoubleCons extends TypeCons
  case class RefCons(ty: TypeExpr) extends TypeCons
  case class IRefCons(ty: TypeExpr) extends TypeCons
  case class WeakRefCons(ty: TypeExpr) extends TypeCons
  case class StructCons(fieldTy: Seq[TypeExpr]) extends TypeCons
  case class ArrayCons(elemTy: TypeExpr, length: Long) extends TypeCons
  case class HybridCons(fixedPart: TypeExpr, varPart: TypeExpr) extends TypeCons
  case object VoidCons extends TypeCons
  case class FuncCons(sig: FuncSigExpr) extends TypeCons
  case object ThreadCons extends TypeCons
  case object StackCons extends TypeCons
  case object TagRef64Cons extends TypeCons

  case class FuncSigDef(name: GID, cons: FuncSigCons) extends TopLevel

  abstract class FuncSigExpr
  case class FuncSigRef(id: GID) extends FuncSigExpr
  case class FuncSigCons(retTy: TypeExpr, paramTy: Seq[TypeExpr]) extends FuncSigExpr

  case class ConstDef(name: GID, ty: TypeExpr, cons: ConstCons) extends TopLevel

  abstract class ConstExpr
  case class ConstRef(id: GID) extends ConstExpr

  abstract class ConstCons extends ConstExpr
  case class IntConstCons(num: BigInt) extends ConstCons
  case class FloatConstCons(num: Float) extends ConstCons
  case class DoubleConstCons(num: Double) extends ConstCons
  case class StructConstCons(fields: Seq[ConstExpr]) extends ConstCons
  case object NullConstCons extends ConstCons

  case class GlobalDataDef(name: GID, ty: TypeExpr) extends TopLevel

  case class FuncDecl(name: GID, sig: FuncSigExpr) extends TopLevel

  case class FuncDef(name: GID, sig: FuncSigExpr, params: Seq[LID], body: FuncBodyDef) extends TopLevel

  case class FuncBodyDef(entry: BasicBlockDef, bbs: Seq[BasicBlockDef])

  case class BasicBlockDef(name: Option[LID], insts: Seq[InstDef])

  case class InstDef(name: Option[LID], cons: InstCons)

  abstract class InstCons

  abstract class ValueExpr
  case class RefValue(id: ID) extends ValueExpr
  case class InlineValue(cons: ConstCons) extends ValueExpr

  case class BinOpCons(op: String, opndTy: TypeExpr, op1: ValueExpr, op2: ValueExpr) extends InstCons
  case class CmpCons(op: String, opndTy: TypeExpr, op1: ValueExpr, op2: ValueExpr) extends InstCons
  case class ConvCons(op: String, fromTy: TypeExpr, toTy: TypeExpr, opnd: ValueExpr) extends InstCons
  case class SelectCons(opndTy: TypeExpr, cond: ValueExpr, ifTrue: ValueExpr, ifFalse: ValueExpr) extends InstCons
  case class BranchCons(dest: LID) extends InstCons
  case class Branch2Cons(cond: ValueExpr, ifTrue: LID, ifFalse: LID) extends InstCons
  case class SwitchCons(opndTy: TypeExpr, opnd: ValueExpr, defDest: LID, cases: Seq[(ValueExpr, LID)]) extends InstCons
  case class PhiCons(opndTy: TypeExpr, cases: Seq[(LID, ValueExpr)]) extends InstCons
  case class CallCons(sig: FuncSigExpr, callee: ValueExpr, args: Seq[ValueExpr], keepAlives: Seq[ValueExpr]) extends InstCons
  case class InvokeCons(sig: FuncSigExpr, callee: ValueExpr, args: Seq[ValueExpr], nor: LID, exc: LID, keepAlives: Seq[ValueExpr]) extends InstCons
  case class TailCallCons(sig: FuncSigExpr, callee: ValueExpr, args: Seq[ValueExpr]) extends InstCons
  case class RetCons(retTy: TypeExpr, retVal: ValueExpr) extends InstCons
  case object RetVoidCons extends InstCons
  case class ThrowCons(excVal: ValueExpr) extends InstCons
  case object LandingpadCons extends InstCons
  case class ExtractValueCons(strTy: TypeExpr, index: Int, opnd: ValueExpr) extends InstCons
  case class InsertValueCons(strTy: TypeExpr, index: Int, opnd: ValueExpr, newVal: ValueExpr) extends InstCons
  case class NewCons(allocTy: TypeExpr) extends InstCons
  case class NewHybridCons(allocTy: TypeExpr, length: ValueExpr) extends InstCons
  case class AllocaCons(allocTy: TypeExpr) extends InstCons
  case class AllocaHybridCons(allocTy: TypeExpr, length: ValueExpr) extends InstCons
  case class GetIRefCons(referentTy: TypeExpr, opnd: ValueExpr) extends InstCons
  case class GetFieldIRefCons(referentTy: TypeExpr, index: Int, opnd: ValueExpr) extends InstCons
  case class GetElemIRefCons(referentTy: TypeExpr, opnd: ValueExpr, index: ValueExpr) extends InstCons
  case class ShiftIRefCons(referentTy: TypeExpr, opnd: ValueExpr, offset: ValueExpr) extends InstCons
  case class GetFixedPartIRefCons(referentTy: TypeExpr, opnd: ValueExpr) extends InstCons
  case class GetVarPartIRefCons(referentTy: TypeExpr, opnd: ValueExpr) extends InstCons
  case class LoadCons(ord: String, referentTy: TypeExpr, loc: ValueExpr) extends InstCons
  case class StoreCons(ord: String, referentTy: TypeExpr, loc: ValueExpr, newVal: ValueExpr) extends InstCons
  case class CmpXchgCons(ordSucc: String, ordFail: String, referentTy: TypeExpr,
    loc: ValueExpr, expected: ValueExpr, desired: ValueExpr) extends InstCons
  case class AtomicRMWCons(ord: String, op: String, referentTy: TypeExpr, loc: ValueExpr, newVal: ValueExpr) extends InstCons
  case class FenceCons(ord: String) extends InstCons
  case class TrapCons(retTy: TypeExpr, nor: LID, exc: LID, keepAlives: Seq[ValueExpr]) extends InstCons
  case class WatchpointCons(wpID: Int, retTy: TypeExpr, dis: LID, nor: LID, exc: LID, keepAlives: Seq[ValueExpr]) extends InstCons
  case class CCallCons(callConv: String, sig: FuncSigExpr, callee: ValueExpr, args: Seq[ValueExpr]) extends InstCons
  case class NewStackCons(sig: FuncSigExpr, callee: ValueExpr, args: Seq[ValueExpr]) extends InstCons
  case class ICallCons(iFunc: GID, args: Seq[ValueExpr], keepAlives: Seq[ValueExpr]) extends InstCons
  case class IInvokeCons(iFunc: GID, args: Seq[ValueExpr], nor: LID, exc: LID, keepAlives: Seq[ValueExpr]) extends InstCons

  abstract class ID(name: String)
  case class GID(name: String) extends ID(name)
  case class LID(name: String) extends ID(name)

  case class IntLit(num: BigInt)
  case class FloatLit(num: Float)
  case class DoubleLit(num: Double)
  case object NullLit
}