package uvm.clientsupport.text

import java.util

import scala.collection.JavaConversions

private[text] object Helpers {
  def buildMap[K, V](iter: TraversableOnce[(K, V)]): util.Map[K, V] = {
    val map = new util.LinkedHashMap[K, V]
    iter foreach { case (k, v) => map.put(k, v) }
    util.Collections unmodifiableMap map
  }
  def buildList[T](iter: TraversableOnce[T]): util.List[T] = {
    val list = new util.ArrayList[T]
    iter foreach list.add
    util.Collections unmodifiableList list
  }
}

import Helpers._

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

/**
 * A Mu IR bundle. This class is more representation-oriented rather than semantic oriented. For example, there are
 * "type definitions" rather than "types". Similarly "function definitions" and "function declarations" are separated
 * rather than contained in each other.
 */
class Bundle(
  cTypeDefs: TraversableOnce[(TypeName, TypeCtor)],
  cFuncSigDefs: TraversableOnce[(FuncSigName, FuncSig)],
  cConstDefs: TraversableOnce[(GlobalVarName, Const)],
  cGlobalCellDefs: TraversableOnce[(GlobalVarName, TypeName)],
  cFuncDecls: TraversableOnce[(GlobalVarName, FuncSigName)],
  cFuncVers: TraversableOnce[(FuncVerName, FuncVer)],
  cFuncExpDefs: TraversableOnce[(GlobalVarName, Expose)],
  cComments: TraversableOnce[(GlobalName, String)]
) {
  lazy val typeDefs = buildMap(cTypeDefs)
  lazy val funcSigDefs = buildMap(cFuncSigDefs)
  lazy val constDefs = buildMap(cConstDefs)
  lazy val globalCellDefs = buildMap(cGlobalCellDefs)
  lazy val funcDecls = buildMap(cFuncDecls)
  lazy val funcVers = buildMap(cFuncVers)
  lazy val funcExpDefs = buildMap(cFuncExpDefs)
  lazy val comments = buildMap(cComments)

  override lazy val toString = {
    val sb = new StringBuilder
    def printBlock[N <: GlobalName, V](block: java.util.Map[N, V])(fn: (N, V) => Unit): Unit =
      if (!block.isEmpty) {
        import JavaConversions._
        sb append "\n"
        block foreach { case (name, value) =>
          Option(comments get name) foreach (_ split '\n' map ("// " + _ + "\n") foreach sb.append)
          fn(name, value)
        }
      }

    printBlock(typeDefs)((name, ctor) => sb append s".typedef $name = $ctor\n")
    printBlock(funcSigDefs)((name, sig) => sb append s".funcsig $name = $sig\n")
    printBlock(constDefs)((name, const) => sb append s".const $name <${const.ty}> = $const\n")
    printBlock(globalCellDefs)((name, ty) => sb append s".global $name <$ty>\n")
    printBlock(funcDecls)((name, sig) => sb append s".funcdecl $name <$sig>\n")
    printBlock(funcExpDefs)((name, exp) => sb append s".expose $name = $exp\n")
    printBlock(funcVers)((name, ver) =>
      sb append s".funcdef ${ver.func} VERSION $name <${ver.sig}> $ver\n\n")

    sb.toString
  }
}

sealed abstract class TypeCtor(str: => String) {
  override lazy val toString: String = str
}
object TypeCtor {
  case class Void() extends TypeCtor("void")
  case class Int(bits: scala.Int) extends TypeCtor(s"int<$bits>")
  case class Float() extends TypeCtor("float")
  case class Double() extends TypeCtor("double")
  case class Thread() extends TypeCtor("thread")
  case class Stack() extends TypeCtor("stack")
  case class Ref(ty: TypeCtor) extends TypeCtor(s"ref<$ty>")
  case class IRef(ty: TypeCtor) extends TypeCtor(s"iref<$ty>")
  case class WeakRef(ty: TypeCtor) extends TypeCtor(s"weakref<$ty>")
  case class TagRef64() extends TypeCtor("tagref64")
  case class Ptr(ty: TypeCtor) extends TypeCtor(s"ptr<$ty>")
  case class FuncPtr(sig: FuncSigName) extends TypeCtor(s"funcptr<$sig>")
  case class Array(ty: TypeCtor, len: scala.Long) extends TypeCtor(s"array<$ty $len>")
  case class Vector(ty: TypeCtor, len: scala.Int) extends TypeCtor(s"vector<$ty $len>")
  case class Hybrid(fixedTy: TypeCtor, varTy: TypeCtor) extends TypeCtor(s"hybrid<$fixedTy $varTy>")
  case class Func(sig: FuncSigName) extends TypeCtor(s"func<$sig>")
  final class Struct(cTypes: Traversable[TypeCtor])
    extends TypeCtor(s"struct<${cTypes mkString " "}>") {
    def this(cTypes: java.lang.Iterable[TypeCtor]) =
      this(JavaConversions iterableAsScalaIterable cTypes)
    lazy val types = buildList(cTypes)
  }
}

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
  final class Struct(val ty: TypeName, cFields: Traversable[GlobalVarName])
    extends Const("{" + (cFields mkString " ")  + "}") {
    def this(ty: TypeName, cFields: java.lang.Iterable[GlobalVarName]) =
      this(ty, JavaConversions iterableAsScalaIterable cFields)
    lazy val fields = buildList(cFields)
  }
  final class Vector(val ty: TypeName, cElems: Traversable[GlobalVarName])
    extends Const("VEC{" + (cElems mkString " ")  + "}") {
    def this(ty: TypeName, cElems: java.lang.Iterable[GlobalVarName]) =
      this(ty, JavaConversions iterableAsScalaIterable cElems)
    lazy val fields = buildList(cElems)
  }
}

final class FuncSig(val retTy: TypeName, cParamTy: TraversableOnce[TypeName]) {
  def this(retTy: TypeName, javaParamTy: java.lang.Iterable[TypeName]) =
    this(retTy, JavaConversions.iterableAsScalaIterable(javaParamTy))
  lazy val paramTy = buildList(cParamTy)
  override def toString =
    s"$retTy (${JavaConversions.asScalaIterator(paramTy.iterator) mkString " "})"
  override def equals(other: Any) = other match {
    case that: FuncSig => paramTy == that.paramTy && retTy == that.retTy
    case _ => false
  }
  override def hashCode() =
    Seq(paramTy, retTy).map(_.hashCode).foldLeft(0)(31 * _ + _)
}

final class FuncVer(
  val func: GlobalVarName,
  val sig: FuncSigName,
  cParams: TraversableOnce[LocalVarName],
  cBbs: TraversableOnce[BasicBlock]
) {
  def this(
    func: GlobalVarName,
    sig: FuncSigName,
    cParams: java.lang.Iterable[LocalVarName],
    cBbs: java.lang.Iterable[BasicBlock]
  ) = this(
    func, sig,
    JavaConversions iterableAsScalaIterable cParams,
    JavaConversions iterableAsScalaIterable cBbs
  )
  lazy val params = buildList(cParams)
  lazy val bbs = buildList(cBbs)
  override lazy val toString = {
    import JavaConversions._
    "(" + params.mkString(" ") + ") {\n" + bbs.mkString("\n") + "\n}"
  }
}

case class Expose(func: GlobalVarName, callConv: Flag, cookie: GlobalVarName) {
  override def toString = s"$func $callConv $cookie"
}

final class BasicBlock(label: LabelName, cInsts: TraversableOnce[NameableInst]) {
  def this(label: LabelName, cInsts: java.lang.Iterable[NameableInst]) =
    this(label, JavaConversions iterableAsScalaIterable cInsts)
  lazy val insts = buildList(cInsts)
  override lazy val toString = s"  $label:\n" +
    ((JavaConversions collectionAsScalaIterable insts) map ("    " + _) mkString "\n")
}

sealed trait NameableInst {
  def nameOption: Option[LocalVarName]
  final def nameOptional: java.util.Optional[LocalVarName] = nameOption match {
    case Some(n) => java.util.Optional.of(n)
    case None => java.util.Optional.empty()
  }
  def inst: Inst
}
sealed abstract class Inst(str: => String) extends NameableInst {
  override def nameOption = None
  override def inst = this
  override def toString = str
}
case class NamedInst(name: LocalVarName, inst: Inst) extends NameableInst {
  override def nameOption = Some(name)
  override def toString = name + " = " + inst
}
object Inst {
  private def ?(opt: Option[AnyRef]): String = opt map(" " + _) getOrElse ""
  private def ?(bool: Boolean, str: String) = if (bool) " " + str else ""

  // Basic Operations
  // ------------------------------------------------------------

  case class BinOp(
    op: BinOp.Value,
    opndTy: TypeName,
    op1: VarName,
    op2: VarName,
    excClause: Option[ExcClause]
  ) extends Inst(s"$op <$opndTy> $op1 $op2" + ?(excClause)) {
    def this(op: BinOp.Value, opndTy: TypeName, op1: VarName, op2: VarName) =
      this(op, opndTy, op1, op2, None)
    def this(op: BinOp.Value, opndTy: TypeName, op1: VarName, op2: VarName, excClause: ExcClause) =
      this(op, opndTy, op1, op2, Some(excClause))
  }
  object BinOp extends Enumeration {
    val Add = Value("ADD")
    val Sub = Value("SUB")
    val Mul = Value("MUL")
    val SignDiv = Value("SDIV")
    val SignRem = Value("SREM")
    val UnsignDiv = Value("UDIV")
    val UnsignRem = Value("UREM")
    val ShiftL = Value("SHL")
    val LogShiftR = Value("LSHR")
    val ArithShiftR = Value("ASHR")
    val And = Value("AND")
    val Or = Value("OR")
    val Xor = Value("XOR")
    val FloatAdd = Value("FADD")
    val FloatSub = Value("FSUB")
    val FloatMul = Value("FMUL")
    val FloatDiv = Value("FDIV")
    val FloatRem = Value("FREM")
  }

  case class Cmp(op: Cmp.Value, opndTy: TypeName, op1: VarName, op2: VarName)
    extends Inst(s"$op <$opndTy> $op1 $op2")
  object Cmp extends Enumeration {
    val Eq = Value("EQ")
    val Ne = Value("NE")
    val SignGe = Value("SGE")
    val SignGt = Value("SGT")
    val SignLe = Value("SLE")
    val SignLt = Value("SLT")
    val UnsignGe = Value("UGE")
    val UnsignGt = Value("UGT")
    val UnsignLe = Value("ULE")
    val UnsignLt = Value("ULT")
    val FloatTrue = Value("FTRUE")
    val FloatFalse = Value("FFALSE")
    val FloatUnord = Value("FUNO")
    val FloatUnordEq = Value("FUEQ")
    val FloatUnordNe = Value("FUNE")
    val FloatUnordGt = Value("FUGT")
    val FloatUnordGe = Value("FUGE")
    val FloatUnordLt = Value("FULT")
    val FloatUnordLe = Value("FULE")
    val FloatOrd = Value("FORD")
    val FloatOrdEq = Value("FOEQ")
    val FloatOrdNe = Value("FONE")
    val FloatOrdGt = Value("FOGT")
    val FloatOrdGe = Value("FOGE")
    val FloatOrdLt = Value("FOLT")
    val FloatOrdLe = Value("FOLE")
  }

  case class Conv(op: Conv.Value, fromTy: TypeName, toTy: TypeName, opnd: VarName)
    extends Inst(s"$op <$fromTy $toTy> $opnd")
  object Conv extends Enumeration {
    val Trunc = Value("TRUNC")
    val ZeroExt = Value("ZEXT")
    val SignExt = Value("SEXT")
    val FloatTrunc = Value("FPTRUNC")
    val FloatExt = Value("FPEXT")
    val FloatToUInt = Value("FPTOUI")
    val FloatToInt = Value("FPTOSI")
    val UIntToFloat = Value("UITOFP")
    val IntToFloat = Value("SITOFP")
    val BitCast = Value("BITCAST")
    val RefCast = Value("REFCAST")
    val PtrCast = Value("PTRCAST")
  }

  case class Select(condTy: TypeName, opndTy: TypeName, cond: VarName, ifTrue: VarName, ifFalse: VarName)
    extends Inst(s"SELECT <$condTy $opndTy> $cond $ifTrue $ifFalse")

  // Intra-function Control Flow
  // ------------------------------------------------------------

  case class Branch(dest: LabelName) extends Inst(s"BRANCH $dest")
  case class Branch2(cond: VarName, ifTrue: LabelName, ifFalse: LabelName)
    extends Inst(s"BRANCH2 $cond $ifTrue $ifFalse")

  final class Switch(
    val opndTy: TypeName,
    val opnd: VarName,
    val defDest: LabelName,
    cCases: Traversable[SwitchCase]
  ) extends Inst(s"SWITCH <$opndTy> $opnd $defDest { ${cCases mkString " "} }") {
    def this(
      opndTy: TypeName, opnd: VarName, defDest: LabelName,
      cCases: java.lang.Iterable[SwitchCase]
    ) = this(opndTy, opnd, defDest, JavaConversions iterableAsScalaIterable cCases)
    lazy val cases = buildList(cCases)
  }

  final class Phi(val opndTy: TypeName, cCases: Traversable[PhiCase])
    extends Inst(s"PHI <$opndTy> { ${cCases mkString " "} }") {
    def this(opndTy: TypeName, cCases: java.lang.Iterable[PhiCase]) =
      this(opndTy, JavaConversions iterableAsScalaIterable cCases)
    lazy val cases = buildList(cCases)
  }

  // Inter-function Control Flow
  // ------------------------------------------------------------

  final class Call(
    val sig: FuncSigName,
    val callee: VarName,
    cArgList: Traversable[VarName],
    val excClause: Option[ExcClause],
    val keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(s"CALL <$sig> $callee (${cArgList mkString " "})" + ?(excClause) + ?(keepAliveClause)) {
    def this(sig: FuncSigName, callee: VarName, cArgList: java.lang.Iterable[VarName]) =
      this(sig, callee, JavaConversions iterableAsScalaIterable cArgList, None, None)
    def this(
      sig: FuncSigName,
      callee: VarName,
      cArgList: java.lang.Iterable[VarName],
      excClause: ExcClause
    ) = this(sig, callee, JavaConversions iterableAsScalaIterable cArgList, Some(excClause), None)
    def this(
      sig: FuncSigName,
      callee: VarName,
      cArgList: java.lang.Iterable[VarName],
      keepAliveClause: KeepAliveClause
    ) = this(sig, callee, JavaConversions iterableAsScalaIterable cArgList, None, Some(keepAliveClause))
    def this(
      sig: FuncSigName,
      callee: VarName,
      cArgList: java.lang.Iterable[VarName],
      excClause: ExcClause,
      keepAliveClause: KeepAliveClause
    ) = this(sig, callee, JavaConversions iterableAsScalaIterable cArgList, Some(excClause), Some(keepAliveClause))
    lazy val argList = buildList(cArgList)
  }

  final class TailCall(val sig: FuncSigName, val callee: VarName, cArgList: Traversable[VarName])
    extends Inst(s"TAILCALL <$sig> $callee (${cArgList mkString " "})") {
    def this(sig: FuncSigName, callee: VarName, cArgList: java.lang.Iterable[VarName]) =
      this(sig, callee, JavaConversions iterableAsScalaIterable cArgList)
    lazy val argList = buildList(cArgList)
  }

  case class Ret(opndTy: TypeName, rv: VarName) extends Inst(s"RET <$opndTy> $rv")
  case class RetVoid() extends Inst("RETVOID")
  case class Throw(exc: VarName) extends Inst(s"THROW $exc")
  case class LandingPad() extends Inst("LANDINGPAD")

  // Aggregate Type Operations
  // ------------------------------------------------------------

  // Struct Operations

  case class ExtractValue(strTy: TypeName, index: Int, opnd: VarName)
    extends Inst(s"EXTRACTVALUE <$strTy $index> $opnd")
  case class InsertValue(strTy: TypeName, index: Int, opnd: VarName, newVal: VarName)
    extends Inst(s"INSERTVALUE <$strTy $index> $opnd $newVal")

  // Vector Operations

  case class ExtractElement(vecTy: TypeName, indTy: TypeName, opnd: VarName, index: VarName)
    extends Inst(s"EXTRACTELEMENT <$vecTy $indTy> $opnd $index")
  case class InsertElement(vecTy: TypeName, indTy: TypeName, opnd: VarName, index: VarName, newVal: VarName)
    extends Inst(s"INSERTELEMENT <$vecTy $indTy> $opnd $index $newVal")
  case class ShuffleVector(vecTy: TypeName, maskTy: TypeName, vec1: VarName, vec2: VarName, mask: VarName)
    extends Inst(s"SHUFFLEVECTOR <$vecTy $maskTy> $vec1 $vec2 $mask")

  // Memory Operations
  // ------------------------------------------------------------

  // Memory Allocation

  case class New(allocTy: TypeName, excClause: Option[ExcClause])
    extends Inst(s"NEW <$allocTy>" + ?(excClause))
  case class NewHybrid(
    allocTy: TypeName, lenTy: TypeName, length: VarName, excClause: Option[ExcClause]
  ) extends Inst(s"NEWHYBRID <$allocTy $lenTy> $length" + ?(excClause))
  case class Alloca(allocTy: TypeName, excClause: Option[ExcClause])
    extends Inst(s"ALLOCA <$allocTy>" + ?(excClause))
  case class AllocaHybrid(
    allocTy: TypeName, lenTy: TypeName, length: VarName, excClause: Option[ExcClause]
  ) extends Inst(s"ALLOCAHYBRID <$allocTy $lenTy> $length" + ?(excClause))

  // Memory Addressing

  case class GetIRef(referentTy: TypeName, opnd: VarName)
    extends Inst(s"GETIREF <$referentTy> $opnd")
  case class GetFieldIRef(ptr: Boolean, referentTy: TypeName, index: Int, opnd: VarName)
    extends Inst(s"GETFIELDIREF${?(ptr, "PTR")} <$referentTy $index> $opnd")
  case class GetElemIRef(
    ptr: Boolean, referentTy: TypeName, indexTy: TypeName, opnd: VarName, index: VarName
  ) extends Inst(s"GETELEMIREF${?(ptr, "PTR")} <$referentTy $indexTy> $opnd $index")
  case class ShiftIRef(
    ptr: Boolean, referentTy: TypeName, offsetTy: TypeName, opnd: VarName, offset: VarName
  ) extends Inst(s"SHIFTIREF${?(ptr, "PTR")} <$referentTy $offsetTy> $opnd $offset")
  case class GetFixedPartIRef(ptr: Boolean, referentTy: TypeName, opnd: VarName)
    extends Inst(s"GETFIXEDPARTIREF${?(ptr, "PTR")} <$referentTy> $opnd")
  case class GetVarPartIRef(ptr: Boolean, referentTy: TypeName, opnd: VarName)
    extends Inst(s"GETVARPARTIREF${?(ptr, "PTR")} <$referentTy> $opnd")

  case class Load(
    ptr: Boolean,
    ord: MemoryOrder.Value,
    locTy: TypeName,
    loc: VarName,
    excClause: Option[ExcClause]
  ) extends Inst(s"LOAD${?(ptr, "PTR")} $ord <$locTy> $loc" + ?(excClause))
  case class Store(
    ptr: Boolean,
    ord: MemoryOrder.Value,
    locTy: TypeName,
    loc: VarName,
    newVal: VarName,
    excClause: Option[ExcClause]
  ) extends Inst(s"STORE${?(ptr, "PTR")} $ord <$locTy> $loc $newVal" + ?(excClause))
  case class CmpXchg(
    ptr: Boolean,
    weak: Boolean,
    ordSucc: MemoryOrder.Value,
    ordFail: MemoryOrder.Value,
    locTy: TypeName,
    loc: VarName,
    expected: VarName,
    desired: VarName,
    excClause: Option[ExcClause]
  ) extends Inst(
    s"CMPXCHG${?(ptr, "PTR")}${?(weak, "WEAK")} $ordSucc $ordFail <$locTy> $loc $expected $desired" + ?(excClause))
  case class AtomicRMW(
    ptr: Boolean,
    ord: MemoryOrder.Value,
    op: AtomicRMWOptr.Value,
    locTy: TypeName,
    loc: VarName,
    opnd: VarName,
    excClause: Option[ExcClause]
  ) extends Inst(
    s"ATOMICRMW${?(ptr, "PTR")} $ord $op <$locTy> $loc $opnd" + ?(excClause))
  case class Fence(ord: MemoryOrder.Value) extends Inst(s"FENCE $ord")

  // Traps
  // ------------------------------------------------------------

  case class Trap(
    retTy: TypeName,
    excClause: Option[ExcClause],
    keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(s"TRAP <$retTy>" + ?(excClause) + ?(keepAliveClause))
  case class WatchPoint(
    wpid: Int,
    retTy: TypeName,
    dis: LabelName,
    ena: LabelName,
    exc: Option[LabelName],
    keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(
    s"WATCHPOINT $wpid <$retTy> $dis $ena" + ?(exc.map("WPEXC("+_+")")) + ?(keepAliveClause))

  // Unsafe Native Call

  final class CCall(
    val callConv: Flag,
    val calleeTy: TypeName,
    val sig: FuncSigName,
    val callee: VarName,
    cArgList: Traversable[VarName],
    val keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(
    s"CCALL $callConv <$calleeTy $sig> $callee (${cArgList mkString " "})" + ?(keepAliveClause)
  ) {
    def this(
      callConv: Flag,
      calleeTy: TypeName,
      sig: FuncSigName,
      callee: VarName,
      cArgList: java.lang.Iterable[VarName]
    ) = this(callConv, calleeTy, sig, callee, JavaConversions iterableAsScalaIterable cArgList, None)
    def this(
      callConv: Flag,
      calleeTy: TypeName,
      sig: FuncSigName,
      callee: VarName,
      cArgList: java.lang.Iterable[VarName],
      keepAliveClause: KeepAliveClause
    ) = this(callConv, calleeTy, sig, callee, JavaConversions iterableAsScalaIterable cArgList,
      Some(keepAliveClause))
    lazy val argList = buildList(cArgList)
  }

  // Thread and Stack
  // ------------------------------------------------------------

  final class NewStack(
    val sig: FuncSigName,
    val func: VarName,
    cArgList: Traversable[VarName],
    val excClause: Option[ExcClause]
  ) extends Inst(s"NEWSTACK <$sig> $func (${cArgList mkString " "})" + ?(excClause)) {
    def this(sig: FuncSigName, func: VarName, cArgList: java.lang.Iterable[VarName]) =
      this(sig, func, JavaConversions iterableAsScalaIterable cArgList, None)
    def this(
      sig: FuncSigName,
      func: VarName,
      cArgList: java.lang.Iterable[VarName],
      excClause: ExcClause
    ) = this(sig, func, JavaConversions iterableAsScalaIterable cArgList, Some(excClause))
    lazy val argList = buildList(cArgList)
  }

  case class SwapStack(
    swappee: VarName,
    curStackClause: CurStackClause,
    newStackClause: NewStackClause,
    excClause: Option[ExcClause],
    keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(
    s"SWAPSTACK $swappee $curStackClause $newStackClause" + ?(excClause) + ?(keepAliveClause)
  ) {
    def this(swappee: VarName, curStackClause: CurStackClause, newStackClause: NewStackClause) =
      this(swappee, curStackClause, newStackClause, None, None)
  }

  // Common Instructions
  // ------------------------------------------------------------

  final class CommInst(
    val instName: GlobalVarName,
    cFlagList: Option[Traversable[Flag]],
    cTypeList: Option[Traversable[TypeName]],
    cFuncSigList: Option[Traversable[FuncSigName]],
    cArgList: Option[Traversable[VarName]],
    val excClause: Option[ExcClause],
    val keepAliveClause: Option[KeepAliveClause]
  ) extends Inst(
    s"COMMINST $instName" +
      ?(cFlagList.map("[" + _.mkString(" ") + "]")) +
      ?(cTypeList.map("<" + _.mkString(" ") + ">")) +
      ?(cFuncSigList.map("<[" + _.mkString(" ") + "]>")) +
      ?(cArgList.map("(" + _.mkString(" ") + ")")) +
      ?(excClause) + ?(keepAliveClause)
  ) {
    // TODO: Java-friendly constructors
    lazy val flagList = cFlagList map buildList
    lazy val typeList = cTypeList map buildList
    lazy val funcSigList = cFuncSigList map buildList
    lazy val argList = cArgList map buildList
  }
}

object MemoryOrder extends Enumeration {
  val NotAtomic = Value("NOT_ATOMIC")
  val Relaxed = Value("RELAXED")
  val Consume = Value("CONSUME")
  val Acquire = Value("ACQUIRE")
  val Release = Value("RELEASE")
  val AcquireRelease = Value("ACQ_REL")
  val SeqConsistent = Value("SEQ_CST")
}

object AtomicRMWOptr extends Enumeration {
  val Exchange = Value("XCHG")
  val Add = Value("ADD")
  val Sub = Value("SUB")
  val And = Value("AND")
  val Nand = Value("NAND")
  val Or = Value("OR")
  val Xor = Value("XOR")
  val SignMax = Value("MAX")
  val SignMin = Value("MIN")
  val UnsignMax = Value("UMAX")
  val UnsignMin = Value("UMIN")
}

final class KeepAliveClause(cVars: TraversableOnce[LocalVarName]) {
  def this(cVars: java.lang.Iterable[LocalVarName]) =
    this(JavaConversions iterableAsScalaIterable cVars)
  lazy val vars = buildList(cVars)
  override def toString = s"KEEPALIVE(${JavaConversions.asScalaIterator(vars.iterator()) mkString " "})"
}
case class ExcClause(nor: LabelName, exc: LabelName) {
  override def toString = s"EXC($nor $exc)"
}
case class SwitchCase(value: VarName, dest: LabelName) {
  override def toString = s"$value: $dest;"
}
case class PhiCase(source: LabelName, value: VarName) {
  override def toString = s"$source: $value;"
}

sealed abstract class CurStackClause
object CurStackClause {
  case class RetWith(retTy: TypeName) extends CurStackClause {
    override def toString = s"RET_WITH <$retTy>"
  }
  case class KillOld() extends CurStackClause {
    override def toString = "KILL_OLD"
  }
}

sealed abstract class NewStackClause
object NewStackClause {
  case class PassValue(argTy: TypeName, arg: VarName) extends NewStackClause {
    override def toString = s"PASS_VALUE <$argTy> $arg"
  }
  case class PassVoid() extends NewStackClause {
    override def toString = "PASS_VOID"
  }
  case class ThrowExc(exc: VarName) extends NewStackClause {
    override def toString = s"THROW_EXC $exc"
  }
}
