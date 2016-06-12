package uvm.clientsupport.text

case class DestClause(dest: LabelName, args: IList[VarName]) {
  override def toString = s"$dest($args)"
}

sealed trait Inst {
  @throws(classOf[TypeResolveException])
  def returnTypes(implicit context: Context): IList[TypeName]
  def referencedVars: IList[VarName]
}
sealed trait PreInst extends Inst {
  def possibleJumps: IList[LabelName]
}
sealed trait PostInst extends Inst
sealed abstract class PrePostInst(str: => String) extends PreInst with PostInst {
  override def toString = str
  override final def possibleJumps = IList()
}
sealed trait SupportsExcClause extends Inst { this: PrePostInst =>
  def preExcString: String = toString
  def postExcString: String = ""
}
case class PreExcClause(inst: SupportsExcClause, nor: LabelName, exc: LabelName) extends PreInst {
  override def returnTypes(implicit context: Context) = inst.returnTypes
  override def referencedVars = IList()
  override def possibleJumps = IList(nor, exc)
  override def toString = s"${inst.preExcString} EXC($nor $exc)${inst.postExcString}"
}
case class PostExcClause(inst: SupportsExcClause, nor: DestClause, exc: DestClause) extends PostInst {
  override def referencedVars = IList()
  override def returnTypes(implicit context: Context) = inst.returnTypes
  override def toString = s"${inst.preExcString} EXC($nor $exc)${inst.postExcString}"
}

case class PreBasicBlock(label: LabelName, insts: IList[(IList[LocalVarName], PreInst)]) {
  override lazy val toString = s"  $label:\n" +
    (insts map { case (n, i) => s"    ($n) = $i" } mkString "\n")
}

case class PostBasicBlock(
  label: LabelName,
  args: IList[(LocalVarName, TypeName)],
  insts: IList[(IList[LocalVarName], PostInst)]
) {
  override lazy val toString = s"  $label(${args map {case (l, t) => s"<$t> $l"} mkString " "}):\n" +
    (insts map { case (n, i) => s"    ($n) = $i" } mkString "\n")
}

object Inst {
  private def ?(opt: Option[AnyRef]): String = opt map(" " + _) getOrElse ""
  private def ?(bool: Boolean, str: String) = if (bool) " " + str else ""

  // Basic Operations
  // ------------------------------------------------------------

  case class Id(ty: TypeName, v: VarName) extends PreInst {
    override def toString = s"ID <$ty> $v"
    override def returnTypes(implicit context: Context) = IList(ty)
    override def referencedVars = IList(v)
    override def possibleJumps = IList()
  }

  case class BinOp(op: BinOp.Value, opndTy: TypeName, op1: VarName, op2: VarName)
    extends PrePostInst(s"$op <$opndTy> $op1 $op2") with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList(opndTy)
    override def referencedVars = IList(op1, op2)
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
    extends PrePostInst(s"$op <$opndTy> $op1 $op2") {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef (context resolve opndTy match {
        case Some(TypeCtor.Vector(_, arity)) => TypeCtor.Vector(TypeCtor.Int(1), arity)
        case _ => TypeCtor.Int(1)
      }))
    override def referencedVars = IList(op1, op2)
  }
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
    extends PrePostInst(s"$op <$fromTy $toTy> $opnd") {
    override def returnTypes(implicit context: Context) = IList(toTy)
    override def referencedVars = IList(opnd)
  }
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
    extends PrePostInst(s"SELECT <$condTy $opndTy> $cond $ifTrue $ifFalse") {
    override def returnTypes(implicit context: Context) = IList(opndTy)
    override def referencedVars = IList(cond, ifTrue, ifFalse)
  }

  // Intra-function Control Flow
  // ------------------------------------------------------------

  case class PreBranch(dest: LabelName) extends PreInst {
    override def toString = s"BRANCH $dest"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList()
    override def possibleJumps = IList(dest)
  }
  case class PreBranch2(cond: VarName, ifTrue: LabelName, ifFalse: LabelName) extends PreInst {
    override def toString = s"BRANCH2 $cond $ifTrue $ifFalse"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList(cond)
    override def possibleJumps = IList(ifTrue, ifFalse)
  }
  case class PreSwitch(
    opndTy: TypeName,
    opnd: VarName,
    defDest: LabelName,
    cases: IList[SwitchCase[LabelName]]
  ) extends PreInst {
    override def toString = s"SWITCH <$opndTy> $opnd $defDest { $cases }"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = new IList(opnd +: (cases.toSeq map (_.value)))
    override def possibleJumps = new IList(cases map (_.dest))
  }

  case class PostBranch(dest: DestClause) extends PostInst {
    override def toString = s"BRANCH $dest"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList()
  }
  case class PostBranch2(cond: VarName, ifTrue: DestClause, ifFalse: DestClause) extends PostInst {
    override def toString = s"BRANCH2 $cond $ifTrue $ifFalse"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList(cond)
  }
  case class PostSwitch(
    opndTy: TypeName,
    opnd: VarName,
    defDest: DestClause,
    cases: IList[SwitchCase[DestClause]]
  ) extends PostInst {
    override def toString = s"SWITCH <$opndTy> $opnd $defDest { $cases }"
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = new IList(opnd +: cases.toSeq.map(_.value))
  }

  // Inter-function Control Flow
  // ------------------------------------------------------------

  case class Call(
    sig: FuncSigName,
    callee: VarName,
    argList: IList[VarName],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PrePostInst(s"CALL <$sig> $callee ($argList)" + ?(keepaliveClause)) with SupportsExcClause {
    def this(sig: FuncSigName, callee: VarName, argList: IList[VarName]) =
      this(sig, callee, argList, None)
    def this(sig: FuncSigName, callee: VarName, argList: IList[VarName], keepaliveClause: KeepaliveClause) =
      this(sig, callee, argList, Some(keepaliveClause))
    override def returnTypes(implicit context: Context) =
      (for (FuncSig(_, retTypes) <- context resolve sig) yield retTypes) getOrElse {
        throw TypeResolveException(s"Undefined function signature: $sig")
      }
    override def referencedVars =
      new IList(callee +: (argList.toSeq ++ keepaliveClause.toSeq.flatMap(_.vars)))
    override def preExcString = s"CALL <$sig> $callee ($argList)"
    override def postExcString = ?(keepaliveClause)
  }

  case class TailCall(sig: FuncSigName, callee: VarName, argList: IList[VarName])
    extends PrePostInst(s"TAILCALL <$sig> $callee ($argList)") with SupportsExcClause {
    override def returnTypes(implicit context: Context) =
      (for (FuncSig(_, retTypes) <- context resolve sig) yield retTypes) getOrElse {
        throw TypeResolveException(s"Undefined function signature: $sig")
      }
    override def referencedVars = new IList(callee +: argList.toSeq)
  }

  case class Ret(rv: IList[VarName]) extends PrePostInst(s"RET ($rv)") {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = rv
  }
  case class RetVoid() extends PrePostInst("RETVOID") {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList()
  }
  case class Throw(exc: VarName) extends PrePostInst(s"THROW $exc") {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList(exc)
  }
  case class LandingPad() extends PrePostInst("LANDINGPAD") {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList()
  }

  // Aggregate Type Operations
  // ------------------------------------------------------------

  // Struct Operations

  case class ExtractValue(strTy: TypeName, index: Int, opnd: VarName)
    extends PrePostInst(s"EXTRACTVALUE <$strTy $index> $opnd") {
    override def returnTypes(implicit context: Context) = context resolve strTy match {
      case Some(TypeCtor.Struct(types)) =>
        if (types.size > index) IList(context typeDef types(index)) else
          throw TypeResolveException(s"Struct type $strTy does not have an element #$index")
      case Some(_) => throw TypeResolveException(s"Not a struct type: $strTy")
      case None => throw TypeResolveException(s"Undefined type: $strTy")
    }
    override def referencedVars = IList(opnd)
  }
  case class InsertValue(strTy: TypeName, index: Int, opnd: VarName, newVal: VarName)
    extends PrePostInst(s"INSERTVALUE <$strTy $index> $opnd $newVal") {
    override def returnTypes(implicit context: Context) = IList(strTy)
    override def referencedVars = IList(opnd, newVal)
  }

  // Vector Operations

  case class ExtractElement(vecTy: TypeName, indTy: TypeName, opnd: VarName, index: VarName)
    extends PrePostInst(s"EXTRACTELEMENT <$vecTy $indTy> $opnd $index") {
    override def returnTypes(implicit context: Context) = context resolve vecTy match {
      case Some(TypeCtor.Vector(t, _)) => IList(context typeDef t)
      case Some(TypeCtor.Array(t, _)) => IList(context typeDef t)
      case Some(_) => throw TypeResolveException(s"Not a vector or array type: $vecTy")
      case None => throw TypeResolveException(s"Undefined type: $vecTy")
    }
    override def referencedVars = IList(opnd, index)
  }
  case class InsertElement(vecTy: TypeName, indTy: TypeName, opnd: VarName, index: VarName, newVal: VarName)
    extends PrePostInst(s"INSERTELEMENT <$vecTy $indTy> $opnd $index $newVal") {
    override def returnTypes(implicit context: Context) = IList(vecTy)
    override def referencedVars = IList(opnd, index, newVal)
  }
  case class ShuffleVector(vecTy: TypeName, maskTy: TypeName, vec1: VarName, vec2: VarName, mask: VarName)
    extends PrePostInst(s"SHUFFLEVECTOR <$vecTy $maskTy> $vec1 $vec2 $mask") {
    override def returnTypes(implicit context: Context) = context resolve vecTy match {
      case Some(TypeCtor.Vector(t, _)) =>
        context resolve maskTy match {
          case Some(TypeCtor.Vector(_, n)) => IList(context typeDef TypeCtor.Vector(t, n))
          case Some(_) => throw TypeResolveException(s"Not a vector type: $maskTy")
          case None => throw TypeResolveException(s"Undefined type: $maskTy")
        }
      case Some(_) => throw TypeResolveException(s"Not a vector type: $vecTy")
      case None => throw TypeResolveException(s"Undefined type: $vecTy")
    }
    override def referencedVars = IList(vec1, vec2, mask)
  }

  // Memory Operations
  // ------------------------------------------------------------

  // Memory Allocation

  case class New(allocTy: TypeName) extends PrePostInst(s"NEW <$allocTy>") with SupportsExcClause {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef TypeCtor.Ref(allocTy))
    override def referencedVars = IList()
  }
  case class NewHybrid(allocTy: TypeName, len: Int, length: VarName)
    extends PrePostInst(s"NEWHYBRID <$allocTy $len> $length") with SupportsExcClause {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef TypeCtor.Ref(allocTy))
    override def referencedVars = IList(length)
  }
  case class Alloca(allocTy: TypeName)
    extends PrePostInst(s"ALLOCA <$allocTy>") with SupportsExcClause {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef TypeCtor.IRef(allocTy))
    override def referencedVars = IList()
  }
  case class AllocaHybrid(allocTy: TypeName, len: Int, length: VarName)
    extends PrePostInst(s"ALLOCAHYBRID <$allocTy $len> $length") with SupportsExcClause {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef TypeCtor.IRef(allocTy))
    override def referencedVars = IList(length)
  }

  // Memory Addressing

  case class GetIRef(referentTy: TypeName, opnd: VarName)
    extends PrePostInst(s"GETIREF <$referentTy> $opnd") {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef TypeCtor.IRef(referentTy))
    override def referencedVars = IList(opnd)
  }
  case class GetFieldIRef(ptr: Boolean, referentTy: TypeName, index: Int, opnd: VarName)
    extends PrePostInst(s"GETFIELDIREF${?(ptr, "PTR")} <$referentTy $index> $opnd") {
    override def returnTypes(implicit context: Context) = context resolve referentTy match {
      case Some(TypeCtor.Struct(types)) =>
        if (types.size > index)
          IList(context typeDef (if (ptr) TypeCtor.UPtr(types(index)) else TypeCtor.IRef(types(index))))
        else throw TypeResolveException(s"Struct type $referentTy does not have an element #$index")
      case Some(_) => throw TypeResolveException(s"Not a struct or hybrid type: $referentTy")
      case None => throw TypeResolveException(s"Undefined type: $referentTy")
    }
    override def referencedVars = IList(opnd)
  }
  case class GetElemIRef(
    ptr: Boolean, referentTy: TypeName, indexTy: TypeName, opnd: VarName, index: VarName
  ) extends PrePostInst(s"GETELEMIREF${?(ptr, "PTR")} <$referentTy $indexTy> $opnd $index") {
    override def returnTypes(implicit context: Context) = context resolve referentTy match {
      case Some(TypeCtor.Array(t, _)) =>
        IList(context typeDef (if (ptr) TypeCtor.UPtr(t) else TypeCtor.IRef(t)))
      case Some(_) => throw TypeResolveException(s"Not an array type: $referentTy")
      case None => throw TypeResolveException(s"Undefined type: $referentTy")
    }
    override def referencedVars = IList(opnd, index)
  }
  case class ShiftIRef(
    ptr: Boolean, referentTy: TypeName, offsetTy: TypeName, opnd: VarName, offset: VarName
  ) extends PrePostInst(s"SHIFTIREF${?(ptr, "PTR")} <$referentTy $offsetTy> $opnd $offset") {
    override def returnTypes(implicit context: Context) =
      IList(context typeDef (if (ptr) TypeCtor.UPtr(referentTy) else TypeCtor.IRef(referentTy)))
    override def referencedVars = IList(opnd, offset)
  }
  case class GetFixedPartIRef(ptr: Boolean, referentTy: TypeName, opnd: VarName)
    extends PrePostInst(s"GETFIXEDPARTIREF${?(ptr, "PTR")} <$referentTy> $opnd") {
    override def returnTypes(implicit context: Context) = context resolve referentTy match {
      case Some(TypeCtor.Hybrid(fixedPart, _)) =>
        IList(context typeDef (if (ptr) TypeCtor.UPtr(fixedPart) else TypeCtor.IRef(fixedPart)))
      case Some(_) => throw TypeResolveException(s"Not a hybrid type: $referentTy")
      case None => throw TypeResolveException(s"Undefined type: $referentTy")
    }
    override def referencedVars = IList(opnd)
  }
  case class GetVarPartIRef(ptr: Boolean, referentTy: TypeName, opnd: VarName)
    extends PrePostInst(s"GETVARPARTIREF${?(ptr, "PTR")} <$referentTy> $opnd") {
    override def returnTypes(implicit context: Context) = context resolve referentTy match {
      case Some(TypeCtor.Hybrid(_, varPart)) =>
        IList(context typeDef (if (ptr) TypeCtor.UPtr(varPart) else TypeCtor.IRef(varPart)))
      case Some(_) => throw TypeResolveException(s"Not a hybrid type: $referentTy")
      case None => throw TypeResolveException(s"Undefined type: $referentTy")
    }
    override def referencedVars = IList(opnd)
  }

  case class Load(ptr: Boolean, ord: MemoryOrder.Value, locTy: TypeName, loc: VarName)
    extends PrePostInst(s"LOAD${?(ptr, "PTR")} $ord <$locTy> $loc") with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList(locTy)
    override def referencedVars = IList(loc)
  }
  case class Store(ptr: Boolean, ord: MemoryOrder.Value, locTy: TypeName, loc: VarName, newVal: VarName)
    extends PrePostInst(s"STORE${?(ptr, "PTR")} $ord <$locTy> $loc $newVal") with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList(loc, newVal)
  }
  case class CmpXchg(
    ptr: Boolean,
    weak: Boolean,
    ordSucc: MemoryOrder.Value,
    ordFail: MemoryOrder.Value,
    locTy: TypeName,
    loc: VarName,
    expected: VarName,
    desired: VarName
  ) extends PrePostInst(
    s"CMPXCHG${?(ptr, "PTR")}${?(weak, "WEAK")} $ordSucc $ordFail <$locTy> $loc $expected $desired")
    with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList(
      context resolve locTy match {
        case Some(TypeCtor.WeakRef(t)) => context typeDef TypeCtor.Ref(t)
        case _ => locTy
      },
      context typeDef TypeCtor.Int(1)
    )
    override def referencedVars = IList(loc, expected, desired)
  }
  case class AtomicRMW(
    ptr: Boolean,
    ord: MemoryOrder.Value,
    op: AtomicRMWOptr.Value,
    locTy: TypeName,
    loc: VarName,
    opnd: VarName
  ) extends PrePostInst(s"ATOMICRMW${?(ptr, "PTR")} $ord $op <$locTy> $loc $opnd")
    with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList(
      context resolve locTy match {
        case Some(TypeCtor.WeakRef(t)) => context typeDef TypeCtor.Ref(t)
        case _ => locTy
      })
    override def referencedVars = IList(loc, opnd)
  }
  case class Fence(ord: MemoryOrder.Value) extends PrePostInst(s"FENCE $ord") {
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars = IList()
  }

  // Traps
  // ------------------------------------------------------------

  case class Trap(
    retTy: IList[TypeName],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PrePostInst(s"TRAP <$retTy>" + ?(keepaliveClause)) with SupportsExcClause {
    def this(retTy: IList[TypeName]) = this(retTy, None)
    def this(retTy: IList[TypeName], keepaliveClause: KeepaliveClause) =
      this(retTy, Some(keepaliveClause))
    override def preExcString = s"TRAP <$retTy>"
    override def postExcString = ?(keepaliveClause)
    override def returnTypes(implicit context: Context) = retTy
    override def referencedVars = new IList(keepaliveClause.toSeq.flatMap(_.vars))
  }

  case class PreWatchPoint(
    wpid: Int,
    retTy: IList[TypeName],
    dis: LabelName,
    ena: LabelName,
    exc: Option[LabelName],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PreInst {
    override def returnTypes(implicit context: Context) = retTy
    override def toString =
      s"WATCHPOINT $wpid <$retTy> $dis $ena" + ?(exc map (l => s"WPEXC($l)")) + ?(keepaliveClause)
    override def referencedVars = new IList(keepaliveClause.toSeq.flatMap(_.vars))
    override def possibleJumps = new IList(Seq(dis, ena) ++ exc)
  }

  case class PostWatchPoint(
    wpid: Int,
    retTy: IList[TypeName],
    dis: DestClause,
    ena: DestClause,
    exc: Option[DestClause],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PostInst {
    override def returnTypes(implicit context: Context) = retTy
    override def toString =
      s"WATCHPOINT $wpid <$retTy> $dis $ena" + ?(exc map (l => s"WPEXC($l)")) + ?(keepaliveClause)
    override def referencedVars =
      new IList(dis.args ++ ena.args ++ exc.toSeq.flatMap(_.args) ++ keepaliveClause.toSeq.flatMap(_.vars))
  }

  // Unsafe Native Call

  case class CCall(
    callConv: Flag,
    calleeTy: TypeName,
    sig: FuncSigName,
    callee: VarName,
    argList: IList[VarName],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PrePostInst(s"CCALL $callConv <$calleeTy $sig> $callee ($argList)" + ?(keepaliveClause)) {
    def this(
      callConv: Flag,
      calleeTy: TypeName,
      sig: FuncSigName,
      callee: VarName,
      argList: IList[VarName]
    ) = this(callConv, calleeTy, sig, callee, argList, None)
    def this(
      callConv: Flag,
      calleeTy: TypeName,
      sig: FuncSigName,
      callee: VarName,
      argList: IList[VarName],
      keepaliveClause: KeepaliveClause
    ) = this(callConv, calleeTy, sig, callee, argList, Some(keepaliveClause))
    override def returnTypes(implicit context: Context) =
      (for (FuncSig(_, retTypes) <- context resolve sig) yield retTypes) getOrElse {
        throw TypeResolveException(s"Undefined function signature: $sig")
      }
    override def referencedVars =
      new IList(callee +: (argList.toSeq ++ keepaliveClause.toSeq.flatMap(_.vars)))
  }

  // Thread and Stack
  // ------------------------------------------------------------

  case class NewThread(stack: VarName, newStackClause: NewStackClause)
    extends PrePostInst(s"NEWTHREAD $stack $newStackClause") with SupportsExcClause {
    override def returnTypes(implicit context: Context) = IList(context typeDef TypeCtor.ThreadRef)
    override def referencedVars = new IList(stack +: newStackClause.referencedVars.toSeq)
  }

  case class SwapStack(
    swappee: VarName,
    curStackClause: CurStackClause,
    newStackClause: NewStackClause,
    keepaliveClause: Option[KeepaliveClause]
  ) extends PrePostInst(s"SWAPSTACK $swappee $curStackClause $newStackClause" + ?(keepaliveClause))
    with SupportsExcClause {
    def this(swappee: VarName, curStackClause: CurStackClause, newStackClause: NewStackClause) =
      this(swappee, curStackClause, newStackClause, None)
    def this(swappee: VarName, curStackClause: CurStackClause, newStackClause: NewStackClause,
      keepaliveClause: KeepaliveClause) =
      this(swappee, curStackClause, newStackClause, Some(keepaliveClause))
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars =
      new IList(swappee +: (newStackClause.referencedVars.toSeq ++ keepaliveClause.toSeq.flatMap(_.vars)))
    override def preExcString = s"SWAPSTACK $swappee $curStackClause $newStackClause"
    override def postExcString = ?(keepaliveClause)
  }

  // Common Instructions
  // ------------------------------------------------------------

  case class CommInst(
    instName: GlobalVarName,
    flagList: Option[IList[Flag]],
    typeList: Option[IList[TypeName]],
    funcSigList: Option[IList[FuncSigName]],
    argList: Option[IList[VarName]],
    keepaliveClause: Option[KeepaliveClause]
  ) extends PrePostInst(
    s"COMMINST $instName" +
      ?(flagList map ("[" + _ + "]")) +
      ?(typeList map ("<" + _ + ">")) +
      ?(funcSigList map ("<[" + _ + "]>")) +
      ?(argList map ("(" + _ + ")")) +
      ?(keepaliveClause)
  ) with SupportsExcClause {
    // FIXME: Need source for comminst return types
    override def returnTypes(implicit context: Context) = IList()
    override def referencedVars =
      new IList(instName +: (argList.toSeq.flatten ++ keepaliveClause.toSeq.flatMap(_.vars)))
    override def preExcString = s"COMMINST $instName" +
      ?(flagList map ("[" + _ + "]")) +
      ?(typeList map ("<" + _ + ">")) +
      ?(funcSigList map ("<[" + _ + "]>")) +
      ?(argList map ("(" + _ + ")"))
    override def postExcString = ?(keepaliveClause)
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

case class KeepaliveClause(vars: IList[LocalVarName]) {
  override def toString = s"KEEPALIVE(${vars mkString " "})"
}
case class SwitchCase[T](value: VarName, dest: T) {
  override def toString = s"$value: $dest;"
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

sealed abstract class NewStackClause {
  def referencedVars: IList[VarName]
}
object NewStackClause {
  case class PassValue(argTy: TypeName, arg: VarName) extends NewStackClause {
    override def toString = s"PASS_VALUE <$argTy> $arg"
    override def referencedVars = IList(arg)
  }
  case class PassVoid() extends NewStackClause {
    override def toString = "PASS_VOID"
    override def referencedVars = IList()
  }
  case class ThrowExc(exc: VarName) extends NewStackClause {
    override def toString = s"THROW_EXC $exc"
    override def referencedVars = IList(exc)
  }
}

case class TypeResolveException(message: String) extends Exception(message)