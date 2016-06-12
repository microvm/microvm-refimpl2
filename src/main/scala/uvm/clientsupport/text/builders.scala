package uvm.clientsupport.text

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.JavaConversions._
import java.util

class BundleBuilder(baseName: String) extends Context {
  protected val typeDefs = new util.LinkedHashMap[TypeName, TypeCtor]
  protected val funcSigDefs = new util.LinkedHashMap[FuncSigName, FuncSig]
  protected val constDefs = new util.LinkedHashMap[GlobalVarName, Const]
  protected val globalCellDefs = new util.LinkedHashMap[GlobalVarName, TypeName]
  protected val funcDecls = new util.LinkedHashMap[GlobalVarName, FuncSigName]
  protected val funcVers = new util.LinkedHashMap[FuncVerName, FunctionBuilder]
  protected val funcExpDefs = new util.LinkedHashMap[GlobalVarName, Expose]
  protected val comments = new util.LinkedHashMap[GlobalName, String]

  protected val usedNames = mutable.Set.empty[String]
  private var nextTypeNumber = 0
  private var nextSigNumber = 0
  private var nextConstNumber = 0
  private var nextGlobalCellNumber = 0
  private var typePrefix = baseName
  private var constPrefix = baseName

  override def resolve(typeName: TypeName) = {
    typeDefs.get(typeName) match {
      case next: TypeName => resolve(next)
      case res => Option(res)
    }
  }
  override def resolve(funcSigName: FuncSigName) =
    Option(funcSigDefs.get(funcSigName))

  // Name generation
  // ------------------------------------------------------------

  private def generateName(prefix: String, n: String): String = {
    var n_ = prefix + "." + n
    while (usedNames contains n_) n_ += "_"
    usedNames += n_
    n_
  }

  def newVarName(): GlobalVarName = {
    var nextName: String = null
    do {
      nextName = "global" + nextGlobalCellNumber
      nextGlobalCellNumber += 1
    } while (usedNames contains nextName)
    newVarName(nextName)
  }

  def newVarName(name: String): GlobalVarName = newVarName(name, baseName)

  def newVarName(name: String, prefix: String): GlobalVarName =
    GlobalVarName(generateName(prefix, name))

  def newTypeName(): TypeName = {
    var nextName: String = null
    do {
      nextName = "type" + nextTypeNumber
      nextTypeNumber += 1
    } while (usedNames contains nextName)
    newTypeName(nextName)
  }

  def newTypeName(name: String) = TypeName(generateName(typePrefix, name))

  def newFuncSigName(): FuncSigName = {
    var nextName: String = null
    do {
      nextName = "sig" + nextSigNumber
      nextSigNumber += 1
    } while (usedNames contains nextName)
    newFuncSigName(nextName)
  }

  def newFuncSigName(name: String) = FuncSigName(generateName(baseName, name))

  def newFuncVerName(function: GlobalVarName): FuncVerName = {
    val nameStr = function.name + "_" + java.util.UUID.randomUUID()
    usedNames += nameStr
    FuncVerName(nameStr)
  }

  def newFuncVerName(name: String): FuncVerName = FuncVerName(generateName(baseName, name))

  def setTypePrefix(newPrefix: String): Unit = typePrefix = newPrefix

  def setConstPrefix(newPrefix: String): Unit = constPrefix = newPrefix

  // Code generation
  // ------------------------------------------------------------

  private def descriptiveTypeName(ctor: TypeCtor): TypeName = ctor match {
    case TypeCtor.Int(n) => newTypeName("i" + n)
    case TypeCtor.Singletons.Float => newTypeName("float")
    case TypeCtor.Singletons.Double => newTypeName("double")
    case TypeCtor.Singletons.StackRef => newTypeName("stack")
    case TypeCtor.Singletons.ThreadRef => newTypeName("thread")
    case TypeCtor.Singletons.Void => newTypeName("void")
    case TypeCtor.Ref(TypeCtor.Singletons.Void) => newTypeName("voidref")
    case TypeCtor.IRef(TypeCtor.Singletons.Void) => newTypeName("voidiref")
    case TypeCtor.UPtr(TypeCtor.Singletons.Void) => newTypeName("voidptr")
    case TypeCtor.Ref(TypeCtor.Int(n)) => newTypeName("i" + n + "ref")
    case TypeCtor.IRef(TypeCtor.Int(n)) => newTypeName("i" + n + "iref")
    case TypeCtor.UPtr(TypeCtor.Int(n)) => newTypeName("i" + n + "ptr")
    case _ => newTypeName()
  }

  override def typeDef(ctor: TypeCtor): TypeName = ctor match {
    case name: TypeName if resolve(name).isDefined => typeDef(resolve(name).get)
    case _ =>
      typeDefs collectFirst {
        case (name, existing) if ctor == existing => name
      } getOrElse
        typeDef (descriptiveTypeName (ctor), ctor)
  }

  def typeDef(name: TypeName, ctor: TypeCtor): TypeName = {
    typeDefs.put(name, ctor)
    name
  }

  override def funcSig(args: IList[TypeName], ret: IList[TypeName]): FuncSigName =
    funcSigDefs collectFirst {
      case (name, sig) if sig.retTy == ret && sig.paramTy.toSeq == args.toSeq => name
    } getOrElse funcSig(newFuncSigName(), args, ret)

  def funcSig(name: FuncSigName, args: IList[TypeName], ret: IList[TypeName]): FuncSigName = {
    funcSigDefs.put(name, new FuncSig(args, ret))
    name
  }

  override def constDef(c: Const): GlobalVarName =
    constDefs collectFirst { case (name, existing) if c == existing => name } getOrElse {
      var nextName: String = null
      do {
        nextName = "const" + nextConstNumber
        nextConstNumber += 1
      } while (usedNames contains nextName)
      constDef(newVarName(nextName, constPrefix), c)
    }

  def constDef(name: GlobalVarName, c: Const): GlobalVarName = {
    constDefs.put(name, c)
    name
  }

  def globalCell(`type`: TypeName): GlobalVarName = globalCell(newVarName(), `type`)

  def globalCell(name: GlobalVarName, `type`: TypeName): GlobalVarName = {
    globalCellDefs.put(name, `type`)
    name
  }

  def funcDecl(name: GlobalVarName, sig: FuncSigName): GlobalVarName = {
    funcDecls.put(name, sig)
    name
  }

  def exposeFunc(
    exposeName: GlobalVarName,
    funcName: GlobalVarName,
    callConv: Flag,
    cookie: GlobalVarName
  ): GlobalVarName = {
    funcExpDefs.put(exposeName, new Expose(funcName, callConv, cookie))
    exposeName
  }

  def newFuncVersion(
    funcName: GlobalVarName,
    paramTy: IList[TypeName],
    retTy: IList[TypeName]
  ): FunctionBuilder = newFuncVersion(funcName, newFuncVerName(funcName), paramTy, retTy)

  def newFuncVersion(
    funcName: GlobalVarName,
    versionName: FuncVerName,
    paramTy: IList[TypeName],
    retTy: IList[TypeName]
  ): FunctionBuilder = {
    val sig = funcSig(paramTy, retTy)
    val builder = new FunctionBuilderImpl(
      this, funcName, sig, versionName, new IList(paramTy.toSeq.indices.map("param"+_)))
    funcVers.put(versionName, builder)
    builder
  }

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    paramNames: IList[String]
  ): FunctionBuilder = newFuncVersion(funcName, sigName, newFuncVerName(funcName), paramNames)

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    versionName: FuncVerName,
    paramNames: IList[String]
  ): FunctionBuilder = {
    val builder = new FunctionBuilderImpl(this, funcName, sigName, versionName, paramNames)
    funcVers.put(versionName, builder)
    builder
  }

  def comment(name: GlobalName, comment: String): Unit = comments.put(name, comment)

  def build(): Bundle = new Bundle(
    cTypeDefs = typeDefs,
    cFuncSigDefs = funcSigDefs,
    cConstDefs = constDefs,
    cGlobalCellDefs = globalCellDefs,
    cFuncDecls = funcDecls,
    cFuncVers = funcVers map { case (k, v) => k -> v.build() },
    cFuncExpDefs = funcExpDefs,
    cComments = comments
  )
}

trait FunctionBuilder extends Context {
  def functionName: GlobalVarName
  def functionSignature: FuncSigName
  def versionName: FuncVerName
  def paramNames: IList[LocalVarName]
  def newVarName(): LocalVarName
  def newVarName(name: String): LocalVarName
  def newLabelName(): LabelName
  def newLabelName(name: String): LabelName
  def currentBlockLabel: LabelName
  def startNewBlock(): LabelName
  def startNewBlock(label: LabelName): LabelName
  def inst(inst: PreInst): LocalVarName
  def inst(names: IList[LocalVarName], inst: PreInst): Unit
  def preBuild(): PreFuncVer
  def build(): PostFuncVer
}

private[text] class FunctionBuilderImpl(
  context: Context,
  override val functionName: GlobalVarName,
  override val functionSignature: FuncSigName,
  override val versionName: FuncVerName,
  val paramNameBases: IList[String]
) extends FunctionBuilder {

  type Block = (LabelName, mutable.ArrayBuffer[(IList[LocalVarName], PreInst)])

  protected val blocks = mutable.ArrayBuffer.empty[Block]
  protected val usedNames = mutable.Set.empty[String]
  private var nextVarNumber = 0
  private var nextLabelNumber = 0
  private var currentBlock: Block = createBlock(newLabelName("entry"))

  override lazy val paramNames = new IList(paramNameBases map newVarName)

  private def generateName(n: String): String = {
    var n_ = n
    while (usedNames contains n_) n_ += "_"
    usedNames += n_
    n_
  }

  override def newVarName(): LocalVarName = {
    var nextName: String = null
    do {
      nextName = "var" + nextVarNumber
      nextVarNumber += 1
    } while (usedNames contains nextName)
    newVarName(nextName)
  }

  override def newVarName(name: String): LocalVarName = LocalVarName(generateName(name))

  override def newLabelName(): LabelName = {
    var nextName: String = null
    do {
      nextName = "label" + nextLabelNumber
      nextLabelNumber += 1
    } while (usedNames contains nextName)
    newLabelName(nextName)
  }

  override def newLabelName(name: String): LabelName = LabelName(generateName(name))

  private def createBlock(name: LabelName): Block = {
    val newBlock = (name, mutable.ArrayBuffer.empty[(IList[LocalVarName], PreInst)])
    blocks += newBlock
    newBlock
  }

  override def currentBlockLabel: LabelName = currentBlock._1

  override def startNewBlock(): LabelName = startNewBlock(newLabelName())

  override def startNewBlock(label: LabelName): LabelName = {
    currentBlock = createBlock(label)
    label
  }

  override def inst(inst: PreInst): LocalVarName = {
      val name = newVarName()
      currentBlock._2 += (IList(name) -> inst)
      name
  }

  override def inst(names: IList[LocalVarName], inst: PreInst): Unit = {
    currentBlock._2 += (names -> inst)
  }

  override def preBuild() = new PreFuncVer(
    functionName, functionSignature, paramNames,
    new IList(blocks map { case (name, insts) => new PreBasicBlock(name, new IList(insts)) })
  )

  def convertFunction(preFunc: PreFuncVer)(implicit context: Context): PostFuncVer = {

    // Step 1: Cache some data about each block (incoming jumps, local variable names, etc.)
    case class BlockData(
      incoming: Set[LabelName],
      locals: Map[LocalVarName, () => TypeCtor],
      args: mutable.Map[LocalVarName, Option[TypeCtor]])
    val blockData = blocks.map {
      case (label, insts) =>
        val locals = (for ((labels, inst) <- insts; (l, i) <- labels.toSeq.zipWithIndex) yield {
          lazy val x = context.resolve(inst.returnTypes get i).get
          l -> (() => x)
        }).toMap
        label -> BlockData(
          incoming = blocks.filter(_._2 flatMap (_._2.possibleJumps) contains label).map(_._1).toSet,
          locals = locals,
          args = mutable.Map((for ((_, inst) <- insts; label <- inst.referencedVars) yield label) collect {
            case v: LocalVarName if !locals.contains(v) => v -> None
          }: _*)
        )
    }.toMap
    for ((name, ty) <- paramNames.toSeq zip resolve(functionSignature).get.paramTy.toSeq) {
      blockData(blocks(0)._1).args.put(name, Some(ty))
    }

    // Step 2: Determine block arities and argument types
    // Iteratively propagate passthrough args and arg types until a fixpoint is reached.
    var continue = true
    while (continue) {
      continue = false
      for (
        (to, BlockData(froms, _, args)) <- blockData;
        from <- froms;
        BlockData(_, fromLocals, fromArgs) = blockData(from);
        arg <- args.keys
      ) {
        (fromLocals ++ fromArgs.collect{case (k, Some(v)) => (k, () => v)}).get(arg) match {
          case Some(fn) =>
            val ctor = fn()
            args(arg) match {
              case Some(current) if ctor == current => // Do nothing.
              case Some(current) =>
                // FIXME: This may throw unnecessary exceptions in the case of TypeNames inside TypeCtors
                // For example: iref<@a> and iref<@b> would be a type mismatch even if @a and @b
                // are the same type. Context.resolve doesn't check nested names yet.
                throw new IllegalStateException(
                  s"Pre-SSA variable type mismatch for $arg in ($from, $to): $ctor != $current")
              case None =>
                continue = true
                args.put(arg, Some(ctor))
            }
          case None =>
            if (!(fromArgs contains arg)) {
              continue = true
              fromArgs.put(arg, None)
            }
        }
      }
    }
    // Step 3: Process preinsts into postinsts, renaming reused labels along the way.
    val newBlocks = blocks map { case (label, insts) =>
      import Inst._
      val BlockData(_, locals, args) = blockData(label)
      val used = mutable.Set(args.keys.toSeq: _*)
      val replacements = mutable.Map.empty[LocalVarName, VarName]
      @tailrec def gensym(n: Int = 0): LocalVarName = {
        val sym = LocalVarName(s"v$n")
        if ((used contains sym) || (locals contains sym)) gensym(n+1) else {
          used += sym
          sym
        }
      }
      @tailrec def r(v: VarName): VarName = v match {
        case lv: LocalVarName => replacements get lv match {
          case Some(lv2) => r(lv2)
          case None => v
        }
        case _ => v
      }
      def l(n: LocalVarName): LocalVarName =
        if (used contains n) {
          val n2 = gensym()
          replacements.put(n, n2)
          n2
        } else { used += n; n }
      def mapKeepalive(ka: Option[KeepaliveClause]) =
        ka map (c => KeepaliveClause(new IList(c.vars map r collect {case lv: LocalVarName => lv})))
      def mapNewStackClause(c: NewStackClause): NewStackClause = c match {
        case NewStackClause.PassValue(argTy, arg) => NewStackClause.PassValue(argTy, r(arg))
        case i: NewStackClause.PassVoid => i
        case NewStackClause.ThrowExc(exc) => NewStackClause.ThrowExc(r(exc))
      }
      def mapDest(lbl: LabelName): DestClause =
        DestClause(lbl, new IList(blockData(lbl).args.keys.toSeq.sortBy(_.name).map(r)))
      def mapInst(names: Traversable[LocalVarName], inst: PreInst):
        Option[(IList[LocalVarName], PostInst)] = inst match {
          case ppinst: PrePostInst =>
            val postInst: PostInst = ppinst match {
              case BinOp(op, opndTy, op1, op2) => BinOp(op, opndTy, r(op1), r(op2))
              case Cmp(op, opndTy, op1, op2) => Cmp(op, opndTy, r(op1), r(op2))
              case Conv(op, fromTy, toTy, opnd) => Conv(op, fromTy, toTy, r(opnd))
              case Select(condTy, opndTy, cond, ifTrue, ifFalse) =>
                Select(condTy, opndTy, r(cond), r(ifTrue), r(ifFalse))
              case Call(sig, callee, argList, ka) =>
                Call(sig, r(callee), new IList(argList map r), mapKeepalive(ka))
              case TailCall(sig, callee, argList) =>
                TailCall(sig, r(callee), new IList(argList map r))
              case Ret(rv) => Ret(new IList(rv map r))
              case i: RetVoid => i
              case Throw(exc) => Throw(r(exc))
              case i: LandingPad => i
              case ExtractValue(strTy, index, opnd) => ExtractValue(strTy, index, r(opnd))
              case InsertValue(strTy, index, opnd, newVal) =>
                InsertValue(strTy, index, r(opnd), r(newVal))
              case ExtractElement(vecTy, indTy, opnd, index) =>
                ExtractElement(vecTy, indTy, r(opnd), r(index))
              case InsertElement(vecTy, indTy, opnd, index, newVal) =>
                InsertElement(vecTy, indTy, r(opnd), r(index), r(newVal))
              case ShuffleVector(vecTy, maskTy, vec1, vec2, mask) =>
                ShuffleVector(vecTy, maskTy, r(vec1), r(vec2), r(mask))
              case i: New => i
              case NewHybrid(allocTy, len, length) => NewHybrid(allocTy, len, r(length))
              case i: Alloca => i
              case AllocaHybrid(allocTy, len, length) => AllocaHybrid(allocTy, len, r(length))
              case GetIRef(refTy, opnd) => GetIRef(refTy, r(opnd))
              case GetFieldIRef(ptr, refTy, index, opnd) => GetFieldIRef(ptr, refTy, index, r(opnd))
              case GetElemIRef(ptr, refTy, indexTy, opnd, index) =>
                GetElemIRef(ptr, refTy, indexTy, r(opnd), r(index))
              case ShiftIRef(ptr, refTy, offTy, opnd, offset) =>
                ShiftIRef(ptr, refTy, offTy, r(opnd), r(offset))
              case GetFixedPartIRef(ptr, refTy, opnd) => GetFixedPartIRef(ptr, refTy, r(opnd))
              case GetVarPartIRef(ptr, refTy, opnd) => GetVarPartIRef(ptr, refTy, r(opnd))
              case Load(ptr, ord, locTy, loc) => Load(ptr, ord, locTy, r(loc))
              case Store(ptr, ord, locTy, loc, newVal) => Store(ptr, ord, locTy, r(loc), r(newVal))
              case i: CmpXchg => i.copy(loc = r(i.loc), expected = r(i.expected), desired = r(i.desired))
              case i: AtomicRMW => i.copy(loc = r(i.loc), opnd = r(i.opnd))
              case i: Fence => i
              case Trap(retTy, ka) => Trap(retTy, mapKeepalive(ka))
              case i: CCall => i.copy(callee = r(i.callee), argList = new IList(i.argList map r),
                keepaliveClause = mapKeepalive(i.keepaliveClause))
              case NewThread(stack, nsClause) => NewThread(r(stack), mapNewStackClause(nsClause))
              case SwapStack(swappee, csClause, nsClause, ka) =>
                SwapStack(r(swappee), csClause, mapNewStackClause(nsClause), mapKeepalive(ka))
              case i: CommInst => i.copy(argList = i.argList map (l => new IList(l map r)),
                keepaliveClause = mapKeepalive(i.keepaliveClause))
            }
            Some(new IList(names map l) -> postInst)
          case Id(_, v) =>
            used += names.head
            replacements.put(names.head, v)
            None
          case PreExcClause(subInst, nor, exc) =>
            mapInst(Nil, subInst.asInstanceOf[PreInst]) map {
              case (_, postSub) =>
                val post = PostExcClause(postSub.asInstanceOf[SupportsExcClause],
                  mapDest(nor), mapDest(exc))
                (new IList(names map l), post)
            }
          case PreBranch(dest) =>
            Some((IList[LocalVarName](), PostBranch(mapDest(dest))))
          case PreBranch2(cond, ifTrue, ifFalse) =>
            Some((IList[LocalVarName](), PostBranch2(r(cond), mapDest(ifTrue), mapDest(ifFalse))))
          case PreSwitch(opndTy, opnd, defDest, cases) =>
            Some((IList[LocalVarName](), PostSwitch(opndTy, r(opnd), mapDest(defDest),
              new IList(cases map (c => SwitchCase(r(c.value), mapDest(c.dest)))))))
          case PreWatchPoint(wpid, retTy, dis, ena, exc, ka) =>
            val post = PostWatchPoint(wpid, retTy, mapDest(dis), mapDest(ena), exc map mapDest,
              mapKeepalive(ka))
            Some((new IList(names map l), post))
        }
      PostBasicBlock(
        label = label,
        args = new IList(blockData(label).args.toSeq sortBy (_._1.name) map {
          case (name, ty) => (name, context typeDef (ty getOrElse {
              throw new IllegalStateException(s"Cannot infer type of argument $name for block $label")
            }))
        }),
        insts = new IList(insts flatMap { case (names, inst) => mapInst(names, inst) })
      )
    }

    PostFuncVer(preFunc.func, preFunc.sig, new IList(newBlocks))
  }

  override def build() = convertFunction(preBuild())(this)

  override def typeDef(ctor: TypeCtor) = context.typeDef(ctor)
  override def funcSig(args: IList[TypeName], ret: IList[TypeName]) = context.funcSig(args, ret)
  override def constDef(c: Const) = context.constDef(c)
  override def resolve(typeName: TypeName) = context.resolve(typeName)
  override def resolve(funcSigName: FuncSigName) = context.resolve(funcSigName)
}

