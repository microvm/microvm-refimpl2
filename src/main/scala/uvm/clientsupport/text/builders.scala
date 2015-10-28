package uvm.clientsupport.text

import java.util

import scala.collection.{JavaConversions, mutable}
import scala.collection.JavaConversions._

class BundleBuilder(baseName: String) {
  protected val typeDefs = mutable.AnyRefMap.empty[TypeName, TypeCtor]
  protected val funcSigDefs = mutable.AnyRefMap.empty[FuncSigName, FuncSig]
  protected val constDefs = mutable.AnyRefMap.empty[GlobalVarName, Const]
  protected val globalCellDefs = mutable.AnyRefMap.empty[GlobalVarName, TypeName]
  protected val funcDecls = mutable.AnyRefMap.empty[GlobalVarName, FuncSigName]
  protected val funcVers = mutable.AnyRefMap.empty[FuncVerName, FunctionBuilder]
  protected val funcExpDefs = mutable.AnyRefMap.empty[GlobalVarName, Expose]
  protected val comments = mutable.AnyRefMap.empty[GlobalName, String]

  protected val usedNames = mutable.Set.empty[String]
  private var nextTypeNumber = 0
  private var nextSigNumber = 0
  private var nextConstNumber = 0
  private var nextGlobalCellNumber = 0
  private var typePrefix = baseName
  private var constPrefix = baseName

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

  def newVarName(name: String) = newVarName(name, baseName)

  def newVarName(name: String, prefix: String) = GlobalVarName(generateName(prefix, name))

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
    case TypeCtor.Float() => newTypeName("float")
    case TypeCtor.Double() => newTypeName("double")
    case TypeCtor.Stack() => newTypeName("stack")
    case TypeCtor.Thread() => newTypeName("thread")
    case TypeCtor.Void() => newTypeName("void")
    case TypeCtor.Ref(TypeCtor.Void()) => newTypeName("voidref")
    case TypeCtor.IRef(TypeCtor.Void()) => newTypeName("voidiref")
    case TypeCtor.Ptr(TypeCtor.Void()) => newTypeName("voidptr")
    case TypeCtor.Ref(TypeCtor.Int(n)) => newTypeName("i" + n + "ref")
    case TypeCtor.IRef(TypeCtor.Int(n)) => newTypeName("i" + n + "iref")
    case TypeCtor.Ptr(TypeCtor.Int(n)) => newTypeName("i" + n + "ptr")
    case _ => newTypeName()
  }

  def typeDef(ctor: TypeCtor): TypeName =
    typeDefs collectFirst { case (name, existing) if ctor == existing => name } getOrElse
      typeDef(descriptiveTypeName(ctor), ctor)

  def typeDef(name: TypeName, ctor: TypeCtor): TypeName = {
    typeDefs.put(name, ctor)
    name
  }

  def funcSig(ret: TypeName, args: Traversable[TypeName]): FuncSigName =
    funcSigDefs collectFirst {
      case (name, sig) if sig.retTy == ret && sig.paramTy.toSeq == args.toSeq => name
    } getOrElse funcSig(newFuncSigName(), ret, args)

  def funcSig(ret: TypeName, args: util.List[TypeName]): FuncSigName =
    funcSig(ret, JavaConversions.asScalaBuffer(args))

  def funcSig(name: FuncSigName, ret: TypeName, args: Traversable[TypeName]): FuncSigName = {
    funcSigDefs.put(name, new FuncSig(ret, args))
    name
  }

  def funcSig(name: FuncSigName, ret: TypeName, args: util.List[TypeName]): FuncSigName =
    funcSig(name, ret, JavaConversions.asScalaBuffer(args))

  def constDef(c: Const): GlobalVarName =
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
    retTy: TypeName,
    paramTy: Traversable[TypeName]
  ): FunctionBuilder = newFuncVersion(funcName, newFuncVerName(funcName), retTy, paramTy)

  def newFuncVersion(
    funcName: GlobalVarName,
    retTy: TypeName,
    paramTy: util.List[TypeName]
  ): FunctionBuilder = newFuncVersion(funcName, retTy, JavaConversions asScalaBuffer paramTy)

  def newFuncVersion(
    funcName: GlobalVarName,
    versionName: FuncVerName,
    retTy: TypeName,
    paramTy: Traversable[TypeName]
  ): FunctionBuilder = {
    val sig = funcSig(retTy, paramTy)
    val builder = new FunctionBuilderImpl(
      this, funcName, sig, versionName, paramTy.toSeq.indices.map("param"+_))
    funcVers.put(versionName, builder)
    builder
  }

  def newFuncVersion(
    funcName: GlobalVarName,
    versionName: FuncVerName,
    retTy: TypeName,
    paramTy: util.List[TypeName]
  ): FunctionBuilder = newFuncVersion(funcName, versionName, retTy, JavaConversions asScalaBuffer paramTy)

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    paramNames: Traversable[String]
  ): FunctionBuilder = newFuncVersion(funcName, sigName, newFuncVerName(funcName), paramNames)

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    paramNames: util.List[String]
  ): FunctionBuilder = newFuncVersion(funcName, sigName, JavaConversions asScalaBuffer paramNames)

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    versionName: FuncVerName,
    paramNames: Traversable[String]
  ): FunctionBuilder = {
    val builder = new FunctionBuilderImpl(this, funcName, sigName, versionName, paramNames)
    funcVers.put(versionName, builder)
    builder
  }

  def newFuncVersion(
    funcName: GlobalVarName,
    sigName: FuncSigName,
    versionName: FuncVerName,
    paramNames: util.List[String]
  ): FunctionBuilder =
    newFuncVersion(funcName, sigName, versionName, JavaConversions asScalaBuffer paramNames)

  def build(): Bundle = new Bundle(
    cTypeDefs = typeDefs,
    cFuncSigDefs = funcSigDefs,
    cConstDefs = constDefs,
    cGlobalCellDefs = globalCellDefs,
    cFuncDecls = funcDecls,
    cFuncVers = funcVers.mapValues(_.build()),
    cFuncExpDefs = funcExpDefs,
    cComments = comments
  )
}

trait FunctionBuilder {
  def functionName: GlobalVarName
  def functionSignature: FuncSigName
  def versionName: FuncVerName
  def paramNames: util.List[LocalVarName]
  def paramName(index: Int): LocalVarName
  def newVarName(): LocalVarName
  def newVarName(name: String): LocalVarName
  def newLabelName(): LabelName
  def newLabelName(name: String): LabelName
  def typeDef(ctor: TypeCtor): TypeName
  def constDef(c: Const): VarName
  def currentBlockLabel: LabelName
  def startNewBlock(): LabelName
  def startNewBlock(label: LabelName): LabelName
  def inst(inst: NameableInst): LocalVarName
  def inst(name: LocalVarName, inst: Inst): LocalVarName
  def build(): FuncVer
}

private[text] class FunctionBuilderImpl(
  bundleBuilder: BundleBuilder,
  override val functionName: GlobalVarName,
  override val functionSignature: FuncSigName,
  override val versionName: FuncVerName,
  cParamNames: TraversableOnce[String]
) extends FunctionBuilder {

  type Block = (LabelName, mutable.ArrayBuffer[NameableInst])

  protected val blocks = mutable.ArrayBuffer.empty[Block]
  protected val usedNames = mutable.Set.empty[String]
  private var nextVarNumber = 0
  private var nextLabelNumber = 0
  private var currentBlock: Block = createBlock(newLabelName("entry"))

  override lazy val paramNames = Helpers.buildList(cParamNames map newVarName)

  override def paramName(index: Int): LocalVarName = paramNames get index

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

  override def typeDef(ctor: TypeCtor): TypeName = bundleBuilder.typeDef(ctor)

  override def constDef(c: Const): VarName = bundleBuilder.constDef(c)

  private def createBlock(name: LabelName): Block = {
    val newBlock = (name, mutable.ArrayBuffer.empty[NameableInst])
    blocks add newBlock
    newBlock
  }

  override def currentBlockLabel: LabelName = currentBlock._1

  override def startNewBlock(): LabelName = startNewBlock(newLabelName())

  override def startNewBlock(label: LabelName): LabelName = {
    currentBlock = createBlock(label)
    label
  }

  override def inst(inst: NameableInst): LocalVarName = inst match {
    case NamedInst(name, _) =>
      currentBlock._2 add inst
      name
    case i: Inst =>
      val name = newVarName()
      currentBlock._2 add NamedInst(name, i)
      name
  }

  override def inst(name: LocalVarName, inst: Inst): LocalVarName = {
    currentBlock._2 add NamedInst(name, inst)
    name
  }

  override def build(): FuncVer = new FuncVer(
    functionName, functionSignature, paramNames,
    blocks map { case (name, insts) => new BasicBlock(name, insts) }
  )
}

