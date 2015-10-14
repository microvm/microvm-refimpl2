package uvm.clientsupport.text

import java.util

import scala.collection.{JavaConversions, mutable}
import scala.collection.JavaConversions._

class BundleBuilder(baseName: String) {
  val typeDefs = mutable.AnyRefMap.empty[TypeName, TypeCtor]
  val funcSigDefs = mutable.AnyRefMap.empty[FuncSigName, FuncSig]
  val constDefs = mutable.AnyRefMap.empty[GlobalVarName, Const]
  val globalCellDefs = mutable.AnyRefMap.empty[GlobalVarName, TypeName]
  val funcDecls = mutable.AnyRefMap.empty[GlobalVarName, FuncSigName]
  val funcVers = mutable.AnyRefMap.empty[FuncVerName, FunctionBuilder]
  val funcExpDefs = mutable.AnyRefMap.empty[GlobalVarName, Expose]
  val comments = mutable.AnyRefMap.empty[GlobalName, String]

  val usedNames = mutable.Set.empty[String]
  private var nextTypeNumber = 0
  private var nextSigNumber = 0
  private var nextConstNumber = 0
  private var nextGlobalCellNumber = 0

  // Name generation
  // ------------------------------------------------------------

  private def generateName(n: String): String = {
    var n_ = baseName + "." + n
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

  def newVarName(name: String) = GlobalVarName(generateName(name))

  def newTypeName(): TypeName = {
    var nextName: String = null
    do {
      nextName = "type" + nextTypeNumber
      nextTypeNumber += 1
    } while (usedNames contains nextName)
    newTypeName(nextName)
  }

  def newTypeName(name: String) = TypeName(generateName(name))

  def newFuncSigName(): FuncSigName = {
    var nextName: String = null
    do {
      nextName = "sig" + nextSigNumber
      nextSigNumber += 1
    } while (usedNames contains nextName)
    newFuncSigName(nextName)
  }

  def newFuncSigName(name: String) = FuncSigName(generateName(name))

  def newFuncVerName(function: GlobalVarName): FuncVerName = {
    val nameStr = function.name + "_" + java.util.UUID.randomUUID()
    usedNames += nameStr
    FuncVerName(nameStr)
  }

  def newFuncVerName(name: String): FuncVerName = FuncVerName(generateName(name))

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

  def const(c: Const): GlobalVarName =
    constDefs collectFirst { case (name, existing) if c == existing => name } getOrElse {
      var nextName: String = null
      do {
        nextName = "const" + nextConstNumber
        nextConstNumber += 1
      } while (usedNames contains nextName)
      const(newVarName(nextName), c)
    }

  def const(name: GlobalVarName, c: Const): GlobalVarName = {
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
    val builder = new FunctionBuilder(funcName, sig, versionName, paramTy.toSeq.indices.map("param"+_))
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
    val builder = new FunctionBuilder(funcName, sigName, versionName, paramNames)
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

class FunctionBuilder(
  val functionName: GlobalVarName,
  val functionSignature: FuncSigName,
  val versionName: FuncVerName,
  cParamNames: TraversableOnce[String]
) {
  type Block = (LabelName, mutable.ArrayBuffer[NameableInst])
  val blocks = mutable.ArrayBuffer.empty[Block]
  val usedNames = mutable.Set.empty[String]
  private var nextVarNumber = 0
  private var nextLabelNumber = 0
  private var currentBlock: Block = createBlock(newLabelName("entry"))

  lazy val paramNames = Helpers.buildList(cParamNames map newVarName)

  def paramName(index: Int): LocalVarName = paramNames get index

  private def generateName(n: String): String = {
    var n_ = n
    while (usedNames contains n_) n_ += "_"
    usedNames += n_
    n_
  }

  def newVarName(): LocalVarName = {
    var nextName: String = null
    do {
      nextName = "var" + nextVarNumber
      nextVarNumber += 1
    } while (usedNames contains nextName)
    newVarName(nextName)
  }

  def newVarName(name: String): LocalVarName = LocalVarName(generateName(name))

  def newLabelName(): LabelName = {
    var nextName: String = null
    do {
      nextName = "label" + nextLabelNumber
      nextLabelNumber += 1
    } while (usedNames contains nextName)
    newLabelName(nextName)
  }

  def newLabelName(name: String): LabelName = LabelName(generateName(name))

  private def createBlock(name: LabelName): Block = {
    val newBlock = (name, mutable.ArrayBuffer.empty[NameableInst])
    blocks add newBlock
    newBlock
  }

  def currentBlockLabel: LabelName = currentBlock._1

  def startNewBlock(): LabelName = startNewBlock(newLabelName())

  def startNewBlock(label: LabelName): LabelName = {
    currentBlock = createBlock(label)
    label
  }

  def inst(inst: NameableInst): LocalVarName = inst match {
    case NamedInst(name, _) =>
      currentBlock._2 add inst
      name
    case i: Inst =>
      val name = newVarName()
      currentBlock._2 add NamedInst(name, i)
      name
  }

  def inst(name: LocalVarName, inst: Inst): LocalVarName = {
    currentBlock._2 add NamedInst(name, inst)
    name
  }

  def build(): FuncVer = new FuncVer(
    functionName, functionSignature, paramNames,
    blocks map { case (name, insts) => new BasicBlock(name, insts) }
  )
}

