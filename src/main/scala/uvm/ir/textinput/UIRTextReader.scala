package uvm.ir.textinput

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode
import uvm._
import uvm.comminsts.CommInsts
import uvm.ir.textinput.gen.UIRParser._
import uvm.ir.textinput.gen._
import uvm.ssavariables._
import uvm.types._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.Stream
import java.io.StringWriter
import java.nio.CharBuffer
import uvm.ir.textinput.gen.UIRParser.TypePtrContext

class UIRTextReader(val idFactory: IDFactory) {
  import uvm.ir.textinput.Later.Laterable

  def read(ir: String, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    read(ir, input, globalBundle)
  }

  def read(ir: java.io.Reader, globalBundle: Bundle): Bundle = {
    val sb = new StringBuilder()
    val cb = new Array[Char](4096)

    var finished = false
    while (!finished) {
      val actualRead = ir.read(cb, 0, 4096)
      if (actualRead > 0) {
        sb.appendAll(cb, 0, actualRead)
      } else {
        finished = true
      }
    }

    read(sb.toString(), globalBundle)
  }

  class AccumulativeAntlrErrorListener(source: String) extends BaseErrorListener {
    val buf = new ArrayBuffer[String]()
    var hasError = false

    lazy val sourceLines = ArrayBuffer(source.lines.toSeq: _*)

    override def syntaxError(recognizer: Recognizer[_, _], offendingSymbol: Object,
                             line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
      val theLine = sourceLines(line - 1)
      val marker = " " * charPositionInLine + "^"
      buf.add("line %d:%d %s\n%s\n%s".format(line, charPositionInLine, msg, theLine, marker))
      hasError = true
    }

    def getMessages(): String = buf.mkString("\n")
  }

  def read(source: String, ais: ANTLRInputStream, globalBundle: Bundle): Bundle = {
    val ea = new AccumulativeAntlrErrorListener(source)

    val lexer = new UIRLexer(ais)
    lexer.removeErrorListeners()
    lexer.addErrorListener(ea)
    val tokens = new CommonTokenStream(lexer)
    val parser = new UIRParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ea)
    val ast = parser.ir()
    if (ea.hasError) {
      throw new TextIRParsingException("Syntax error:\n" + ea.getMessages)
    }
    read(ast, globalBundle)

  }

  implicit def terminalToString(tn: TerminalNode): String = tn.getText()
  implicit def nameToString(name: NameContext): String = name.getText()
  implicit def tokenToString(tok: Token): String = tok.getText()

  val IntRe = """([+-]?)(0x|0|)([0-9a-fA-F]*)""".r

  implicit def IntLiteralToBigInt(il: IntLiteralContext): BigInt = {
    val txt = il.getText()

    txt match {
      case IntRe(sign, prefix, nums) => {
        val neg = sign match {
          case "+" => false
          case "-" => true
          case ""  => false
        }
        val abs = prefix match {
          case "0x" => BigInt(nums, 16)
          case "0"  => if (nums == "") BigInt(0) else BigInt(nums, 8)
          case ""   => BigInt(nums, 10)
        }
        return if (neg) -abs else abs
      }
    }
  }

  implicit def floatLiteralToFloat(fl: FloatLiteralContext): Float = fl match {
    case num: FloatNumberContext => num.FP_NUM.getText.toFloat
    case fi: FloatInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Float.NEGATIVE_INFINITY
      else java.lang.Float.POSITIVE_INFINITY
    }
    case _: FloatNanContext     => java.lang.Float.NaN
    case bits: FloatBitsContext => java.lang.Float.intBitsToFloat(bits.intLiteral().intValue())
  }

  implicit def doubleLiteralToDouble(dl: DoubleLiteralContext): Double = dl match {
    case num: DoubleNumberContext => num.FP_NUM.getText.toDouble
    case fi: DoubleInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Double.NEGATIVE_INFINITY
      else java.lang.Double.POSITIVE_INFINITY
    }
    case _: DoubleNanContext     => java.lang.Double.NaN
    case bits: DoubleBitsContext => java.lang.Double.longBitsToDouble(bits.intLiteral().longValue())
  }

  def cascadeLookup[T <: Identified](name: String, ns1: Namespace[T], ns2: Namespace[T]): T =
    ns1.get(name).getOrElse(ns2(name))

  // Printing context information (line, column, near some token)

  def inCtx(ctx: ParserRuleContext, s: String): String = nearTok(ctx.getStart, s)
  def inCtx(ctx: TerminalNode, s: String): String = nearTok(ctx.getSymbol, s)

  def nearTok(tok: Token, s: String): String = {
    val line = tok.getLine()
    val column = tok.getCharPositionInLine()
    val near = tok.getText()
    return "At %d:%d near '%s': %s".format(line, column, near, s)
  }

  def catchIn[T](ctx: ParserRuleContext, s: String)(func: => T): T = try {
    func
  } catch {
    case e: Exception => throw new TextIRParsingException(inCtx(ctx, s), e)
  }

  def read(ir: IrContext, globalBundle: Bundle): Bundle = {
    val bundle = new Bundle()

    // Resolve global entities. (If any resXxxx is not present, that's because it is simply not currently used)

    implicit def resTy(ctx: TypeContext): Type = catchIn(ctx, "Unable to resolve type") { resTyByName(ctx.getText) }
    def resTyByName(name: String): Type = cascadeLookup(name, bundle.typeNs, globalBundle.typeNs)

    implicit def resSig(ctx: FuncSigContext): FuncSig = catchIn(ctx, "Unable to resolve sig") { resSigByName(ctx.getText) }
    def resSigByName(name: String): FuncSig = cascadeLookup(name, bundle.funcSigNs, globalBundle.funcSigNs)

    implicit def resConst(ctx: ConstantContext): Constant = catchIn(ctx, "Unable to resolve const") { resConstByName(ctx.getText) }
    def resConstByName(name: String): Constant = cascadeLookup(name, bundle.constantNs, globalBundle.constantNs)

    def resGlobalVar(name: String): GlobalVariable = cascadeLookup(name, bundle.globalVarNs, globalBundle.globalVarNs)
    def resFunc(name: String): Function = cascadeLookup(name, bundle.funcNs, globalBundle.funcNs)

    implicit def convFlag(f: FlagContext): Flag = Flag(f.FLAG().getText)
    implicit def convFlagList(a: FlagListContext): Seq[Flag] = a.flag().map(convFlag)

    // Add entities to namespaces.

    def addTy(obj: Type): Unit = {
      bundle.add(obj)
    }
    def addSig(obj: FuncSig): Unit = {
      bundle.add(obj)
    }
    def addConst(obj: Constant): Unit = {
      bundle.add(obj)
    }
    def addGlobalCell(obj: GlobalCell): Unit = {
      bundle.add(obj)
    }
    def addFunc(obj: Function): Unit = {
      bundle.add(obj)
    }
    def addExpFunc(obj: ExposedFunc): Unit = {
      bundle.add(obj)
    }
    def addLocalVar(obj: LocalVariable, localNs: Namespace[LocalVariable]) = {
      localNs.add(obj)
      bundle.add(obj)
    }
    def addFuncVer(obj: FuncVer): Unit = {
      bundle.add(obj)
    }

    // Resolve types, with parse-time checking.

    def needType[E <: Type](tc: TypeContext, expectedType: Class[E], n: String): E = {
      val t = resTy(tc)
      if (!(expectedType.isAssignableFrom(t.getClass))) {
        throw new UnexpectedTypeException(inCtx(tc, "Expected %s, actually %s.".format(n, t)))
      }
      t.asInstanceOf[E]
    }

    def needInt[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeInt], "int")
    def needStruct[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeStruct], "struct")
    def needArray[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeArray], "array")
    def needVector[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeVector], "vector")
    def needHybrid[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeHybrid], "hybrid")
    def needSeq[T <: Type](tc: TypeContext) = needType(tc, classOf[AbstractSeqType], "array or vector")

    def needConstInt64(ctx: ParserRuleContext, name: String): ConstInt = {
      val c = resConstByName(name)
      if (!c.isInstanceOf[ConstInt]) {
        throw new UnexpectedTypeException(inCtx(ctx, "Expected 64-bit integer constant, actually %s.".format(c.getClass)))
      }
      c.asInstanceOf[ConstInt]
    }

    // Make types and sigs

    val phase1 = new Later() // Resolve inter-references between types and sigs

    def mkType(tc: TypeConstructorContext): Type = {
      val ty = tc match {
        case t: TypeIntContext      => TypeInt(t.length.intValue())
        case t: TypeFloatContext    => TypeFloat()
        case t: TypeDoubleContext   => TypeDouble()
        case t: TypeRefContext      => TypeRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeIRefContext     => TypeIRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeWeakRefContext  => TypeWeakRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeStructContext   => TypeStruct(null).later(phase1) { _.fieldTy = t.`type`().map(resTy) }
        case t: TypeArrayContext    => TypeArray(null, t.length.longValue()).later(phase1) { _.elemTy = resTy(t.`type`()) }
        case t: TypeHybridContext   => TypeHybrid(null, null).later(phase1) { tt => tt.fixedTy = resTy(t.fixedTy); tt.varTy = resTy(t.varTy) }
        case t: TypeVoidContext     => TypeVoid()
        case t: TypeFuncContext     => TypeFunc(null).later(phase1) { _.sig = resSig(t.funcSig()) }
        case t: TypeThreadContext   => TypeThread()
        case t: TypeStackContext    => TypeStack()
        case t: TypeTagRef64Context => TypeTagRef64()
        case t: TypeVectorContext   => TypeVector(null, t.length.longValue()).later(phase1) { _.elemTy = resTy(t.`type`()) }
        case t: TypePtrContext      => TypePtr(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeFuncPtrContext  => TypeFuncPtr(null).later(phase1) { _.sig = resSig(t.funcSig()) }
        case _                      => throw new TextIRParsingException("foo")
      }
      return ty
    }

    def mkSig(fsc: FuncSigConstructorContext): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTy = resTy(fsc.retTy)
        sig.paramTy = for (t <- fsc.paramTy) yield resTy(t)
      }
      return sig
    }

    ir.topLevelDef.map(_.getChild(0)).foreach {
      case td: TypeDefContext => {
        val ty = mkType(td.typeConstructor)
        ty.id = idFactory.getID()
        ty.name = Some(td.nam)
        addTy(ty)
      }
      case fsd: FuncSigDefContext => {
        val sig = mkSig(fsd.funcSigConstructor)
        sig.id = idFactory.getID()
        sig.name = Some(fsd.nam)
        addSig(sig)
      }
      case _ =>
    }

    phase1.doAll()

    val phase2 = new Later() // Resolve inter-references from constants to other global variables

    def mkConst(t: Type, c: ConstConstructorContext): Constant = {
      val con = c match {
        case cc: ConstIntContext => t match {
          case _: TypeInt             => ConstInt(t, cc.intLiteral)
          case _: AbstractPointerType => ConstPointer(t, cc.intLiteral().longValue())
        }
        case cc: ConstFloatContext  => ConstFloat(t, cc.floatLiteral)
        case cc: ConstDoubleContext => ConstDouble(t, cc.doubleLiteral)
        case cc: ConstStructContext => ConstStruct(t, null).later(phase2) {
          _.fields = for (gn <- cc.GLOBAL_NAME()) yield resGlobalVar(gn)
        }
        case _: ConstNullContext => ConstNull(t)
        case cc: ConstVectorContext => ConstVector(t, null).later(phase2) {
          _.elems = for (c <- cc.constant()) yield resConst(c)
        }
      }
      return con
    }

    def mkGlobalCell(t: Type): GlobalCell = GlobalCell(t)

    def mkFunc(sig: FuncSig): Function = {
      val func = new Function()
      func.sig = sig
      return func
    }

    def mkExpo(c: ExposeDefContext): ExposedFunc = {
      val efun = ExposedFunc(null, c.callConv, null).later(phase2) { ee =>
        ee.func = resFunc(c.funcName)
        ee.cookie = needConstInt64(c, c.cookie)
      }
      return efun
    }

    def tryReuseFuncID(name: String): Option[Int] = {
      globalBundle.funcNs.get(name).map(_.id)
    }

    def declFunc(n: String, s: FuncSigContext): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      val maybeOldID = tryReuseFuncID(n)
      func.id = maybeOldID.getOrElse(idFactory.getID())
      func.name = Some(n)
      addFunc(func)

      return func
    }

    var funcDefs: List[(Function, FuncDefContext)] = Nil

    ir.topLevelDef().map(_.getChild(0)).foreach {
      case cdctx: ConstDefContext => {
        val ty = resTy(cdctx.`type`)
        val con = mkConst(ty, cdctx.constConstructor)
        con.id = idFactory.getID()
        con.name = Some(cdctx.nam)
        addConst(con)
      }
      case gdctx: GlobalDefContext => {
        val ty = resTy(gdctx.`type`)
        val gc = mkGlobalCell(ty)
        gc.id = idFactory.getID()
        gc.name = Some(gdctx.nam)
        addGlobalCell(gc)
      }
      case fdecl: FuncDeclContext => {
        declFunc(fdecl.nam, fdecl.funcSig)

      }
      case fdef: FuncDefContext => {
        val func = declFunc(fdef.nam, fdef.funcSig)
        funcDefs = (func, fdef) :: funcDefs
      }
      case edef: ExposeDefContext => {
        val efun = mkExpo(edef)
        efun.id = idFactory.getID()
        efun.name = Some(edef.nam)
        addExpFunc(efun)
      }
      case _ => {}
    }

    phase2.doAll()

    def defFunc(func: Function, fDefCtx: FuncDefContext) {
      val ver = new FuncVer()
      ver.id = idFactory.getID()
      ver.name = Some(fDefCtx.ver)
      addFuncVer(ver)

      ver.func = func
      func.versions = ver :: func.versions

      def globalize(name: String): String = UIRTextReader.globalize(name, ver.name.get)

      ver.params = fDefCtx.params.name().zipWithIndex.map {
        case (n, i) =>
          val param = Parameter(ver, i)
          param.id = idFactory.getID()
          param.name = Some(globalize(n))
          addLocalVar(param, ver.localVarNs)
          param
      }

      val phase4 = new Later() // Resolve references from instructions to other variables (global or local) and basic blocks

      def makeBB(bbCtx: BasicBlockContext): BasicBlock = {
        val bb = new BasicBlock()
        bb.id = idFactory.getID()
        bb.name = Some(globalize(bbCtx.label().name()))
        ver.bbNs.add(bb)
        bundle.add(bb)

        bb.insts = bbCtx.inst.map(mkInst)

        return bb
      }

      // Resolve local entities

      implicit def resBB(ctx: BbNameContext): BasicBlock = catchIn(ctx, "Unable to resolve basic block") {
        resBBByName(ctx.name.getText)
      }
      def resBBByName(name: String): BasicBlock = {
        val globalName = globalize(name)
        ver.bbNs(globalName)
      }

      implicit def resVar(ctx: ValueContext): SSAVariable = catchIn(ctx, "Unable to resolve variable") {
        resVarByName(ctx.name)
      }
      def resVarByName(name: String): SSAVariable = {
        val globalName = globalize(name)
        cascadeLookup(globalName, bundle.varNs, globalBundle.varNs)
      }

      def resLocalVar(ctx: ValueContext): LocalVariable = catchIn(ctx, "Unable to resolve local variable") {
        resLocalVarByName(ctx.name)
      }
      def resLocalVarByName(name: String): LocalVariable = {
        val globalName = globalize(name)
        ver.localVarNs(globalName)
      }

      // Resolve special structures

      implicit def resTypeList(a: TypeListContext): Seq[Type] = a.`type`.map(resTy)
      implicit def resFuncSigList(a: FuncSigListContext): Seq[FuncSig] = a.funcSig().map(resSig)

      implicit def resArgList(a: ArgListContext): Seq[SSAVariable] = a.value.map(resVar)

      implicit def resKA(ka: KeepAliveClauseContext): Seq[LocalVariable] = ka.value.map(resLocalVar)

      implicit def resOrd(ord: MemordContext): MemoryOrder.Value = {
        if (ord == null) {
          MemoryOrder.NOT_ATOMIC
        } else {
          MemoryOrder.withName(ord.getText)
        }
      }

      def resFuncCallBody(fcb: FuncCallBodyContext): (FuncSig, SSAVariable, Seq[SSAVariable]) =
        (fcb.funcSig, fcb.callee, fcb.argList)

      def asgnFuncCallBody(cl: CallLike, fcb: FuncCallBodyContext): Unit = {
        val (sig, callee, argList) = resFuncCallBody(fcb)
        cl.sig = sig; cl.callee = callee; cl.argList = argList
      }

      implicit def resExcClause(ec: ExcClauseContext): Option[ExcClause] =
        if (ec.nor == null) {
          None
        } else {
          val nor = resBB(ec.nor)
          val exc = resBB(ec.exc)
          Some(ExcClause(nor, exc))
        }

      // Make instruction

      def mkInst(instDef: InstContext): Instruction = {

        val inst: Instruction = instDef.instBody match {
          case ii: InstBinOpContext =>
            InstBinOp(BinOptr.withName(ii.binop.getText), ii.`type`, null, null, null).later(phase4) { i =>
              i.op1 = ii.op1; i.op2 = ii.op2; i.excClause = ii.excClause
            }
          case ii: InstCmpContext =>
            InstCmp(CmpOptr.withName(ii.cmpop.getText), ii.`type`, null, null).later(phase4) { i =>
              i.op1 = ii.op1; i.op2 = ii.op2
            }
          case ii: InstConversionContext =>
            InstConv(ConvOptr.withName(ii.convop.getText), ii.fromTy, ii.toTy, null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstSelectContext =>
            InstSelect(ii.condTy, ii.resTy, null, null, null).later(phase4) { i =>
              i.cond = ii.cond; i.ifTrue = ii.ifTrue; i.ifFalse = ii.ifFalse
            }
          case ii: InstBranchContext =>
            InstBranch(null).later(phase4) { i =>
              i.dest = ii.bbName
            }
          case ii: InstBranch2Context =>
            InstBranch2(null, null, null).later(phase4) { i =>
              i.cond = ii.cond; i.ifTrue = ii.ifTrue; i.ifFalse = ii.ifFalse
            }
          case ii: InstSwitchContext =>
            InstSwitch(ii.`type`, null, null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.defDest = ii.defDest
              i.cases = for ((v, b) <- ii.caseVal.zip(ii.caseDest)) yield (resVar(v), resBB(b))
            }
          case ii: InstPhiContext =>
            InstPhi(ii.`type`, null).later(phase4) { i =>
              i.cases = for ((b, v) <- ii.caseSrc.zip(ii.caseVal)) yield (resBB(b), resVar(v))
            }
          case ii: InstCallContext =>
            InstCall(null, null, null, null, null).later(phase4) { i =>
              asgnFuncCallBody(i, ii.funcCallBody)
              i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
            }
          case ii: InstTailCallContext =>
            InstTailCall(null, null, null).later(phase4) { i =>
              asgnFuncCallBody(i, ii.funcCallBody)
            }
          case ii: InstRetContext =>
            InstRet(ii.`type`, null).later(phase4) { i =>
              i.retVal = ii.retVal
            }
          case ii: InstRetVoidContext =>
            InstRetVoid()
          case ii: InstThrowContext =>
            InstThrow(null).later(phase4) { i =>
              i.excVal = ii.exc
            }
          case ii: InstLandingPadContext =>
            InstLandingPad()
          case ii: InstExtractValueContext =>
            InstExtractValue(needStruct(ii.`type`), ii.intLiteral.intValue, null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstInsertValueContext =>
            InstInsertValue(needStruct(ii.`type`), ii.intLiteral.intValue, null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.newVal = ii.newVal
            }
          case ii: InstExtractElementContext =>
            InstExtractElement(needVector(ii.vecTy), needInt(ii.indTy), null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.index = ii.index
            }
          case ii: InstInsertElementContext =>
            InstInsertElement(needVector(ii.vecTy), needInt(ii.indTy), null, null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.index = ii.index; i.newVal = ii.newVal
            }
          case ii: InstShuffleVectorContext =>
            InstShuffleVector(needVector(ii.vecTy), needVector(ii.maskTy), null, null, null).later(phase4) { i =>
              i.vec1 = ii.vec1; i.vec2 = ii.vec2; i.mask = ii.mask
            }
          case ii: InstNewContext =>
            InstNew(ii.allocTy, null).later(phase4) { i =>
              i.excClause = ii.excClause
            }
          case ii: InstNewHybridContext =>
            InstNewHybrid(needHybrid(ii.allocTy), needInt(ii.lenTy), null, null).later(phase4) { i =>
              i.length = ii.length; i.excClause = ii.excClause
            }
          case ii: InstAllocaContext =>
            InstAlloca(ii.allocTy, null).later(phase4) { i =>
              i.excClause = ii.excClause
            }
          case ii: InstAllocaHybridContext =>
            InstAllocaHybrid(needHybrid(ii.allocTy), needInt(ii.lenTy), null, null).later(phase4) { i =>
              i.length = ii.length; i.excClause = ii.excClause
            }
          case ii: InstGetIRefContext =>
            InstGetIRef(ii.refTy, null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstGetFieldIRefContext =>
            InstGetFieldIRef(ii.ptr != null, needStruct(ii.refTy), ii.intLiteral.intValue, null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstGetElemIRefContext =>
            InstGetElemIRef(ii.ptr != null, needSeq(ii.refTy), needInt(ii.indTy), null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.index = ii.index
            }
          case ii: InstShiftIRefContext =>
            InstShiftIRef(ii.ptr != null, ii.refTy, needInt(ii.offTy), null, null).later(phase4) { i =>
              i.opnd = ii.opnd; i.offset = ii.offset
            }
          case ii: InstGetFixedPartIRefContext =>
            InstGetFixedPartIRef(ii.ptr != null, needHybrid(ii.refTy), null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstGetVarPartIRefContext =>
            InstGetVarPartIRef(ii.ptr != null, needHybrid(ii.refTy), null).later(phase4) { i =>
              i.opnd = ii.opnd
            }
          case ii: InstLoadContext =>
            InstLoad(ii.ptr != null, ii.memord, ii.`type`, null, null).later(phase4) { i =>
              i.loc = ii.loc
              i.excClause = ii.excClause
            }
          case ii: InstStoreContext =>
            InstStore(ii.ptr != null, ii.memord, ii.`type`, null, null, null).later(phase4) { i =>
              i.loc = ii.loc; i.newVal = ii.newVal
              i.excClause = ii.excClause
            }
          case ii: InstCmpXchgContext =>
            InstCmpXchg(ii.ptr != null, ii.isWeak != null, ii.ordSucc, ii.ordFail, ii.`type`, null, null, null, null).later(phase4) { i =>
              i.loc = ii.loc; i.expected = ii.expected; i.desired = ii.desired
              i.excClause = ii.excClause
            }
          case ii: InstAtomicRMWContext =>
            InstAtomicRMW(ii.ptr != null, ii.memord, AtomicRMWOptr.withName(ii.atomicrmwop.getText), ii.`type`, null, null, null).later(phase4) { i =>
              i.loc = ii.loc; i.opnd = ii.opnd
              i.excClause = ii.excClause
            }
          case ii: InstFenceContext =>
            InstFence(ii.memord)
          case ii: InstTrapContext =>
            InstTrap(ii.`type`, null, null).later(phase4) { i =>
              i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
            }
          case ii: InstWatchPointContext =>
            InstWatchPoint(ii.intLiteral.intValue(), ii.`type`, null, null, null, null).later(phase4) { i =>
              i.dis = ii.dis; i.ena = ii.ena; i.exc = Option(ii.wpExc).map(resBB); i.keepAlives = ii.keepAliveClause
            }
          case ii: InstCCallContext =>
            InstCCall(ii.callConv, ii.funcTy, ii.funcSig, null, null, null).later(phase4) { i =>
              i.callee = ii.callee; i.argList = ii.argList; i.keepAlives = ii.keepAliveClause
            }
          case ii: InstNewStackContext =>
            InstNewStack(null, null, null, null).later(phase4) { i =>
              asgnFuncCallBody(i, ii.funcCallBody)
              i.excClause = ii.excClause
            }
          case ii: InstSwapStackContext =>
            InstSwapStack(null, null, null, null, null).later(phase4) { i =>
              i.swappee = ii.swappee;
              i.curStackAction = ii.curStackClause match {
                case a: CurStackRetWithContext => RetWith(a.`type`)
                case a: CurStackKillOldContext => KillOld()
              }
              i.newStackAction = ii.newStackClause match {
                case a: NewStackPassValueContext => PassValue(a.`type`, a.value)
                case a: NewStackPassVoidContext  => PassVoid()
                case a: NewStackThrowExcContext  => ThrowExc(a.exc)
              }
              i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
            }
          case ii: InstCommInstContext =>
            InstCommInst(CommInsts(ii.nam), Option(ii.flagList()).map(convFlagList).getOrElse(Seq()), null, null, null, null, null).later(phase4) { i =>
              i.typeList = Option(ii.typeList).map(resTypeList).getOrElse(Seq())
              i.funcSigList = Option(ii.funcSigList).map(resFuncSigList).getOrElse(Seq())
              i.argList = Option(ii.argList).map(resArgList).getOrElse(Seq())
              i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
            }
        }

        inst.id = idFactory.getID()
        inst.name = Option(instDef.name).map(n => globalize(n.getText))

        addLocalVar(inst, ver.localVarNs)

        return inst
      }

      val bbs = fDefCtx.funcBody.basicBlock().map(makeBB)

      ver.bbs = bbs
      ver.entry = bbs.head

      phase4.doAll()
    }

    for ((func, fDefCtx) <- funcDefs) {
      defFunc(func, fDefCtx)
    }

    return bundle
  }
}

object UIRTextReader {
  def globalize(name: String, fvName: String): String = {
    val sigil = name.charAt(0)
    sigil match {
      case '@' => name
      case '%' => fvName + "." + name.substring(1)
      case _   => throw new UvmException("Illegal name '%s'. Name must begin with either '@' or '%%'".format(name))
    }
  }
}