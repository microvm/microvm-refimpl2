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
import uvm.utils.IOHelpers
import uvm.utils.IDFactory
import uvm.utils.AdvancedAntlrHelper
import uvm.utils.AntlrHelpers.AccumulativeAntlrErrorListener
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

class UIRTextReader(val idFactory: IDFactory, val recordSourceInfo: Boolean = true) {
  import UIRTextReader._
  
  def read(ir: java.io.Reader, globalBundle: GlobalBundle): TrantientBundle = {
    logger.debug("Slurping from reader...")
    val src = IOHelpers.slurp(ir)
    read(src, globalBundle)
  }

  def read(ir: String, globalBundle: GlobalBundle): TrantientBundle = {
    logger.debug("Creating ANTLRInputStream...")
    val input = new ANTLRInputStream(ir)
    read(ir, input, globalBundle)
  }
  
  def parse(source: String, ais: ANTLRInputStream): IrContext = {
    logger.debug("Creating AccumulativeAntlrErrorListener...")
    val ea = new AccumulativeAntlrErrorListener(source)

    logger.debug("Creating Antlr Lexer and Parser...")
    val lexer = new UIRLexer(ais)
    lexer.removeErrorListeners()
    lexer.addErrorListener(ea)
    val tokens = new CommonTokenStream(lexer)
    val parser = new UIRParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ea)
    
    logger.debug("Antlr parsing IR...")
    val ast = parser.ir()
    if (ea.hasError) {
      throw new TextIRParsingException("Syntax error:\n" + ea.getMessages)
    }
    
    ast
  }

  def read(source: String, ais: ANTLRInputStream, globalBundle: GlobalBundle): TrantientBundle = {
    val ast = parse(source, ais)
    logger.debug("Antlr parsed. Reading UIR AST into a Bundle...")
    val instanceReader = new InstanceUIRTextReader(idFactory, source, ast, globalBundle, recordSourceInfo)
    val bundle = instanceReader.read()
    logger.debug("Bundle read from AST.")
    bundle
  }
}

private[textinput] class InstanceUIRTextReader(idFactory: IDFactory, source: String, ir: IrContext,
    globalBundle: GlobalBundle, val recordSourceInfo: Boolean) extends AdvancedAntlrHelper {
  import UIRTextReader._
  import uvm.ir.textinput.Later.Laterable

  def sourceLines = source.lines.toIndexedSeq

  val bundle = new TrantientBundle()

  // Printing context information (line, column, near some token)

  def catchIn[T](ctx: ParserRuleContext, s: => String)(func: => T): T = try {
    func
  } catch {
    case e: Exception => throw new TextIRParsingException(inCtx(ctx, s), e)
  }

  def resolveByNameAndCatch[T](kind: String, ctx: ParserRuleContext, func: String => T): T = {
    catchIn(ctx, "Unable to resolve %s %s".format(kind, ctx.getText)) { func(ctx.getText) }
  }

  implicit def resTy(ctx: TypeContext): Type = resolveByNameAndCatch("type", ctx, resTyByName)
  def resTyByName(name: String): Type = cascadeLookup(name, bundle.typeNs, globalBundle.typeNs)

  implicit def resSig(ctx: FuncSigContext): FuncSig = resolveByNameAndCatch("function signature", ctx, resSigByName)
  def resSigByName(name: String): FuncSig = cascadeLookup(name, bundle.funcSigNs, globalBundle.funcSigNs)

  implicit def resConst(ctx: ConstantContext): Constant = resolveByNameAndCatch("constant", ctx, resConstByName)
  def resConstByName(name: String): Constant = cascadeLookup(name, bundle.constantNs, globalBundle.constantNs)

  implicit def resGlobalVar(ctx: GlobalVarContext): GlobalVariable = resolveByNameAndCatch("global variable", ctx, resGlobalVarByName)
  def resGlobalVarByName(name: String): GlobalVariable = cascadeLookup(name, bundle.globalVarNs, globalBundle.globalVarNs)

  implicit def resFunc(ctx: FuncContext): Function = resolveByNameAndCatch("function", ctx, resFuncByName)
  def resFuncByName(name: String): Function = cascadeLookup(name, bundle.funcNs, globalBundle.funcNs)

  implicit def convFlag(f: FlagContext): Flag = Flag(f.FLAG().getText)
  implicit def convFlagList(a: FlagListContext): Seq[Flag] = a.flag().map(convFlag)

  // Resolve global entities from special structures.
  implicit def resTypeList(a: TypeListContext): Seq[Type] = a.`type`.map(resTy)
  implicit def resFuncSigList(a: FuncSigListContext): Seq[FuncSig] = a.funcSig().map(resSig)

  // Resolve special structures

  // Resolve global entities. (If any resXxxx is not present, that's because it is simply not currently used)
  // Add entities to namespaces.

  private def addWithSourceInfo[T <: Identified](ns: Namespace[T], obj: T, sourceInfo: SourceInfo): Unit = {
    try {
      ns.add(obj)
      bundle.sourceInfoRepo(obj) = sourceInfo
    } catch {
      case e: NameConflictException => {
        throw new TextIRParsingException("Name conflict: %s. The newer definition %s:\n%s\nconflicts with older definition %s:\n%s".format(
          obj.name.get, e.newItem.repr, sourceInfo.prettyPrint(), e.oldItem.repr, bundle.sourceInfoRepo(e.oldItem).prettyPrint()))
      }
    }
  }

  def addTy(obj: Type, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.typeNs, obj, sourceInfo)
  def addSig(obj: FuncSig, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.funcSigNs, obj, sourceInfo)
  def addFuncVer(obj: FuncVer, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.funcVerNs, obj, sourceInfo)
  def addConst(obj: Constant, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.constantNs, obj, sourceInfo)
  def addGlobalCell(obj: GlobalCell, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.globalCellNs, obj, sourceInfo)
  def addFunc(obj: Function, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.funcNs, obj, sourceInfo)
  def addExpFunc(obj: ExposedFunc, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bundle.expFuncNs, obj, sourceInfo)

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
  def needAbsStruct[T <: Type](tc: TypeContext) = needType(tc, classOf[AbstractStructType], "struct or hybrid")
  def needArray[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeArray], "array")
  def needVector[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeVector], "vector")
  def needHybrid[T <: Type](tc: TypeContext) = needType(tc, classOf[TypeHybrid], "hybrid")
  def needSeq[T <: Type](tc: TypeContext) = needType(tc, classOf[AbstractSeqType], "array or vector")

  def needConstInt64(ctx: ParserRuleContext, cc: ConstantContext): ConstInt = {
    val c = resConst(cc)
    if (!c.isInstanceOf[ConstInt]) {
      throw new UnexpectedTypeException(inCtx(ctx, "Expected 64-bit integer constant, actually %s.".format(c.getClass)))
    }
    c.asInstanceOf[ConstInt]
  }

  def read(): TrantientBundle = {

    // Make types and sigs

    logger.debug("Phase 1")
    val phase1 = new Later() // Resolve inter-references between types and sigs

    def mkType(tc: TypeConstructorContext): Type = {
      val ty = tc match {
        case t: TypeIntContext       => TypeInt(t.length.intValue())
        case t: TypeFloatContext     => TypeFloat()
        case t: TypeDoubleContext    => TypeDouble()
        case t: TypeRefContext       => TypeRef(null).later(phase1) { _.ty = t.ty }
        case t: TypeIRefContext      => TypeIRef(null).later(phase1) { _.ty = t.ty }
        case t: TypeWeakRefContext   => TypeWeakRef(null).later(phase1) { _.ty = t.ty }
        case t: TypeStructContext    => TypeStruct(null).later(phase1) { _.fieldTys = t.fieldTys.map(resTy) }
        case t: TypeArrayContext     => TypeArray(null, t.length.longValue()).later(phase1) { _.elemTy = t.ty }
        case t: TypeHybridContext    => TypeHybrid(null, null).later(phase1) { tt => tt.fieldTys = t.fieldTys.map(resTy); tt.varTy = t.varTy }
        case t: TypeVoidContext      => TypeVoid()
        case t: TypeFuncRefContext   => TypeFuncRef(null).later(phase1) { _.sig = t.funcSig() }
        case t: TypeThreadRefContext => TypeThreadRef()
        case t: TypeStackRefContext  => TypeStackRef()
        case t: TypeTagRef64Context  => TypeTagRef64()
        case t: TypeVectorContext    => TypeVector(null, t.length.longValue()).later(phase1) { _.elemTy = t.ty }
        case t: TypeUPtrContext      => TypeUPtr(null).later(phase1) { _.ty = t.ty }
        case t: TypeUFuncPtrContext  => TypeUFuncPtr(null).later(phase1) { _.sig = t.funcSig }
      }
      return ty
    }

    def mkSig(fsc: FuncSigConstructorContext): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTys = for (t <- fsc.retTys) yield resTy(t)
        sig.paramTys = for (t <- fsc.paramTys) yield resTy(t)
      }
      return sig
    }

    logger.debug("Reading types and sigs...")
    ir.topLevelDef.map(_.getChild(0)).foreach {
      case td: TypeDefContext => {
        val ty = mkType(td.typeConstructor)
        ty.id = idFactory.getID()
        ty.name = Some(td.nam)
        addTy(ty, toSourceInfo(td.nam))
      }
      case fsd: FuncSigDefContext => {
        val sig = mkSig(fsd.funcSigConstructor)
        sig.id = idFactory.getID()
        sig.name = Some(fsd.nam)
        addSig(sig, toSourceInfo(fsd.nam))
      }
      case _ =>
    }

    logger.debug("Resolving types and sigs...")
    phase1.doAll()

    logger.debug("Phase 2")
    val phase2 = new Later() // Resolve inter-references from constants to other global variables

    def mkConst(t: Type, c: ConstConstructorContext): Constant = {
      val con = c match {
        case cc: CtorIntContext    => ConstInt(t, cc.intLiteral)
        case cc: CtorFloatContext  => ConstFloat(t, cc.floatLiteral)
        case cc: CtorDoubleContext => ConstDouble(t, cc.doubleLiteral)
        case cc: CtorListContext => ConstSeq(t, null).later(phase2) {
          _.elems = for (gn <- cc.globalVar()) yield resGlobalVar(gn)
        }
        case _: CtorNullContext => ConstNull(t)
      }
      return con
    }

    def mkGlobalCell(t: Type): GlobalCell = GlobalCell(t)

    def mkFunc(sig: FuncSig): Function = {
      val func = new Function()
      func.sig = sig
      return func
    }

    def mkExpo(c: FuncExpDefContext): ExposedFunc = {
      val efun = ExposedFunc(null, c.callConv, null).later(phase2) { ee =>
        ee.func = resFunc(c.func)
        ee.cookie = needConstInt64(c, c.cookie)
      }
      return efun
    }

    def findOldFunc(name: String): Option[Function] = {
      globalBundle.funcNs.get(name)
    }

    def declFunc(n: String, s: FuncSigContext, sourceInfo: SourceInfo): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      func.id = idFactory.getID()
      func.name = Some(n)
      addFunc(func, sourceInfo)

      func
    }

    logger.debug("Reading consts, globals, funcdefs, funcdecls and expfuncs...")
    val funcDefs = new ArrayBuffer[(Function, FuncDefContext)]

    ir.topLevelDef().map(_.getChild(0)).foreach {
      case cdctx: ConstDefContext => {
        val ty = resTy(cdctx.ty)
        val con = mkConst(ty, cdctx.constConstructor)
        con.id = idFactory.getID()
        con.name = Some(cdctx.nam)
        addConst(con, toSourceInfo(cdctx.nam))
      }
      case gdctx: GlobalDefContext => {
        val ty = resTy(gdctx.ty)
        val gc = mkGlobalCell(ty)
        gc.id = idFactory.getID()
        gc.name = Some(gdctx.nam)
        addGlobalCell(gc, toSourceInfo(gdctx.nam))
      }
      case fdecl: FuncDeclContext => {
        val name = globalNameToString(fdecl.nam)
        findOldFunc(name) match {
          case Some(oldFunc) =>
            throw new TextIRParsingException(inCtx(fdecl, "Function %s already declared in previous bundles. Old func: %s".format(name, oldFunc.repr)))
          case None => declFunc(name, fdecl.funcSig, toSourceInfo(fdecl.nam))
        }
      }
      case fdef: FuncDefContext => {
        val func = findOldFunc(fdef.nam).getOrElse(declFunc(fdef.nam, fdef.funcSig, toSourceInfo(fdef.nam)))
        //bundle.defFuncNs.add(func)
        funcDefs += ((func, fdef))
      }
      case edef: FuncExpDefContext => {
        val efun = mkExpo(edef)
        efun.id = idFactory.getID()
        efun.name = Some(edef.nam)
        addExpFunc(efun, toSourceInfo(edef.nam))
      }
      case _ => {}
    }

    logger.debug("Resolving global value inter-references")
    phase2.doAll()

    logger.debug("Phase 3")
    def defFunc(func: Function, fDefCtx: FuncDefContext) {
      logger.trace("Visiting function %s".format(func.name))
      val ver = new FuncVer()
      val verName = globalize(fDefCtx.ver, func.name.get)
      ver.id = idFactory.getID()
      ver.name = Some(verName)
      addFuncVer(ver, toSourceInfo(fDefCtx.ver))

      ver.func = func

      ver.bbNs = bundle.allNs.makeSubSpace[BasicBlock]("basic block")

      def addBB(bb: BasicBlock, sourceInfo: SourceInfo): Unit = addWithSourceInfo(ver.bbNs, bb, sourceInfo)

      def globalizeBB(name: String): String = globalize(name, verName)

      // Resolve function version local entities

      implicit def resBB(ctx: BbContext): BasicBlock = catchIn(ctx, "Unable to resolve basic block %s".format(ctx.name.getText)) {
        resBBByName(ctx.name.getText)
      }
      def resBBByName(name: String): BasicBlock = {
        val globalName = globalize(name, verName)
        ver.bbNs(globalName)
      }

      val phase4 = new Later() // Resolve references from instructions to other variables (global or local) and basic blocks

      def makeBB(bbCtx: BasicBlockContext): BasicBlock = {
        val label = bbCtx.label()
        val bbName = globalize(label.name(), verName)
        val bb = new BasicBlock()
        bb.id = idFactory.getID()
        bb.name = Some(bbName)
        addBB(bb, toSourceInfo(bbCtx))

        bb.localVarNs = bundle.allNs.makeSubSpace[LocalVariable]("local variable")
        bb.localInstNs = bundle.allNs.makeSubSpace[Instruction]("instruction")
        def addLocalVar(lv: LocalVariable, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bb.localVarNs, lv, sourceInfo)
        def addInst(inst: Instruction, sourceInfo: SourceInfo): Unit = addWithSourceInfo(bb.localInstNs, inst, sourceInfo)

        def mkNorParam(ty: Type, name: String): NorParam = {
          val param = NorParam(ty)
          param.id = idFactory.getID()
          param.name = Some(globalize(name, bbName))
          param
        }

        def mkExcParam(name: String): ExcParam = {
          val param = ExcParam()
          param.id = idFactory.getID()
          param.name = Some(globalize(name, bbName))
          param
        }

        bb.norParams = label.bbParam.map { p =>
          val param = mkNorParam(p.`type`(), p.name())
          val sourceInfo = toSourceInfo(p)
          addLocalVar(param, sourceInfo)
          param
        }.to[ArrayBuffer]
        bb.excParam = Option(label.excParam).map { p =>
          val param = mkExcParam(p.name())
          val sourceInfo = toSourceInfo(p)
          addLocalVar(param, sourceInfo)
          param
        }

        // Resolve basic block local entities (variables)

        def resLocalVar(ctx: ValueContext): LocalVariable = catchIn(ctx, "Unable to resolve local variable %s".format(ctx.getText)) {
          resLocalVarByName(ctx.name)
        }
        def resLocalVarByName(name: String): LocalVariable = {
          val globalName = globalize(name, bbName)
          bb.localVarNs(globalName)
        }

        implicit def resVar(ctx: ValueContext): SSAVariable = catchIn(ctx, "Unable to resolve variable %s".format(ctx.getText)) {
          resVarByName(ctx.name)
        }
        def resVarByName(name: String): SSAVariable = {
          val globalName = globalize(name, bbName)
          bb.localVarNs.get(globalName).getOrElse {
            cascadeLookup(globalName, bundle.globalVarNs, globalBundle.globalVarNs)
          }
        }

        implicit def resArgList(a: ArgListContext): Seq[SSAVariable] = a.value.map(resVar)

        implicit def resKA(ka: KeepAliveClauseContext): Seq[LocalVariable] = ka.value.map(resLocalVar)

        def resFuncCallBody(fcb: FuncCallBodyContext): (FuncSig, SSAVariable, Seq[SSAVariable]) =
          (fcb.funcSig, fcb.callee, fcb.argList)

        def asgnFuncCallBody(cl: CallLike, fcb: FuncCallBodyContext): Unit = {
          val (sig, callee, argList) = resFuncCallBody(fcb)
          cl.sig = sig; cl.callee = callee; cl.argList = argList
        }

        implicit def resDestClause(dc: DestClauseContext): DestClause = {
          DestClause(dc.bb, dc.argList())
        }

        implicit def resExcClause(ec: ExcClauseContext): Option[ExcClause] =
          if (ec.nor == null) {
            None
          } else {
            Some(ExcClause(ec.nor, ec.exc))
          }

        implicit def resThreadLocalClause(tlc: ThreadLocalClauseContext): Option[SSAVariable] =
          Option(tlc).map { theTlc => resVar(theTlc.value()) } 

        implicit def resNewStackClause(nsc: NewStackClauseContext): NewStackAction = {
          nsc match {
            case a: NewStackPassValueContext => PassValues(a.typeList(), a.argList())
            case a: NewStackThrowExcContext  => ThrowExc(a.exc)
          }
        }

        implicit def resCurStackClause(csc: CurStackClauseContext): CurStackAction = {
          csc match {
            case a: CurStackRetWithContext => RetWith(a.typeList())
            case a: CurStackKillOldContext => KillOld()
          }
        }

        // Make instruction

        def mkInst(bb: BasicBlock, instDef: InstContext): Instruction = {

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
                i.dest = ii.dest
              }
            case ii: InstBranch2Context =>
              InstBranch2(null, null, null).later(phase4) { i =>
                i.cond = ii.cond; i.ifTrue = ii.ifTrue; i.ifFalse = ii.ifFalse
              }
            case ii: InstSwitchContext =>
              InstSwitch(ii.`type`, null, null, null).later(phase4) { i =>
                i.opnd = ii.opnd; i.defDest = ii.defDest
                i.cases = for ((v, d) <- ii.caseVal.zip(ii.caseDest)) yield (resVar(v), resDestClause(d))
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
              InstRet(ver, null).later(phase4) { i =>
                i.retVals = ii.retVals.vals.map(resVar)
              }
            case ii: InstThrowContext =>
              InstThrow(null).later(phase4) { i =>
                i.excVal = ii.exc
              }
            case ii: InstExtractValueContext =>
              InstExtractValue(needStruct(ii.`type`), ii.intLiteral.intValue, null).later(phase4) { i =>
                i.opnd = ii.opnd
              }
            case ii: InstInsertValueContext =>
              InstInsertValue(needStruct(ii.`type`), ii.intLiteral.intValue, null, null).later(phase4) { i =>
                i.opnd = ii.opnd; i.newVal = ii.newVal
              }
            case ii: InstExtractElementContext =>
              InstExtractElement(needSeq(ii.seqTy), needInt(ii.indTy), null, null).later(phase4) { i =>
                i.opnd = ii.opnd; i.index = ii.index
              }
            case ii: InstInsertElementContext =>
              InstInsertElement(needSeq(ii.seqTy), needInt(ii.indTy), null, null, null).later(phase4) { i =>
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
              InstGetFieldIRef(ii.ptr != null, needAbsStruct(ii.refTy), ii.intLiteral.intValue, null).later(phase4) { i =>
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
              InstTrap(ii.typeList(), null, null).later(phase4) { i =>
                i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
              }
            case ii: InstWatchPointContext =>
              InstWatchPoint(ii.wpid.intValue(), ii.typeList(), null, null, null, null).later(phase4) { i =>
                i.dis = ii.dis; i.ena = ii.ena; i.exc = Option(ii.wpExc).map(resDestClause); i.keepAlives = ii.keepAliveClause
              }
            case ii: InstWPBranchContext =>
              InstWPBranch(ii.wpid.intValue(), null, null).later(phase4) { i =>
                i.dis = ii.dis; i.ena = ii.ena
              }
            case ii: InstCCallContext =>
              InstCCall(ii.callConv, ii.funcTy, ii.funcSig, null, null, null, null).later(phase4) { i =>
                i.callee = ii.callee; i.argList = ii.argList; i.excClause = ii.excClause; i.keepAlives = ii.keepAliveClause
              }
            case ii: InstNewThreadContext =>
              InstNewThread(null, null, null, null).later(phase4) { i =>
                i.stack = ii.stack
                i.threadLocal = ii.threadLocalClause
                i.newStackAction = ii.newStackClause
                i.excClause = ii.excClause
              }
            case ii: InstSwapStackContext =>
              InstSwapStack(null, null, null, null, null).later(phase4) { i =>
                i.swappee = ii.swappee
                i.curStackAction = ii.curStackClause
                i.newStackAction = ii.newStackClause
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
          inst.name = Option(instDef.name).map(n => globalize(n.getText, bbName))

          addInst(inst, toSourceInfo(instDef))

          val instRess: ArrayBuffer[InstResult] = Option(instDef.instResults) match {
            case None => ArrayBuffer()
            case Some(r) => (for ((instResDef, index) <- r.results.zipWithIndex) yield {
              val resName = globalize(instResDef.getText, bbName)

              val instRes = InstResult(inst, index)
              instRes.id = idFactory.getID()
              instRes.name = Some(resName)
              addLocalVar(instRes, toSourceInfo(instResDef))

              instRes
            }).to[ArrayBuffer]
          }

          inst.bb = bb
          inst.results = instRess

          return inst
        }

        bb.insts = bbCtx.inst.map(i => mkInst(bb, i)).to[ArrayBuffer]

        return bb
      }

      val bbs = fDefCtx.funcBody.basicBlock().map(makeBB).to[ArrayBuffer]

      ver.bbs = bbs

      phase4.doAll()
    }

    logger.debug("Starting visiting function definitions...")
    for ((func, fDefCtx) <- funcDefs) {
      defFunc(func, fDefCtx)
    }

    logger.debug("Finished visiting function definitions. Bundle generated.")

    return bundle
  }
}

object UIRTextReader {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName()))

  implicit def terminalToString(tn: TerminalNode): String = tn.getText()
  implicit def nameToString(name: NameContext): String = name.getText()
  implicit def globalNameToString(gn: GlobalNameContext): String = gn.getText()
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

  def globalize(name: String, parentName: String): String = {
    val sigil = name.charAt(0)
    sigil match {
      case '@' => name
      case '%' => parentName + "." + name.substring(1)
      case _   => throw new UvmException("Illegal name '%s'. Name must begin with either '@' or '%%'".format(name))
    }
  }

  implicit def resOrd(ord: MemordContext): MemoryOrder.Value = {
    if (ord == null) {
      MemoryOrder.NOT_ATOMIC
    } else {
      MemoryOrder.withName(ord.getText)
    }
  }
}
