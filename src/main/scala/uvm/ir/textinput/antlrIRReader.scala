package uvm.ir.textinput

import scala.collection.JavaConversions._

import uvm._
import uvm.types._
import uvm.ssavalues._
import uvm.ifuncs._
import uvm.ir.textinput.gen._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode
import uIRParser._

object AntlrUvmIRReader {
  import Later.Laterable

  def read(ir: String, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast, globalBundle)
  }

  def read(ir: java.io.Reader, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast, globalBundle)
  }

  object IDFactory {
    private var id: Int = 65536
    def getID(): Int = {
      val myID = id
      id = id + 1
      return myID
    }
  }

  implicit def terminalToString(tn: TerminalNode): String = tn.getText()

  implicit def IntLiteralToBigInt(il: IntLiteralContext): BigInt = {
    val txt = il.getText()
    val (neg, beg) = txt(0) match {
      case '+' => (false, 1)
      case '-' => (true, 1)
      case _ => (false, 0)
    }
    val abs = il match {
      case dec: DecIntLiteralContext => BigInt(txt.substring(beg), 10)
      case oct: OctIntLiteralContext => BigInt(txt.substring(beg), 8)
      case hex: HexIntLiteralContext => BigInt(txt.substring(beg + 2), 16)
    }
    return if (neg) -abs else abs
  }

  implicit def floatLiteralToFloat(fl: FloatLiteralContext): Float = fl match {
    case num: FloatNumberContext => num.FP_NUM.getText.toFloat
    case fi: FloatInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Float.NEGATIVE_INFINITY
      else java.lang.Float.POSITIVE_INFINITY
    }
    case _: FloatNanContext => java.lang.Float.NaN
    case bits: FloatBitsContext => java.lang.Float.intBitsToFloat(bits.intLiteral().intValue())
  }

  implicit def doubleLiteralToDouble(dl: DoubleLiteralContext): Double = dl match {
    case num: DoubleNumberContext => num.FP_NUM.getText.toDouble
    case fi: DoubleInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Double.NEGATIVE_INFINITY
      else java.lang.Double.POSITIVE_INFINITY
    }
    case _: DoubleNanContext => java.lang.Double.NaN
    case bits: DoubleBitsContext => java.lang.Double.longBitsToDouble(bits.intLiteral().longValue())
  }

  def read(ir: IrContext, globalBundle: Bundle): Bundle = {

    val bundle = new Bundle()

    val phase1 = new Later()

    def resTy(te: TypeContext): Type = te match {
      case rt: ReferencedTypeContext => bundle.typeNs.get(rt.GLOBAL_ID()).getOrElse(globalBundle.typeNs(rt.GLOBAL_ID()))
      case it: InLineTypeContext => mkType(it.typeConstructor())
    }

    def mkType(tc: TypeConstructorContext): Type = {
      val ty = tc match {
        case it: IntTypeContext => TypeInt(it.intLiteral().intValue())
        case ft: FloatTypeContext => TypeFloat()
        case dt: DoubleTypeContext => TypeDouble()
        case rt: RefTypeContext => TypeRef(null).later(phase1) { _.ty = resTy(rt.`type`()) }
        case irt: IRefTypeContext => TypeIRef(null).later(phase1) { _.ty = resTy(irt.`type`()) }
        case wrt: WeakRefTypeContext => TypeWeakRef(null).later(phase1) { _.ty = resTy(wrt.`type`()) }
        case st: StructTypeContext => TypeStruct(null).later(phase1) { _.fieldTy = st.`type`().map(resTy) }
        case at: ArrayTypeContext => TypeArray(null, at.intLiteral().longValue()).later(phase1) { _.elemTy = resTy(at.`type`()) }
        case ht: HybridTypeContext => TypeHybrid(null, null).later(phase1) { t => t.fixedPart = resTy(ht.`type`(0)); t.varPart = resTy(ht.`type`(1)) }
        case vt: VoidTypeContext => TypeVoid()
        case ft: FuncTypeContext => TypeFunc(null).later(phase1) { _.sig = resSig(ft.funcSig()) }
        case thr: ThreadTypeContext => TypeThread()
        case sta: StackTypeContext => TypeStack()
        case tr64: TagRef64TypeContext => TypeTagRef64()
        case _ => throw new TextIRParsingException("foo")
      }
      ty.id = IDFactory.getID()
      return ty
    }

    def resSig(fs: FuncSigContext): FuncSig = fs match {
      case rfs: ReferencedFuncSigContext => bundle.funcSigNs.get(rfs.GLOBAL_ID()).getOrElse(globalBundle.funcSigNs(rfs.GLOBAL_ID()))
      case ilfs: InLineFuncSigContext => mkSig(ilfs.funcSigConstructor())
    }

    def mkSig(fsc: FuncSigConstructorContext): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTy = resTy(fsc.`type`().head)
        sig.paramTy = fsc.`type`().tail.map(resTy)
      }
      sig.id = IDFactory.getID()
      return sig
    }

    ir.metaData.map(_.getChild(0)).foreach {
      case td: TypeDefContext => {
        val ty = mkType(td.typeConstructor)
        ty.name = Some(td.GLOBAL_ID)
        bundle.typeNs.add(ty)
      }
      case fsd: FuncSigDefContext => {
        val sig = mkSig(fsd.funcSigConstructor)
        sig.name = Some(fsd.GLOBAL_ID)
        bundle.funcSigNs.add(sig)
      }
      case _ =>
    }

    phase1.doAll()

    val phase2 = new Later()

    def resGV(t: Type, ce: ConstantContext): GlobalValue = ce match {
      case rcc: ReferencedConstContext => bundle.globalValueNs.get(rcc.GLOBAL_ID).getOrElse(globalBundle.globalValueNs(rcc.GLOBAL_ID))
      case icc: InLineConstContext => mkConst(t, icc.constExpr)
    }

    def mkConst(t: Type, c: ConstExprContext): DeclaredConstant = {
      val con = c match {
        case icc: IntConstContext => ConstInt(t, icc.intLiteral)
        case fcc: FloatConstContext => ConstFloat(t, fcc.floatLiteral)
        case dcc: DoubleConstContext => ConstDouble(t, dcc.doubleLiteral)
        case scc: StructConstContext => ConstStruct(t, null).later(phase2) {
          _.fields = for ((ft, f) <- t.asInstanceOf[TypeStruct].fieldTy.zip(scc.constant)) yield resGV(ft, f)
        }
        case _: NullConstContext => ConstNull(t)
      }
      con.id = IDFactory.getID()
      return con
    }

    def mkGlobalData(t: Type): GlobalData = {
      val gd = GlobalData(t)
      gd.id = IDFactory.getID()
      return gd
    }

    def mkGlobalDataConst(gd: GlobalData): ConstGlobalData = {
      val gdc = ConstGlobalData(gd)
      return gdc
    }

    def mkFunc(sig: FuncSig): Function = {
      val func = new Function()
      func.sig = sig
      return func
    }

    def mkFuncConst(func: Function): ConstFunc = {
      val fc = ConstFunc(func)
      return fc
    }

    def tryReuseFuncID(name: String): Option[Int] = {
      globalBundle.funcNs.get(name).map(_.id)
    }

    def declFunc(n: String, s: FuncSigContext): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      val maybeOldID = tryReuseFuncID(n)
      func.id = maybeOldID.getOrElse(IDFactory.getID())
      func.name = Some(n)
      bundle.funcNs.add(func)

      if (maybeOldID == None) {
        val fc = mkFuncConst(func)
        bundle.globalValueNs.add(fc)
      }

      return func
    }

    var funcDefs: List[(Function, Seq[String], FuncBodyContext)] = Nil

    ir.metaData.map(_.getChild(0)).foreach {
      case cdctx: ConstDefContext => {
        val ty = resTy(cdctx.`type`)
        val con = mkConst(ty, cdctx.constExpr)
        con.name = Some(cdctx.GLOBAL_ID)
        bundle.declConstNs.add(con)
        bundle.globalValueNs.add(con)
      }
      case gdctx: GlobalDefContext => {
        val ty = resTy(gdctx.`type`)
        val gd = mkGlobalData(ty)
        gd.name = Some(gdctx.GLOBAL_ID)
        bundle.globalDataNs.add(gd)

        val gdc = mkGlobalDataConst(gd)
        bundle.globalValueNs.add(gdc)
      }
      case fdecl: FuncDeclContext => {
        declFunc(fdecl.GLOBAL_ID, fdecl.funcSig)
      }
      case fdef: FuncDefContext => {
        val func = declFunc(fdef.GLOBAL_ID, fdef.funcSig)
        funcDefs = (func, fdef.paramList.LOCAL_ID.map(_.getText), fdef.funcBody) :: funcDefs
      }
      case _ => {}
    }

    phase2.doAll()

    def defFunc(func: Function, ps: Seq[String], body: FuncBodyContext) {
      val cfg = new CFG()
      cfg.func = func
      func.cfg = Some(cfg)

      cfg.params = ps.zipWithIndex.map {
        case (n, i) =>
          val param = Parameter(func.sig, i)
          param.id = IDFactory.getID()
          param.name = Some(n)
          cfg.lvNs.add(param)
          param
      }

      val phase3 = new Later()

      val phase4 = new Later()

      def makeBB(name: Option[String], insts: Seq[InstContext]): BasicBlock = {
        val bb = new BasicBlock()
        bb.id = IDFactory.getID()
        bb.name = name
        cfg.bbNs.add(bb)

        phase3 { () =>
          bb.insts = for (instDef <- insts) yield {
            val inst = mkInst(instDef)
            cfg.lvNs.add(inst)
            inst
          }
        }

        return bb
      }

      def makeEntryBB(eb: EntryBlockContext): BasicBlock = {
        val name: Option[String] = Option(eb.label).map(_.LOCAL_ID.getText)
        makeBB(name, eb.inst)
      }

      def makeRegularBB(eb: RegularBlockContext): BasicBlock = {
        val name: Option[String] = Some(eb.label.LOCAL_ID.getText)
        makeBB(name, eb.inst)
      }

      def resBB(n: String): BasicBlock = cfg.bbNs(n)

      def resVal(ty: Option[Type], vc: ValueContext): Value = vc match {
        case rvc: ReferencedValueContext => {
          val text = rvc.identifier().getText()
          if (text.startsWith("@")) {
            bundle.globalValueNs.get(text).getOrElse(globalBundle.globalValueNs(text))
          } else {
            cfg.lvNs(text)
          }
        }
        case icvc: InlineConstValueContext => ty match {
          case None => throw new TextIRParsingException(
            "Cannot use inline constant value when its type is not a user-defined type.")
          case Some(t) => mkConst(t, icvc.constExpr)
        }
      }

      def vc(ty: Type, vc: ValueContext): Value = resVal(Some(ty), vc)
      def vnc(vc: ValueContext): Value = resVal(None, vc)

      def resArgs(s: FuncSig, a: Seq[ValueContext]): Seq[Value] = s.paramTy.zip(a).map {
        case (t, v) => vc(t, v)
      }

      def resKA(ka: KeepAliveContext): Seq[Value] = {
        if (ka == null) {
          return Seq()
        } else {
          ka.value().map(n => cfg.lvNs(n.getText))
        }
      }

      implicit def resOrd(ord: AtomicordContext): MemoryOrdering.Value = {
        if (ord == null) {
          MemoryOrdering.NOT_ATOMIC
        } else {
          MemoryOrdering.withName(ord.getText)
        }
      }

      implicit class RichCallLike[T <: CallLike](inst: T) {
        def cl(fcb: FuncCallBodyContext): T = {
          inst.sig = resSig(fcb.funcSig)
          inst.later(phase4) { i =>
            i.callee = vnc(fcb.value); i.args = resArgs(i.sig, fcb.args.value())
          }
          inst
        }
      }

      implicit class RichHasExceptionalDest[T <: HasExceptionalDest](inst: T) {
        def nc(norExc: Seq[TerminalNode]): T = {
          inst.nor = resBB(norExc(0).getText())
          inst.exc = resBB(norExc(1).getText())
          inst
        }
      }

      implicit class RichHasKeepAlives[T <: HasKeepAlives](inst: T) {
        def kas(kas: KeepAliveContext): T = {
          inst.later(phase4) { i =>
            i.keepAlives = resKA(kas)
          }
          inst
        }
      }

      def mkInst(instDef: InstContext): Instruction = {

        val inst: Instruction = instDef.instBody match {
          case ii: InstBinOpContext =>
            InstBinOp(BinOptr.withName(ii.binops.getText), resTy(ii.`type`), null, null).later(phase4) { i =>
              i.op1 = vc(i.opndTy, ii.value(0)); i.op2 = vc(i.opndTy, ii.value(1))
            }
          case ii: InstCmpContext =>
            InstCmp(CmpOptr.withName(ii.cmpops.getText), resTy(ii.`type`), null, null).later(phase4) { i =>
              i.op1 = vc(i.opndTy, ii.value(0)); i.op2 = vc(i.opndTy, ii.value(1))
            }
          case ii: InstConversionContext =>
            InstConv(ConvOptr.withName(ii.convops.getText), resTy(ii.`type`(0)), resTy(ii.`type`(1)), null).later(phase4) { i =>
              i.opnd = vc(i.fromTy, ii.value)
            }
          case ii: InstSelectContext =>
            InstSelect(resTy(ii.`type`), null, null, null).later(phase4) { i =>
              i.cond = vnc(ii.value(0)); i.ifTrue = vc(i.opndTy, ii.value(1)); i.ifFalse = vc(i.opndTy, ii.value(2))
            }
          case ii: InstBranchContext =>
            InstBranch(resBB(ii.LOCAL_ID))
          case ii: InstBranch2Context =>
            InstBranch2(null, resBB(ii.LOCAL_ID(0)), resBB(ii.LOCAL_ID(1))).later(phase4) { i =>
              i.cond = vnc(ii.value)
            }
          case ii: InstSwitchContext =>
            InstSwitch(resTy(ii.`type`), null, resBB(ii.LOCAL_ID(0)), null).later(phase4) { i =>
              i.opnd = vc(i.opndTy, ii.value(0))
              i.cases = ii.value().zip(ii.LOCAL_ID()).tail.map {
                case (v, b) =>
                  (vc(i.opndTy, v), resBB(b))
              }
            }
          case ii: InstPhiContext =>
            InstPhi(resTy(ii.`type`), null).later(phase4) { i =>
              i.cases = ii.LOCAL_ID().zip(ii.value()).map {
                case (b, v) =>
                  (resBB(b), vc(i.opndTy, v))
              }
            }
          case ii: InstCallContext =>
            InstCall(null, null, null, null).cl(ii.funcCallBody).kas(ii.keepAlive)
          case ii: InstInvokeContext =>
            InstInvoke(null, null, null, null, null, null).cl(ii.funcCallBody).nc(ii.LOCAL_ID()).kas(ii.keepAlive)
          case ii: InstTailCallContext =>
            InstTailCall(resSig(ii.funcCallBody.funcSig), null, null).cl(ii.funcCallBody)
          case ii: InstRetContext =>
            InstRet(resTy(ii.`type`), null).later(phase4) { i =>
              i.retVal = vc(i.retTy, ii.value)
            }
          case ii: InstRetVoidContext =>
            InstRetVoid()
          case ii: InstThrowContext =>
            InstThrow(null).later(phase4) { i =>
              i.excVal = vnc(ii.value)
            }
          case ii: InstLandingPadContext =>
            InstLandingpad()
          case ii: InstExtractValueContext =>
            InstExtractValue(resTy(ii.`type`).asInstanceOf[TypeStruct], ii.intLiteral.intValue, null).later(phase4) { i =>
              i.opnd = vc(i.strTy, ii.value)
            }
          case ii: InstInsertValueContext =>
            InstInsertValue(resTy(ii.`type`).asInstanceOf[TypeStruct], ii.intLiteral.intValue, null, null).later(phase4) { i =>
              i.opnd = vc(i.strTy, ii.value(0)); i.newVal = vc(i.strTy.fieldTy(i.index), ii.value(1))
            }
          case ii: InstNewContext => InstNew(resTy(ii.`type`))
          case ii: InstNewHybridContext =>
            InstNewHybrid(resTy(ii.`type`).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.length = vnc(ii.value)
            }
          case ii: InstAllocaContext => InstAlloca(resTy(ii.`type`))
          case ii: InstAllocaHybridContext =>
            InstAllocaHybrid(resTy(ii.`type`).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.length = vnc(ii.value)
            }
          case ii: InstGetIRefContext =>
            InstGetIRef(resTy(ii.`type`), null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value)
            }
          case ii: InstGetFieldIRefContext =>
            InstGetFieldIRef(resTy(ii.`type`).asInstanceOf[TypeStruct], ii.intLiteral.intValue, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value)
            }
          case ii: InstGetElemIRefContext =>
            InstGetElemIRef(resTy(ii.`type`).asInstanceOf[TypeArray], null, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value(0)); i.index = vc(i.referentTy, ii.value(1))
            }
          case ii: InstShiftIRefContext =>
            InstShiftIRef(resTy(ii.`type`), null, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value(0)); i.offset = vc(i.referentTy, ii.value(1))
            }
          case ii: InstGetFixedPartIRefContext =>
            InstGetFixedPartIRef(resTy(ii.`type`).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value)
            }
          case ii: InstGetVarPartIRefContext =>
            InstGetVarPartIRef(resTy(ii.`type`).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, ii.value)
            }
          case ii: InstLoadContext =>
            InstLoad(ii.atomicord, resTy(ii.`type`), null).later(phase4) { i =>
              i.loc = vnc(ii.value)
            }
          case ii: InstStoreContext =>
            InstStore(ii.atomicord, resTy(ii.`type`), null, null).later(phase4) { i =>
              i.loc = vnc(ii.value(0)); i.newVal = vc(i.referentTy, ii.value(1))
            }
          case ii: InstCmpXchgContext =>
            InstCmpXchg(ii.atomicord(0), ii.atomicord(1),
              resTy(ii.`type`), null, null, null).later(phase4) { i =>
                i.loc = vnc(ii.value(0)); i.expected = vc(i.referentTy, ii.value(1)); i.desired = vc(i.referentTy, ii.value(2))
              }
          case ii: InstAtomicRMWContext =>
            InstAtomicRMW(ii.atomicord, AtomicRMWOptr.withName(ii.atomicrmwop.getText),
              resTy(ii.`type`), null, null).later(phase4) { i =>
                i.loc = vnc(ii.value(0)); i.opnd = vc(i.referentTy, ii.value(1))
              }
          case ii: InstFenceContext => InstFence(resOrd(ii.atomicord))
          case ii: InstTrapContext =>
            InstTrap(resTy(ii.`type`), null, null, null).nc(ii.LOCAL_ID()).kas(ii.keepAlive)
          case ii: InstWatchPointContext =>
            InstWatchpoint(ii.intLiteral.intValue(), resTy(ii.`type`), resBB(ii.LOCAL_ID(0)), null, null, null).nc(ii.LOCAL_ID().tail).kas(ii.keepAlive)
          case ii: InstCCallContext =>
            InstCCall(CallConv.withName(ii.callconv.getText), null, null, null).cl(ii.funcCallBody)
          case ii: InstNewStackContext =>
            InstNewStack(null, null, null).cl(ii.funcCallBody)
          case ii: InstICallContext =>
            InstICall(IFuncs(ii.GLOBAL_ID), null, null).kas(ii.keepAlive).later(phase4) { i =>
              i.args = resArgs(i.iFunc.sig, ii.args.value())
            }
          case ii: InstIInvokeContext =>
            InstIInvoke(IFuncs(ii.GLOBAL_ID), null, null, null, null).nc(ii.LOCAL_ID).kas(ii.keepAlive).later(phase4) { i =>
              i.args = resArgs(i.iFunc.sig, ii.args.value())
            }
        }

        inst.id = IDFactory.getID()
        inst.name = Option(instDef.LOCAL_ID).map(_.getText)

        return inst
      }

      val entry = makeEntryBB(body.basicBlocks.entryBlock)
      val rest = body.basicBlocks.regularBlock.map(makeRegularBB)

      val bbs = Seq(entry) ++ rest

      cfg.bbs = bbs
      cfg.entry = entry

      phase3.doAll()

      phase4.doAll()

      for (bb <- cfg.bbs; i <- bb.insts) {
        i.resolve()
      }
    }

    for ((func, ps, body) <- funcDefs) {
      defFunc(func, ps, body)
    }

    return bundle
  }
}