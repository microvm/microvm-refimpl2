package uvm.ir.textinput

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode
import uvm._
import uvm.ir.textinput.gen.UIRParser._
import uvm.ir.textinput.gen._
import uvm.ssavariables._
import uvm.types._

import scala.collection.JavaConversions._

class UIRTextReader(val idFactory: IDFactory) {
  import uvm.ir.textinput.Later.Laterable

  def read(ir: String, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    read(input, globalBundle)
  }

  def read(ir: java.io.Reader, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    read(input, globalBundle)
  }

  def read(ir: ANTLRInputStream, globalBundle: Bundle): Bundle = {
    val lexer = new UIRLexer(ir)
    val tokens = new CommonTokenStream(lexer)
    val parser = new UIRParser(tokens)
    val ast = parser.ir()
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
          case "" => true
        }
        val abs = prefix match {
          case "0x" => BigInt(nums, 16)
          case "0" => BigInt(nums, 8)
          case "" => BigInt(nums, 10)
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

  def cascadeLookup[T](name:String, ns1: Namespace[T], ns2: Namespace[T]): T =
    ns1.get(name).getOrElse(ns2(name))

  def read(ir: IrContext, globalBundle: Bundle): Bundle = {
    val bundle = new Bundle()

    val phase1 = new Later()

    def resTy(ctx: TypeContext): Type = resTy(ctx.getText)
    def resTy(name: String): Type = cascadeLookup(name, bundle.typeNs, globalBundle.typeNs)

    def resSig(ctx: FuncSigContext): FuncSig = resSig(ctx.getText)
    def resSig(name: String): FuncSig = cascadeLookup(name, bundle.funcSigNs, globalBundle.funcSigNs)

    def resConst(ctx: ConstantContext): Constant = resConst(ctx.getText)
    def resConst(name: String): Constant = cascadeLookup(name, bundle.constantNs, globalBundle.constantNs)

    def resVar(ctx: ValueContext): Type = resTy(ctx.getText)
    def resVar(name: String): SSAVariable = cascadeLookup(name, bundle.varNs, globalBundle.varNs)

    def resGlobalVar(name: String): GlobalVariable = cascadeLookup(name, bundle.globalVarNs, globalBundle.globalVarNs)

    def mkType(tc: TypeConstructorContext): Type = {
      val ty = tc match {
        case t: TypeIntContext => TypeInt(t.length.intValue())
        case t: TypeFloatContext => TypeFloat()
        case t: TypeDoubleContext => TypeDouble()
        case t: TypeRefContext => TypeRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeIRefContext => TypeIRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeWeakRefContext => TypeWeakRef(null).later(phase1) { _.ty = resTy(t.`type`()) }
        case t: TypeStructContext => TypeStruct(null).later(phase1) { _.fieldTy = t.`type`().map(resTy) }
        case t: TypeArrayContext => TypeArray(null, t.length.longValue()).later(phase1) { _.elemTy = resTy(t.`type`()) }
        case t: TypeHybridContext => TypeHybrid(null, null).later(phase1) { tt => tt.fixedTy = resTy(t.fixedTy); tt.varTy = resTy(t.varTy) }
        case t: TypeVoidContext => TypeVoid()
        case t: TypeFuncContext => TypeFunc(null).later(phase1) { _.sig = resSig(t.funcSig()) }
        case t: TypeThreadContext => TypeThread()
        case t: TypeStackContext => TypeStack()
        case t: TypeTagRef64Context => TypeTagRef64()
        case t: TypeVectorContext => TypeVector(null, t.length.longValue()).later(phase1) { _.elemTy = resTy(t.`type`()) }
        case _ => throw new TextIRParsingException("foo")
      }
      return ty
    }

    def mkSig(fsc: FuncSigConstructorContext): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTy = resTy(fsc.retTy)
        sig.paramTy = for (t<-fsc.paramTy) yield resTy(t)
      }
      return sig
    }

    ir.topLevelDef.map(_.getChild(0)).foreach {
      case td: TypeDefContext => {
        val ty = mkType(td.typeConstructor)
        ty.id = idFactory.getID()
        ty.name = Some(td.nam)
        bundle.typeNs.add(ty)
      }
      case fsd: FuncSigDefContext => {
        val sig = mkSig(fsd.funcSigConstructor)
        sig.id = idFactory.getID()
        sig.name = Some(fsd.nam)
        bundle.funcSigNs.add(sig)
      }
      case _ =>
    }

    phase1.doAll()

    val phase2 = new Later()

    def mkConst(t: Type, c: ConstConstructorContext): Constant = {
      val con = c match {
        case cc: ConstIntContext => ConstInt(t, cc.intLiteral)
        case cc: ConstFloatContext => ConstFloat(t, cc.floatLiteral)
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

    def tryReuseFuncID(name: String): Option[Int] = {
      globalBundle.funcNs.get(name).map(_.id)
    }

    def declFunc(n: String, s: FuncSigContext): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      val maybeOldID = tryReuseFuncID(n)
      func.id = maybeOldID.getOrElse(idFactory.getID())
      func.name = Some(n)
      bundle.funcNs.add(func)

      return func
    }

    var funcDefs: List[(Function, Seq[String], FuncBodyContext)] = Nil

    ir.topLevelDef().map(_.getChild(0)).foreach {
      case cdctx: ConstDefContext => {
        val ty = resTy(cdctx.`type`)
        val con = mkConst(ty, cdctx.constConstructor)
        con.id = idFactory.getID()
        con.name = Some(cdctx.nam)
        bundle.constantNs.add(con)
        bundle.globalVarNs.add(con)
        bundle.varNs.add(con)
      }
      case gdctx: GlobalDefContext => {
        val ty = resTy(gdctx.`type`)
        val gc = mkGlobalCell(ty)
        gc.name = Some(gdctx.nam)
        bundle.globalCellNs.add(gc)
        bundle.globalVarNs.add(gc)
        bundle.varNs.add(gc)
      }
      case fdecl: FuncDeclContext => {
        declFunc(fdecl.nam, fdecl.funcSig)
      }
      case fdef: FuncDefContext => {
        val func = declFunc(fdef.nam, fdef.funcSig)
        funcDefs = (func, fdef.paramList.name().map(_.getText), fdef.funcBody) :: funcDefs
      }
      case _ => {}
    }

    phase2.doAll()

    def defFunc(func: Function, ps: Seq[String], body: FuncBodyContext) {
      val ver = new FuncVer()
      ver.func = func
      func.versions = ver :: func.versions

      def globalize(name: String): String = if(name(0)=="@") name else (ver.name.get + name.substring(1))

      ver.params = ps.zipWithIndex.map {
        case (n, i) =>
          val param = Parameter(ver, i)
          param.id = idFactory.getID()
          param.name = Some(globalize(n))
          ver.localVarNs.add(param)
          param
      }

      val phase3 = new Later()

      val phase4 = new Later()

      def makeBB(name: Option[String], insts: Seq[InstContext]): BasicBlock = {
        val bb = new BasicBlock()
        bb.id = IDFactory.getID()
        bb.name = name
        ver.bbNs.add(bb)

        phase3 { () =>
          bb.insts = for (instDef <- insts) yield {
            val inst = mkInst(instDef)
            ver.lvNs.add(inst)
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

      def resBB(n: String): BasicBlock = ver.bbNs(n)

      def resVal(ty: Option[Type], vc: ValueContext): Value = vc match {
        case rvc: ReferencedValueContext => {
          val text = rvc.identifier().getText()
          if (text.startsWith("@")) {
            bundle.globalValueNs.get(text).getOrElse(globalBundle.globalValueNs(text))
          } else {
            ver.lvNs(text)
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
          ka.value().map(n => ver.lvNs(n.getText))
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

      ver.bbs = bbs
      ver.entry = entry

      phase3.doAll()

      phase4.doAll()

      for (bb <- ver.bbs; i <- bb.insts) {
        i.resolve()
      }
    }

    for ((func, ps, body) <- funcDefs) {
      defFunc(func, ps, body)
    }

    return bundle
  }
}