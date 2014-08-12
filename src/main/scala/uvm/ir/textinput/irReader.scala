package uvm.ir.textinput

import uvm._
import uvm.types._
import uvm.ssavalues._
import uvm.ifuncs._

object UvmIRReader { 
  import UvmIRAST._
  import Later.Laterable

  def read(ir: CharSequence, globalBundle: Bundle): Bundle = {
    val ast = UvmIRParser(ir)
    read(ast, globalBundle)
  }

  def read(ir: java.io.Reader, globalBundle: Bundle): Bundle = {
    val ast = UvmIRParser(ir)
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

  def read(ir: IR, globalBundle: Bundle): Bundle = {
    val bundle = new Bundle()

    val phase1 = new Later()

    def resTy(te: TypeExpr): Type = te match {
      case ReferredType(g) => bundle.typeNs.get(g.name).getOrElse(globalBundle.typeNs(g.name))
      case tc: TypeCons => mkType(tc)
    }

    def mkType(tc: TypeCons): Type = {
      val ty = tc match {
        case IntCons(l) => TypeInt(l)
        case FloatCons => TypeFloat()
        case DoubleCons => TypeDouble()
        case RefCons(t) => TypeRef(null).later(phase1) { _.ty = resTy(t) }
        case IRefCons(t) => TypeIRef(null).later(phase1) { _.ty = resTy(t) }
        case WeakRefCons(t) => TypeWeakRef(null).later(phase1) { _.ty = resTy(t) }
        case StructCons(fs) => TypeStruct(null).later(phase1) { _.fieldTy = fs.map(resTy) }
        case ArrayCons(et, l) => TypeArray(null, l).later(phase1) { _.elemTy = resTy(et) }
        case HybridCons(fp, vp) => TypeHybrid(null, null).later(phase1) { h => h.fixedPart = resTy(fp); h.varPart = resTy(vp) }
        case VoidCons => TypeVoid()
        case FuncCons(s) => TypeFunc(null).later(phase1) { _.sig = resSig(s) }
        case ThreadCons => TypeThread()
        case StackCons => TypeStack()
        case TagRef64Cons => TypeTagRef64()
        case _ => throw new RuntimeException("foo")
      }
      ty.id = IDFactory.getID()
      return ty
    }

    def resSig(se: FuncSigExpr): FuncSig = se match {
      case ReferredFuncSig(g) => bundle.funcSigNs.get(g.name).getOrElse(globalBundle.funcSigNs(g.name))
      case FuncSigCons(r, ps) => mkSig(r, ps)
    }

    def mkSig(r: TypeExpr, ps: Seq[TypeExpr]): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTy = resTy(r)
        sig.paramTy = ps.map(resTy)
      }
      sig.id = IDFactory.getID()
      return sig
    }

    ir.topLevels.foreach {
      case TypeDef(n, c) => {
        val ty = mkType(c)
        ty.name = Some(n.name)
        bundle.typeNs.add(ty)
      }
      case FuncSigDef(n, FuncSigCons(r, ps)) => {
        val sig = mkSig(r, ps)
        sig.name = Some(n.name)
        bundle.funcSigNs.add(sig)
      }
      case _ => {}
    }

    phase1.doAll()

    val phase2 = new Later()

    def resGV(t: Type, ce: ConstExpr): GlobalValue = ce match {
      case ReferredConst(g) => bundle.globalValueNs.get(g.name).getOrElse(globalBundle.globalValueNs(g.name))
      case c: ConstCons => mkConst(t, c)
    }

    def mkConst(t: Type, c: ConstCons): DeclaredConstant = {
      val con = c match {
        case IntConstCons(num) => ConstInt(t, num)
        case FloatConstCons(num) => ConstFloat(t, num)
        case DoubleConstCons(num) => ConstDouble(t, num)
        case StructConstCons(fs) => ConstStruct(t, null).later(phase2) {
          _.fields = for ((ft,f) <- t.asInstanceOf[TypeStruct].fieldTy.zip(fs)) yield resGV(ft, f)
        }
        case NullConstCons => ConstNull(t)
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

    def declFunc(n: GID, s: FuncSigExpr): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      val maybeOldID = tryReuseFuncID(n.name)
      func.id = maybeOldID.getOrElse(IDFactory.getID())
      func.name = Some(n.name)
      bundle.funcNs.add(func)
      
      if (maybeOldID == None) {
        val fc = mkFuncConst(func)
        bundle.globalValueNs.add(fc)
      }

      return func
    }

    var funcDefs: List[(Function, Seq[LID], FuncBodyDef)] = Nil

    ir.topLevels.foreach {
      case ConstDef(n, t, c) => {
        val ty = resTy(t)
        val con = mkConst(ty, c)
        con.name = Some(n.name)
        bundle.declConstNs.add(con)
        bundle.globalValueNs.add(con)
      }
      case GlobalDataDef(n, t) => {
        val ty = resTy(t)
        val gd = mkGlobalData(ty)
        gd.name = Some(n.name)
        bundle.globalDataNs.add(gd)

        val gdc = mkGlobalDataConst(gd)
        bundle.globalValueNs.add(gdc)
      }
      case FuncDecl(n, s) => {
        declFunc(n, s)
      }
      case FuncDef(n, s, ps, body) => {
        val func = declFunc(n, s)
        funcDefs = (func, ps, body) :: funcDefs
      }
      case _ => {}
    }

    phase2.doAll()

    def defFunc(func: Function, ps: Seq[LID], body: FuncBodyDef) {
      val cfg = new CFG()
      cfg.func = func
      func.cfg = Some(cfg)

      cfg.params = ps.zipWithIndex.map {
        case (n, i) =>
          val param = Parameter(func.sig, i)
          param.id = IDFactory.getID()
          param.name = Some(n.name)
          cfg.lvNs.add(param)
          param
      }

      val phase3 = new Later()

      val phase4 = new Later()

      def makeBB(bbd: BasicBlockDef): BasicBlock = {
        val bb = new BasicBlock()
        bb.id = IDFactory.getID()
        bb.name = bbd.name.map(_.name)
        cfg.bbNs.add(bb)

        phase3 { () =>
          bb.insts = for (instDef <- bbd.insts) yield {
            val inst = mkInst(instDef)
            cfg.lvNs.add(inst)
            inst
          }
        }

        return bb
      }

      def resBB(n: LID): BasicBlock = cfg.bbNs(n.name)

      def resVal(ty: Option[Type], ve: ValueExpr): Value = ve match {
        case RefValue(id) => id match {
          case GID(n) => bundle.globalValueNs(n)
          case LID(n) => cfg.lvNs(n)
        }
        case InlineValue(cc) => ty match {
          case None => throw new TextIRParsingException(
            "Cannot use inline constant value when its type is not a user-defined type.")
          case Some(t) => mkConst(t, cc)
        }
      }

      def vc(ty: Type, ve: ValueExpr): Value = resVal(Some(ty), ve)
      def vnc(ve: ValueExpr): Value = resVal(None, ve)

      def resArgs(s: FuncSig, a: Seq[ValueExpr]): Seq[Value] = s.paramTy.zip(a).map {
        case (t, v) => vc(t, v)
      }

      def resKA(ka: Seq[LID]): Seq[Value] = ka.map(n => cfg.lvNs(n.name))

      def mkInst(instDef: InstDef): Instruction = {

        val inst: Instruction = instDef.cons match {
          case BinOpCons(optr, t, op1, op2) =>
            InstBinOp(BinOptr.withName(optr), resTy(t), null, null).later(phase4) { i =>
              i.op1 = vc(i.opndTy, op1); i.op2 = vc(i.opndTy, op2)
            }
          case CmpCons(optr, t, op1, op2) =>
            InstCmp(CmpOptr.withName(optr), resTy(t), null, null).later(phase4) { i =>
              i.op1 = vc(i.opndTy, op1); i.op2 = vc(i.opndTy, op2)
            }
          case ConvCons(optr, t1, t2, op) =>
            InstConv(ConvOptr.withName(optr), resTy(t1), resTy(t2), null).later(phase4) { i =>
              i.opnd = vc(i.fromTy, op)
            }
          case SelectCons(t, c, tr, fa) =>
            InstSelect(resTy(t), null, null, null).later(phase4) { i =>
              i.cond = vnc(c); i.ifTrue = vc(i.opndTy, tr); i.ifFalse = vc(i.opndTy, fa)
            }
          case BranchCons(d) =>
            InstBranch(resBB(d))
          case Branch2Cons(c, tr, fa) =>
            InstBranch2(null, resBB(tr), resBB(fa)).later(phase4) { i =>
              i.cond = vnc(c)
            }
          case SwitchCons(t, o, d, cs) =>
            InstSwitch(resTy(t), null, resBB(d), null).later(phase4) { i =>
              i.opnd = vc(i.opndTy, o)
              i.cases = cs.map {
                case (v, b) =>
                  (vc(i.opndTy, v), resBB(b))
              }
            }
          case PhiCons(t, cs) =>
            InstPhi(resTy(t), null).later(phase4) { i =>
              i.cases = cs.map {
                case (b, v) =>
                  (resBB(b), vc(i.opndTy, v))
              }
            }
          case CallCons(s, f, a, ka) =>
            InstCall(resSig(s), null, null, null).later(phase4) { i =>
              i.callee = vnc(f); i.args = resArgs(i.sig, a); i.keepAlives = resKA(ka)
            }
          case InvokeCons(s, f, a, n, e, ka) =>
            InstInvoke(resSig(s), null, null, resBB(n), resBB(e), null).later(phase4) { i =>
              i.callee = vnc(f); i.args = resArgs(i.sig, a); i.keepAlives = resKA(ka)
            }
          case TailCallCons(s, f, a) =>
            InstTailCall(resSig(s), null, null).later(phase4) { i =>
              i.callee = vnc(f); i.args = resArgs(i.sig, a)
            }
          case RetCons(t, v) =>
            InstRet(resTy(t), null).later(phase4) { i =>
              i.retVal = vc(i.retTy, v)
            }
          case RetVoidCons =>
            InstRetVoid()
          case ThrowCons(v) =>
            InstThrow(null).later(phase4) { i =>
              i.excVal = vnc(v)
            }
          case LandingpadCons =>
            InstLandingpad()
          case ExtractValueCons(t, n, v) =>
            InstExtractValue(resTy(t).asInstanceOf[TypeStruct], n, null).later(phase4) { i =>
              i.opnd = vc(i.strTy, v)
            }
          case InsertValueCons(t, n, v, nv) =>
            InstInsertValue(resTy(t).asInstanceOf[TypeStruct], n, null, null).later(phase4) { i =>
              i.opnd = vc(i.strTy, v); i.newVal = vc(i.strTy.fieldTy(n), nv)
            }
          case NewCons(t) => InstNew(resTy(t))
          case NewHybridCons(t, v) =>
            InstNewHybrid(resTy(t).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.length = vnc(v)
            }
          case AllocaCons(t) => InstAlloca(resTy(t))
          case AllocaHybridCons(t, v) =>
            InstAllocaHybrid(resTy(t).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.length = vnc(v)
            }
          case GetIRefCons(t, v) =>
            InstGetIRef(resTy(t), null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v)
            }
          case GetFieldIRefCons(t, n, v) =>
            InstGetFieldIRef(resTy(t).asInstanceOf[TypeStruct], n, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v)
            }
          case GetElemIRefCons(t, v, n) =>
            InstGetElemIRef(resTy(t).asInstanceOf[TypeArray], null, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v); i.index = vc(i.referentTy, n)
            }
          case ShiftIRefCons(t, v, n) =>
            InstShiftIRef(resTy(t), null, null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v); i.offset = vc(i.referentTy, n)
            }
          case GetFixedPartIRefCons(t, v) =>
            InstGetFixedPartIRef(resTy(t).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v)
            }
          case GetVarPartIRefCons(t, v) =>
            InstGetVarPartIRef(resTy(t).asInstanceOf[TypeHybrid], null).later(phase4) { i =>
              i.opnd = vc(i.referentTy, v)
            }
          case LoadCons(o, t, v) =>
            InstLoad(MemoryOrdering.withName(o), resTy(t), null).later(phase4) { i =>
              i.loc = vnc(v)
            }
          case StoreCons(o, t, v, nv) =>
            InstStore(MemoryOrdering.withName(o), resTy(t), null, null).later(phase4) { i =>
              i.loc = vnc(v); i.newVal = vc(i.referentTy, nv)
            }
          case CmpXchgCons(os, of, t, v, e, d) =>
            InstCmpXchg(MemoryOrdering.withName(os), MemoryOrdering.withName(of),
              resTy(t), null, null, null).later(phase4) { i =>
                i.loc = vnc(v); i.expected = vc(i.referentTy, e); i.desired = vc(i.referentTy, d)
              }
          case AtomicRMWCons(o, op, t, v, v2) =>
            InstAtomicRMW(MemoryOrdering.withName(o), AtomicRMWOptr.withName(op),
              resTy(t), null, null).later(phase4) { i =>
                i.loc = vnc(v); i.opnd = vc(i.referentTy, v2)
              }
          case FenceCons(o) => InstFence(MemoryOrdering.withName(o))
          case TrapCons(t, n, e, ka) =>
            InstTrap(resTy(t), resBB(n), resBB(e), null).later(phase4) { i =>
              i.keepAlives = resKA(ka)
            }
          case WatchpointCons(wid, t, d, n, e, ka) =>
            InstWatchpoint(wid, resTy(t), resBB(d), resBB(n), resBB(e), null).later(phase4) { i =>
              i.keepAlives = resKA(ka)
            }
          case CCallCons(cc, s, f, a) =>
            InstCCall(CallConv.withName(cc), resSig(s), null, null).later(phase4) { i =>
              i.callee = vnc(f); i.args = resArgs(i.sig, a)
            }
          case NewStackCons(s, f, a) =>
            InstNewStack(resSig(s), null, null).later(phase4) { i =>
              i.callee = vnc(f); i.args = resArgs(i.sig, a)
            }
          case ICallCons(f, a, ka) =>
            InstICall(IFuncs(f.name), null, null).later(phase4) { i =>
              i.args = resArgs(i.iFunc.sig, a); i.keepAlives = resKA(ka)
            }
          case IInvokeCons(f, a, n, e, ka) =>
            InstIInvoke(IFuncs(f.name), null, resBB(n), resBB(e), null).later(phase4) { i =>
              i.args = resArgs(i.iFunc.sig, a); i.keepAlives = resKA(ka)
            }

        }

        inst.id = IDFactory.getID()
        inst.name = for (lid <- instDef.name) yield lid.name

        return inst
      }

      val entry = makeBB(body.entry)
      val rest = body.bbs.map(makeBB)

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