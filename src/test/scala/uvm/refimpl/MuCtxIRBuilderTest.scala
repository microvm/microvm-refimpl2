package uvm.refimpl

import org.scalatest._

import ch.qos.logback.classic.Level._
import uvm._
import uvm.comminsts.CommInsts
import uvm.ir.irbuilder.DestKind
import uvm.ir.textinput.ExtraMatchers
import uvm.refimpl._
import uvm.refimpl.RichMuCtx._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.ssavariables._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.MemoryOrder._
import uvm.types._

object MuCtxIRBuilderTest {
  implicit class MagicalHandle(val handle: MuInstNode) extends AnyVal {
    def R(implicit ctx: MuCtx): MuInstResNode = ctx.newInstRes(handle)
    def R2(implicit ctx: MuCtx): (MuInstResNode, MuInstResNode) = {
      val h0 = ctx.newInstRes(handle)
      val h1 = ctx.newInstRes(handle)
      (h0, h1)
    }
  }
}

class MuCtxIRBuilderTest extends UvmBundleTesterBase with ExtraMatchers {
  import MuCtxIRBuilderTest._

  setLogLevels(ROOT_LOGGER_NAME -> INFO,
    "uvm" -> DEBUG)
    
  override def makeMicroVM() = new MicroVM(new VMConf())

  behavior of "The IR Builder of MuCtx"

  it should "create an empty bundle" in {
    val ctx = microVM.newContext()

    val b = ctx.newBundle()
    ctx.loadBundleFromNode(b)

    ctx.closeContext()
  }

  it should "create a bundle that contains types and sigs" in {
    val ctx = microVM.newContext()

    val b = ctx.newBundle()
    val hi1 = ctx.newTypeInt(b, 1)
    val hi8 = ctx.newTypeInt(b, 8)
    val hi8_id = ctx.getID(b, hi8)
    val hi16 = ctx.newTypeInt(b, 16)
    val hi16_id = ctx.getID(b, hi16)
    val hi32 = ctx.newTypeInt(b, 32)
    val hi32_id = ctx.getID(b, hi32)
    val hi64 = ctx.newTypeInt(b, 64)
    val hi64_id = ctx.getID(b, hi64)
    ctx.setName(b, hi64, "@i64")
    val hf = ctx.newTypeFloat(b)
    val hd = ctx.newTypeDouble(b)
    val hs = ctx.newTypeStruct(b, Seq(hi8, hi16, hi32, hi64))
    val hh = ctx.newTypeHybrid(b, Seq(hf, hd), hi8)
    val ha = ctx.newTypeArray(b, hi8, 100)
    val hv = ctx.newTypeVector(b, hf, 4)

    val hsig = ctx.newFuncSig(b, Seq(hi8, hi16), Seq(hi32))
    val hsig_id = ctx.getID(b, hsig)

    val htr64 = ctx.newTypeTagRef64(b)
    val hsr = ctx.newTypeStackRef(b)
    val htr = ctx.newTypeThreadRef(b)
    val hfcr = ctx.newTypeFrameCursorRef(b)
    val hinr = ctx.newTypeIRNodeRef(b)

    val hp = ctx.newTypeUPtr(b)
    val hfp = ctx.newTypeUFuncPtr(b)
    val hr = ctx.newTypeRef(b)
    val hir = ctx.newTypeIRef(b)
    val hwr = ctx.newTypeWeakRef(b)
    val hfr = ctx.newTypeFuncRef(b)

    ctx.setTypeUPtr(hp, hi64)
    ctx.setTypeUFuncPtr(hfp, hsig)
    ctx.setTypeRef(hr, hi64)
    ctx.setTypeIRef(hir, hi64)
    ctx.setTypeWeakRef(hwr, hi64)
    ctx.setTypeFuncRef(hfr, hsig)
    
    val hr1 = ctx.newTypeRef(b)
    val hs1 = ctx.newTypeStruct(b, Seq(hi64, hr1))
    ctx.setTypeRef(hr1, hs1)
    val hr1_id = ctx.getID(b, hr1)
    val hs1_id = ctx.getID(b, hs1)

    ctx.loadBundleFromNode(b)

    val gb = microVM.globalBundle
    
    gb.typeNs(hi64_id) shouldBeA[TypeInt] { its => its.length shouldBe 64 }
    gb.typeNs("@i64") shouldBe gb.typeNs(hi64_id)
    gb.typeNs(hr1_id) shouldBeA[TypeRef] { its => its.ty shouldBe gb.typeNs(hs1_id) }
    gb.typeNs(hs1_id) shouldBeA[TypeStruct] { its => its.fieldTys shouldBe Seq(gb.typeNs(hi64_id), gb.typeNs(hr1_id)) }
    gb.funcSigNs(hsig_id) shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe Seq(gb.typeNs(hi8_id), gb.typeNs(hi16_id))
      its.retTys shouldBe Seq(gb.typeNs(hi32_id))
    }

    ctx.closeContext()
  }

  it should "create a bundle with constants and global cells" in {
    val ctx = microVM.newContext()

    val b = ctx.newBundle()
    val hi64 = ctx.newTypeInt(b, 64)
    val hf = ctx.newTypeFloat(b)
    val hd = ctx.newTypeDouble(b)
    val hs = ctx.newTypeStruct(b, Seq(hi64, hi64))
    val hr = ctx.newTypeRef(b)
    ctx.setTypeRef(hr, hi64)
    
    val hc1 = ctx.newConstInt(b, hi64, 0x123456789abcdef0L)
    val hc2 = ctx.newConstInt(b, hi64, 0xfedcba9876543210L)
    val hc3 = ctx.newConstSeq(b, hs, Seq(hc1, hc2))
    val hc4 = ctx.newConstFloat(b, hf, 3.14F)
    val hc5 = ctx.newConstDouble(b, hd, 3.14)
    val hc6 = ctx.newConstNull(b, hr)

    val hc1_id = ctx.getID(b, hc1)
    val hc2_id = ctx.getID(b, hc2)
    val hc3_id = ctx.getID(b, hc3)
    val hc4_id = ctx.getID(b, hc4)
    val hc5_id = ctx.getID(b, hc5)
    val hc6_id = ctx.getID(b, hc6)
    
    val hg = ctx.newGlobalCell(b, hi64)
    val hg_id = ctx.getID(b, hg)

    ctx.loadBundleFromNode(b)
    
    val gb = microVM.globalBundle
    gb.constantNs(hc1_id) shouldBeA[ConstInt] { its => its.num shouldBe 0x123456789abcdef0L }
    gb.constantNs(hc2_id) shouldBeA[ConstInt] { its => its.num shouldBe 0xfedcba9876543210L }
    gb.constantNs(hc3_id) shouldBeA[ConstSeq] { its =>
      its.elems.map(_.asInstanceOf[ConstInt].num) shouldBe Seq(0x123456789abcdef0L, 0xfedcba9876543210L)
    }
    gb.constantNs(hc4_id) shouldBeA[ConstFloat] { its => its.num shouldBe 3.14f }
    gb.constantNs(hc5_id) shouldBeA[ConstDouble] { its => its.num shouldBe 3.14 }
    gb.constantNs(hc6_id) shouldBeA[ConstNull] thatsIt
    
    gb.globalCellNs(hg_id) shouldBeA[GlobalCell] { its => its.cellTy shouldBeA[TypeInt] { itss => itss.length shouldBe 64 }}
    
    val hc1v = ctx.handleFromConst(hc1_id).asInstanceOf[MuIntValue]
    val hc1vv = ctx.handleToSInt(hc1v)
    hc1vv shouldBe 0x123456789abcdef0L
    
    val hirg = ctx.handleFromGlobal(hg_id)
    val hirg_v0 = ctx.load(NOT_ATOMIC, hirg).asInstanceOf[MuIntValue]
    val hirg_v0v = ctx.handleToSInt(hirg_v0)
    hirg_v0v shouldBe 0

    ctx.store(NOT_ATOMIC, hirg, hc1v) 

    val hirg_v1 = ctx.load(NOT_ATOMIC, hirg).asInstanceOf[MuIntValue]
    val hirg_v1v = ctx.handleToSInt(hirg_v1)
    hirg_v1v shouldBe 0x123456789abcdef0L

    ctx.closeContext()
  }

  it should "create a bundle with functions and can execute the function" in {
    val ctx = microVM.newContext()

    val b = ctx.newBundle()
    val hi1 = ctx.newTypeInt(b, 1)
    val hi8 = ctx.newTypeInt(b, 8)
    val hi8_id = ctx.getID(b, hi8)
    val hi16 = ctx.newTypeInt(b, 16)
    val hi16_id = ctx.getID(b, hi16)
    val hi32 = ctx.newTypeInt(b, 32)
    val hi32_id = ctx.getID(b, hi32)
    val hi64 = ctx.newTypeInt(b, 64)
    val hi64_id = ctx.getID(b, hi64)
    val hf = ctx.newTypeFloat(b)
    val hd = ctx.newTypeDouble(b)
    val hs = ctx.newTypeStruct(b, Seq(hi8, hi16, hi32, hi64))
    val hh = ctx.newTypeHybrid(b, Seq(hf, hd), hi8)
    val ha = ctx.newTypeArray(b, hi8, 100)
    val hv = ctx.newTypeVector(b, hf, 4)
    val hr = ctx.newTypeRef(b)
    ctx.setTypeRef(hr, hi64)
    
    val hsig = ctx.newFuncSig(b, Seq(hi64, hi64, hd, hd), Seq())
    
    val hfunc = ctx.newFunc(b, hsig)
    val hfunc_id = ctx.getID(b, hfunc)
    val hfv = ctx.newFuncVer(b, hfunc)
    val hfv_id = ctx.getID(b,hfv)
    
    val hentry = ctx.newBB(hfv)
    val hp0 = ctx.newNorParam(hentry, hi64)
    val hp1 = ctx.newNorParam(hentry, hi64)
    val hp2 = ctx.newNorParam(hentry, hd)
    val hp3 = ctx.newNorParam(hentry, hd)
    
    val hadd = ctx.newBinOp(hentry, BinOptr.ADD, hi64, hp0, hp1)
    val hadd_r = ctx.newInstRes(hadd)
    val hfadd = ctx.newBinOp(hentry, BinOptr.FADD, hd, hp2, hp3)
    val hfadd_r = ctx.newInstRes(hfadd)

    val hslt = ctx.newCmp(hentry, CmpOptr.SLT, hi64, hp0, hp1)
    val hslt_r = ctx.newInstRes(hslt)
    
    val hbb1 = ctx.newBB(hfv)
    val hbb1p0 = ctx.newNorParam(hbb1, hi64)
    val hbb1p1 = ctx.newNorParam(hbb1, hd)
    val hbb2 = ctx.newBB(hfv)
    
    val hbr2 = ctx.newBranch2(hentry, hslt_r)
    ctx.addDest(hbr2, DestKind.TRUE, hbb1, Seq(hadd_r, hfadd_r))
    ctx.addDest(hbr2, DestKind.FALSE, hbb2, Seq())

    val htrap1 = ctx.newTrap(hbb1, Seq())
    val htrap1_id = ctx.getID(b, htrap1)
    ctx.addKeepalives(htrap1, Seq(hbb1p0, hbb1p1))
    val hthreadexit1 = ctx.newCommInst(hbb1, CommInsts("@uvm.thread_exit").id, Seq(), Seq(), Seq(), Seq())
    
    val htrap2 = ctx.newTrap(hbb2, Seq())
    val htrap2_id = ctx.getID(b, htrap2)
    val hthreadexit2 = ctx.newCommInst(hbb2, CommInsts("@uvm.thread_exit").id, Seq(), Seq(), Seq(), Seq())
    
    ctx.loadBundleFromNode(b)
    
    val func = ctx.handleFromFunc(hfunc_id)
    val args = Seq(
        ctx.handleFromInt(100, 64),
        ctx.handleFromInt(200, 64),
        ctx.handleFromDouble(3.5),
        ctx.handleFromDouble(4.5)
        )
        
    var trap1Reached = false
    var trap2Reached = false
    
    testFunc(ctx, func, args, None) { (ctx, th, st, wpid) =>
      val cursor = ctx.newCursor(st)
      val cf = ctx.curFunc(cursor)
      val cfv = ctx.curFuncVer(cursor)
      val ci = ctx.curInst(cursor)
      ci match {
        case `htrap1_id` => {
          trap1Reached = true
          
          val Seq(hv0: MuIntValue, hv1: MuDoubleValue) = ctx.dumpKeepalives(cursor)
          ctx.closeCursor(cursor)
          
          ctx.handleToSInt(hv0) shouldBe 300
          ctx.handleToDouble(hv1) shouldBe 8.0
        }
        case `htrap2_id` => {
          trap2Reached = true
        }
      }
      TrapHandlerResult.ThreadExit()
    }
    
    trap1Reached shouldBe true
    trap2Reached shouldBe false

    ctx.closeContext()
  }

  it should "create multiple functions and call each other" in {
    val ctx = microVM.newContext()

    val b = ctx.newBundle()
    val hi64 = ctx.newTypeInt(b, 64)
    val hi64_id = ctx.getID(b, hi64)
    val hsig1 = ctx.newFuncSig(b, Seq(), Seq())
    val hsig2 = ctx.newFuncSig(b, Seq(hi64), Seq(hi64))
    
    val hfunc1 = ctx.newFunc(b, hsig1)
    val hfunc1_id = ctx.getID(b, hfunc1)
    ctx.setName(b, hfunc1, "@func1")
    val hfunc2 = ctx.newFunc(b, hsig2)
    val hfunc2_id = ctx.getID(b, hfunc2)
    ctx.setName(b, hfunc1, "@func2")

    val hci5 = ctx.newConstInt(b, hi64, 5L)

    val hf1v1 = ctx.newFuncVer(b, hfunc1)
    val hf1v1_id = ctx.getID(b,hf1v1)
    ctx.setName(b, hf1v1, "@func1.v1")
    
    val trap_id = {
      val entry = ctx.newBB(hf1v1)
      ctx.setName(b, entry, "@func1.v1.entry")
      
      val call = ctx.newCall(entry, hsig2, hfunc2, Seq(hci5))
      ctx.setName(b, call, "@func1.v1.entry.call")
      val res = ctx.newInstRes(call)
      ctx.setName(b, res, "@func1.v1.entry.res")
      
      val trap = ctx.newTrap(entry, Seq())
      val trap_id = ctx.getID(b, trap)
      ctx.setName(b, trap, "@func1.v1.entry.trap")
      ctx.addKeepalives(trap, Seq(res))
    
      val hthreadexit = ctx.newCommInst(entry, CommInsts("@uvm.thread_exit").id, Seq(), Seq(), Seq(), Seq())
      
      trap_id
    }

    val hf2v1 = ctx.newFuncVer(b, hfunc2)
    val hf2v1_id = ctx.getID(b,hf2v1)
    ctx.setName(b, hf2v1, "@func2.v1")

    {
      val entry = ctx.newBB(hf2v1)
      ctx.setName(b, entry, "@func2.v1.entry")
      val p0 = ctx.newNorParam(entry, hi64)
      
      val bbexit = ctx.newBB(hf2v1)
      ctx.setName(b, bbexit, "@func2.v1.exit")

      val bbrec = ctx.newBB(hf2v1)
      ctx.setName(b, bbexit, "@func2.v1.rec")
      val rp0 = ctx.newNorParam(bbrec, hi64)
      
      val const0 = ctx.newConstInt(b, hi64, 0)
      val eq0 = ctx.newCmp(entry, CmpOptr.EQ, hi64, p0, const0)
      val eq0_r = ctx.newInstRes(eq0)
      val br2 = ctx.newBranch2(entry, eq0_r)
      ctx.addDest(br2, DestKind.TRUE, bbexit, Seq())
      ctx.addDest(br2, DestKind.FALSE, bbrec, Seq(p0))
      
      val const1 = ctx.newConstInt(b, hi64, 1)
      ctx.newRet(bbexit, Seq(const1))
      
      val sub = ctx.newBinOp(bbrec, BinOptr.SUB, hi64, rp0, const1)
      val sub_r = ctx.newInstRes(sub)
      val reccall = ctx.newCall(bbrec, hsig2, hfunc2, Seq(sub_r))
      val reccall_r = ctx.newInstRes(reccall)
      val mul = ctx.newBinOp(bbrec, BinOptr.MUL, hi64, reccall_r, rp0)
      val mul_r = ctx.newInstRes(mul)
      ctx.newRet(bbrec, Seq(mul_r))
    }
    
    ctx.loadBundleFromNode(b)
    
    val func = ctx.handleFromFunc(hfunc1_id)
    val args = Seq()
    
    testFunc(ctx, func, args, None) { (ctx, th, st, wpid) =>
      val cursor = ctx.newCursor(st)
      val cf = ctx.curFunc(cursor)
      val cfv = ctx.curFuncVer(cursor)
      val ci = ctx.curInst(cursor)
      ci match {
        case `trap_id` => {
          
          val Seq(hres: MuIntValue) = ctx.dumpKeepalives(cursor)
          ctx.closeCursor(cursor)
          
          ctx.handleToSInt(hres) shouldBe 120
        }
      }
      TrapHandlerResult.Rebind(st, HowToResume.PassValues(Seq()))

    }
    
    ctx.closeContext()
  }
  
  
  it should "create more instructions" in {
    implicit val ctx = microVM.newContext()
    
    val b = ctx.newBundle()
    val hi1 = ctx.newTypeInt(b, 1)
    val hi8 = ctx.newTypeInt(b, 8)
    val hi64 = ctx.newTypeInt(b, 64)
    val hf = ctx.newTypeFloat(b)
    val hf4 = ctx.newTypeVector(b, hf, 4)
    
    val cf0 = ctx.newConstFloat(b, hf, 0.0f)
    val cf1 = ctx.newConstFloat(b, hf, 1.0f)
    val cf2 = ctx.newConstFloat(b, hf, 2.0f)
    val cf3 = ctx.newConstFloat(b, hf, 3.0f)
    val cv0 = ctx.newConstSeq(b, hf4, Seq(cf0, cf0, cf1, cf1))
    val cv0_id = ctx.getID(b, cv0)
    val cv1 = ctx.newConstSeq(b, hf4, Seq(cf2, cf3, cf2, cf3))
    val cv1_id = ctx.getID(b, cv1)
    
    val hsig = ctx.newFuncSig(b, Seq(hf4, hf4), Seq())
    val hfunc = ctx.newFunc(b, hsig)
    val hfunc_id = ctx.getID(b, hfunc)
    val hfv = ctx.newFuncVer(b, hfunc)
    val hentry = ctx.newBB(hfv)
    val hentry_p0 = ctx.newNorParam(hentry, hf4)
    val hentry_p1 = ctx.newNorParam(hentry, hf4)
    val hadd_r = ctx.newBinOp(hentry, BinOptr.FADD, hf4, hentry_p0, hentry_p1).R
    
    val const0 = ctx.newConstInt(b, hi64, 0)
    val const1 = ctx.newConstInt(b, hi64, 1)
    val hp00_r = ctx.newExtractElement(hentry, hf4, hi64, hentry_p0, const0).R
    val hp01_r = ctx.newExtractElement(hentry, hf4, hi64, hentry_p0, const1).R
    val hfolt_r = ctx.newCmp(hentry, CmpOptr.FOLT, hf, hp00_r, hp01_r).R
    val hsel_r = ctx.newSelect(hentry, hi1, hf, hfolt_r, hp00_r, hp01_r).R
    
    val hts = ctx.newTypeStruct(b, Seq(hi64, hf))
    val constf1 = ctx.newConstFloat(b, hf, 1.0f)
    val consts = ctx.newConstSeq(b, hts, Seq(const1, constf1))
    
    val hev_r = ctx.newExtractValue(hentry, hts, 0, consts).R
    val hiv_r = ctx.newInsertValue(hentry, hts, 0, consts, const0).R
    
    val hnew_r = ctx.newNew(hentry, hi64).R
    
    val hth = ctx.newTypeHybrid(b, Seq(hi64), hi8)
    val hnewhybrid_r = ctx.newNewHybrid(hentry, hth, hi64, const1).R
    
    val halloca_r = ctx.newAlloca(hentry, hi64).R
    val hallocahybrid_r = ctx.newAllocaHybrid(hentry, hth, hi64, const1).R
    
    val hgetiref_r = ctx.newGetIRef(hentry, hi64, hnew_r).R
    val hgetfieldiref_r = ctx.newGetFieldIRef(hentry, false, hth, 0, hallocahybrid_r).R
    
    val hta = ctx.newTypeArray(b, hi64, 10)
    val halloca2_r = ctx.newAlloca(hentry, hta).R
    
    val hgetelemiref_r = ctx.newGetElemIRef(hentry, false, hta, hi64, halloca2_r, const0).R
    val hshiftiref_r = ctx.newShiftIRef(hentry, false, hi64, hi64, hgetelemiref_r, const1).R
    val hgetvarpartiref_r = ctx.newGetVarPartIRef(hentry, false, hth, hgetiref_r).R
    
    val hload_r = ctx.newLoad(hentry, false, MemoryOrder.SEQ_CST, hi64, halloca_r).R
    val hstore = ctx.newStore(hentry, false, MemoryOrder.SEQ_CST, hi64, halloca_r, const1)
    
    val (hcmpxchg_r, hcmpxchg_s) = ctx.newCmpXchg(hentry, false, false, MemoryOrder.SEQ_CST, MemoryOrder.SEQ_CST,
        hi64, halloca_r, const1, const0).R2
        
    val hatomicrmw_r = ctx.newAtomicRMW(hentry, false, MemoryOrder.SEQ_CST, AtomicRMWOptr.ADD,
        hi64, halloca_r, const1).R

    val hfence = ctx.newFence(hentry, MemoryOrder.SEQ_CST)
    
    val hsigvv = ctx.newFuncSig(b, Seq(), Seq())
    val hdumbfunc = ctx.newFunc(b, hsigvv)
    val hdumbfv = ctx.newFuncVer(b, hdumbfunc)
    val hdumbentry = ctx.newBB(hdumbfv)
    val hdumbthreadexit = ctx.newCommInst(hdumbentry, CommInsts("@uvm.thread_exit").id, Seq(), Seq(), Seq(), Seq())
    
    val hnewstack_r = ctx.newCommInst(hentry, CommInsts("@uvm.new_stack").id, Seq(), Seq(), Seq(hsigvv), Seq(hdumbfunc)).R
    val hnewthread = ctx.newNewThread(hentry, hnewstack_r, None)
    val hnewthread_r = hnewthread.R
    ctx.setNewStackPassValues(hnewthread, Seq(), Seq())
    
    val htrap = ctx.newTrap(hentry, Seq())

    val hthreadexit = ctx.newCommInst(hentry, CommInsts("@uvm.thread_exit").id, Seq(), Seq(), Seq(), Seq())
    
    ctx.loadBundleFromNode(b)
    
    val func = ctx.handleFromFunc(hfunc_id)
    val a0 = ctx.handleFromConst(cv0_id)
    val a1 = ctx.handleFromConst(cv0_id)

    testFunc(ctx, func, Seq(a0, a1), None) { (ctx, th, st, wpid) =>
      TrapHandlerResult.Rebind(st, HowToResume.PassValues(Seq()))
    }
    
    ctx.closeContext()
  }
  
}