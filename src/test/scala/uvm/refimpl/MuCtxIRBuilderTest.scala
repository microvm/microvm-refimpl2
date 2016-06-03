package uvm.refimpl

import org.scalatest._

import ch.qos.logback.classic.Level._
import uvm._
import uvm.refimpl._
import uvm.refimpl.RichMuCtx._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.ssavariables._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.MemoryOrder._
import uvm.types._
import uvm.ir.textinput.ExtraMatchers

class MuCtxIRBuilderTest extends UvmBundleTesterBase with ExtraMatchers {
  setLogLevels(ROOT_LOGGER_NAME -> INFO,
    "uvm" -> INFO)
    
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
}