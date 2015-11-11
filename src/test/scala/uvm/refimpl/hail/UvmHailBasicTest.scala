package uvm.refimpl.hail

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import MemoryOrder._
import AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.refimpl.TrapHandlerResult.{ ThreadExit, Rebind }
import uvm.refimpl.HowToResume.{ PassValues, ThrowExc }
import ch.qos.logback.classic.Level._

import uvm.refimpl.RichMuCtx._

class UvmHailBasicTest extends UvmHailTesterBase {
  import UvmHailBasicTest._

  setLogLevels(ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.hail" -> DEBUG //"uvm.refimpl.mem" -> DEBUG,
    //"uvm.refimpl.itpr" -> DEBUG
    )

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-hail-test/basic-hail-test.uir")

  behavior of "The HAIL reader"

  it should "create objects of primitive types and assign to global cells" in {
    val mc = microVM.newContext()

    loadHailFromFile(mc, "tests/uvm-hail-test/basic-hail-test.hail")

    mc.loadIntGlobal("@g_ri8") shouldBe 0x5a
    mc.loadIntGlobal("@g_ri16") shouldBe 0x55aa
    mc.loadIntGlobal("@g_ri32") shouldBe 0x55aa55aa
    mc.loadIntGlobal("@g_ri64") shouldBe 0x5a5a55aa5a5a55aaL
    mc.loadFloatGlobal("@g_rfloat") shouldBe 3.14F
    mc.loadDoubleGlobal("@g_rdouble") shouldBe 6.28

    mc.refEq(mc.loadRefGlobal("@g_rrv"), mc.handleFromConst(InternalTypes.NULL_REF_VOID.id).asInstanceOf[MuGenRefValue]) shouldBe true
    mc.refEq(mc.loadIRefGlobal("@g_rirv"), mc.handleFromConst(InternalTypes.NULL_IREF_VOID.id).asInstanceOf[MuGenRefValue]) shouldBe true
    mc.refEq(mc.loadRefGlobal("@g_rwrv"), mc.handleFromConst(InternalTypes.NULL_REF_VOID.id).asInstanceOf[MuGenRefValue]) shouldBe true
    mc.refEq(mc.loadFuncRefGlobal("@g_rfrv_v"), mc.handleFromConst(InternalTypes.NULL_FUNCREF_VV.id).asInstanceOf[MuGenRefValue]) shouldBe true
    mc.refEq(mc.loadThreadRefGlobal("@g_rthread"), mc.handleFromConst(InternalTypes.NULL_THREADREF.id).asInstanceOf[MuGenRefValue]) shouldBe true
    mc.refEq(mc.loadStackRefGlobal("@g_rstack"), mc.handleFromConst(InternalTypes.NULL_STACKREF.id).asInstanceOf[MuGenRefValue]) shouldBe true

    mc.closeContext()
  }

  it should "evaluate values from HAIL names and global names" in {
    val mc = microVM.newContext()

    loadHailFromFile(mc, "tests/uvm-hail-test/basic-hail-test-2.hail")

    mc.loadIntGlobal("@g_ri8") shouldBe 42
    mc.loadIntGlobal("@g_ri16") shouldBe 43
    mc.loadIntGlobal("@g_ri32") shouldBe 4
    mc.loadIntGlobal("@g_ri64") shouldBe 5
    mc.loadFloatGlobal("@g_rfloat") shouldBe 6.0F
    mc.loadDoubleGlobal("@g_rdouble") shouldBe 7.0

    val hr = mc.loadGlobal("@g_rrv").asInstanceOf[MuRefValue]

    println(hr)

    val hr_ir = mc.getIRef(hr)

    println(hr_ir)

    val hr_content = mc.loadRef(NOT_ATOMIC, hr_ir)

    mc.refEq(hr, hr_content) shouldBe true
    mc.refEq(mc.loadIRefGlobal("@g_rirv"), mc.handleFromGlobal("@g_rirv")) shouldBe true
    mc.refEq(mc.loadFuncRefGlobal("@g_rfrv_v"), mc.handleFromFunc("@foo")) shouldBe true

    mc.closeContext()
  }

  it should "initialise pointers and tagref64" in {
    val mc = microVM.newContext()

    loadHailFromFile(mc, "tests/uvm-hail-test/basic-hail-test-3.hail")

    def assertPtr(id: Int, v: Long) {
      val hp = mc.loadGlobal(id).asInstanceOf[MuUPtrValue]
      mc.handleToPtr(hp) shouldBe v
    }
    def assertFP(id: Int, v: Long) {
      val hp = mc.loadGlobal(id).asInstanceOf[MuUFPValue]
      mc.handleToFP(hp) shouldBe v
    }
    def assertTR64FP(id: Int, v: Double) {
      val htr = mc.loadGlobal(id).asInstanceOf[MuTagRef64Value]
      mc.tr64IsFp(htr) shouldBe true
      val hd = mc.tr64ToFp(htr)
      mc.handleToDouble(hd) shouldBe v
    }
    def assertTR64Int(id: Int, v: Long) {
      val htr = mc.loadGlobal(id).asInstanceOf[MuTagRef64Value]
      mc.tr64IsInt(htr) shouldBe true
      val hi = mc.tr64ToInt(htr)
      mc.handleToUInt(hi) shouldBe v
    }
    def assertTR64Ref(id: Int, hr: MuRefValue, t: Int) {
      val htr = mc.loadGlobal(id).asInstanceOf[MuTagRef64Value]
      mc.tr64IsRef(htr) shouldBe true
      val htr_r = mc.tr64ToRef(htr)
      val htr_t = mc.tr64ToTag(htr)
      mc.refEq(htr_r, hr) shouldBe true
      mc.handleToUInt(htr_t) shouldBe t
    }

    assertPtr("@g_uptr1", 42)
    assertPtr("@g_uptr2", 0x123456789abcdef0L)

    assertFP("@g_ufp1", 43)
    assertFP("@g_ufp2", 0xfedcba9876543210L)
    val foo_native = mc.handleFromExpose("@foo_native")
    assertFP("@g_ufp3", mc.handleToFP(foo_native))

    assertTR64FP("@g_tr1", 3.14)
    assertTR64Int("@g_tr2", 0xfedcba9876543L)

    val nullref = mc.handleFromConst("@NULLREF").asInstanceOf[MuRefValue]
    assertTR64Ref("@g_tr3", nullref, 31)

    assertTR64FP("@g_tr4", 7.0)
    assertTR64Int("@g_tr5", 99L)

    val g_empty = mc.loadGlobal("@g_empty").asInstanceOf[MuRefValue]
    assertTR64Ref("@g_tr6", g_empty, 15)

    mc.closeContext()
  }

  it should "initialise composite types" in {
    val mc = microVM.newContext()

    loadHailFromFile(mc, "tests/uvm-hail-test/basic-hail-test-4.hail")

    {
      val hr = mc.handleFromGlobal("@g_small_struct")

      { val fr = mc.getFieldIRef(hr, 0); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 1

      { val fr = mc.getFieldIRef(hr, 1); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 2

      { val fr = mc.getFieldIRef(hr, 2); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 3

      { val fr = mc.getFieldIRef(hr, 3); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 4
    }

    {
      val hr = mc.handleFromGlobal("@g_big_struct")

      { val fr = mc.getFieldIRef(hr, 0); val v = mc.loadFloat(NOT_ATOMIC, fr); mc.handleToFloat(v) } shouldBe 5.0f

      {
        val hr1 = mc.getFieldIRef(hr, 1)

        { val fr = mc.getFieldIRef(hr1, 0); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 6

        { val fr = mc.getFieldIRef(hr1, 1); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 7

        { val fr = mc.getFieldIRef(hr1, 2); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 8

        { val fr = mc.getFieldIRef(hr1, 3); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 9
      }

      { val fr = mc.getFieldIRef(hr, 2); val v = mc.loadDouble(NOT_ATOMIC, fr); mc.handleToDouble(v) } shouldBe 10.0d
    }

    {
      val hs = mc.handleFromGlobal("@g_small_array")
      for (i <- 0 until 6) mc.autoDispose { x =>
        val hi = x << mc.handleFromInt(i, 64)
        val he = x << mc.getElemIRef(hs, hi)
        val hev = x << mc.loadInt(NOT_ATOMIC, he)
        val v = mc.handleToSInt(hev)
        v shouldEqual (i + 1)
      }
    }

    {
      val hs = mc.handleFromGlobal("@g_vector")
      for (i <- 0 until 4) mc.autoDispose { x =>
        val hi = x << mc.handleFromInt(i, 64)
        val he = x << mc.getElemIRef(hs, hi)
        val hev = x << mc.loadFloat(NOT_ATOMIC, he)
        val v = mc.handleToFloat(hev)
        v shouldEqual (i + 1.0f)
      }
    }

    {
      val hr = mc.handleFromGlobal("@g_big_struct2")

      { val fr = mc.getFieldIRef(hr, 0); val v = mc.loadFloat(NOT_ATOMIC, fr); mc.handleToFloat(v) } shouldBe 3.14f

      {
        val hr1 = mc.getFieldIRef(hr, 1)

        { val fr = mc.getFieldIRef(hr1, 0); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 0

        { val fr = mc.getFieldIRef(hr1, 1); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 0

        { val fr = mc.getFieldIRef(hr1, 2); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 3

        { val fr = mc.getFieldIRef(hr1, 3); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 0
      }

      { val fr = mc.getFieldIRef(hr, 2); val v = mc.loadDouble(NOT_ATOMIC, fr); mc.handleToDouble(v) } shouldBe 0.0d
    }

    {
      val hs = mc.handleFromGlobal("@g_small_array2")
      for (i <- 0 until 6) mc.autoDispose { x =>
        val hi = x << mc.handleFromInt(i, 64)
        val he = x << mc.getElemIRef(hs, hi)
        val hev = x << mc.loadInt(NOT_ATOMIC, he)
        val v = mc.handleToSInt(hev)
        if (i >= 3 && i <= 4) {
          v shouldEqual 0
        } else {
          v shouldEqual (i + 1)
        }
      }
    }

    {

      val hrgh = mc.handleFromGlobal("@g_my_hybrid_r")
      val hrh = mc.loadRef(NOT_ATOMIC, hrgh)
      val hrhi = mc.getIRef(hrh)

      { val fr = mc.getFieldIRef(hrhi, 0); val v = mc.loadInt(NOT_ATOMIC, fr); mc.handleToSInt(v) } shouldBe 1

      { val fr = mc.getFieldIRef(hrhi, 1); val v = mc.loadDouble(NOT_ATOMIC, fr); mc.handleToDouble(v) } shouldBe 2.0

      {
        val hrvp = mc.getVarPartIRef(hrhi)
        for (i <- 0 until 3) mc.autoDispose { x =>
          val hi = x << mc.handleFromInt(i, 64)
          val he = x << mc.shiftIRef(hrvp, hi)
          val hev = x << mc.loadInt(NOT_ATOMIC, he)
          val v = mc.handleToSInt(hev)
          v shouldEqual (i + 3)
        }
      }

    }
    mc.closeContext()
  }

  it should "recognise the '&' expression to assign internal references" in {
    val mc = microVM.newContext()
    
    loadHailFromFile(mc, "tests/uvm-hail-test/basic-hail-test-5.hail")

    val hBar = mc.loadGlobal("@g_my_hybrid_r").asInstanceOf[MuRefValue]
    val hBarIr = mc.getIRef(hBar)

    def assertIREqual(globalID: Int, expected: MuIRefValue): Unit = {
      val hIr = mc.loadGlobal(globalID).asInstanceOf[MuIRefValue]
      // printf("found offset: 0x%x expected offset: 0x%x\n", hIr.vb.offset, expected.vb.offset)
      mc.refEq(hIr, expected) shouldBe true
    }

    assertIREqual("@g_iri64", mc.getFieldIRef(hBarIr, 0))
    assertIREqual("@g_irdouble", mc.getFieldIRef(hBarIr, 1))
    
    val hBarIr_vp = mc.getVarPartIRef(hBarIr)
    
    assertIREqual("@g_iri8", hBarIr_vp)
    
    val hBarIr_vp_2 = mc.shiftIRef(hBarIr_vp, mc.handleFromInt(2, 64))
    val hBarIr_vp_3 = mc.shiftIRef(hBarIr_vp, mc.handleFromInt(3, 64))

    assertIREqual("@g_iri8_2", hBarIr_vp_2)
    assertIREqual("@g_irvoid", hBarIr_vp_3)

    mc.closeContext()
  }
}

object UvmHailBasicTest {
  implicit class RicherMuCtx(val mc: MuCtx) extends AnyVal {
    import uvm.refimpl.RichMuCtx._
    def loadGlobal(id: Int): MuValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      mc.load(NOT_ATOMIC, hg)
    }

    def loadIntGlobal(id: Int): Long = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hi = x << mc.loadInt(NOT_ATOMIC, hoir)
      mc.handleToSInt(hi).toLong
    }
    def loadFloatGlobal(id: Int): Float = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hf = x << mc.loadFloat(NOT_ATOMIC, hoir)
      mc.handleToFloat(hf)
    }
    def loadDoubleGlobal(id: Int): Double = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hd = x << mc.loadDouble(NOT_ATOMIC, hoir)
      mc.handleToDouble(hd)
    }
    def loadRefGlobal(id: Int): MuRefValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hr = mc.loadRef(NOT_ATOMIC, hoir)
      hr
    }
    def loadIRefGlobal(id: Int): MuIRefValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hr = mc.loadIRef(NOT_ATOMIC, hoir)
      hr
    }
    def loadFuncRefGlobal(id: Int): MuFuncRefValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hr = mc.loadFuncRef(NOT_ATOMIC, hoir)
      hr
    }
    def loadThreadRefGlobal(id: Int): MuThreadRefValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hr = mc.loadThreadRef(NOT_ATOMIC, hoir)
      hr
    }
    def loadStackRefGlobal(id: Int): MuStackRefValue = mc.autoDispose { x =>
      val hg = x << mc.handleFromGlobal(id)
      val ho = x << mc.loadRef(NOT_ATOMIC, hg)
      val hoir = x << mc.getIRef(ho)
      val hr = mc.loadStackRef(NOT_ATOMIC, hoir)
      hr
    }
  }
}