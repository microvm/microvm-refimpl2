package uvm.refimpl.mem

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import MemoryOrder._
import AtomicRMWOptr._

class UvmMemOperationsSpec extends UvmBundleTesterBase {

  // The heap size is intentionally reduced to make GC more often
  // The heap is divided in two halves. There is a 256KiB small object space (with 8 32KiB blocks) and a 256KiB large
  // object space.
  override def makeMicroVM() = new MicroVM(heapSize = 512L * 1024L)

  microVM.memoryManager.heap.space.debugLogBlockStates()

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/uvm-mem-test-bundle.uir")

  microVM.memoryManager.heap.space.debugLogBlockStates()

  behavior of "UVM memory manager"

  it should "allocate scalar objects" in {
    val ctx = microVM.newContext()

    val h = ctx.newFixed("@i64")
    val h2 = ctx.newFixed("@a0")

    ctx.closeContext()

    microVM.memoryManager.heap.space.debugLogBlockStates()
  }

  it should "allocate hybrid objects" in {
    val ctx = microVM.newContext()

    val hlen = ctx.handleFromInt64(1024)
    val h = ctx.newHybrid("@h0", hlen)

    val hlen2 = ctx.handleFromInt64(128 * 1024)
    val h2 = ctx.newHybrid("@h0", hlen2)

    ctx.closeContext()
  }

  it should "automatically trigger GC if the memory is full" in {
    val ctx = microVM.newContext()

    val hheld = ctx.newFixed("@i64") // Objects held in the CA should survive the GC

    val hlen = ctx.handleFromInt64(1024)

    val allocCount = 300 // enough to fill the 256KiB small object space at least once.

    for (i <- (0 until allocCount)) {
      val h = ctx.newHybrid("@h0", hlen)
      ctx.deleteValue(h)
    }

    ctx.closeContext()
  }

  it should "defrag heavily fragmented heap" in {
    val ctx = microVM.newContext()

    val hlen = ctx.handleFromInt64(1024)

    val allocCount = 300 // enough to fill the 256KiB small object space at least once.

    for (i <- (0 until allocCount)) {
      val h = ctx.newHybrid("@h0", hlen)
      ctx.deleteValue(h)
      val hBreadcrumb = ctx.newFixed("@i64")
    }

    ctx.closeContext()
  }

  it should "perform non-atomic memory access to all supported data types" in {
    val ctx = microVM.newContext()

    val hs = ctx.newFixed("@s1")

    val hsi = ctx.getIRef(hs)

    val hf0 = ctx.getFieldIRef(hsi, 0)
    val hf1 = ctx.getFieldIRef(hsi, 1)
    val hf2 = ctx.getFieldIRef(hsi, 2)
    val hf3 = ctx.getFieldIRef(hsi, 3)
    val hf4 = ctx.getFieldIRef(hsi, 4)
    val hf5 = ctx.getFieldIRef(hsi, 5)
    val hf6 = ctx.getFieldIRef(hsi, 6)
    val hf7 = ctx.getFieldIRef(hsi, 7)
    val hf8 = ctx.getFieldIRef(hsi, 8)

    def testIntWriteRead(len: Int, field: MuIRefValue, v: BigInt): Unit = {
      val hv = ctx.handleFromInt(v, len)
      ctx.store(NOT_ATOMIC, field, hv)
      val hvOut = ctx.load(NOT_ATOMIC, field).asInstanceOf[MuIntValue]
      val vOut = ctx.handleToUInt(hvOut)
      vOut shouldEqual v
    }

    testIntWriteRead(8, hf0, 42)
    testIntWriteRead(8, hf0, 0xffL)
    testIntWriteRead(16, hf1, 42)
    testIntWriteRead(16, hf1, 0xffffL)
    testIntWriteRead(32, hf2, 42)
    testIntWriteRead(32, hf2, 0xffffffffL)
    testIntWriteRead(64, hf3, 42)
    testIntWriteRead(64, hf3, BigInt("ffffffffffffffff", 16))

    val hfv = ctx.handleFromFloat(42.0f)
    ctx.store(NOT_ATOMIC, hf4, hfv)
    val hfOut = ctx.load(NOT_ATOMIC, hf4).asInstanceOf[MuFloatValue]
    val fOut = ctx.handleToFloat(hfOut)
    fOut shouldEqual 42.0f

    val hdv = ctx.handleFromDouble(42.0d)
    ctx.store(NOT_ATOMIC, hf5, hdv)
    val hdOut = ctx.load(NOT_ATOMIC, hf5).asInstanceOf[MuDoubleValue]
    val dOut = ctx.handleToDouble(hdOut)
    dOut shouldEqual 42.0d

    val hrv = ctx.refcast(hs, "@refvoid")
    val hirv = ctx.refcast(hf0, "@irefvoid")

    ctx.store(NOT_ATOMIC, hf6, hrv)
    val hrvOut = ctx.load(NOT_ATOMIC, hf6)
    hrvOut.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

    ctx.store(NOT_ATOMIC, hf7, hirv)
    val hirvOut = ctx.load(NOT_ATOMIC, hf7)
    hirvOut.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
    hirvOut.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

    ctx.store(NOT_ATOMIC, hf8, hrv)
    val hwrvOut = ctx.load(NOT_ATOMIC, hf8)
    // GC cannot clear the weakref because there is a handle to the referent object.
    hwrvOut.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

    val hs2 = ctx.newFixed("@s2")

    val hs2i = ctx.getIRef(hs2)
    val hs2f0 = ctx.getFieldIRef(hs2i, 0)
    val hs2f1 = ctx.getFieldIRef(hs2i, 1)
    val hs2f2 = ctx.getFieldIRef(hs2i, 2)
    val hs2f3 = ctx.getFieldIRef(hs2i, 3)

    val hFun = ctx.handleFromFunc("@fun")
    ctx.store(NOT_ATOMIC, hs2f0, hFun)
    val hFunOut = ctx.load(NOT_ATOMIC, hs2f0)
    hFunOut.vb.asInstanceOf[BoxFunc].func shouldEqual hFun.vb.asInstanceOf[BoxFunc].func

    // TODO: Test thread/stack when implemented.

    val hTRFp = ctx.tr64FromFp(hdv)
    ctx.store(NOT_ATOMIC, hs2f3, hTRFp)
    val hTRFpOut = ctx.load(NOT_ATOMIC, hs2f3).asInstanceOf[MuTagRef64Value]
    val hTRFpToDouble = ctx.tr64ToFp(hTRFpOut)
    val hTRFpVal = ctx.handleToDouble(hTRFpToDouble)
    hTRFpVal shouldEqual 42.0d

    val hI52 = ctx.handleFromInt(0xfedcba9876543L, 52)
    val hTRInt = ctx.tr64FromInt(hI52)
    ctx.store(NOT_ATOMIC, hs2f3, hTRInt)
    val hTRIntOut = ctx.load(NOT_ATOMIC, hs2f3).asInstanceOf[MuTagRef64Value]
    val hTRIntToI52 = ctx.tr64ToInt(hTRIntOut)
    val hTRIntVal = ctx.handleToUInt(hTRIntToI52)
    hTRIntVal shouldEqual 0xfedcba9876543L

    val hI6 = ctx.handleFromInt(29, 6)
    val hTRRef = ctx.tr64FromRef(hrv, hI6)
    ctx.store(NOT_ATOMIC, hs2f3, hTRRef)
    val hTRRefOut = ctx.load(NOT_ATOMIC, hs2f3).asInstanceOf[MuTagRef64Value]
    val hTRRefToRef = ctx.tr64ToRef(hTRRefOut)
    val hTRRefToTag = ctx.tr64ToTag(hTRRefOut)
    val hTRRefTagVal = ctx.handleToUInt(hTRRefToTag)
    hTRRefToRef.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef
    hTRRefTagVal shouldEqual 29

    val hAry = ctx.newFixed("@a1")
    val hAryI = ctx.getIRef(hAry)

    for (i <- 0 until 10) {
      val hD = ctx.handleFromDouble(i.toDouble)
      val hI = ctx.handleFromInt64(i)
      val hAryElem = ctx.getElemIRef(hAryI, hI)
      val hAryElem0 = ctx.getFieldIRef(hAryElem, 0)
      val hAryElem1 = ctx.getFieldIRef(hAryElem, 1)

      ctx.store(NOT_ATOMIC, hAryElem0, hD)
      ctx.store(NOT_ATOMIC, hAryElem1, hI)
      val hDOut = ctx.load(NOT_ATOMIC, hAryElem0).asInstanceOf[MuDoubleValue]
      val hIOut = ctx.load(NOT_ATOMIC, hAryElem1).asInstanceOf[MuIntValue]
      val hDOutVal = ctx.handleToDouble(hDOut)
      val hIOutVal = ctx.handleToSInt(hIOut.asInstanceOf[MuIntValue])
      hDOutVal shouldEqual i.toDouble
      hIOutVal.intValue shouldEqual i
    }

    val hRVI32 = ctx.newFixed("@4xi32")
    val hIrVI32 = ctx.getIRef(hRVI32)
    val vi32: Seq[BigInt] = Seq(1, 2, 3, 4)
    val hVI32 = ctx.handleFromConst("@4xI32_V3")
    ctx.store(NOT_ATOMIC, hIrVI32, hVI32)
    val hVI32Out = ctx.load(NOT_ATOMIC, hIrVI32).asInstanceOf[MuVectorValue]
    val vi32Out = Seq(0, 1, 2, 3).map { n =>
      val hN = ctx.handleFromInt(n, 32)
      val elem = ctx.extractElement(hVI32Out, hN).asInstanceOf[MuIntValue]
      val v = ctx.handleToSInt(elem)
      ctx.deleteValue(hN)
      ctx.deleteValue(elem)
      v
    }
    vi32Out shouldEqual vi32

    for (i <- (0 until 4)) {
      val hI = ctx.handleFromInt64(i)
      val hElem = ctx.getElemIRef(hIrVI32, hI)
      val hv = ctx.load(NOT_ATOMIC, hElem).asInstanceOf[MuIntValue]
      val v = ctx.handleToSInt(hv)
      v shouldEqual vi32(i)
    }

    val hRVF = ctx.newFixed("@4xfloat")
    val hIrVF = ctx.getIRef(hRVF)
    val vf: Seq[Float] = Seq(1.0f, 2.0f, 3.0f, 4.0f)
    val hVF = ctx.handleFromConst("@4xF_V3")
    ctx.store(NOT_ATOMIC, hIrVF, hVF)
    val hVFOut = ctx.load(NOT_ATOMIC, hIrVF).asInstanceOf[MuVectorValue]
    val vfOut = Seq(0, 1, 2, 3).map { n =>
      val hN = ctx.handleFromInt(n, 32)
      val elem = ctx.extractElement(hVFOut, hN).asInstanceOf[MuFloatValue]
      val v = ctx.handleToFloat(elem)
      ctx.deleteValue(hN)
      ctx.deleteValue(elem)
      v
    }
    vfOut shouldEqual vf

    for (i <- (0 until 4)) {
      val hI = ctx.handleFromInt64(i)
      val hElem = ctx.getElemIRef(hIrVF, hI)
      val hv = ctx.load(NOT_ATOMIC, hElem).asInstanceOf[MuFloatValue]
      val v = ctx.handleToFloat(hv)
      v shouldEqual vf(i)
    }

    val hRVD = ctx.newFixed("@2xdouble")
    val hIrVD = ctx.getIRef(hRVD)
    val vd: Seq[Double] = Seq(1.0d, 2.0d)
    val hVD = ctx.handleFromConst("@2xD_V3")
    ctx.store(NOT_ATOMIC, hIrVD, hVD)
    val hVDOut = ctx.load(NOT_ATOMIC, hIrVD).asInstanceOf[MuVectorValue]
    val vdOut = Seq(0, 1).map { n =>
      ctx.autoDispose { x =>
        val hN = x(ctx.handleFromInt(n, 32))
        val elem = x(ctx.extractElement(hVDOut, hN).asInstanceOf[MuDoubleValue])
        ctx.handleToDouble(elem)
      }
    }
    vdOut shouldEqual vd

    for (i <- (0 until 2)) {
      val hI = ctx.handleFromInt64(i)
      val hElem = ctx.getElemIRef(hIrVD, hI)
      val hv = ctx.load(NOT_ATOMIC, hElem).asInstanceOf[MuDoubleValue]
      val v = ctx.handleToDouble(hv)
      v shouldEqual vd(i)
    }

    val hSz = ctx.handleFromInt32(10)
    val hh = ctx.newHybrid("@h1", hSz)

    val hhi = ctx.getIRef(hh)
    val hf = ctx.getFieldIRef(hhi, 0)
    val hv0 = ctx.getVarPartIRef(hhi)

    val hfix0 = ctx.getFieldIRef(hf, 0)
    val hfix1 = ctx.getFieldIRef(hf, 1)

    val hOff = ctx.handleFromInt(5, 16)
    val hv5 = ctx.shiftIRef(hv0, hOff)

    for (i <- 0 until 10) {
      val hI = ctx.handleFromInt64(i)
      val hHybElem = ctx.shiftIRef(hv0, hI)

      ctx.store(NOT_ATOMIC, hHybElem, hI)
      val hIOut = ctx.load(NOT_ATOMIC, hHybElem)
      val hIOutVal = ctx.handleToSInt(hIOut.asInstanceOf[MuIntValue])
      hIOutVal.intValue shouldEqual i
    }

    ctx.closeContext()
  }

  it should "perform atomic memory access to all supported data types" in {
    val ctx = microVM.newContext()

    val hs = ctx.newFixed("@s1")

    val hsi = ctx.getIRef(hs)

    val hf2 = ctx.getFieldIRef(hsi, 2)
    val hf3 = ctx.getFieldIRef(hsi, 3)

    val hf6 = ctx.getFieldIRef(hsi, 6)
    val hf7 = ctx.getFieldIRef(hsi, 7)
    val hf8 = ctx.getFieldIRef(hsi, 8)

    def testIntCmpXchg(len: Int, field: MuIRefValue, old: BigInt, expected: BigInt, desired: BigInt): Unit = {
      val hExpected = ctx.handleFromInt(expected, len)
      val hDesired = ctx.handleFromInt(desired, len)
      val (hResult: MuIntValue, succ) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, field, hExpected, hDesired)
      succ shouldEqual (old == expected)
      val result = ctx.handleToSInt(hResult)
      result shouldEqual old

      val hResult2 = ctx.load(SEQ_CST, field).asInstanceOf[MuIntValue]
      val result2 = ctx.handleToSInt(hResult2)
      result2 shouldEqual (if (old == expected) desired else old)
    }

    def testIntXchg(len: Int, field: MuIRefValue, old: BigInt, newVal: BigInt): Unit = {
      val hNewVal = ctx.handleFromInt(newVal, len)
      val hResult = ctx.atomicRMW(SEQ_CST, XCHG, field, hNewVal).asInstanceOf[MuIntValue]
      val result = ctx.handleToSInt(hResult)
      result shouldEqual old

      val hResult2 = ctx.load(SEQ_CST, field).asInstanceOf[MuIntValue]
      val result2 = ctx.handleToSInt(hResult2)
      result2 shouldEqual newVal
    }

    testIntCmpXchg("@i32", hf2, 0, 0, 1)
    testIntCmpXchg("@i32", hf2, 1, 0, 2)
    testIntXchg("@i32", hf2, 1, 3)
    testIntCmpXchg("@i64", hf3, 0, 0, 1)
    testIntCmpXchg("@i64", hf3, 1, 0, 2)
    testIntXchg("@i64", hf3, 1, 3)

    val hrv = ctx.refcast(hs, "@refvoid")
    val hirv = ctx.refcast(hsi, "@irefvoid")

    val hs2 = ctx.newFixed("@s2")

    val hs2i = ctx.getIRef(hs2)
    val hs2f0 = ctx.getFieldIRef(hs2i, 0)
    val hs2f1 = ctx.getFieldIRef(hs2i, 1)
    val hs2f2 = ctx.getFieldIRef(hs2i, 2)
    val hs2f3 = ctx.getFieldIRef(hs2i, 3)

    val hrv2 = ctx.refcast(hs2, "@refvoid")
    val hir2 = ctx.getIRef(hs2)
    val hirv2 = ctx.refcast(hir2, "@irefvoid")

    val hNullRv = ctx.handleFromConst("@NULLREF")
    val hNullIrv = ctx.handleFromConst("@NULLIREF")

    {
      val (hResult1, succ1) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf6, hNullRv, hrv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxRef].objRef shouldEqual 0L

      val (hResult2, succ2) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf6, hNullRv, hrv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult3 = ctx.atomicRMW(SEQ_CST, XCHG, hf6, hrv2)
      hResult3.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult4 = ctx.load(SEQ_CST, hf6)
      hResult4.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv2.vb.asInstanceOf[BoxRef].objRef
    }

    {
      val (hResult1, succ1) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf7, hNullIrv, hirv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxIRef].objRef shouldEqual 0L
      hResult1.vb.asInstanceOf[BoxIRef].offset shouldEqual 0L

      val (hResult2, succ2) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf7, hNullIrv, hirv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
      hResult2.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

      val hResult3 = ctx.atomicRMW(SEQ_CST, XCHG, hf7, hirv2)
      hResult3.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
      hResult3.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

      val hResult4 = ctx.load(SEQ_CST, hf7)
      hResult4.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv2.vb.asInstanceOf[BoxIRef].objRef
      hResult4.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv2.vb.asInstanceOf[BoxIRef].offset
    }

    {
      val (hResult1, succ1) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf8, hNullRv, hrv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxRef].objRef shouldEqual 0L

      val (hResult2, succ2) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hf8, hNullRv, hrv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult3 = ctx.atomicRMW(SEQ_CST, XCHG, hf8, hrv2)
      hResult3.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult4 = ctx.load(SEQ_CST, hf8)
      hResult4.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv2.vb.asInstanceOf[BoxRef].objRef
    }

    val hFun = ctx.handleFromFunc("@fun")
    val hFun2 = ctx.handleFromFunc("@fun2")
    val hNullF0 = ctx.handleFromConst("@NULLF0")

    {
      val (hResult1, succ1) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hs2f0, hNullF0, hFun)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxFunc].func shouldEqual None

      val (hResult2, succ2) = ctx.cmpXchg(SEQ_CST, SEQ_CST, false, hs2f0, hNullF0, hFun2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxFunc].func shouldEqual hFun.vb.asInstanceOf[BoxFunc].func
    }

    // TODO: Test thread and stack after implemented.

    def testIntAtomicRMW(len: Int, loc: MuIRefValue, optr: AtomicRMWOptr, lhs: BigInt, rhs: BigInt, expected: BigInt, signed: Boolean = false): Unit = {
      val hLhs = ctx.handleFromInt(lhs, len)
      val hRhs = ctx.handleFromInt(rhs, len)
      ctx.store(SEQ_CST, loc, hLhs)
      val hResult1 = ctx.atomicRMW(SEQ_CST, optr, loc, hRhs).asInstanceOf[MuIntValue]
      val result1 = ctx.handleToInt(hResult1, signed)
      result1 shouldEqual lhs
      val hResult2 = ctx.load(SEQ_CST, loc).asInstanceOf[MuIntValue]
      val result2 = ctx.handleToInt(hResult2, signed)
      result2 shouldEqual expected
    }

    testIntAtomicRMW("@i32", hf2, XCHG, 42, 43, 43)
    testIntAtomicRMW("@i32", hf2, ADD, 1, 2, 3)
    testIntAtomicRMW("@i32", hf2, SUB, 3, 2, 1)
    testIntAtomicRMW("@i32", hf2, AND, 0x55aa, 0x5a5a, 0x500a)
    testIntAtomicRMW("@i32", hf2, NAND, 0x55aa, 0x5a5a, ~0x500a, signed = true)
    testIntAtomicRMW("@i32", hf2, OR, 0x55aa, 0x5a5a, 0x5ffa)
    testIntAtomicRMW("@i32", hf2, XOR, 0x55aa, 0x5a5a, 0x0ff0)
    testIntAtomicRMW("@i32", hf2, MIN, -3, -2, -3, signed = true)
    testIntAtomicRMW("@i32", hf2, MAX, -3, -2, -2, signed = true)
    testIntAtomicRMW("@i32", hf2, UMIN, 3, 2, 2)
    testIntAtomicRMW("@i32", hf2, UMAX, 3, 2, 3)

    testIntAtomicRMW("@i64", hf3, XCHG, 42, 43, 43)
    testIntAtomicRMW("@i64", hf3, ADD, 1, 2, 3)
    testIntAtomicRMW("@i64", hf3, SUB, 3, 2, 1)
    testIntAtomicRMW("@i64", hf3, AND, 0x55aa, 0x5a5a, 0x500a)
    testIntAtomicRMW("@i64", hf3, NAND, 0x55aa, 0x5a5a, ~0x500a, signed = true)
    testIntAtomicRMW("@i64", hf3, OR, 0x55aa, 0x5a5a, 0x5ffa)
    testIntAtomicRMW("@i64", hf3, XOR, 0x55aa, 0x5a5a, 0x0ff0)
    testIntAtomicRMW("@i64", hf3, MIN, -3, -2, -3, signed = true)
    testIntAtomicRMW("@i64", hf3, MAX, -3, -2, -2, signed = true)
    testIntAtomicRMW("@i64", hf3, UMIN, 3, 2, 2)
    testIntAtomicRMW("@i64", hf3, UMAX, 3, 2, 3)

    ctx.closeContext()
  }
}