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

class UvmMemSpec extends FlatSpec with Matchers with BeforeAndAfter {

  // The heap size is intentionally reduced to make GC more often
  // The heap is divided in two halves. There is a 256KiB small object space (with 8 32KiB blocks) and a 256KiB large
  // object space.
  val microVM = new MicroVM(heapSize = 512L * 1024L);

  implicit def idOf(name: String): Int = microVM.globalBundle.allNs(name).id

  {
    microVM.memoryManager.heap.space.debugLogBlockStates()

    val ca = microVM.newClientAgent()

    val r = new FileReader("tests/uvm-refimpl-test/uvm-mem-test-bundle.uir")
    ca.loadBundle(r)

    r.close()
    ca.close()

    microVM.memoryManager.heap.space.debugLogBlockStates()
  }

  behavior of "UVM memory manager"

  it should "allocate scalar objects" in {
    val ca = microVM.newClientAgent()

    val h = ca.newFixed("@i64")
    val h2 = ca.newFixed("@a0")

    ca.close()

    microVM.memoryManager.heap.space.debugLogBlockStates()
  }

  it should "allocate hybrid objects" in {
    val ca = microVM.newClientAgent()

    val hlen = ca.putInt("@i64", 1024)
    val h = ca.newHybrid("@h0", hlen)

    val hlen2 = ca.putInt("@i64", 128 * 1024)
    val h2 = ca.newHybrid("@h0", hlen2)

    ca.close()
  }

  it should "automatically trigger GC if the memory is full" in {
    val ca = microVM.newClientAgent()

    val hheld = ca.newFixed("@i64") // Objects held in the CA should survive the GC

    val hlen = ca.putInt("@i64", 1024)

    val allocCount = 300 // enough to fill the 256KiB small object space at least once.

    for (i <- (0 until allocCount)) {
      val h = ca.newHybrid("@h0", hlen)
      ca.deleteHandle(h)
    }

    ca.close()
  }

  it should "defrag heavily fragmented heap" in {
    val ca = microVM.newClientAgent()

    val hlen = ca.putInt("@i64", 1024)

    val allocCount = 300 // enough to fill the 256KiB small object space at least once.

    for (i <- (0 until allocCount)) {
      val h = ca.newHybrid("@h0", hlen)
      ca.deleteHandle(h)
      val hBreadcrumb = ca.newFixed("@i64")
    }

    ca.close()
  }

  it should "perform non-atomic memory access to all supported data types" in {
    val ca = microVM.newClientAgent()

    val hs = ca.newFixed("@s1")

    val hsi = ca.getIRef(hs)

    val hf0 = ca.getFieldIRef(hsi, 0)
    val hf1 = ca.getFieldIRef(hsi, 1)
    val hf2 = ca.getFieldIRef(hsi, 2)
    val hf3 = ca.getFieldIRef(hsi, 3)
    val hf4 = ca.getFieldIRef(hsi, 4)
    val hf5 = ca.getFieldIRef(hsi, 5)
    val hf6 = ca.getFieldIRef(hsi, 6)
    val hf7 = ca.getFieldIRef(hsi, 7)
    val hf8 = ca.getFieldIRef(hsi, 8)

    def testIntWriteRead(tid: Int, field: Handle, v: BigInt): Unit = {
      val hv = ca.putInt(tid, v)
      ca.store(NOT_ATOMIC, field, hv)
      val hvOut = ca.load(NOT_ATOMIC, field)
      val vOut = ca.toInt(hvOut, signExt = false)
      vOut shouldEqual v
    }

    testIntWriteRead("@i8", hf0, 42)
    testIntWriteRead("@i8", hf0, 0xffL)
    testIntWriteRead("@i16", hf1, 42)
    testIntWriteRead("@i16", hf1, 0xffffL)
    testIntWriteRead("@i32", hf2, 42)
    testIntWriteRead("@i32", hf2, 0xffffffffL)
    testIntWriteRead("@i64", hf3, 42)
    testIntWriteRead("@i64", hf3, BigInt("ffffffffffffffff", 16))

    val hfv = ca.putFloat("@float", 42.0f)
    ca.store(NOT_ATOMIC, hf4, hfv)
    val hfOut = ca.load(NOT_ATOMIC, hf4)
    val fOut = ca.toFloat(hfOut)
    fOut shouldEqual 42.0f

    val hdv = ca.putDouble("@double", 42.0d)
    ca.store(NOT_ATOMIC, hf5, hdv)
    val hdOut = ca.load(NOT_ATOMIC, hf5)
    val dOut = ca.toDouble(hdOut)
    dOut shouldEqual 42.0d

    val hrv = ca.refCast(hs, "@rv")
    val hirv = ca.refCast(hf0, "@irv")

    ca.store(NOT_ATOMIC, hf6, hrv)
    val hrvOut = ca.load(NOT_ATOMIC, hf6)
    hrvOut.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

    ca.store(NOT_ATOMIC, hf7, hirv)
    val hirvOut = ca.load(NOT_ATOMIC, hf7)
    hirvOut.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
    hirvOut.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

    ca.store(NOT_ATOMIC, hf8, hrv)
    val hwrvOut = ca.load(NOT_ATOMIC, hf8)
    // GC cannot clear the weakref because there is a handle to the referent object.
    hwrvOut.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

    val hs2 = ca.newFixed("@s2")

    val hs2i = ca.getIRef(hs2)
    val hs2f0 = ca.getFieldIRef(hs2i, 0)
    val hs2f1 = ca.getFieldIRef(hs2i, 1)
    val hs2f2 = ca.getFieldIRef(hs2i, 2)
    val hs2f3 = ca.getFieldIRef(hs2i, 3)

    val hFun = ca.putFunction("@fun")
    ca.store(NOT_ATOMIC, hs2f0, hFun)
    val hFunOut = ca.load(NOT_ATOMIC, hs2f0)
    hFunOut.vb.asInstanceOf[BoxFunc].func shouldEqual hFun.vb.asInstanceOf[BoxFunc].func

    // TODO: Test thread/stack when implemented.

    val hTRFp = ca.tr64FromFp(hdv)
    ca.store(NOT_ATOMIC, hs2f3, hTRFp)
    val hTRFpOut = ca.load(NOT_ATOMIC, hs2f3)
    val hTRFpToDouble = ca.tr64ToFp(hTRFpOut)
    val hTRFpVal = ca.toDouble(hTRFpToDouble)
    hTRFpVal shouldEqual 42.0d

    val hI52 = ca.putInt("@i52", 0xfedcba9876543L)
    val hTRInt = ca.tr64FromInt(hI52)
    ca.store(NOT_ATOMIC, hs2f3, hTRInt)
    val hTRIntOut = ca.load(NOT_ATOMIC, hs2f3)
    val hTRIntToI52 = ca.tr64ToInt(hTRIntOut)
    val hTRIntVal = ca.toInt(hTRIntToI52)
    hTRIntVal shouldEqual 0xfedcba9876543L

    val hI6 = ca.putInt("@i6", 29)
    val hTRRef = ca.tr64FromRef(hrv, hI6)
    ca.store(NOT_ATOMIC, hs2f3, hTRRef)
    val hTRRefOut = ca.load(NOT_ATOMIC, hs2f3)
    val hTRRefToRef = ca.tr64ToRef(hTRRefOut)
    val hTRRefToTag = ca.tr64ToTag(hTRRefOut)
    val hTRRefTagVal = ca.toInt(hTRRefToTag)
    hTRRefToRef.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef
    hTRRefTagVal shouldEqual 29

    val hAry = ca.newFixed("@a1")
    val hAryI = ca.getIRef(hAry)

    for (i <- 0 until 10) {
      val hD = ca.putDouble("@double", i.toDouble)
      val hI = ca.putInt("@i64", i)
      val hAryElem = ca.getElemIRef(hAryI, hI)
      val hAryElem0 = ca.getFieldIRef(hAryElem, 0)
      val hAryElem1 = ca.getFieldIRef(hAryElem, 1)

      ca.store(NOT_ATOMIC, hAryElem0, hD)
      ca.store(NOT_ATOMIC, hAryElem1, hI)
      val hDOut = ca.load(NOT_ATOMIC, hAryElem0)
      val hIOut = ca.load(NOT_ATOMIC, hAryElem1)
      val hDOutVal = ca.toDouble(hDOut)
      val hIOutVal = ca.toInt(hIOut, true)
      hDOutVal shouldEqual i.toDouble
      hIOutVal.intValue shouldEqual i
    }

    val hRVI32 = ca.newFixed("@4xi32")
    val hIrVI32 = ca.getIRef(hRVI32)
    val vi32: Seq[BigInt] = Seq(1, 2, 3, 4)
    val hVI32 = ca.putIntVec("@4xi32", vi32)
    ca.store(NOT_ATOMIC, hIrVI32, hVI32)
    val hVI32Out = ca.load(NOT_ATOMIC, hIrVI32)
    val vi32Out = ca.toIntVec(hVI32Out, signExt = true)
    vi32Out shouldEqual vi32

    for (i <- (0 until 4)) {
      val hI = ca.putInt("@i64", i)
      val hElem = ca.getElemIRef(hIrVI32, hI)
      val hv = ca.load(NOT_ATOMIC, hElem)
      val v = ca.toInt(hv)
      v shouldEqual vi32(i)
    }

    val hRVF = ca.newFixed("@4xfloat")
    val hIrVF = ca.getIRef(hRVF)
    val vf: Seq[Float] = Seq(1.0f, 2.0f, 3.0f, 4.0f)
    val hVF = ca.putFloatVec("@4xfloat", vf)
    ca.store(NOT_ATOMIC, hIrVF, hVF)
    val hVFOut = ca.load(NOT_ATOMIC, hIrVF)
    val vfOut = ca.toFloatVec(hVFOut)
    vfOut shouldEqual vf

    for (i <- (0 until 4)) {
      val hI = ca.putInt("@i64", i)
      val hElem = ca.getElemIRef(hIrVF, hI)
      val hv = ca.load(NOT_ATOMIC, hElem)
      val v = ca.toFloat(hv)
      v shouldEqual vf(i)
    }

    val hRVD = ca.newFixed("@2xdouble")
    val hIrVD = ca.getIRef(hRVD)
    val vd: Seq[Double] = Seq(1.0d, 2.0d)
    val hVD = ca.putDoubleVec("@2xdouble", vd)
    ca.store(NOT_ATOMIC, hIrVD, hVD)
    val hVDOut = ca.load(NOT_ATOMIC, hIrVD)
    val vdOut = ca.toDoubleVec(hVDOut)
    vdOut shouldEqual vd

    for (i <- (0 until 2)) {
      val hI = ca.putInt("@i64", i)
      val hElem = ca.getElemIRef(hIrVD, hI)
      val hv = ca.load(NOT_ATOMIC, hElem)
      val v = ca.toDouble(hv)
      v shouldEqual vd(i)
    }
    ca.close()
  }

  it should "perform atomic memory access to all supported data types" in {
    val ca = microVM.newClientAgent()

    val hs = ca.newFixed("@s1")

    val hsi = ca.getIRef(hs)

    val hf2 = ca.getFieldIRef(hsi, 2)
    val hf3 = ca.getFieldIRef(hsi, 3)

    val hf6 = ca.getFieldIRef(hsi, 6)
    val hf7 = ca.getFieldIRef(hsi, 7)
    val hf8 = ca.getFieldIRef(hsi, 8)

    def testIntCmpXchg(tid: Int, field: Handle, old: BigInt, expected: BigInt, desired: BigInt): Unit = {
      val hExpected = ca.putInt(tid, expected)
      val hDesired = ca.putInt(tid, desired)
      val (succ, hResult) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, field, hExpected, hDesired)
      succ shouldEqual (old == expected)
      val result = ca.toInt(hResult)
      result shouldEqual old

      val hResult2 = ca.load(SEQ_CST, field)
      val result2 = ca.toInt(hResult2)
      result2 shouldEqual (if (old == expected) desired else old)
    }

    def testIntXchg(tid: Int, field: Handle, old: BigInt, newVal: BigInt): Unit = {
      val hNewVal = ca.putInt(tid, newVal)
      val hResult = ca.atomicRMW(SEQ_CST, XCHG, field, hNewVal)
      val result = ca.toInt(hResult)
      result shouldEqual old

      val hResult2 = ca.load(SEQ_CST, field)
      val result2 = ca.toInt(hResult2)
      result2 shouldEqual newVal
    }

    testIntCmpXchg("@i32", hf2, 0, 0, 1)
    testIntCmpXchg("@i32", hf2, 1, 0, 2)
    testIntXchg("@i32", hf2, 1, 3)
    testIntCmpXchg("@i64", hf3, 0, 0, 1)
    testIntCmpXchg("@i64", hf3, 1, 0, 2)
    testIntXchg("@i64", hf3, 1, 3)

    val hrv = ca.refCast(hs, "@rv")
    val hirv = ca.refCast(hsi, "@irv")

    val hs2 = ca.newFixed("@s2")

    val hs2i = ca.getIRef(hs2)
    val hs2f0 = ca.getFieldIRef(hs2i, 0)
    val hs2f1 = ca.getFieldIRef(hs2i, 1)
    val hs2f2 = ca.getFieldIRef(hs2i, 2)
    val hs2f3 = ca.getFieldIRef(hs2i, 3)

    val hrv2 = ca.refCast(hs2, "@rv")
    val hir2 = ca.getIRef(hs2)
    val hirv2 = ca.refCast(hir2, "@irv")

    val hNullRv = ca.putConstant("@NULLRV")
    val hNullIrv = ca.putConstant("@NULLIRV")

    {
      val (succ1, hResult1) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf6, hNullRv, hrv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxRef].objRef shouldEqual 0L

      val (succ2, hResult2) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf6, hNullRv, hrv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult3 = ca.atomicRMW(SEQ_CST, XCHG, hf6, hrv2)
      hResult3.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult4 = ca.load(SEQ_CST, hf6)
      hResult4.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv2.vb.asInstanceOf[BoxRef].objRef
    }

    {
      val (succ1, hResult1) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf7, hNullIrv, hirv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxIRef].objRef shouldEqual 0L
      hResult1.vb.asInstanceOf[BoxIRef].offset shouldEqual 0L

      val (succ2, hResult2) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf7, hNullIrv, hirv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
      hResult2.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

      val hResult3 = ca.atomicRMW(SEQ_CST, XCHG, hf7, hirv2)
      hResult3.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv.vb.asInstanceOf[BoxIRef].objRef
      hResult3.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv.vb.asInstanceOf[BoxIRef].offset

      val hResult4 = ca.load(SEQ_CST, hf7)
      hResult4.vb.asInstanceOf[BoxIRef].objRef shouldEqual hirv2.vb.asInstanceOf[BoxIRef].objRef
      hResult4.vb.asInstanceOf[BoxIRef].offset shouldEqual hirv2.vb.asInstanceOf[BoxIRef].offset
    }

    {
      val (succ1, hResult1) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf8, hNullRv, hrv)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxRef].objRef shouldEqual 0L

      val (succ2, hResult2) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hf8, hNullRv, hrv2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult3 = ca.atomicRMW(SEQ_CST, XCHG, hf8, hrv2)
      hResult3.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv.vb.asInstanceOf[BoxRef].objRef

      val hResult4 = ca.load(SEQ_CST, hf8)
      hResult4.vb.asInstanceOf[BoxRef].objRef shouldEqual hrv2.vb.asInstanceOf[BoxRef].objRef
    }

    val hFun = ca.putFunction("@fun")
    val hFun2 = ca.putFunction("@fun2")
    val hNullF0 = ca.putConstant("@NULLF0")

    {
      val (succ1, hResult1) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hs2f0, hNullF0, hFun)
      succ1 shouldBe true
      hResult1.vb.asInstanceOf[BoxFunc].func shouldEqual None

      val (succ2, hResult2) = ca.cmpXchg(SEQ_CST, SEQ_CST, false, hs2f0, hNullF0, hFun2)
      succ2 shouldBe false
      hResult2.vb.asInstanceOf[BoxFunc].func shouldEqual hFun.vb.asInstanceOf[BoxFunc].func
    }

    // TODO: Test thread and stack after implemented.

    def testIntAtomicRMW(tid: Int, loc: Handle, optr: AtomicRMWOptr, lhs: BigInt, rhs: BigInt, expected: BigInt, signed: Boolean = false): Unit = {
      val hLhs = ca.putInt(tid, lhs)
      val hRhs = ca.putInt(tid, rhs)
      ca.store(SEQ_CST, loc, hLhs)
      val hResult1 = ca.atomicRMW(SEQ_CST, optr, loc, hRhs)
      val result1 = ca.toInt(hResult1, signed)
      result1 shouldEqual lhs
      val hResult2 = ca.load(SEQ_CST, loc)
      val result2 = ca.toInt(hResult2, signed)
      result2 shouldEqual expected
    }

    testIntAtomicRMW("@i32", hf2, XCHG, 42, 43, 43)
    testIntAtomicRMW("@i32", hf2, ADD, 1, 2, 3)
    testIntAtomicRMW("@i32", hf2, SUB, 3, 2, 1)
    testIntAtomicRMW("@i32", hf2, AND, 0x55aa, 0x5a5a, 0x500a)
    testIntAtomicRMW("@i32", hf2, NAND, 0x55aa, 0x5a5a, ~0x500a, signed=true)
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
    testIntAtomicRMW("@i64", hf3, NAND, 0x55aa, 0x5a5a, ~0x500a, signed=true)
    testIntAtomicRMW("@i64", hf3, OR, 0x55aa, 0x5a5a, 0x5ffa)
    testIntAtomicRMW("@i64", hf3, XOR, 0x55aa, 0x5a5a, 0x0ff0)
    testIntAtomicRMW("@i64", hf3, MIN, -3, -2, -3, signed = true)
    testIntAtomicRMW("@i64", hf3, MAX, -3, -2, -2, signed = true)
    testIntAtomicRMW("@i64", hf3, UMIN, 3, 2, 2)
    testIntAtomicRMW("@i64", hf3, UMAX, 3, 2, 3)

    ca.close()
  }
}