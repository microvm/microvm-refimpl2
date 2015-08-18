package uvm.refimpl.itpr

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

import ch.qos.logback.classic.Level._

class UvmInterpreterSpec extends UvmBundleTesterBase {

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl.mem.simpleimmix.SimpleImmixCollector$" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  override def makeMicroVM = new MicroVM(heapSize = 8L * 1024L * 1024L)

  preloadBundles("tests/uvm-refimpl-test/basic-tests.uir")

  "The constant pool" should "contain appropriate constant values" in {
    def gvb(name: String) = microVM.constantPool.getGlobalVarBox(microVM.globalBundle.globalVarNs(name))

    gvb("@TRUE").asUInt(1) shouldBe 1
    gvb("@FALSE").asUInt(1) shouldBe 0

    gvb("@I32_1").asUInt(32) shouldBe 1
    gvb("@I32_2").asUInt(32) shouldBe 2
    gvb("@I32_7").asUInt(32) shouldBe 7

    gvb("@I64_5").asUInt(64) shouldBe 5
  }

  "Binary operations" should "work on int<32>" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops32")
    val arg0 = ca.putInt("@i32", 42)
    val arg1 = ca.putInt("@i32", 3)

    testFunc(ca, func, Seq(arg0, arg1)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      add.vb.asSInt(32) shouldEqual 45
      sub.vb.asSInt(32) shouldEqual 39
      mul.vb.asSInt(32) shouldEqual 126
      udiv.vb.asUInt(32) shouldEqual 14
      sdiv.vb.asSInt(32) shouldEqual 14
      urem.vb.asUInt(32) shouldEqual 0
      srem.vb.asSInt(32) shouldEqual 0
      shl.vb.asSInt(32) shouldEqual 336
      lshr.vb.asUInt(32) shouldEqual 5
      ashr.vb.asSInt(32) shouldEqual 5
      and.vb.asSInt(32) shouldEqual 2
      or.vb.asSInt(32) shouldEqual 43
      xor.vb.asSInt(32) shouldEqual 41

      TrapRebindPassVoid(st)
    }

    val arg2 = ca.putInt("@i32", -100)
    val arg3 = ca.putInt("@i32", 6)

    testFunc(ca, func, Seq(arg2, arg3)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)
      sdiv.vb.asSInt(32) shouldEqual -16
      srem.vb.asSInt(32) shouldEqual -4
      lshr.vb.asUInt(32) shouldEqual 67108862
      ashr.vb.asSInt(32) shouldEqual -2

      TrapRebindPassVoid(st)
    }

    val arg4 = ca.putInt("@i32", 42)
    val arg5 = ca.putInt("@i32", -15)

    testFunc(ca, func, Seq(arg4, arg5)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)
      sdiv.vb.asSInt(32) shouldEqual -2
      srem.vb.asSInt(32) shouldEqual 12

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Binary operations" should "work on int<64>" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops64")
    val arg0 = ca.putInt("@i64", 42)
    val arg1 = ca.putInt("@i64", 3)

    testFunc(ca, func, Seq(arg0, arg1)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      add.vb.asSInt(64) shouldEqual 45
      sub.vb.asSInt(64) shouldEqual 39
      mul.vb.asSInt(64) shouldEqual 126
      udiv.vb.asUInt(64) shouldEqual 14
      sdiv.vb.asSInt(64) shouldEqual 14
      urem.vb.asUInt(64) shouldEqual 0
      srem.vb.asSInt(64) shouldEqual 0
      shl.vb.asSInt(64) shouldEqual 336
      lshr.vb.asUInt(64) shouldEqual 5
      ashr.vb.asSInt(64) shouldEqual 5
      and.vb.asSInt(64) shouldEqual 2
      or.vb.asSInt(64) shouldEqual 43
      xor.vb.asSInt(64) shouldEqual 41

      TrapRebindPassVoid(st)
    }

    val arg2 = ca.putInt("@i64", -0x8000000000000000L)
    val arg3 = ca.putInt("@i64", -1L)

    testFunc(ca, func, Seq(arg2, arg3)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      sdiv.vb.asSInt(64) shouldEqual -0x8000000000000000L
      srem.vb.asSInt(64) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    val arg4 = ca.putInt("@i64", 13)
    val arg5 = ca.putInt("@i64", 63)
    val arg6 = ca.putInt("@i64", 64)
    val arg7 = ca.putInt("@i64", 65)

    testFunc(ca, func, Seq(arg4, arg5)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual BigInt("8000000000000000", 16)
      lshr.vb.asUInt(64) shouldEqual 0
      ashr.vb.asSInt(64) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(arg4, arg6)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual 13
      lshr.vb.asUInt(64) shouldEqual 13
      ashr.vb.asSInt(64) shouldEqual 13

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(arg4, arg7)) { (ca, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ca.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual 26
      lshr.vb.asUInt(64) shouldEqual 6
      ashr.vb.asSInt(64) shouldEqual 6

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Binary operations" should "safely handle integer division by zero" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops64_div0")
    val a = ca.putInt("@i64", 42)
    val one = ca.putInt("@i64", 1)
    val zero = ca.putInt("@i64", 0)

    testFunc(ca, func, Seq(a, zero, one, one, one)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@binops64_div0_v1.trapexc"
      TrapRebindPassVoid(st)
    }
    testFunc(ca, func, Seq(a, one, zero, one, one)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@binops64_div0_v1.trapexc"
      TrapRebindPassVoid(st)
    }
    testFunc(ca, func, Seq(a, one, one, zero, one)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@binops64_div0_v1.trapexc"
      TrapRebindPassVoid(st)
    }
    testFunc(ca, func, Seq(a, one, one, one, zero)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@binops64_div0_v1.trapexc"
      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Binary operations" should "work on float" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops_f")
    val arg0 = ca.putFloat("@float", 45.0f)
    val arg1 = ca.putFloat("@float", 4.0f)

    testFunc(ca, func, Seq(arg0, arg1)) { (ca, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ca.dumpKeepalives(st, 0)

      fadd.vb.asFloat shouldEqual 49.0f
      fsub.vb.asFloat shouldEqual 41.0f
      fmul.vb.asFloat shouldEqual 180.0f
      fdiv.vb.asFloat shouldEqual 11.25f
      frem.vb.asFloat shouldEqual 1.0f

      TrapRebindPassVoid(st)
    }

    val arg2 = ca.putFloat("@float", Float.NaN)

    testFunc(ca, func, Seq(arg0, arg2)) { (ca, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ca.dumpKeepalives(st, 0)

      fadd.vb.asFloat.isNaN shouldEqual true
      fsub.vb.asFloat.isNaN shouldEqual true
      fmul.vb.asFloat.isNaN shouldEqual true
      fdiv.vb.asFloat.isNaN shouldEqual true
      frem.vb.asFloat.isNaN shouldEqual true

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Binary operations" should "work on double" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops_d")
    val arg0 = ca.putDouble("@double", 45.0d)
    val arg1 = ca.putDouble("@double", 4.0d)

    testFunc(ca, func, Seq(arg0, arg1)) { (ca, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ca.dumpKeepalives(st, 0)

      fadd.vb.asDouble shouldEqual 49.0d
      fsub.vb.asDouble shouldEqual 41.0d
      fmul.vb.asDouble shouldEqual 180.0d
      fdiv.vb.asDouble shouldEqual 11.25d
      frem.vb.asDouble shouldEqual 1.0d

      TrapRebindPassVoid(st)
    }

    val arg2 = ca.putDouble("@double", Double.NaN)

    testFunc(ca, func, Seq(arg0, arg2)) { (ca, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ca.dumpKeepalives(st, 0)

      fadd.vb.asDouble.isNaN shouldEqual true
      fsub.vb.asDouble.isNaN shouldEqual true
      fmul.vb.asDouble.isNaN shouldEqual true
      fdiv.vb.asDouble.isNaN shouldEqual true
      frem.vb.asDouble.isNaN shouldEqual true

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Binary operations" should "work on vector types" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@binops_vec")

    val a0 = ca.putIntVec("@4xi32", Seq(1, 2, 3, 4))
    val a1 = ca.putIntVec("@4xi32", Seq(10, 20, 30, 40))
    val a2 = ca.putFloatVec("@4xfloat", Seq(1.0f, 2.0f, 3.0f, 4.0f))
    val a3 = ca.putFloatVec("@4xfloat", Seq(10.0f, 20.0f, 30.0f, 40.0f))
    val a4 = ca.putDoubleVec("@2xdouble", Seq(1.0d, 2.0d))
    val a5 = ca.putDoubleVec("@2xdouble", Seq(10.0d, 20.0d))

    testFunc(ca, func, Seq(a0, a1, a2, a3, a4, a5)) { (ca, th, st, wp) =>
      val Seq(addi, subi, addf, subf, addd, subd) = ca.dumpKeepalives(st, 0)

      addi.vb.asVec.map(_.asSInt(32)) shouldEqual Seq(11, 22, 33, 44)
      subi.vb.asVec.map(_.asSInt(32)) shouldEqual Seq(-9, -18, -27, -36)
      addf.vb.asVec.map(_.asFloat) shouldEqual Seq(11.0f, 22.0f, 33.0f, 44.0f)
      subf.vb.asVec.map(_.asFloat) shouldEqual Seq(-9.0f, -18.0f, -27.0f, -36.0f)
      addd.vb.asVec.map(_.asDouble) shouldEqual Seq(11.0d, 22.0d)
      subd.vb.asVec.map(_.asDouble) shouldEqual Seq(-9.0d, -18.0d)

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Comparing operations" should "work on int<64>" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@cmp64")

    val a0 = ca.putInt("@i64", 1)
    val a1 = ca.putInt("@i64", 2)

    testFunc(ca, func, Seq(a0, a1)) { (ca, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ca.dumpKeepalives(st, 0)

      eq.vb.asUInt(1) shouldEqual 0
      ne.vb.asUInt(1) shouldEqual 1
      ult.vb.asUInt(1) shouldEqual 1
      ule.vb.asUInt(1) shouldEqual 1
      ugt.vb.asUInt(1) shouldEqual 0
      uge.vb.asUInt(1) shouldEqual 0
      slt.vb.asUInt(1) shouldEqual 1
      sle.vb.asUInt(1) shouldEqual 1
      sgt.vb.asUInt(1) shouldEqual 0
      sge.vb.asUInt(1) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0, a0)) { (ca, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ca.dumpKeepalives(st, 0)

      eq.vb.asUInt(1) shouldEqual 1
      ne.vb.asUInt(1) shouldEqual 0
      ult.vb.asUInt(1) shouldEqual 0
      ule.vb.asUInt(1) shouldEqual 1
      ugt.vb.asUInt(1) shouldEqual 0
      uge.vb.asUInt(1) shouldEqual 1
      slt.vb.asUInt(1) shouldEqual 0
      sle.vb.asUInt(1) shouldEqual 1
      sgt.vb.asUInt(1) shouldEqual 0
      sge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    val a2 = ca.putInt("@i64", -3)

    testFunc(ca, func, Seq(a0, a2)) { (ca, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ca.dumpKeepalives(st, 0)

      eq.vb.asUInt(1) shouldEqual 0
      ne.vb.asUInt(1) shouldEqual 1
      slt.vb.asUInt(1) shouldEqual 0
      sle.vb.asUInt(1) shouldEqual 0
      sgt.vb.asUInt(1) shouldEqual 1
      sge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Comparing operations" should "work on float" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@cmp_f")

    val a0 = ca.putFloat("@float", 1.0f)
    val a1 = ca.putFloat("@float", 2.0f)

    testFunc(ca, func, Seq(a0, a1)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 1
      foeq.vb.asUInt(1) shouldEqual 0
      fone.vb.asUInt(1) shouldEqual 1
      folt.vb.asUInt(1) shouldEqual 1
      fole.vb.asUInt(1) shouldEqual 1
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 0
      funo.vb.asUInt(1) shouldEqual 0
      fueq.vb.asUInt(1) shouldEqual 0
      fune.vb.asUInt(1) shouldEqual 1
      fult.vb.asUInt(1) shouldEqual 1
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 0
      fuge.vb.asUInt(1) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0, a0)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 1
      foeq.vb.asUInt(1) shouldEqual 1
      fone.vb.asUInt(1) shouldEqual 0
      folt.vb.asUInt(1) shouldEqual 0
      fole.vb.asUInt(1) shouldEqual 1
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 1
      funo.vb.asUInt(1) shouldEqual 0
      fueq.vb.asUInt(1) shouldEqual 1
      fune.vb.asUInt(1) shouldEqual 0
      fult.vb.asUInt(1) shouldEqual 0
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 0
      fuge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    val a2 = ca.putFloat("@float", Float.NaN)

    testFunc(ca, func, Seq(a0, a2)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 0
      foeq.vb.asUInt(1) shouldEqual 0
      fone.vb.asUInt(1) shouldEqual 0
      folt.vb.asUInt(1) shouldEqual 0
      fole.vb.asUInt(1) shouldEqual 0
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 0
      funo.vb.asUInt(1) shouldEqual 1
      fueq.vb.asUInt(1) shouldEqual 1
      fune.vb.asUInt(1) shouldEqual 1
      fult.vb.asUInt(1) shouldEqual 1
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 1
      fuge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "Comparing operations" should "work on double" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@cmp_d")

    val a0 = ca.putDouble("@double", 1.0d)
    val a1 = ca.putDouble("@double", 2.0d)

    testFunc(ca, func, Seq(a0, a1)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 1
      foeq.vb.asUInt(1) shouldEqual 0
      fone.vb.asUInt(1) shouldEqual 1
      folt.vb.asUInt(1) shouldEqual 1
      fole.vb.asUInt(1) shouldEqual 1
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 0
      funo.vb.asUInt(1) shouldEqual 0
      fueq.vb.asUInt(1) shouldEqual 0
      fune.vb.asUInt(1) shouldEqual 1
      fult.vb.asUInt(1) shouldEqual 1
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 0
      fuge.vb.asUInt(1) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0, a0)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 1
      foeq.vb.asUInt(1) shouldEqual 1
      fone.vb.asUInt(1) shouldEqual 0
      folt.vb.asUInt(1) shouldEqual 0
      fole.vb.asUInt(1) shouldEqual 1
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 1
      funo.vb.asUInt(1) shouldEqual 0
      fueq.vb.asUInt(1) shouldEqual 1
      fune.vb.asUInt(1) shouldEqual 0
      fult.vb.asUInt(1) shouldEqual 0
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 0
      fuge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    val a2 = ca.putDouble("@double", Double.NaN)

    testFunc(ca, func, Seq(a0, a2)) { (ca, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ca.dumpKeepalives(st, 0)

      ftrue.vb.asUInt(1) shouldEqual 1
      ffalse.vb.asUInt(1) shouldEqual 0
      ford.vb.asUInt(1) shouldEqual 0
      foeq.vb.asUInt(1) shouldEqual 0
      fone.vb.asUInt(1) shouldEqual 0
      folt.vb.asUInt(1) shouldEqual 0
      fole.vb.asUInt(1) shouldEqual 0
      fogt.vb.asUInt(1) shouldEqual 0
      foge.vb.asUInt(1) shouldEqual 0
      funo.vb.asUInt(1) shouldEqual 1
      fueq.vb.asUInt(1) shouldEqual 1
      fune.vb.asUInt(1) shouldEqual 1
      fult.vb.asUInt(1) shouldEqual 1
      fule.vb.asUInt(1) shouldEqual 1
      fugt.vb.asUInt(1) shouldEqual 1
      fuge.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "Comparing operations" should "work on vectors" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@cmp_vec")

    val a0 = ca.putIntVec("@4xi32", Seq(1, 1, 2, 2))
    val a1 = ca.putIntVec("@4xi32", Seq(1, 2, 1, 2))
    val a2 = ca.putFloatVec("@4xfloat", Seq(1.0f, 1.0f, 2.0f, 2.0f))
    val a3 = ca.putFloatVec("@4xfloat", Seq(1.0f, 2.0f, 1.0f, 2.0f))
    val a4 = ca.putDoubleVec("@2xdouble", Seq(1.0d, 2.0d))
    val a5 = ca.putDoubleVec("@2xdouble", Seq(2.0d, 2.0d))

    testFunc(ca, func, Seq(a0, a1, a2, a3, a4, a5)) { (ca, th, st, wp) =>
      val Seq(eq, slt, foeqf, fultf, foeqd, fultd) = ca.dumpKeepalives(st, 0)

      eq.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0, 0, 1)
      slt.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1, 0, 0)
      foeqf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0, 0, 1)
      fultf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1, 0, 0)
      foeqd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1)
      fultd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0)

      TrapRebindPassVoid(st)
    }

    val a6 = ca.putFloatVec("@4xfloat", Seq(Float.NaN, Float.NaN, Float.NaN, Float.NaN))
    val a7 = ca.putDoubleVec("@2xdouble", Seq(Double.NaN, Double.NaN))

    testFunc(ca, func, Seq(a0, a1, a2, a6, a4, a7)) { (ca, th, st, wp) =>
      val Seq(eq, slt, foeqf, fultf, foeqd, fultd) = ca.dumpKeepalives(st, 0)

      foeqf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 0, 0, 0)
      fultf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 1, 1, 1)
      foeqd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 0)
      fultd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 1)

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "Comparing operations" should "work on general reference types" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@cmp_ref")

    val a0 = ca.newFixed("@i64")
    val a1 = ca.newFixed("@i64")
    val a2 = ca.putGlobal("@dummy_global1")
    val a3 = ca.putGlobal("@dummy_global2")
    val a4 = ca.putFunction("@dummy_func1")
    val a5 = ca.putFunction("@dummy_func2")
    val a6 = ca.newStack(a5, Seq())
    val a7 = ca.newStack(a5, Seq())

    testFunc(ca, func, Seq(a0, a0, a2, a2, a4, a4, a6, a6)) { (ca, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ca.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 1
      rne.vb.asUInt(1) shouldEqual 0
      ieq.vb.asUInt(1) shouldEqual 1
      ine.vb.asUInt(1) shouldEqual 0
      feq.vb.asUInt(1) shouldEqual 1
      fne.vb.asUInt(1) shouldEqual 0
      seq.vb.asUInt(1) shouldEqual 1
      sne.vb.asUInt(1) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0, a1, a2, a3, a4, a5, a6, a7)) { (ca, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ca.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 0
      rne.vb.asUInt(1) shouldEqual 1
      ieq.vb.asUInt(1) shouldEqual 0
      ine.vb.asUInt(1) shouldEqual 1
      feq.vb.asUInt(1) shouldEqual 0
      fne.vb.asUInt(1) shouldEqual 1
      seq.vb.asUInt(1) shouldEqual 0
      sne.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    val nr = ca.putConstant("@NULLREF_I64")
    val ni = ca.putConstant("@NULLIREF_I64")
    val nf = ca.putConstant("@NULLFUNC")
    val ns = ca.putConstant("@NULLSTACK")

    testFunc(ca, func, Seq(nr, nr, ni, ni, nf, nf, ns, ns)) { (ca, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ca.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 1
      rne.vb.asUInt(1) shouldEqual 0
      ieq.vb.asUInt(1) shouldEqual 1
      ine.vb.asUInt(1) shouldEqual 0
      feq.vb.asUInt(1) shouldEqual 1
      fne.vb.asUInt(1) shouldEqual 0
      seq.vb.asUInt(1) shouldEqual 1
      sne.vb.asUInt(1) shouldEqual 0

      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0, nr, a2, ni, a4, nf, a6, ns)) { (ca, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ca.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 0
      rne.vb.asUInt(1) shouldEqual 1
      ieq.vb.asUInt(1) shouldEqual 0
      ine.vb.asUInt(1) shouldEqual 1
      feq.vb.asUInt(1) shouldEqual 0
      fne.vb.asUInt(1) shouldEqual 1
      seq.vb.asUInt(1) shouldEqual 0
      sne.vb.asUInt(1) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Conversions" should "work on scalar types" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@conv")

    val a0 = ca.putInt("@i32", 0xfedcba98L)
    val a1 = ca.putInt("@i64", 0x123456789abcdef0L)
    val a2 = ca.putFloat("@float", 1.5f)
    val a3 = ca.putDouble("@double", 6.25d)

    testFunc(ca, func, Seq(a0, a1, a2, a3)) { (ca, th, st, wp) =>
      val Seq(trunc, zext, sext, fptrunc, fpext, fptoui1, fptosi1, fptoui2, fptosi2, uitofp, sitofp,
        bitcast1, bitcast2, bitcast3, bitcast4) = ca.dumpKeepalives(st, 0)

      trunc.vb.asUInt(32) shouldBe 0x9abcdef0L
      zext.vb.asUInt(64) shouldBe 0xfedcba98L
      sext.vb.asUInt(64) shouldBe BigInt("fffffffffedcba98", 16)

      fptrunc.vb.asFloat shouldBe 6.25f
      fpext.vb.asDouble shouldBe 1.5d

      fptoui1.vb.asSInt(64) shouldBe 6
      fptosi1.vb.asSInt(64) shouldBe 6
      fptoui2.vb.asSInt(32) shouldBe 1
      fptosi2.vb.asSInt(32) shouldBe 1
      uitofp.vb.asDouble shouldBe 0x123456789abcdef0L.doubleValue()
      sitofp.vb.asDouble shouldBe 0x123456789abcdef0L.doubleValue()

      bitcast1.vb.asSInt(32) shouldBe java.lang.Float.floatToRawIntBits(1.5f)
      bitcast2.vb.asSInt(64) shouldBe java.lang.Double.doubleToRawLongBits(6.25d)
      bitcast3.vb.asFloat shouldBe 1.5f
      bitcast4.vb.asDouble shouldBe 6.25d

      TrapRebindPassVoid(st)
    }
    val a5 = ca.putInt("@i64", -0x123456789abcdef0L)

    testFunc(ca, func, Seq(a0, a5, a2, a3)) { (ca, th, st, wp) =>
      val Seq(trunc, zext, sext, fptrunc, fpext, fptoui1, fptosi1, fptoui2, fptosi2, uitofp, sitofp,
        bitcast1, bitcast2, bitcast3, bitcast4) = ca.dumpKeepalives(st, 0)

      sitofp.vb.asDouble shouldBe (-0x123456789abcdef0L).doubleValue()

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Conversions" should "work on vector types" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@conv_vec")

    val a0 = ca.putIntVec("@4xi32", Seq(0x12345678L, 0x9abcdef0L, 0xfedcba98L, 0x76543210L))
    val a1 = ca.putFloatVec("@4xfloat", Seq(1.0f, 1.5f, 2.0f, 2.5f))
    val a2 = ca.putDoubleVec("@2xdouble", Seq(1.0d, 1.5d))

    testFunc(ca, func, Seq(a0, a1, a2)) { (ca, th, st, wp) =>
      val Seq(trunc, zext, sext, fptrunc, fpext) = ca.dumpKeepalives(st, 0)

      trunc.vb.asVec.map(_.asUInt(16)) shouldBe Seq(0x5678L, 0xdef0L, 0xba98L, 0x3210L)
      zext.vb.asVec.map(_.asUInt(64)) shouldBe Seq(0x12345678L, 0x9abcdef0L, 0xfedcba98L, 0x76543210L)
      sext.vb.asVec.map(_.asUInt(64)) shouldBe Seq(0x12345678L, BigInt("ffffffff9abcdef0", 16), BigInt("fffffffffedcba98", 16), 0x76543210L)

      fptrunc.vb.asVec.map(_.asFloat) shouldBe Seq(1.0f, 1.5f)
      fpext.vb.asVec.map(_.asDouble) shouldBe Seq(1.0d, 1.5d, 2.0d, 2.5d)

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "The SELECT instruction" should "work on both scalars and vectors and allow per-element select" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@select")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(sel1, sel2, sel3, sel4, sel5) = ca.dumpKeepalives(st, 0)

      sel1.vb.asSInt(64) shouldBe 2
      sel2.vb.asSInt(64) shouldBe 3

      sel3.vb.asVec.map(_.asSInt(32)) shouldBe Seq(0, 5, 6, 3)

      sel4.vb.asVec.map(_.asSInt(32)) shouldBe Seq(0, 1, 2, 3)
      sel5.vb.asVec.map(_.asSInt(32)) shouldBe Seq(4, 5, 6, 7)

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Branching" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@branch")

    val a0 = ca.putInt("@i64", 0)
    val a1 = ca.putInt("@i64", 1)

    testFunc(ca, func, Seq(a0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@branch_v1.traptrue"
      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a1)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) shouldBe "@branch_v1.trapfalse"
      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "SWTICH and PHI" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@switch_phi")

    val a0 = ca.putInt("@i64", 0)
    val a1 = ca.putInt("@i64", 1)
    val a2 = ca.putInt("@i64", 2)
    val a3 = ca.putInt("@i64", 3)

    def expectFlow(midTrapName: String, phiValue: BigInt): TrapHandlerFunction = { (ca, th, st, wp) =>
      val M = midTrapName
      nameOf(ca.currentInstruction(st, 0)) match {
        case M => {}
        case "@switch_phi_v1.trapend" => {
          val Seq(phi) = ca.dumpKeepalives(st, 0)
          phi.vb.asSInt(64) shouldBe phiValue
        }
        case trapName => fail("Trap %s should not be reached. Should reach %s.".format(trapName))
      }
      TrapRebindPassVoid(st)
    }

    testFunc(ca, func, Seq(a0))(expectFlow("@switch_phi_v1.trapdef", 4))
    testFunc(ca, func, Seq(a1))(expectFlow("@switch_phi_v1.trapone", 5))
    testFunc(ca, func, Seq(a2))(expectFlow("@switch_phi_v1.traptwo", 6))
    testFunc(ca, func, Seq(a3))(expectFlow("@switch_phi_v1.trapthree", 7))

    ca.close()
  }

  "PHI instructions in a basic block" should "be assigned at the same time in CFG edges" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@phi_cyclic_dep_test")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(x, y) = ca.dumpKeepalives(st, 0)

      ca.toInt(x, signExt = true) shouldEqual 2
      ca.toInt(y, signExt = true) shouldEqual 1

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "CALL and RET" should "work for normal returns" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@call_ret")

    val a0 = ca.putInt("@i64", 3)
    val a1 = ca.putInt("@i64", 4)

    testFunc(ca, func, Seq(a0, a1)) { (ca, th, st, wp) =>
      val Seq(ss) = ca.dumpKeepalives(st, 0)

      ss.vb.asInt shouldEqual 25

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "CALL, THROW and LANDINGPAD" should "handle exceptions" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@call_throw")

    val a0 = ca.putInt("@i64", 3)

    testFunc(ca, func, Seq(a0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@call_throw_v1.trapnor" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)
          rv.vb.asInt shouldEqual 3
        }
        case "@call_throw_v1.trapexc" => {
          fail("Should not receive exception")
        }
      }

      TrapRebindPassVoid(st)
    }

    val a1 = ca.putInt("@i64", 0)

    testFunc(ca, func, Seq(a1)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@call_throw_v1.trapnor" => {
          fail("Should not return normally")
        }
        case "@call_throw_v1.trapexc" => {
          val Seq(lp) = ca.dumpKeepalives(st, 0)
          lp.vb.asRef shouldEqual 0L
        }
      }

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "EXTRACTVALUE and INSERTVALUE" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@aggregate_struct")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(f1, f12) = ca.dumpKeepalives(st, 0)

      f1.vb.asSInt(64) shouldEqual 2
      f12.vb.asSInt(64) shouldEqual 7

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "EXTRACTELEMENT and INSERTELEMENT" should "work on vectors" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@aggregate_vector")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(ee0, ie0, sv0) = ca.dumpKeepalives(st, 0)

      ee0.vb.asFloat shouldEqual 0.0f
      ie0.vb.asVec.map(_.asFloat) shouldEqual Seq(0.0f, 6.0f, 2.0f, 3.0f)
      sv0.vb.asVec.map(_.asFloat) shouldEqual Seq(0.0f, 2.0f, 4.0f, 6.0f)

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "NEW, NEWHYBRID, ALLOCA, ALLOCAHYBRID" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@allocs")

    val sz = ca.putInt("@i64", 20)

    testFunc(ca, func, Seq(sz)) { (ca, th, st, wp) =>
      val Seq(n, nh, a, ah) = ca.dumpKeepalives(st, 0)

      // nothing to check at this moment

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "GETIREF, GETFIELDIREF, GITELEMIREF, SHIFTIREF, GETFIXEDPARTIREF AND GETVARPARTIREF" should "work" in {
    implicit def typeOf(name: String): Type = microVM.globalBundle.typeNs(name)
    implicit def structTypeOf(name: String): TypeStruct = typeOf(name).asInstanceOf[TypeStruct]
    implicit def seqTypeOf(name: String): AbstractSeqType = typeOf(name).asInstanceOf[AbstractSeqType]
    implicit def hybridTypeOf(name: String): TypeHybrid = typeOf(name).asInstanceOf[TypeHybrid]

    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@memAddressing")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(barRef, barIRef, bar3, bazIRef, baz3, baz6, jaRef, jaIRef, jaFix, jaVar) = ca.dumpKeepalives(st, 0)

      barIRef.vb.asIRef shouldEqual (barRef.vb.asRef, 0L)
      bar3.vb.asIRefAddr shouldEqual (barRef.vb.asRef + TypeSizes.fieldOffsetOf("@StructBar", 3))

      baz3.vb.asIRefAddr shouldEqual (bazIRef.vb.asIRefAddr + TypeSizes.elemOffsetOf("@ArrayBaz", 3))
      baz6.vb.asIRefAddr shouldEqual (bazIRef.vb.asIRefAddr + TypeSizes.elemOffsetOf("@ArrayBaz", 6))

      jaIRef.vb.asIRefAddr shouldEqual (jaRef.vb.asRef)
      jaFix.vb.asIRefAddr shouldEqual (jaRef.vb.asRef)
      jaVar.vb.asIRefAddr shouldEqual (jaRef.vb.asRef + TypeSizes.varPartOffsetOf("@JavaLikeByteArray"))

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "LOAD and STORE" should "work in good cases" in {
    val ca = microVM.newClientAgent()
    val func = ca.putFunction("@memAccessing")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(voidR, voidIR, li8, li16, li32, li64, lf, ld, lr, lir, lwr, lfunc) = ca.dumpKeepalives(st, 0)

      li8.vb.asSInt(8) shouldBe 41
      li16.vb.asSInt(16) shouldBe 42
      li32.vb.asSInt(32) shouldBe 43
      li64.vb.asSInt(64) shouldBe 44
      lf.vb.asFloat shouldBe 45.0f
      ld.vb.asDouble shouldBe 46.0d

      lr.vb.asRef shouldBe voidR.vb.asRef
      lir.vb.asIRef shouldBe voidIR.vb.asIRef
      lwr.vb.asRef shouldBe voidR.vb.asRef

      lfunc.vb.asFunc shouldBe Some(microVM.globalBundle.funcNs("@memAccessing"))

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "CMPXCHG and ATOMICRMW" should "work in good cases" in {
    val ca = microVM.newClientAgent()
    val func = ca.putFunction("@memAccessingAtomic")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val kas = ca.dumpKeepalives(st, 0)

      // Scala limits unpacking of Seq to 22 elements
      val Seq(voidR, voidR2, voidR3, cx32_1, cx32_2, cx64_1, cx64_2, l32, l64, cxr_1, cxr_2, lr,
        rmw0, rmw1, rmw2, rmw3, rmw4, rmw5, rmw6, rmw7, rmw8, rmw9) = kas.take(22)
      val Seq(rmwA, l64_2) = kas.drop(22)

      cx32_1.vb.asSInt(32) shouldBe 43
      cx32_2.vb.asSInt(32) shouldBe 53
      cx64_1.vb.asSInt(64) shouldBe 44
      cx64_2.vb.asSInt(64) shouldBe 54

      l32.vb.asSInt(32) shouldBe 53
      l64.vb.asSInt(64) shouldBe 54

      cxr_1.vb.asRef shouldBe voidR.vb.asRef
      cxr_2.vb.asRef shouldBe voidR2.vb.asRef
      lr.vb.asRef shouldBe voidR2.vb.asRef

      rmw0.vb.asSInt(64) shouldBe 1L
      rmw1.vb.asSInt(64) shouldBe 0x55abL
      rmw2.vb.asSInt(64) shouldBe 0x55aeL
      rmw3.vb.asSInt(64) shouldBe 0x55aaL
      rmw4.vb.asSInt(64) shouldBe 0x500aL
      rmw5.vb.asSInt(64) shouldBe ~0x500aL
      rmw6.vb.asSInt(64) shouldBe ~0x000aL
      rmw7.vb.asSInt(64) shouldBe ~0x55a0L
      rmw8.vb.asSInt(64) shouldBe -0x7fffffffffffffdeL
      rmw9.vb.asSInt(64) shouldBe 42L
      rmwA.vb.asSInt(64) shouldBe 11L

      l64_2.vb.asSInt(64) shouldBe 0xffffffffffffffdeL

      TrapRebindPassVoid(st)
    }
    ca.close()
  }

  "LOAD, STORE, CMPXCHG and ATOMICRMW" should "jump to the exceptional destination on NULL ref access" in {
    val ca = microVM.newClientAgent()
    val func = ca.putFunction("@memAccessingNull")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@memAccessingNull_v1.trap_exit" => TrapRebindPassVoid(st)
        case n                                => fail("Unexpected trap " + n)
      }
    }

    ca.close()
  }

  "TRAP" should "work with all supported destinations" in {
    val ca = microVM.newClientAgent()

    val exc = ca.newFixed("@void")
    val fortyTwo = ca.putInt("@i64", 42L)
    val fortyTwoPointZero = ca.putDouble("@double", 42.0d)

    val func = ca.putFunction("@traptest")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@traptest_v1.t1" => {
          TrapRebindPassValue(st, fortyTwo)
        }
        case "@traptest_v1.t2" => {
          TrapRebindPassValue(st, fortyTwoPointZero)
        }
        case "@traptest_v1.t3" => {
          TrapRebindThrowExc(st, exc)
        }
        case "@traptest_v1.trap_exit" => {
          val Seq(t1, t2, lp) = ca.dumpKeepalives(st, 0)

          t1.vb.asSInt(64) shouldBe 42L
          t2.vb.asDouble shouldBe 42.0d
          lp.vb.asRef shouldBe exc.vb.asRef
          TrapRebindPassVoid(st)
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ca.close()
  }

  "WATCHPOINT" should "do nothing when disabled" in {
    val ca = microVM.newClientAgent()

    ca.disableWatchPoint(1)

    val func = ca.putFunction("@wptest")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@wptest_v1.trap_dis" => {
          TrapRebindPassVoid(st)
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ca.close()
  }

  "WATCHPOINT" should "work with all supported destinations when enabled" in {
    val ca = microVM.newClientAgent()

    ca.enableWatchPoint(1)

    val exc = ca.newFixed("@i32")
    val fortyTwo = ca.putInt("@i64", 42L)
    val fortyTwoPointZero = ca.putDouble("@double", 42.0d)

    val func = ca.putFunction("@wptest")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@wptest_v1.w1" => {
          wp shouldBe 1
          TrapRebindPassValue(st, fortyTwo)
        }
        case "@wptest_v1.w2" => {
          wp shouldBe 1
          TrapRebindPassValue(st, fortyTwoPointZero)
        }
        case "@wptest_v1.w3" => {
          wp shouldBe 1
          TrapRebindThrowExc(st, exc)
        }
        case "@wptest_v1.trap_exit" => {
          wp shouldBe 0
          val Seq(w1, w2, lp) = ca.dumpKeepalives(st, 0)

          w1.vb.asSInt(64) shouldBe 42L
          w2.vb.asDouble shouldBe 42.0d
          lp.vb.asRef shouldBe exc.vb.asRef
          TrapRebindPassVoid(st)
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ca.close()
  }

  "TRAP and WATCHPOINT" should "throw exceptions out of function when no exceptional dest" in {
    val ca = microVM.newClientAgent()

    ca.enableWatchPoint(2)

    val exc1 = ca.newFixed("@void")
    val exc2 = ca.newFixed("@void")

    val func = ca.putFunction("@trapExc")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@trapThrow_v1.t" => {
          TrapRebindThrowExc(st, exc1)
        }
        case "@wpThrow_v1.w" => {
          wp shouldBe 2
          TrapRebindThrowExc(st, exc2)
        }
        case "@trapExc_v1.trap_exit" => {
          val Seq(lp1, lp2) = ca.dumpKeepalives(st, 0)

          lp1.vb.asRef shouldBe exc1.vb.asRef
          lp2.vb.asRef shouldBe exc2.vb.asRef

          TrapRebindPassVoid(st)
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ca.close()
  }

  "SWAPSTAK" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testswapstack")

    var coro1Reached = false
    var coro2Reached = false
    var main1Reached = false
    var main2Reached = false

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@corostackfunc_v1.trap_coro1" => {
          val Seq(fromSta, p) = ca.dumpKeepalives(st, 0)

          fromSta.vb.asStack.get.top.funcVer shouldBe microVM.globalBundle.funcVerNs("@testswapstack_v1")
          p.vb.asSInt(64) shouldBe 2L

          coro1Reached = true
          TrapRebindPassVoid(st)
        }
        case "@corostackfunc_v1.trap_coro2" => {
          val Seq(css1) = ca.dumpKeepalives(st, 0)

          css1.vb.asDouble shouldBe 3.0d

          coro2Reached = true
          TrapRebindPassVoid(st)
        }
        case "@testswapstack_v1.trap_main1" => {
          val Seq(mss1) = ca.dumpKeepalives(st, 0)

          mss1.vb.asSInt(64) shouldBe 3L

          main1Reached = true
          TrapRebindPassVoid(st)
        }
        case "@testswapstack_v1.trap_main2" => {
          val Seq(excVal) = ca.dumpKeepalives(st, 0)

          excVal.vb.asSInt(64) shouldBe 7L

          main2Reached = true
          TrapRebindPassVoid(st)
        }
      }
    }

    coro1Reached shouldBe true
    coro2Reached shouldBe true
    main1Reached shouldBe true
    main2Reached shouldBe true

    ca.close()
  }

  "COMMINST @uvm.tr64.*" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testtr64")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(rv, f, i, r,
        f_is_f, f_is_i, f_is_r, i_is_f, i_is_i, i_is_r, r_is_f, r_is_i, r_is_r,
        fb, ib, rb, tb) = ca.dumpKeepalives(st, 0)

      f.vb.asTR64Raw shouldBe OpHelper.fpToTr64(42.0d)
      i.vb.asTR64Raw shouldBe OpHelper.intToTr64(0xfedcba9876543L)
      r.vb.asTR64Raw shouldBe OpHelper.refToTr64(rv.vb.asRef, 31L)

      (Seq(f_is_f, f_is_i, f_is_r, i_is_f, i_is_i, i_is_r, r_is_f, r_is_i, r_is_r).map(_.vb.asUInt(1)) shouldBe
        Seq(1, 0, 0, 0, 1, 0, 0, 0, 1))

      fb.vb.asDouble shouldBe 42.0d
      ib.vb.asUInt(52) shouldBe 0xfedcba9876543L
      rb.vb.asRef shouldBe rv.vb.asRef
      tb.vb.asUInt(6) shouldBe 31L

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "COMMINST @uvm.kill_dependency" should "do nothing" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testdependency")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(b) = ca.dumpKeepalives(st, 0)

      b.vb.asSInt(64) shouldBe 3

      TrapRebindPassVoid(st)
    }

    ca.close()
  }
}