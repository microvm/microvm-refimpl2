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
import uvm.refimpl.TrapHandlerResult.{ ThreadExit, Rebind }
import uvm.refimpl.HowToResume.{ PassValues, ThrowExc }

import ch.qos.logback.classic.Level._

class UvmInterpreterSpec extends UvmBundleTesterBase {

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl.mem.simpleimmix.SimpleImmixCollector$" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  override def makeMicroVM = new MicroVM(new VMConf(sosSize = 4L * 1024L * 1024L, losSize = 4L * 1024L * 1024L))

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/basic-tests.uir")

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
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops32")
    val arg0 = ctx.handleFromInt32(42)
    val arg1 = ctx.handleFromInt32(3)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val arg2 = ctx.handleFromInt(-100, 32)
    val arg3 = ctx.handleFromInt(6, 32)

    testFunc(ctx, func, Seq(arg2, arg3)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)
      sdiv.vb.asSInt(32) shouldEqual -16
      srem.vb.asSInt(32) shouldEqual -4
      lshr.vb.asUInt(32) shouldEqual 67108862
      ashr.vb.asSInt(32) shouldEqual -2

      Rebind(st, PassValues(Seq()))
    }

    val arg4 = ctx.handleFromInt(42, 32)
    val arg5 = ctx.handleFromInt(-15, 32)

    testFunc(ctx, func, Seq(arg4, arg5)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)
      sdiv.vb.asSInt(32) shouldEqual -2
      srem.vb.asSInt(32) shouldEqual 12

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Binary operations" should "work on int<64>" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops64")
    val arg0 = ctx.handleFromInt64(42)
    val arg1 = ctx.handleFromInt64(3)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val arg2 = ctx.handleFromInt64(-0x8000000000000000L)
    val arg3 = ctx.handleFromInt64(-1L)

    testFunc(ctx, func, Seq(arg2, arg3)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

      sdiv.vb.asSInt(64) shouldEqual -0x8000000000000000L
      srem.vb.asSInt(64) shouldEqual 0

      Rebind(st, PassValues(Seq()))
    }

    val arg4 = ctx.handleFromInt64(13)
    val arg5 = ctx.handleFromInt64(63)
    val arg6 = ctx.handleFromInt64(64)
    val arg7 = ctx.handleFromInt64(65)

    testFunc(ctx, func, Seq(arg4, arg5)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual BigInt("8000000000000000", 16)
      lshr.vb.asUInt(64) shouldEqual 0
      ashr.vb.asSInt(64) shouldEqual 0

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(arg4, arg6)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual 13
      lshr.vb.asUInt(64) shouldEqual 13
      ashr.vb.asSInt(64) shouldEqual 13

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(arg4, arg7)) { (ctx, th, st, wp) =>
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = ctx.dumpKeepalives(st, 0)

      shl.vb.asUInt(64) shouldEqual 26
      lshr.vb.asUInt(64) shouldEqual 6
      ashr.vb.asSInt(64) shouldEqual 6

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Binary operations" should "safely handle integer division by zero" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops64_div0")
    val a = ctx.handleFromInt64(42)
    val one = ctx.handleFromInt64(1)
    val zero = ctx.handleFromInt64(0)

    testFunc(ctx, func, Seq(a, zero, one, one, one)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@binops64_div0.v1.exc.trapexc"
      Rebind(st, PassValues(Seq()))
    }
    testFunc(ctx, func, Seq(a, one, zero, one, one)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@binops64_div0.v1.exc.trapexc"
      Rebind(st, PassValues(Seq()))
    }
    testFunc(ctx, func, Seq(a, one, one, zero, one)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@binops64_div0.v1.exc.trapexc"
      Rebind(st, PassValues(Seq()))
    }
    testFunc(ctx, func, Seq(a, one, one, one, zero)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@binops64_div0.v1.exc.trapexc"
      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Binary operations" should "work on float" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops_f")
    val arg0 = ctx.handleFromFloat(45.0f)
    val arg1 = ctx.handleFromFloat(4.0f)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ctx.dumpKeepalives(st, 0)

      fadd.vb.asFloat shouldEqual 49.0f
      fsub.vb.asFloat shouldEqual 41.0f
      fmul.vb.asFloat shouldEqual 180.0f
      fdiv.vb.asFloat shouldEqual 11.25f
      frem.vb.asFloat shouldEqual 1.0f

      Rebind(st, PassValues(Seq()))
    }

    val arg2 = ctx.handleFromFloat(Float.NaN)

    testFunc(ctx, func, Seq(arg0, arg2)) { (ctx, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ctx.dumpKeepalives(st, 0)

      fadd.vb.asFloat.isNaN shouldEqual true
      fsub.vb.asFloat.isNaN shouldEqual true
      fmul.vb.asFloat.isNaN shouldEqual true
      fdiv.vb.asFloat.isNaN shouldEqual true
      frem.vb.asFloat.isNaN shouldEqual true

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Binary operations" should "work on double" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops_d")
    val arg0 = ctx.handleFromDouble(45.0d)
    val arg1 = ctx.handleFromDouble(4.0d)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ctx.dumpKeepalives(st, 0)

      fadd.vb.asDouble shouldEqual 49.0d
      fsub.vb.asDouble shouldEqual 41.0d
      fmul.vb.asDouble shouldEqual 180.0d
      fdiv.vb.asDouble shouldEqual 11.25d
      frem.vb.asDouble shouldEqual 1.0d

      Rebind(st, PassValues(Seq()))
    }

    val arg2 = ctx.handleFromDouble(Double.NaN)

    testFunc(ctx, func, Seq(arg0, arg2)) { (ctx, th, st, wp) =>
      val Seq(fadd, fsub, fmul, fdiv, frem) = ctx.dumpKeepalives(st, 0)

      fadd.vb.asDouble.isNaN shouldEqual true
      fsub.vb.asDouble.isNaN shouldEqual true
      fmul.vb.asDouble.isNaN shouldEqual true
      fdiv.vb.asDouble.isNaN shouldEqual true
      frem.vb.asDouble.isNaN shouldEqual true

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Binary operations" should "work on vector types" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops_vec")

    val a0 = ctx.handleFromConst("@4xI32_V3")
    val a1 = ctx.handleFromConst("@4xI32_V4")
    val a2 = ctx.handleFromConst("@4xF_V3")
    val a3 = ctx.handleFromConst("@4xF_V4")
    val a4 = ctx.handleFromConst("@2xD_V3")
    val a5 = ctx.handleFromConst("@2xD_V4")

    testFunc(ctx, func, Seq(a0, a1, a2, a3, a4, a5)) { (ctx, th, st, wp) =>
      val Seq(addi, subi, addf, subf, addd, subd) = ctx.dumpKeepalives(st, 0)

      addi.vb.asVec.map(_.asSInt(32)) shouldEqual Seq(11, 22, 33, 44)
      subi.vb.asVec.map(_.asSInt(32)) shouldEqual Seq(-9, -18, -27, -36)
      addf.vb.asVec.map(_.asFloat) shouldEqual Seq(11.0f, 22.0f, 33.0f, 44.0f)
      subf.vb.asVec.map(_.asFloat) shouldEqual Seq(-9.0f, -18.0f, -27.0f, -36.0f)
      addd.vb.asVec.map(_.asDouble) shouldEqual Seq(11.0d, 22.0d)
      subd.vb.asVec.map(_.asDouble) shouldEqual Seq(-9.0d, -18.0d)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Comparing operations" should "work on int<64>" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp64")

    val a0 = ctx.handleFromInt64(1)
    val a1 = ctx.handleFromInt64(2)

    testFunc(ctx, func, Seq(a0, a1)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, a0)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val a2 = ctx.handleFromInt64(-3)

    testFunc(ctx, func, Seq(a0, a2)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

      eq.vb.asUInt(1) shouldEqual 0
      ne.vb.asUInt(1) shouldEqual 1
      slt.vb.asUInt(1) shouldEqual 0
      sle.vb.asUInt(1) shouldEqual 0
      sgt.vb.asUInt(1) shouldEqual 1
      sge.vb.asUInt(1) shouldEqual 1

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Comparing operations" should "work on pointer types" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmpptr")

    val a0 = ctx.handleFromPtr("@ptri64", 1)
    val a1 = ctx.handleFromPtr("@ptri64", 2)

    testFunc(ctx, func, Seq(a0, a1)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, a0)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val a2 = ctx.handleFromPtr("@ptri64", -3)

    testFunc(ctx, func, Seq(a0, a2)) { (ctx, th, st, wp) =>
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = ctx.dumpKeepalives(st, 0)

      eq.vb.asUInt(1) shouldEqual 0
      ne.vb.asUInt(1) shouldEqual 1
      slt.vb.asUInt(1) shouldEqual 0
      sle.vb.asUInt(1) shouldEqual 0
      sgt.vb.asUInt(1) shouldEqual 1
      sge.vb.asUInt(1) shouldEqual 1

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Comparing operations" should "work on float" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp_f")

    val a0 = ctx.handleFromFloat(1.0f)
    val a1 = ctx.handleFromFloat(2.0f)

    testFunc(ctx, func, Seq(a0, a1)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, a0)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val a2 = ctx.handleFromFloat(Float.NaN)

    testFunc(ctx, func, Seq(a0, a2)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "Comparing operations" should "work on double" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp_d")

    val a0 = ctx.handleFromDouble(1.0d)
    val a1 = ctx.handleFromDouble(2.0d)

    testFunc(ctx, func, Seq(a0, a1)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, a0)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }

    val a2 = ctx.handleFromDouble(Double.NaN)

    testFunc(ctx, func, Seq(a0, a2)) { (ctx, th, st, wp) =>
      val Seq(ftrue, ffalse, ford, foeq, fone, folt, fole, fogt, foge, funo, fueq, fune, fult, fule, fugt, fuge) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "Comparing operations" should "work on vectors" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp_vec")

    val a0 = ctx.handleFromConst("@4xI32_V5")
    val a1 = ctx.handleFromConst("@4xI32_V6")
    val a2 = ctx.handleFromConst("@4xF_V5")
    val a3 = ctx.handleFromConst("@4xF_V6")
    val a4 = ctx.handleFromConst("@2xD_V5")
    val a5 = ctx.handleFromConst("@2xD_V6")

    testFunc(ctx, func, Seq(a0, a1, a2, a3, a4, a5)) { (ctx, th, st, wp) =>
      val Seq(eq, slt, foeqf, fultf, foeqd, fultd) = ctx.dumpKeepalives(st, 0)

      eq.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0, 0, 1)
      slt.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1, 0, 0)
      foeqf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0, 0, 1)
      fultf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1, 0, 0)
      foeqd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 1)
      fultd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 0)

      Rebind(st, PassValues(Seq()))
    }

    val a6 = ctx.handleFromConst("@4xF_V7")
    val a7 = ctx.handleFromConst("@2xD_V7")

    testFunc(ctx, func, Seq(a0, a1, a2, a6, a4, a7)) { (ctx, th, st, wp) =>
      val Seq(eq, slt, foeqf, fultf, foeqd, fultd) = ctx.dumpKeepalives(st, 0)

      foeqf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 0, 0, 0)
      fultf.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 1, 1, 1)
      foeqd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(0, 0)
      fultd.vb.asVec.map(_.asUInt(1)) shouldEqual Seq(1, 1)

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "Comparing operations" should "work on general reference types" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp_ref")

    val a0 = ctx.newFixed("@i64")
    val a1 = ctx.newFixed("@i64")
    val a2 = ctx.handleFromGlobal("@dummy_global1")
    val a3 = ctx.handleFromGlobal("@dummy_global2")
    val a4 = ctx.handleFromFunc("@dummy_func1")
    val a5 = ctx.handleFromFunc("@dummy_func2")
    val a6 = ctx.newStack(a4)
    val a7 = ctx.newStack(a5)

    testFunc(ctx, func, Seq(a0, a0, a2, a2, a4, a4, a6, a6)) { (ctx, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ctx.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 1
      rne.vb.asUInt(1) shouldEqual 0
      ieq.vb.asUInt(1) shouldEqual 1
      ine.vb.asUInt(1) shouldEqual 0
      feq.vb.asUInt(1) shouldEqual 1
      fne.vb.asUInt(1) shouldEqual 0
      seq.vb.asUInt(1) shouldEqual 1
      sne.vb.asUInt(1) shouldEqual 0

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, a1, a2, a3, a4, a5, a6, a7)) { (ctx, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ctx.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 0
      rne.vb.asUInt(1) shouldEqual 1
      ieq.vb.asUInt(1) shouldEqual 0
      ine.vb.asUInt(1) shouldEqual 1
      feq.vb.asUInt(1) shouldEqual 0
      fne.vb.asUInt(1) shouldEqual 1
      seq.vb.asUInt(1) shouldEqual 0
      sne.vb.asUInt(1) shouldEqual 1

      Rebind(st, PassValues(Seq()))
    }

    val nr = ctx.handleFromConst("@NULLREF_I64")
    val ni = ctx.handleFromConst("@NULLIREF_I64")
    val nf = ctx.handleFromConst("@NULLFUNC")
    val ns = ctx.handleFromConst("@NULLSTACK")

    testFunc(ctx, func, Seq(nr, nr, ni, ni, nf, nf, ns, ns)) { (ctx, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ctx.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 1
      rne.vb.asUInt(1) shouldEqual 0
      ieq.vb.asUInt(1) shouldEqual 1
      ine.vb.asUInt(1) shouldEqual 0
      feq.vb.asUInt(1) shouldEqual 1
      fne.vb.asUInt(1) shouldEqual 0
      seq.vb.asUInt(1) shouldEqual 1
      sne.vb.asUInt(1) shouldEqual 0

      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0, nr, a2, ni, a4, nf, a6, ns)) { (ctx, th, st, wp) =>
      val Seq(req, rne, ieq, ine, feq, fne, seq, sne) = ctx.dumpKeepalives(st, 0)

      req.vb.asUInt(1) shouldEqual 0
      rne.vb.asUInt(1) shouldEqual 1
      ieq.vb.asUInt(1) shouldEqual 0
      ine.vb.asUInt(1) shouldEqual 1
      feq.vb.asUInt(1) shouldEqual 0
      fne.vb.asUInt(1) shouldEqual 1
      seq.vb.asUInt(1) shouldEqual 0
      sne.vb.asUInt(1) shouldEqual 1

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Conversions" should "work on scalar types" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@conv")

    val a0 = ctx.handleFromInt32(0xfedcba98L)
    val a1 = ctx.handleFromInt64(0x123456789abcdef0L)
    val a2 = ctx.handleFromFloat(1.5f)
    val a3 = ctx.handleFromDouble(6.25d)

    testFunc(ctx, func, Seq(a0, a1, a2, a3)) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)

      val Seq(trunc, zext, sext, fptrunc, fpext, fptoui1, fptosi1, fptoui2, fptosi2, uitofp, sitofp) = kas.take(11)
      val Seq(bitcast1, bitcast2, bitcast3, bitcast4, ptrcast1, ptrcast2, ptrcast3, ptrcast4, ptrcast5, ptrcast6) = kas.drop(11)

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

      ptrcast1.vb.asPointer shouldBe 0x123456789abcdef0L
      ptrcast2.vb.asPointer shouldBe 0x123456789abcdef0L
      ptrcast3.vb.asSInt(64) shouldBe 0x123456789abcdef0L

      ptrcast4.vb.asSInt(64) shouldBe 0xdeadbeef13572468L
      ptrcast5.vb.asPointer shouldBe 0x55aa55aa5a5a5a5aL
      ptrcast6.vb.asInt shouldBe 0x13572468L

      Rebind(st, PassValues(Seq()))
    }
    val a5 = ctx.handleFromInt64(-0x123456789abcdef0L)

    testFunc(ctx, func, Seq(a0, a5, a2, a3)) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)

      val Seq(trunc, zext, sext, fptrunc, fpext, fptoui1, fptosi1, fptoui2, fptosi2, uitofp, sitofp) = kas.take(11)
      val Seq(bitcast1, bitcast2, bitcast3, bitcast4, ptrcast1, ptrcast2, ptrcast3, ptrcast4, ptrcast5, ptrcast6) = kas.drop(11)

      sitofp.vb.asDouble shouldBe (-0x123456789abcdef0L).doubleValue()

      ptrcast1.vb.asPointer shouldBe -0x123456789abcdef0L
      ptrcast2.vb.asPointer shouldBe -0x123456789abcdef0L
      ptrcast3.vb.asSInt(64) shouldBe -0x123456789abcdef0L

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Conversions" should "work on vector types" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@conv_vec")

    val a0 = ctx.handleFromConst("@4xI32_ABNORMALV1")
    val a1 = ctx.handleFromConst("@4xF_ABNORMALV1")
    val a2 = ctx.handleFromConst("@2xD_ABNORMALV1")

    testFunc(ctx, func, Seq(a0, a1, a2)) { (ctx, th, st, wp) =>
      val Seq(trunc, zext, sext, fptrunc, fpext) = ctx.dumpKeepalives(st, 0)

      trunc.vb.asVec.map(_.asUInt(16)) shouldBe Seq(0x5678L, 0xdef0L, 0xba98L, 0x3210L)
      zext.vb.asVec.map(_.asUInt(64)) shouldBe Seq(0x12345678L, 0x9abcdef0L, 0xfedcba98L, 0x76543210L)
      sext.vb.asVec.map(_.asUInt(64)) shouldBe Seq(0x12345678L, BigInt("ffffffff9abcdef0", 16), BigInt("fffffffffedcba98", 16), 0x76543210L)

      fptrunc.vb.asVec.map(_.asFloat) shouldBe Seq(1.0f, 1.5f)
      fpext.vb.asVec.map(_.asDouble) shouldBe Seq(1.0d, 1.5d, 2.0d, 2.5d)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "The SELECT instruction" should "work on both scalars and vectors and allow per-element select" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@select")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(sel1, sel2, sel3, sel4, sel5) = ctx.dumpKeepalives(st, 0)

      sel1.vb.asSInt(64) shouldBe 2
      sel2.vb.asSInt(64) shouldBe 3

      sel3.vb.asVec.map(_.asSInt(32)) shouldBe Seq(0, 5, 6, 3)

      sel4.vb.asVec.map(_.asSInt(32)) shouldBe Seq(0, 1, 2, 3)
      sel5.vb.asVec.map(_.asSInt(32)) shouldBe Seq(4, 5, 6, 7)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Branching" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@branch")

    val a0 = ctx.handleFromInt64(0)
    val a1 = ctx.handleFromInt64(1)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@branch.v1.iftrue.traptrue"
      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a1)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) shouldBe "@branch.v1.iffalse.trapfalse"
      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "SWTICH" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@switch")

    val a0 = ctx.handleFromInt64(0)
    val a1 = ctx.handleFromInt64(1)
    val a2 = ctx.handleFromInt64(2)
    val a3 = ctx.handleFromInt64(3)

    def expectFlow(midTrapName: String, phiValue: BigInt): TrapHandlerFunction = { (ctx, th, st, wp) =>
      val M = midTrapName
      nameOf(ctx.curInst(st, 0)) match {
        case M => {}
        case "@switch.v1.exit.trapend" => {
          val Seq(v) = ctx.dumpKeepalives(st, 0)
          v.vb.asSInt(64) shouldBe phiValue
        }
        case trapName => fail("Trap %s should not be reached. Should reach %s.".format(trapName))
      }
      Rebind(st, PassValues(Seq()))
    }

    testFunc(ctx, func, Seq(a0))(expectFlow("@switch.v1.def.trapdef", 4))
    testFunc(ctx, func, Seq(a1))(expectFlow("@switch.v1.one.trapone", 5))
    testFunc(ctx, func, Seq(a2))(expectFlow("@switch.v1.two.traptwo", 6))
    testFunc(ctx, func, Seq(a3))(expectFlow("@switch.v1.three.trapthree", 7))

    ctx.closeContext()
  }

  "Parameters of a basic block" should "be assigned at the same time in CFG edges" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@edge_asgn_test")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(x, y) = ctx.dumpKeepalives(st, 0)

      ctx.handleToSInt(x.asInstanceOf[MuIntValue]) shouldEqual 2
      ctx.handleToSInt(y.asInstanceOf[MuIntValue]) shouldEqual 1

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "CALL and RET" should "work for normal returns" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@call_ret")

    val a0 = ctx.handleFromInt64(3)
    val a1 = ctx.handleFromInt64(4)

    testFunc(ctx, func, Seq(a0, a1)) { (ctx, th, st, wp) =>
      val Seq(ss) = ctx.dumpKeepalives(st, 0)

      ss.vb.asInt shouldEqual 25

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "CALL, THROW and the exception parameter" should "handle exceptions" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@call_throw")

    val a0 = ctx.handleFromInt64(3)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@call_throw.v1.nor.trapnor" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)
          rv.vb.asInt shouldEqual 3
        }
        case "@call_throw.v1.exctrapexc" => {
          fail("Should not receive exception")
        }
      }

      Rebind(st, PassValues(Seq()))
    }

    val a1 = ctx.handleFromInt64(0)

    testFunc(ctx, func, Seq(a1)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@call_throw.v1.nor.trapnor" => {
          fail("Should not return normally")
        }
        case "@call_throw.v1.exc.trapexc" => {
          val Seq(theExc) = ctx.dumpKeepalives(st, 0)
          theExc.vb.asRef shouldEqual 0L
        }
      }

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "CALL" should "be able to receive multiple return values or no return values" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@call_multi_return")

    var checkpointReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@checkpoint.v1.entry.trap" => {
          checkpointReached = true
        }
        case "@call_multi_return.v1.entry.trap" => {
          val Seq(a, b) = ctx.dumpKeepalives(st, 0)
          a.asSInt(64) shouldEqual 8
          b.asSInt(64) shouldEqual 7
          Rebind(st, PassValues(Seq()))
        }
      }

      Rebind(st, PassValues(Seq()))
    }

    checkpointReached shouldBe true

    ctx.closeContext()
  }

  "EXTRACTVALUE and INSERTVALUE" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@aggregate_struct")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(f1, f12) = ctx.dumpKeepalives(st, 0)

      f1.vb.asSInt(64) shouldEqual 2
      f12.vb.asSInt(64) shouldEqual 7

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "EXTRACTELEMENT and INSERTELEMENT" should "work on vectors and arrays" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@aggregate_seq")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(ee0, ie0, sv0, ee1, ie1) = ctx.dumpKeepalives(st, 0)

      ee0.vb.asFloat shouldEqual 0.0f
      ie0.vb.asVec.map(_.asFloat) shouldEqual Seq(0.0f, 6.0f, 2.0f, 3.0f)
      sv0.vb.asVec.map(_.asFloat) shouldEqual Seq(0.0f, 2.0f, 4.0f, 6.0f)
      ee1.vb.asInt shouldEqual 7
      ie1.vb.asSeq.map(_.asInt) shouldEqual Seq(0, 2, 4, 6, 7, 1)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "NEW, NEWHYBRID, ALLOCA, ALLOCAHYBRID" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@allocs")

    val sz = ctx.handleFromInt64(20)

    testFunc(ctx, func, Seq(sz)) { (ctx, th, st, wp) =>
      val Seq(n, nh, a, ah) = ctx.dumpKeepalives(st, 0)

      // nothing to check at this moment

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "GETIREF, GETFIELDIREF, GITELEMIREF, SHIFTIREF, GETFIXEDPARTIREF AND GETVARPARTIREF" should "work with iref" in {
    implicit def typeOf(name: String): Type = microVM.globalBundle.typeNs(name)
    implicit def structTypeOf(name: String): AbstractStructType = typeOf(name).asInstanceOf[AbstractStructType]
    implicit def seqTypeOf(name: String): AbstractSeqType = typeOf(name).asInstanceOf[AbstractSeqType]
    implicit def hybridTypeOf(name: String): TypeHybrid = typeOf(name).asInstanceOf[TypeHybrid]

    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@memAddressing")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(barRef, barIRef, bar3, bazIRef, baz3, baz6, jaRef, jaIRef, jaVar) = ctx.dumpKeepalives(st, 0)

      barIRef.vb.asIRef shouldEqual (barRef.vb.asRef, 0L)
      bar3.vb.asIRefAddr shouldEqual (barRef.vb.asRef + TypeSizes.fieldOffsetOf(structTypeOf("@StructBar"), 3))

      baz3.vb.asIRefAddr shouldEqual (bazIRef.vb.asIRefAddr + TypeSizes.elemOffsetOf("@ArrayBaz", 3))
      baz6.vb.asIRefAddr shouldEqual (bazIRef.vb.asIRefAddr + TypeSizes.elemOffsetOf("@ArrayBaz", 6))

      jaIRef.vb.asIRefAddr shouldEqual (jaRef.vb.asRef)
      jaVar.vb.asIRefAddr shouldEqual (jaRef.vb.asRef + TypeSizes.varPartOffsetOf("@JavaLikeByteArray"))

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "GETIREF, GETFIELDIREF, GITELEMIREF, SHIFTIREF, GETFIXEDPARTIREF AND GETVARPARTIREF" should "work with pointers" in {
    implicit def typeOf(name: String): Type = microVM.globalBundle.typeNs(name)
    implicit def structTypeOf(name: String): AbstractStructType = typeOf(name).asInstanceOf[AbstractStructType]
    implicit def seqTypeOf(name: String): AbstractSeqType = typeOf(name).asInstanceOf[AbstractSeqType]
    implicit def hybridTypeOf(name: String): TypeHybrid = typeOf(name).asInstanceOf[TypeHybrid]

    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@memAddressingPtr")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(barPtr, bazPtr, jaPtr, bar3, baz3, baz6, jaVar) = ctx.dumpKeepalives(st, 0)

      val base = 1024L

      barPtr.vb.asPointer shouldEqual base
      bazPtr.vb.asPointer shouldEqual base
      jaPtr.vb.asPointer shouldEqual base

      bar3.vb.asPointer shouldEqual (base + TypeSizes.fieldOffsetOf(structTypeOf("@StructBar"), 3))

      baz3.vb.asPointer shouldEqual (base + TypeSizes.elemOffsetOf("@ArrayBaz", 3))
      baz6.vb.asPointer shouldEqual (base + TypeSizes.elemOffsetOf("@ArrayBaz", 6))

      jaVar.vb.asPointer shouldEqual (base + TypeSizes.varPartOffsetOf("@JavaLikeByteArray"))

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "LOAD and STORE" should "work with iref in good cases" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessing")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(voidR, voidIR, li8, li16, li32, li64, lf, ld, lr, lir, lwr, lfunc) = ctx.dumpKeepalives(st, 0)

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

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "LOAD and STORE" should "work with pointer in good cases" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessingPtr")

    val myms = new MemorySupport(1024)
    val begin = myms.muMemoryBegin

    val a0 = ctx.handleFromPtr("@ptri8", begin)
    val a1 = ctx.handleFromPtr("@ptri16", begin + 8L)
    val a2 = ctx.handleFromPtr("@ptri32", begin + 16L)
    val a3 = ctx.handleFromPtr("@ptri64", begin + 32L)
    val a4 = ctx.handleFromPtr("@ptrfloat", begin + 40L)
    val a5 = ctx.handleFromPtr("@ptrdouble", begin + 48L)
    val a6 = ctx.handleFromPtr("@ptrptrvoid", begin + 56L)
    val a7 = ctx.handleFromPtr("@ptrfpi_i", begin + 64L)

    testFunc(ctx, func, Seq(a0, a1, a2, a3, a4, a5, a6, a7)) { (ctx, th, st, wp) =>
      val Seq(li8, li16, li32, li64, lf, ld, lp, lfp) = ctx.dumpKeepalives(st, 0)

      li8.vb.asSInt(8) shouldBe 41
      li16.vb.asSInt(16) shouldBe 42
      li32.vb.asSInt(32) shouldBe 43
      li64.vb.asSInt(64) shouldBe 44
      lf.vb.asFloat shouldBe 45.0f
      ld.vb.asDouble shouldBe 46.0d

      lp.vb.asPointer shouldBe 0x55aaL
      lfp.vb.asPointer shouldBe 0x55aaL

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "CMPXCHG and ATOMICRMW" should "work with iref in good cases" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessingAtomic")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)

      // Scala limits unpacking of Seq to 22 elements
      val Seq(voidR, voidR2, voidR3) = kas.take(3)
      val Seq(cx32_1, succ32_1, cx32_2, succ32_2, cx64_1, succ64_1, cx64_2, succ64_2) = kas.drop(3).take(8)
      val Seq(l32, l64) = kas.drop(11).take(2)
      val Seq(cxr_1, succr_1, cxr_2, succr_2, lr) = kas.drop(13).take(5)
      val Seq(rmw0, rmw1, rmw2, rmw3, rmw4, rmw5, rmw6, rmw7, rmw8, rmw9, rmwA) = kas.drop(18).take(11)
      val Seq(l64_2) = kas.drop(29).take(1)

      cx32_1.vb.asSInt(32) shouldBe 43
      succ32_1.vb.asUInt(1) shouldBe 1
      cx32_2.vb.asSInt(32) shouldBe 53
      succ32_2.vb.asUInt(1) shouldBe 0
      cx64_1.vb.asSInt(64) shouldBe 44
      succ64_1.vb.asUInt(1) shouldBe 1
      cx64_2.vb.asSInt(64) shouldBe 54
      succ64_2.vb.asUInt(1) shouldBe 0

      l32.vb.asSInt(32) shouldBe 53
      l64.vb.asSInt(64) shouldBe 54

      cxr_1.vb.asRef shouldBe voidR.vb.asRef
      succr_1.vb.asUInt(1) shouldBe 1
      cxr_2.vb.asRef shouldBe voidR2.vb.asRef
      succr_2.vb.asUInt(1) shouldBe 0
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

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "CMPXCHG and ATOMICRMW" should "work with pointer in good cases" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessingAtomicPtr")

    val myms = new MemorySupport(1024)
    val begin = myms.muMemoryBegin

    val a0 = ctx.handleFromPtr("@ptri8", begin)
    val a1 = ctx.handleFromPtr("@ptri16", begin + 8L)
    val a2 = ctx.handleFromPtr("@ptri32", begin + 16L)
    val a3 = ctx.handleFromPtr("@ptri64", begin + 32L)
    val a4 = ctx.handleFromPtr("@ptrfloat", begin + 40L)
    val a5 = ctx.handleFromPtr("@ptrdouble", begin + 48L)
    val a6 = ctx.handleFromPtr("@ptrptrvoid", begin + 56L)
    val a7 = ctx.handleFromPtr("@ptrfpi_i", begin + 64L)

    testFunc(ctx, func, Seq(a0, a1, a2, a3, a4, a5, a6, a7)) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)

      // Scala limits unpacking of Seq to 22 elements
      val Seq(cx32_1, succ32_1, cx32_2, succ32_2, cx64_1, succ64_1, cx64_2, succ64_2) = kas.take(8)
      val Seq(l32, l64) = kas.drop(8).take(2)
      val Seq(cxp_1, succp_1, cxp_2, succp_2, cxfp_1, succfp_1, cxfp_2, succfp_2) = kas.drop(10).take(8)
      val Seq(lp, lfp) = kas.drop(18).take(2)
      val Seq(rmw0, rmw1, rmw2, rmw3, rmw4, rmw5, rmw6, rmw7, rmw8, rmw9, rmwA, l64_2) = kas.drop(20).take(12)

      cx32_1.vb.asSInt(32) shouldBe 43
      succ32_1.vb.asUInt(1) shouldBe 1
      cx32_2.vb.asSInt(32) shouldBe 53
      succ32_2.vb.asUInt(1) shouldBe 0
      cx64_1.vb.asSInt(64) shouldBe 44
      succ64_1.vb.asUInt(1) shouldBe 1
      cx64_2.vb.asSInt(64) shouldBe 54
      succ64_2.vb.asUInt(1) shouldBe 0

      l32.vb.asSInt(32) shouldBe 53
      l64.vb.asSInt(64) shouldBe 54

      cxp_1.vb.asPointer shouldBe 0x55abL
      succp_1.vb.asUInt(1) shouldBe 1
      cxp_2.vb.asPointer shouldBe 0x5a5aL
      succp_2.vb.asUInt(1) shouldBe 0
      cxfp_1.vb.asPointer shouldBe 0x55abL
      succfp_1.vb.asUInt(1) shouldBe 1
      cxfp_2.vb.asPointer shouldBe 0x5a5aL
      succfp_2.vb.asUInt(1) shouldBe 0

      lp.vb.asPointer shouldBe 0x5a5aL
      lfp.vb.asPointer shouldBe 0x5a5aL

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

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }

  "LOAD, STORE, CMPXCHG and ATOMICRMW" should "jump to the exceptional destination on NULL ref access" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessingNull")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@memAccessingNull.v1.exit.trap_exit" => Rebind(st, PassValues(Seq()))
        case n                                     => fail("Unexpected trap " + n)
      }
    }

    ctx.closeContext()
  }

  "TRAP" should "work with all supported destinations" in {
    val ctx = microVM.newContext()

    val exc = ctx.newFixed("@void")
    val fortyTwo = ctx.handleFromInt64(42L)
    val fortyTwoPointZero = ctx.handleFromFloat(42.0f)
    val fortyTwoPointFive = ctx.handleFromDouble(42.5d)

    val func = ctx.handleFromFunc("@traptest")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@traptest.v1.entry.t1" => {
          Rebind(st, PassValues(Seq(fortyTwo)))
        }
        case "@traptest.v1.entry.t2" => {
          Rebind(st, PassValues(Seq(fortyTwoPointZero, fortyTwoPointFive)))
        }
        case "@traptest.v1.bb2.t3" => {
          Rebind(st, ThrowExc(exc))
        }
        case "@traptest.v1.exit.trap_exit" => {
          val Seq(v1, v2, v3, lp) = ctx.dumpKeepalives(st, 0)

          v1.vb.asSInt(64) shouldBe 42L
          v2.vb.asFloat shouldBe 42.0f
          v3.vb.asDouble shouldBe 42.5d
          lp.vb.asRef shouldBe exc.vb.asRef
          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ctx.closeContext()
  }

  "WATCHPOINT" should "do nothing when disabled" in {
    val ctx = microVM.newContext()

    ctx.disableWatchpoint(1)

    val func = ctx.handleFromFunc("@wptest")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@wptest.v1.dis.trap_dis" => {
          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ctx.closeContext()
  }

  "WATCHPOINT" should "work with all supported destinations when enabled" in {
    val ctx = microVM.newContext()

    ctx.enableWatchpoint(1)

    val exc = ctx.newFixed("@i32")
    val fortyTwo = ctx.handleFromInt64(42L)
    val fortyTwoPointZero = ctx.handleFromDouble(42.0d)

    val func = ctx.handleFromFunc("@wptest")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@wptest.v1.entry.w1" => {
          wp shouldBe 1
          Rebind(st, PassValues(Seq(fortyTwo)))
        }
        case "@wptest.v1.bb2.w2" => {
          wp shouldBe 1
          Rebind(st, PassValues(Seq(fortyTwoPointZero)))
        }
        case "@wptest.v1.bb3.w3" => {
          wp shouldBe 1
          Rebind(st, ThrowExc(exc))
        }
        case "@wptest.v1.exit.trap_exit" => {
          wp shouldBe 0
          val Seq(v1, v2, lp) = ctx.dumpKeepalives(st, 0)

          v1.vb.asSInt(64) shouldBe 42L
          v2.vb.asDouble shouldBe 42.0d
          lp.vb.asRef shouldBe exc.vb.asRef
          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ctx.closeContext()
  }

  "TRAP and WATCHPOINT" should "throw exceptions out of function when no exceptional dest" in {
    val ctx = microVM.newContext()

    ctx.enableWatchpoint(2)

    val exc1 = ctx.newFixed("@void")
    val exc2 = ctx.newFixed("@void")

    val func = ctx.handleFromFunc("@trapExc")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@trapThrow.v1.entry.t" => {
          Rebind(st, ThrowExc(exc1))
        }
        case "@wpThrow.v1.entry.w" => {
          wp shouldBe 2
          Rebind(st, ThrowExc(exc2))
        }
        case "@trapExc.v1.exit.trap_exit" => {
          val Seq(lp1, lp2) = ctx.dumpKeepalives(st, 0)

          lp1.vb.asRef shouldBe exc1.vb.asRef
          lp2.vb.asRef shouldBe exc2.vb.asRef

          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    ctx.closeContext()
  }

  "WPBRANCH" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@wpbranch")

    ctx.disableWatchpoint(42)

    var disReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@wpbranch.v1.dis.trap" => {
          disReached = true
          Rebind(st, PassValues(Seq()))
        }
        case "@wpbranch.v1.ena.trap" => {
          fail("Should not reach %ena")
          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    disReached shouldBe true

    ctx.enableWatchpoint(42)

    var enaReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@wpbranch.v1.dis.trap" => {
          fail("Should not reach %dis")
          Rebind(st, PassValues(Seq()))
        }
        case "@wpbranch.v1.ena.trap" => {
          enaReached = true
          Rebind(st, PassValues(Seq()))
        }
        case n => fail("Unexpected trap " + n)
      }
    }

    enaReached shouldBe true

    ctx.closeContext()
  }

  "SWAPSTAK" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testswapstack")

    var coro1Reached = false
    var coro2Reached = false
    var main1Reached = false
    var main2Reached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@corostackfunc.v1.entry.trap_coro1" => {
          val Seq(fromSta, p) = ctx.dumpKeepalives(st, 0)

          fromSta.asInstanceOf[MuStackRefValue].vb.stack.get.top.asInstanceOf[DefinedMuFrame].funcVer shouldBe microVM.globalBundle.funcVerNs("@testswapstack.v1")
          p.vb.asSInt(64) shouldBe 2L

          coro1Reached = true
          Rebind(st, PassValues(Seq()))
        }
        case "@corostackfunc.v1.entry.trap_coro2" => {
          val Seq(v1) = ctx.dumpKeepalives(st, 0)

          v1.vb.asDouble shouldBe 3.0d

          coro2Reached = true
          Rebind(st, PassValues(Seq()))
        }
        case "@testswapstack.v1.entry.trap_main1" => {
          val Seq(v1) = ctx.dumpKeepalives(st, 0)

          v1.vb.asSInt(64) shouldBe 3L

          main1Reached = true
          Rebind(st, PassValues(Seq()))
        }
        case "@testswapstack.v1.exit.trap_main2" => {
          val Seq(excVal) = ctx.dumpKeepalives(st, 0)

          excVal.vb.asSInt(64) shouldBe 7L

          main2Reached = true
          Rebind(st, PassValues(Seq()))
        }
      }
    }

    coro1Reached shouldBe true
    coro2Reached shouldBe true
    main1Reached shouldBe true
    main2Reached shouldBe true

    ctx.closeContext()
  }

  "COMMINST @uvm.tr64.*" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testtr64")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(rv, f, i, r,
        f_is_f, f_is_i, f_is_r, i_is_f, i_is_i, i_is_r, r_is_f, r_is_i, r_is_r,
        fb, ib, rb, tb) = ctx.dumpKeepalives(st, 0)

      f.vb.asTR64Raw shouldBe OpHelper.fpToTr64(42.0d)
      i.vb.asTR64Raw shouldBe OpHelper.intToTr64(0xfedcba9876543L)
      r.vb.asTR64Raw shouldBe OpHelper.refToTr64(rv.vb.asRef, 31L)

      (Seq(f_is_f, f_is_i, f_is_r, i_is_f, i_is_i, i_is_r, r_is_f, r_is_i, r_is_r).map(_.vb.asUInt(1)) shouldBe
        Seq(1, 0, 0, 0, 1, 0, 0, 0, 1))

      fb.vb.asDouble shouldBe 42.0d
      ib.vb.asUInt(52) shouldBe 0xfedcba9876543L
      rb.vb.asRef shouldBe rv.vb.asRef
      tb.vb.asUInt(6) shouldBe 31L

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "COMMINST @uvm.kill_dependency" should "do nothing" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testdependency")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(b) = ctx.dumpKeepalives(st, 0)

      b.vb.asSInt(64) shouldBe 3

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "COMMINST @uvm.native.pin and @uvm.native.unpin" should "expose ref/iref as ptr for access" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@objectpinning")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(a, b, c, d, e, f) = ctx.dumpKeepalives(st, 0)

      val aAddr = a.vb.asRef
      val (bObj, bOff) = b.vb.asIRef
      val cAddr = c.vb.asPointer
      val dAddr = d.vb.asPointer

      aAddr shouldEqual cAddr
      bObj shouldEqual dAddr
      cAddr shouldEqual dAddr

      e.vb.asSInt(64) shouldEqual 42
      f.vb.asSInt(64) shouldEqual 42

      val thr = th.vb.asThread.get
      thr.pinSet should contain(cAddr)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }
}