package uvm.refimpl.itpr

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import MemoryOrder._
import AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word

class UvmInterpreterSpec extends FlatSpec with Matchers {

  { // Configure logger
    import org.slf4j.LoggerFactory
    import org.slf4j.{ Logger => SLogger }
    import ch.qos.logback.classic.{ Logger => LLogger, Level }
    import ch.qos.logback.classic.Level._

    def setLevel(name: String, level: Level): Unit = {
      LoggerFactory.getLogger(name).asInstanceOf[LLogger].setLevel(level)
    }

    setLevel(SLogger.ROOT_LOGGER_NAME, INFO)
    setLevel("uvm.refimpl.itpr", DEBUG)
  }

  val microVM = new MicroVM();

  implicit def idOf(name: String): Int = microVM.globalBundle.allNs(name).id
  implicit def nameOf(id: Int): String = microVM.globalBundle.allNs(id).name.get

  {
    val ca = microVM.newClientAgent()

    val r = new FileReader("tests/uvm-refimpl-test/basic-tests.uir")
    ca.loadBundle(r)

    r.close()
    ca.close()
  }

  type TrapHandlerFunction = (ClientAgent, Handle, Handle, Int) => TrapHandlerResult

  class MockTrapHandler(thf: TrapHandlerFunction) extends TrapHandler {
    def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      thf(ca, thread, stack, watchPointID)
    }
  }

  def testFunc(ca: ClientAgent, func: Handle, args: Seq[Handle])(handler: TrapHandlerFunction): Unit = {
    microVM.trapManager.trapHandler = new MockTrapHandler(handler)
    val hStack = ca.newStack(func, args)
    val hThread = ca.newThread(hStack)
    microVM.threadStackManager.joinAll()
  }

  implicit class MagicalBox(vb: ValueBox) {
    def asInt: BigInt = vb.asInstanceOf[BoxInt].value
    def asSInt(l: Int): BigInt = OpHelper.prepareSigned(vb.asInstanceOf[BoxInt].value, l)
    def asUInt(l: Int): BigInt = OpHelper.prepareUnsigned(vb.asInstanceOf[BoxInt].value, l)
    def asFloat: Float = vb.asInstanceOf[BoxFloat].value
    def asDouble: Double = vb.asInstanceOf[BoxDouble].value
    def asRef: Word = vb.asInstanceOf[BoxRef].objRef
    def asIRef: (Word, Word) = { val b = vb.asInstanceOf[BoxIRef]; (b.objRef, b.offset) }
    def asIRefAddr: Word = { val b = vb.asInstanceOf[BoxIRef]; b.objRef + b.offset }
    def asStruct: Seq[ValueBox] = vb.asInstanceOf[BoxStruct].values
    def asFunc: Option[Function] = vb.asInstanceOf[BoxFunc].func
    def asThread: Option[InterpreterThread] = vb.asInstanceOf[BoxThread].thread
    def asStack: Option[InterpreterStack] = vb.asInstanceOf[BoxStack].stack
    def asTR64Box: BoxTagRef64 = vb.asInstanceOf[BoxTagRef64]
    def asTR64Raw: Long = vb.asInstanceOf[BoxTagRef64].raw
    def asVec: Seq[ValueBox] = vb.asInstanceOf[BoxVector].values
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
}