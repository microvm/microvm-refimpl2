package uvm.refimpl.osr

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

class UvmOSRTests extends UvmBundleTesterBase {

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl.mem" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/osr-tests.uir")

  "Stack introspection" should "see functions and keepalive value in all frames" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@intro_test_base")

    val arg0 = ctx.handleFromInt64(3)

    testFunc(ctx, func, Seq(arg0)) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@intro_rec_v1.zero.trap_rec" => {
          val Seq(n0: MuIntValue) = ctx.dumpKeepalives(fc)

          ctx.handleToUInt(n0) shouldBe 0

          for (i <- 1 to 3) {
            ctx.nextFrame(fc)
            nameOf(ctx.curFunc(fc)) shouldBe "@intro_rec"
            nameOf(ctx.curFuncVer(fc)) shouldBe "@intro_rec_v1"
            nameOf(ctx.curInst(fc)) shouldBe "@intro_rec_v1.nz.call"
            val Seq(ni: MuIntValue, nm1i: MuIntValue) = ctx.dumpKeepalives(st, i)
            ctx.handleToUInt(ni) shouldBe i
            ctx.handleToUInt(nm1i) shouldBe (i - 1)
          }

          ctx.nextFrame(fc)
          nameOf(ctx.curInst(fc)) shouldBe "@intro_test_base_v1.entry.call"

          ctx.closeCursor(fc)

          Rebind(st, PassValues(Seq(n0)))
        }
      }
    }

    ctx.closeContext()
  }

  "@sum" should "give the sum when n < 5" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@osr_test_base")

    val arg0 = ctx.handleFromInt64(4)

    testFunc(ctx, func, Seq(arg0)) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToUInt(rv) shouldBe 6

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }

  "@sum" should "trigger trap when n >= 5 and work with OSR" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@osr_test_base")

    val arg0 = ctx.handleFromInt64(8)

    testFunc(ctx, func, Seq(arg0)) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@sum_v1.opt.trap_opt" => {
          val Seq(n:MuIntValue, i:MuIntValue, s:MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToUInt(s) shouldBe 10
          ctx.handleToUInt(i) shouldBe 5
          ctx.handleToUInt(n) shouldBe 8

          // Emulate optimising compiling by loading a pre-optimised version.
          val r = new FileReader("tests/uvm-refimpl-test/osr-tests-part2.uir")
          try {
            ctx.loadBundle(r)
          } finally {
            r.close()
          }

          // OSR
          ctx.nextFrame(fc)
          ctx.popFramesTo(fc)
          ctx.closeCursor(fc)

          val oneShotFunc = ctx.handleFromFunc("@sum_osr_oneshot")
          ctx.pushFrame(st, oneShotFunc)

          // Continue
          Rebind(st, PassValues(Seq(s, i, n)))
        }
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToUInt(rv) shouldBe 28

          returnFromTrap(st)
        }
      }
    }

    // The second time when it is called, it should call the second version
    // and OSR should be unnecessary.
    testFunc(ctx, func, Seq(arg0)) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToUInt(rv) shouldBe 28

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }

  /**
   * @param funcs many functions. funcs(0) is the bottom function.
   * @param args arguments to the top function
   */
  def testFuncMulti(ctx: MuCtx, funcs: Seq[MuFuncRefValue], args: Seq[MuValue])(handler: TrapHandlerFunction): Unit = {
    microVM.setTrapHandler(new MockTrapHandler(handler))
    val hStack = ctx.newStack(funcs(0))
    for (f <- funcs.tail) {
      ctx.pushFrame(hStack, f)
    }
    val hThread = ctx.newThread(hStack, None, uvm.refimpl.HowToResume.PassValues(args))
    microVM.execute()
  }

  "The return values" should "become the parameters of the previous frame" in {
    val ctx = microVM.newContext()

    val func1 = ctx.handleFromFunc("@consecutive_push_main")
    val func2 = ctx.handleFromFunc("@forty_two_returner")

    val arg0 = ctx.handleFromInt64(8)

    testFuncMulti(ctx, Seq(func1, func2), Seq()) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@consecutive_push_main.v1.entry.trap" => {
          val Seq(n: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToUInt(n) shouldBe 42

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }

  "Multiple consecutively pushed frames" should "pass return values to their respective parent frames" in {
    val ctx = microVM.newContext()

    val func1 = ctx.handleFromFunc("@consecutive_push_main")
    val func2 = ctx.handleFromFunc("@forty_two_returner")
    val func3 = ctx.handleFromFunc("@add_one")

    val arg0 = ctx.handleFromInt64(8)

    var addOneReachCount = 0L

    testFuncMulti(ctx, Seq(func1, func3, func3, func3, func2), Seq()) { (ctx, th, st, wp) =>
      val fc = ctx.newCursor(st)
      nameOf(ctx.curInst(fc)) match {
        case "@add_one.v1.entry.trap" => {
          val Seq(n: MuIntValue, n2: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToSInt(n).toLong shouldBe (42L + addOneReachCount)
          ctx.handleToSInt(n2) shouldBe (42L + addOneReachCount + 1L)
          addOneReachCount += 1L

          returnFromTrap(st)
        }
        case "@consecutive_push_main.v1.entry.trap" => {
          val Seq(n: MuIntValue) = ctx.dumpKeepalives(fc)
          ctx.closeCursor(fc)

          ctx.handleToSInt(n).toLong shouldBe 45L

          returnFromTrap(st)
        }
      }
    }

    addOneReachCount shouldBe 3L

    ctx.closeContext()
  }
}