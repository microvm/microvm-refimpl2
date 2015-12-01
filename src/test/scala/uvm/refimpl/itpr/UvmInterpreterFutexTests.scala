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
import ch.qos.logback.classic.Level._
import uvm.refimpl.UvmBundleTesterBase
import uvm.refimpl.TrapHandlerResult.{ ThreadExit, Rebind }
import uvm.refimpl.HowToResume.{ PassValues, ThrowExc }

class UvmInterpreterFutexTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/futex-tests.uir")

  "Futex" should "wake up the waiting thread" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@futex_setter")

    var trapWaiterReached = false
    var trapSetterReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@futex_waiter_v1.entry.trap_waiter" => {
          val Seq(rv, sv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToSInt(rv.asInstanceOf[MuIntValue]) shouldBe 0
          ctx.handleToSInt(sv.asInstanceOf[MuIntValue]) shouldBe 42

          trapWaiterReached = true

          returnFromTrap(st)
        }
        case "@futex_setter_v1.wait_exit.trap_setter" => {
          trapSetterReached = true

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()

    trapWaiterReached shouldBe true
    trapSetterReached shouldBe true
  }

  "Futex" should "wake up if not explicitly woken within the give timeout" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@futex_delayer")

    var trapDelayerReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@futex_delayer_v1.entry.trap_delayer" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToSInt(rv.asInstanceOf[MuIntValue]) shouldBe -3

          trapDelayerReached = true

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()

    trapDelayerReached shouldBe true
  }

  "Futex" should "not sleep if the memory location does not hold the expected value" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@futex_no_sleep")

    var trapNoSleepReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@futex_no_sleep_v1.entry.trap_no_sleep" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToSInt(rv.asInstanceOf[MuIntValue]) shouldBe -1

          trapNoSleepReached = true

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()

    trapNoSleepReached shouldBe true
  }

  "Futex" should "wake up requeued threads" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@futex_requeue_test")

    var mainExit = false
    var subExit = 0

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@futex_requeue_waiter_v1.entry.trap_requeue_waiter" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToSInt(rv.asInstanceOf[MuIntValue]) shouldBe 0

          subExit += 1

          returnFromTrap(st)
        }
        case "@futex_requeue_test_v1.wait_body.trap_wait" => {
          val Seq(nt, nt2) = ctx.dumpKeepalives(st, 0)

          val nthr = nt.vb.asThread.get
          val nthr2 = nt2.vb.asThread.get

          if (nthr.isFutexWaiting && nthr2.isFutexWaiting) {
            val one = ctx.handleFromInt32(1)
            Rebind(st, PassValues(Seq(one)))
          } else {
            val zero = ctx.handleFromInt32(0)
            Rebind(st, PassValues(Seq(zero)))
          }
        }
        case "@futex_requeue_test_v1.wait_exit.trap_setter" => {
          val Seq(nwakes, nwakes2) = ctx.dumpKeepalives(st, 0)

          ctx.handleToSInt(nwakes.asInstanceOf[MuIntValue]) shouldBe 1
          ctx.handleToSInt(nwakes2.asInstanceOf[MuIntValue]) shouldBe 1

          mainExit = true

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()

    mainExit shouldBe true
    subExit shouldBe 2
  }

  def gc() = microVM.memoryManager.heap.mutatorTriggerAndWaitForGCEnd(false)

  /** Disable some logger for allocation-heavy parts. */
  def verboseGC[T](f: => T): T = {
    setLogLevels("uvm.refimpl.mem" -> DEBUG)

    val rv = f

    setLogLevels("uvm.refimpl.mem" -> null)

    rv
  }

  "Futex" should "work across GC" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@futex_with_gc")

    var mainExit = false
    var subExit = false

    verboseGC {

      testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
        val trapName = nameOf(ctx.curInst(st, 0))

        trapName match {
          case "@futex_gc_waiter_v1.entry.trap_gc_waiter" => {
            val Seq(rv) = ctx.dumpKeepalives(st, 0)

            ctx.handleToSInt(rv.asInstanceOf[MuIntValue]) shouldBe 0

            subExit = true

            returnFromTrap(st)
          }
          case "@futex_with_gc_v1.wait_body.trap_wait" => {
            val Seq(nt) = ctx.dumpKeepalives(st, 0)

            val nthr = nt.vb.asThread.get

            if (nthr.isFutexWaiting) {
              val one = ctx.handleFromInt32(1)
              Rebind(st, PassValues(Seq(one)))
            } else {
              val zero = ctx.handleFromInt32(0)
              Rebind(st, PassValues(Seq(zero)))
            }
          }
          case "@futex_with_gc_v1.wait_exit.trap_gc" => {
            gc()

            returnFromTrap(st)
          }
          case "@futex_with_gc_v1.wait_exit.trap_exit" => {
            val Seq(nwakes) = ctx.dumpKeepalives(st, 0)

            ctx.handleToSInt(nwakes.asInstanceOf[MuIntValue]) shouldBe 1

            mainExit = true

            returnFromTrap(st)
          }
          case _ => fail("Should not hit " + trapName)
        }
      }

      ctx.closeContext()
    }

    mainExit shouldBe true
    subExit shouldBe true
  }
}