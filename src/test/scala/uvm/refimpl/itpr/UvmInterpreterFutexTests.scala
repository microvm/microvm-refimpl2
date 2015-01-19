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

class UvmInterpreterFutexTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/futex-tests.uir")

  "Futex" should "wake up the waiting thread" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@futex_setter")

    var trapWaiterReached = false
    var trapSetterReached = false

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val trapName = nameOf(ca.currentInstruction(st, 0))

      trapName match {
        case "@futex_waiter_v1.trap_waiter" => {
          val Seq(rv, sv) = ca.dumpKeepalives(st, 0)

          ca.toInt(rv, signExt = true) shouldBe 0
          ca.toInt(sv, signExt = true) shouldBe 42

          trapWaiterReached = true

          TrapRebindPassVoid(st)
        }
        case "@futex_setter_v1.trap_setter" => {
          trapSetterReached = true

          TrapRebindPassVoid(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ca.close()

    trapWaiterReached shouldBe true
    trapSetterReached shouldBe true
  }

  "Futex" should "wake up if not explicitly woken within the give timeout" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@futex_delayer")

    var trapDelayerReached = false

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val trapName = nameOf(ca.currentInstruction(st, 0))

      trapName match {
        case "@futex_delayer_v1.trap_delayer" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)

          ca.toInt(rv, signExt = true) shouldBe -3

          trapDelayerReached = true

          TrapRebindPassVoid(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ca.close()

    trapDelayerReached shouldBe true
  }

  "Futex" should "not sleep if the memory location does not hold the expected value" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@futex_no_sleep")

    var trapNoSleepReached = false

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val trapName = nameOf(ca.currentInstruction(st, 0))

      trapName match {
        case "@futex_no_sleep_v1.trap_no_sleep" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)

          ca.toInt(rv, signExt = true) shouldBe -1

          trapNoSleepReached = true

          TrapRebindPassVoid(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ca.close()

    trapNoSleepReached shouldBe true
  }

  "Futex" should "wake up requeued threads" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@futex_requeue_test")

    var mainExit = false
    var subExit = 0

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val trapName = nameOf(ca.currentInstruction(st, 0))

      trapName match {
        case "@futex_requeue_waiter_v1.trap_requeue_waiter" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)

          ca.toInt(rv, signExt = true) shouldBe 0

          subExit += 1

          TrapRebindPassVoid(st)
        }
        case "@futex_requeue_test_v1.trap_wait" => {
          val Seq(nt, nt2) = ca.dumpKeepalives(st, 0)

          val nthr = nt.vb.asThread.get
          val nthr2 = nt2.vb.asThread.get

          if (nthr.isFutexWaiting && nthr2.isFutexWaiting) {
            val one = ca.putInt("@i32", 1)
            TrapRebindPassValue(st, one)
          } else {
            val zero = ca.putInt("@i32", 0)
            TrapRebindPassValue(st, zero)
          }
        }
        case "@futex_requeue_test_v1.trap_setter" => {
          val Seq(nwakes, nwakes2) = ca.dumpKeepalives(st, 0)

          ca.toInt(nwakes, signExt = true) shouldBe 1
          ca.toInt(nwakes2, signExt = true) shouldBe 1

          mainExit = true

          TrapRebindPassVoid(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ca.close()

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
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@futex_with_gc")

    var mainExit = false
    var subExit = false

    verboseGC {

      testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
        val trapName = nameOf(ca.currentInstruction(st, 0))

        trapName match {
          case "@futex_gc_waiter_v1.trap_gc_waiter" => {
            val Seq(rv) = ca.dumpKeepalives(st, 0)

            ca.toInt(rv, signExt = true) shouldBe 0

            subExit = true

            TrapRebindPassVoid(st)
          }
          case "@futex_with_gc_v1.trap_wait" => {
            val Seq(nt) = ca.dumpKeepalives(st, 0)

            val nthr = nt.vb.asThread.get

            if (nthr.isFutexWaiting) {
              val one = ca.putInt("@i32", 1)
              TrapRebindPassValue(st, one)
            } else {
              val zero = ca.putInt("@i32", 0)
              TrapRebindPassValue(st, zero)
            }
          }
          case "@futex_with_gc_v1.trap_gc" => {
            gc()

            TrapRebindPassVoid(st)
          }
          case "@futex_with_gc_v1.trap_exit" => {
            val Seq(nwakes) = ca.dumpKeepalives(st, 0)

            ca.toInt(nwakes, signExt = true) shouldBe 1

            mainExit = true

            TrapRebindPassVoid(st)
          }
          case _ => fail("Should not hit " + trapName)
        }
      }

      ca.close()
    }

    mainExit shouldBe true
    subExit shouldBe true
  }
}