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

import ch.qos.logback.classic.Level._

class UvmOSRTests extends UvmBundleTesterBase {

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl.mem" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/osr-tests.uir")

  "Stack introspection" should "see functions and keepalive value in all frames" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@intro_test_base")

    val arg0 = ca.putInt("@i64", 3)

    testFunc(ca, func, Seq(arg0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@intro_rec_v1.zero.trap_rec" => {
          val Seq(n0) = ca.dumpKeepalives(st, 0)
          ca.toInt(n0) shouldBe 0

          for (i <- 1 to 3) {
            nameOf(ca.currentInstruction(st, i)) shouldBe "@intro_rec_v1.nz.rv"
            val Seq(ni, nm1i) = ca.dumpKeepalives(st, i)
            ca.toInt(ni) shouldBe i
            ca.toInt(nm1i) shouldBe (i - 1)
          }

          nameOf(ca.currentInstruction(st, 4)) shouldBe "@intro_test_base_v1.entry.rv"

          TrapRebindPassValue(st, n0)
        }
      }
    }

    ca.close()
  }

  "@sum" should "give the sum when n < 5" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@osr_test_base")

    val arg0 = ca.putInt("@i64", 4)

    testFunc(ca, func, Seq(arg0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)
          ca.toInt(rv) shouldBe 6

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()
  }

  "@sum" should "trigger trap when n >= 5 and work with OSR" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@osr_test_base")

    val arg0 = ca.putInt("@i64", 8)

    testFunc(ca, func, Seq(arg0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@sum_v1.opt.trap_opt" => {
          val Seq(n, i, s) = ca.dumpKeepalives(st, 0)
          
          ca.toInt(s) shouldBe 10
          ca.toInt(i) shouldBe 5
          ca.toInt(n) shouldBe 8

          // Emulate optimising compiling by loading a pre-optimised version.
          val r = new FileReader("tests/uvm-refimpl-test/osr-tests-part2.uir")
          try {
            ca.loadBundle(r)
          } finally {
            r.close()
          }
          
          // OSR
          ca.popFrame(st)
          
          val oneShotFunc = ca.putFunction("@sum_osr_oneshot")
          ca.pushFrame(st, oneShotFunc, Seq(s, i, n))

          // Continue
          TrapRebindPassVoid(st)
        }
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)
          ca.toInt(rv) shouldBe 28

          TrapRebindPassVoid(st)
        }
      }
    }
    
    // The second time when it is called, it should call the second version
    // and OSR should be unnecessary.
    testFunc(ca, func, Seq(arg0)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@osr_test_base_v1.entry.trap_base_exit" => {
          val Seq(rv) = ca.dumpKeepalives(st, 0)
          ca.toInt(rv) shouldBe 28

          TrapRebindPassVoid(st)
        }
      }
    }
    
    ca.close()
  }
}