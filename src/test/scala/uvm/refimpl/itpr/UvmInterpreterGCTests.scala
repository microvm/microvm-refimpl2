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

class UvmInterpreterGCTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG,
    "uvm.refimpl.mem" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/gc-tests.uir")

  def gc() = microVM.memoryManager.heap.mutatorTriggerAndWaitForGCEnd(false)

  /** Disable some logger for allocation-heavy parts. */
  def quiet[T](f: => T): T = {
    setLogLevels(
      "uvm.refimpl.itpr" -> INFO,
      "uvm.refimpl.mem.simpleimmix.SimpleImmixMutator$" -> INFO,
      "uvm.refimpl.mem.HeaderUtils" -> INFO)

    val rv = f

    setLogLevels("uvm.refimpl.itpr" -> DEBUG,
      "uvm.refimpl.mem.simpleimmix.SimpleImmixMutator$" -> null,
      "uvm.refimpl.mem.HeaderUtils" -> null)

    rv
  }

  "The memory manager" should "retain global reference between gc." in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@keepglobal")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@keepglobal_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }
        case "@keepglobal_v1.entry.checktrap" => {
          val Seq(obj2, obj2val) = ca.dumpKeepalives(st, 0)

          obj2.vb.asRef shouldNot be(0L)
          obj2val.vb.asSInt(64) shouldBe 42

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()
  }

  "The memory manager" should "not retain references in dead alloca cells between gc." in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@nokeepalloca")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@nokeepalloca_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }
        case "@allocatest_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()
  }

  "The large object space" should "withstand repeated non-retained allocations" in {
    val ca = microVM.newClientAgent()

    quiet {
      val func = ca.putFunction("@crazy_allocation_test")
      testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ca.close()

    gc()
  }

  "The small object space" should "withstand repeated fragmented allocations" in {
    val ca = microVM.newClientAgent()

    quiet {
      val func = ca.putFunction("@breadcrumbs")
      testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ca.close()

    gc()
  }

  "The garbage collector" should "nullify weak references if there is no strong references to the referent" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testweakref")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@testweakref_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }
        case "@peekweakref_v1.entry.checknztrap" => {
          val Seq(refval) = ca.dumpKeepalives(st, 0)

          refval.vb.asRef shouldNot be(0L)

          TrapRebindPassVoid(st)
        }
        case "@testweakref_v1.entry.checkztrap" => {
          val Seq(refval) = ca.dumpKeepalives(st, 0)

          refval.vb.asRef should be(0L)

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()

    gc()
  }
  
  "The garbage collector" should "treat tagref64 as reference when it actually is" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testtagrefgc")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@testtagrefgc_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }

        case "@testtagrefgc_v1.entry.checktrap" => {
          val Seq(tr, refv, tagv, iv) = ca.dumpKeepalives(st, 0)

          ca.toInt(tagv) shouldBe 13
          ca.toInt(iv) shouldBe 42

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()

    gc()
  }
  
  "The garbage collector" should "scan tagref64 references in the memory" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@testtagrefgcmem")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@testtagrefgcmem_v1.entry.gctrap" => {
          gc()
          TrapRebindPassVoid(st)
        }

        case "@testtagrefgcmem_v1.entry.checktrap" => {
          val Seq(tr, refv, tagv, iv) = ca.dumpKeepalives(st, 0)

          ca.toInt(tagv) shouldBe 13
          ca.toInt(iv) shouldBe 42

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()

    gc()
  }
  
  "The small object space" should "withstand repeated fragmented allocations when objects are referred by tagref64" in {
    val ca = microVM.newClientAgent()

    quiet {
      val func = ca.putFunction("@breadcrumbstr64")
      testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ca.close()

    gc()
  }
}