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

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/gc-tests.uir")

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
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@keepglobal")
    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@keepglobal_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }
        case "@keepglobal_v1.entry.checktrap" => {
          val Seq(obj2, obj2val) = ctx.dumpKeepalives(st, 0)

          obj2.vb.asRef shouldNot be(0L)
          obj2val.vb.asSInt(64) shouldBe 42

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }

  "The memory manager" should "not retain references in dead alloca cells between gc." in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@nokeepalloca")
    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@nokeepalloca_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }
        case "@allocatest_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }

  "The large object space" should "withstand repeated non-retained allocations" in {
    val ctx = microVM.newContext()

    quiet {
      val func = ctx.handleFromFunc("@crazy_allocation_test")
      testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ctx.closeContext()

    gc()
  }

  "The small object space" should "withstand repeated fragmented allocations" in {
    val ctx = microVM.newContext()

    quiet {
      val func = ctx.handleFromFunc("@breadcrumbs")
      testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ctx.closeContext()

    gc()
  }

  "The garbage collector" should "nullify weak references if there is no strong references to the referent" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testweakref")
    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@testweakref_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }
        case "@peekweakref_v1.entry.checknztrap" => {
          val Seq(refval) = ctx.dumpKeepalives(st, 0)

          refval.vb.asRef shouldNot be(0L)

          returnFromTrap(st)
        }
        case "@testweakref_v1.entry.checkztrap" => {
          val Seq(refval) = ctx.dumpKeepalives(st, 0)

          refval.vb.asRef should be(0L)

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()

    gc()
  }
  
  "The garbage collector" should "treat tagref64 as reference when it actually is" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testtagrefgc")
    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@testtagrefgc_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }

        case "@testtagrefgc_v1.entry.checktrap" => {
          val Seq(tr, refv, tagv, iv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToUInt(tagv.asInstanceOf[MuIntValue]) shouldBe 13
          ctx.handleToUInt(iv.asInstanceOf[MuIntValue]) shouldBe 42

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()

    gc()
  }
  
  "The garbage collector" should "scan tagref64 references in the memory" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@testtagrefgcmem")
    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@testtagrefgcmem_v1.entry.gctrap" => {
          gc()
          returnFromTrap(st)
        }

        case "@testtagrefgcmem_v1.entry.checktrap" => {
          val Seq(tr, refv, tagv, iv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToUInt(tagv.asInstanceOf[MuIntValue]) shouldBe 13
          ctx.handleToUInt(iv.asInstanceOf[MuIntValue]) shouldBe 42

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()

    gc()
  }
  
  "The small object space" should "withstand repeated fragmented allocations when objects are referred by tagref64" in {
    val ctx = microVM.newContext()

    quiet {
      val func = ctx.handleFromFunc("@breadcrumbstr64")
      testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
        fail("No traps in this test")
      }
    }

    ctx.closeContext()

    gc()
  }
}