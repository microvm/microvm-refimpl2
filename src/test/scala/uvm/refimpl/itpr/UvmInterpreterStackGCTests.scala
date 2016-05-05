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

class UvmInterpreterStackGCTests extends UvmBundleTesterBase {
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

  // With 1MiB space is LOS. It can accommodate 16 stacks.
  override def makeMicroVM = new MicroVM(new GCConf(losSize = 1L * 1024L * 1024L, stackSize = 63L * 1024L))

  "The memory manager" should "collect unreachable stacks." in {
    val ctx = microVM.newContext()

    val nStacks = ctx.handleFromInt64(13)

    val func = ctx.handleFromFunc("@stackcollecttest")
    testFunc(ctx, func, Seq(nStacks)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@stackcollecttest_v1.endloop.trap" => {
          gc()
          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }
}