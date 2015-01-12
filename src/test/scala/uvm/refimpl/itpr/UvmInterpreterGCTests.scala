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

  "The memory manager" should "retain global reference between gc." in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@keepglobal")
    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      gc()
      
            

      TrapRebindPassVoid(st)
    }

    ca.close()
  }
}