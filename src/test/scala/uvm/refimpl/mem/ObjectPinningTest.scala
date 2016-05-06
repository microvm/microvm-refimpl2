package uvm.refimpl.mem

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import MemoryOrder._
import AtomicRMWOptr._
import scala.collection.mutable.ArrayBuffer
import uvm.refimpl.mem.TypeSizes.Word
import ch.qos.logback.classic.Level._

class ObjectPinningTest extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> INFO,
    "uvm.refimpl.mem" -> INFO,
    "uvm.refimpl.mem.simpleimmix.SimpleImmixCollector$" -> DEBUG)

  override def makeMicroVM() = new MicroVM(new VMConf(sosSize = 256L*1024L, losSize = 256L * 1024L))

  def gc() = microVM.memoryManager.heap.mutatorTriggerAndWaitForGCEnd(false)

  microVM.memoryManager.heap.space.debugLogBlockStates()

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/uvm-mem-test-bundle.uir")

  microVM.memoryManager.heap.space.debugLogBlockStates()

  behavior of "The garbage collector"

  it should "not collect pinned objects." in {
    val ctx = microVM.newContext()

    val hRefsKeep = new ArrayBuffer[MuValue]

    val hRefsPin = new ArrayBuffer[MuValue]
    val hPtrsPin = new ArrayBuffer[MuValue]
    val addrsPin = new ArrayBuffer[Word]

    val keepPer = 1000
    val pinPer = 6000

    for (i <- 0 until 20000) {
      val hRef = ctx.newFixed("@i64")
      if (i % keepPer == 0) {
        hRefsKeep += hRef

        if (i % pinPer == 0) {
          val hPtr = ctx.pin(hRef)
          val addr = ctx.handleToPtr(hPtr)
          hRefsPin += hRef
          hPtrsPin += hPtr
          addrsPin += addr
        }
      } else {
        ctx.deleteValue(hRef)
      }
    }

    gc()

    val addrs2Pin = hRefsPin.map { h =>
      val hPtr2 = ctx.pin(h)
      val addr2 = ctx.handleToPtr(hPtr2)
      addr2
    }

    addrsPin shouldEqual addrs2Pin

    for (hRef <- hRefsPin) {
      ctx.unpin(hRef)
      ctx.unpin(hRef)
    }
    
    gc()

    for (hRef <- hRefsKeep) {
      ctx.deleteValue(hRef)
    }

    gc()

    ctx.closeContext()
  }

}