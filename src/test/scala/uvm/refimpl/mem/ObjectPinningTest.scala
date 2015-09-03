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

  override def makeMicroVM() = new MicroVM(heapSize = 512L * 1024L)

  def gc() = microVM.memoryManager.heap.mutatorTriggerAndWaitForGCEnd(false)

  microVM.memoryManager.heap.space.debugLogBlockStates()

  preloadBundles("tests/uvm-refimpl-test/uvm-mem-test-bundle.uir")

  microVM.memoryManager.heap.space.debugLogBlockStates()

  behavior of "The garbage collector"

  it should "not collect pinned objects." in {
    val ca = microVM.newClientAgent()

    val hRefsKeep = new ArrayBuffer[Handle]

    val hRefsPin = new ArrayBuffer[Handle]
    val hPtrsPin = new ArrayBuffer[Handle]
    val addrsPin = new ArrayBuffer[Word]

    val keepPer = 1000
    val pinPer = 6000

    for (i <- 0 until 20000) {
      val hRef = ca.newFixed("@i64")
      if (i % keepPer == 0) {
        hRefsKeep += hRef

        if (i % pinPer == 0) {
          val hPtr = ca.pin(hRef)
          val addr = ca.toPointer(hPtr)
          hRefsPin += hRef
          hPtrsPin += hPtr
          addrsPin += addr
        }
      } else {
        ca.deleteHandle(hRef)
      }
    }

    gc()

    val addrs2Pin = hRefsPin.map { h =>
      val hPtr2 = ca.pin(h)
      val addr2 = ca.toPointer(hPtr2)
      addr2
    }

    addrsPin shouldEqual addrs2Pin

    for (hRef <- hRefsPin) {
      ca.unpin(hRef)
      ca.unpin(hRef)
    }
    
    gc()

    for (hRef <- hRefsKeep) {
      ca.deleteHandle(hRef)
    }

    gc()

    ca.close()
  }

}