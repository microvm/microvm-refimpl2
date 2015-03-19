package uvm.refimpl.mem.scanning

import uvm.refimpl._
import uvm.refimpl.mem._
import TypeSizes.Word
import uvm.refimpl.itpr._
import java.util.ArrayDeque
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object AllScanner {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

/**
 * Traverse through all references in the micro VM.
 */
class AllScanner(val microVM: MicroVM, val handler: RefFieldHandler) extends RefFieldHandler {
  import AllScanner._

  private val queue = new ArrayDeque[Word]()

  def scanAll() {
    traceRoots()
    doTransitiveClosure()
  }

  private def traceRoots() {
    logger.debug("Tracing external roots...")
    traceClientAgents()
    logger.debug("Tracing globals...")
    traceGlobal()
    logger.debug("Tracing stacks...")
    traceStacks()
  }

  private def traceClientAgents() {
    for (ca <- microVM.clientAgents; h <- ca.handles) {
      h.vb match {
        case hor: HasObjRef => this.fromBox(hor)
        case _ =>
      }
    }
  }

  private def traceGlobal() {
    microVM.memoryManager.globalMemory.allocator.traverseFields(this)
  }

  private def traceStacks() {
    for (sta <- microVM.threadStackManager.iterateAllLiveStacks) {
      logger.debug(s"Tracing stack ${sta.id} for registers...")

      for (fra <- sta.frames; vb <- fra.boxes.values if vb.isInstanceOf[HasObjRef]) {
        val rvb = vb.asInstanceOf[HasObjRef]
        fromBox(rvb)
      }

      logger.debug(s"Tracing stack ${sta.id} memory chunk in LOS...")
      val stackMemory = sta.stackMemory
      val stackMemObjAddr = stackMemory.stackObjRef
      fromInternal(stackMemObjAddr) // This is a hack: A reference from nowhere.

      logger.debug(s"Tracing stack ${sta.id} for allocas...")
      stackMemory.traverseFields(this)
    }
  }

  private def doTransitiveClosure() {
    while (!queue.isEmpty) {
      val objRef = queue.pollFirst()
      logger.debug("Scanning heap object 0x%x...".format(objRef))
      MemoryDataScanner.scanAllocUnit(objRef, objRef, microVM, this)
    }
  }

  override def fromBox(box: HasObjRef): Option[Word] = {
    val rv = handler.fromBox(box)
    rv.foreach(queue.add)
    rv
  }
  
  override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
    val rv = handler.fromMem(objRef, iRef, toObj, isWeak, isTR64)
    rv.foreach(queue.add)
    rv
  }
  
  override def fromInternal(toObj: Word): Option[Word] = {
    val rv = handler.fromInternal(toObj)
    rv.foreach(queue.add)
    rv
  }

}