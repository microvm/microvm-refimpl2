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
 * Traverse through all references in the µVM.
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
    val sr = microVM.threadStackManager.stackRegistry
    for (sta <- sr.values if sta.state != StackState.Dead) {
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
      MemoryDataScanner.scanAllocUnit(objRef, objRef, microVM, this)
    }
  }

  override def fromBox(box: HasObjRef): Boolean = {
    val toEnqueue = handler.fromBox(box)
    if (toEnqueue) {
      queue.add(box.getObjRef())
    }
    toEnqueue
  }
  
  override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Boolean = {
    val toEnqueue = handler.fromMem(objRef, iRef, toObj, isWeak, isTR64)
    if (toEnqueue) {
      queue.add(toObj)
    }
    toEnqueue
  }

}