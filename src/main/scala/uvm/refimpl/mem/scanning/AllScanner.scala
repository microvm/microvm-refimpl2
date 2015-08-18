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

  private val addrQueue = new ArrayDeque[Word]()
  private val stackQueue = new ArrayDeque[InterpreterStack]()

  def scanAll() {
    traceRoots()
    doTransitiveClosure()
  }

  private def traceRoots() {
    logger.debug("Tracing external roots...")
    traceClientAgents()
    logger.debug("Tracing globals...")
    traceGlobal()
    logger.debug("Tracing threads...")
    traceThreads()
  }

  private def traceClientAgents() {
    for (ca <- microVM.clientAgents; h <- ca.handles) {
      h.vb match {
        case hor: HasObjRef => this.boxToHeap(hor)
        case bst: BoxStack  => this.boxToStack(bst)
        case _              =>
      }
    }
  }

  private def traceGlobal() {
    microVM.memoryManager.globalMemory.allocator.traverseFields(this)
  }

  private def traceThreads() {
    for (thr <- microVM.threadStackManager.iterateAllLiveThreads) {
      logger.debug(s"Tracing live thread ${thr.id} for its stack")
      this.threadToStack(thr, thr.stack)
    }
  }

  private def traceStack(sta: InterpreterStack) {
    logger.debug(s"Tracing stack ${sta.id} for registers...")

    for (fra <- sta.frames; vb <- fra.boxes.values) vb match {
      case hor: HasObjRef => this.boxToHeap(hor)
      case bst: BoxStack  => this.boxToStack(bst)
      case _              =>
    }

    logger.debug(s"Tracing stack ${sta.id} memory chunk in LOS...")
    val stackMemory = sta.stackMemory
    val stackMemObjAddr = stackMemory.stackObjRef
    this.stackToStackMem(sta, stackMemObjAddr)

    logger.debug(s"Tracing stack ${sta.id} for allocas...")
    stackMemory.traverseFields(this)
  }

  private def doTransitiveClosure() {
    var allEmpty = false

    while (!allEmpty) {
      allEmpty = true
      while (!stackQueue.isEmpty) {
        allEmpty = false
        val stack = stackQueue.pollFirst()
        logger.debug("Scanning stack %d...".format(stack.id))
        traceStack(stack)
      }
      while (!addrQueue.isEmpty) {
        allEmpty = false
        val objRef = addrQueue.pollFirst()
        logger.debug("Scanning heap object 0x%x...".format(objRef))
        MemoryDataScanner.scanAllocUnit(objRef, objRef, microVM, this)
      }
    }
  }

  override def boxToHeap(box: HasObjRef): Option[Word] = {
    val rv = handler.boxToHeap(box)
    rv.foreach(addrQueue.add)
    rv
  }

  override def boxToStack(box: BoxStack): Option[InterpreterStack] = {
    val rv = handler.boxToStack(box)
    rv.foreach(stackQueue.add)
    rv
  }

  override def memToHeap(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
    val rv = handler.memToHeap(objRef, iRef, toObj, isWeak, isTR64)
    rv.foreach(addrQueue.add)
    rv
  }

  override def memToStack(objRef: Word, iRef: Word, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
    val rv = handler.memToStack(objRef, iRef, toStack)
    rv.foreach(stackQueue.add)
    rv
  }

  override def stackToStackMem(stack: InterpreterStack, toObj: Word): Option[Word] = {
    val rv = handler.stackToStackMem(stack, toObj)
    rv.foreach(addrQueue.add)
    rv
  }

  override def threadToStack(thread: InterpreterThread, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
    val rv = handler.threadToStack(thread, toStack)
    rv.foreach(stackQueue.add)
    rv
  }

}