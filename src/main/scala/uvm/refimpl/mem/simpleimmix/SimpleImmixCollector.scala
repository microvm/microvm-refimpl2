package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.MicroVM
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import uvm.refimpl.mem.scanning._
import TypeSizes._
import uvm.types._
import SimpleImmixCollector._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import scala.collection.mutable.ArrayBuffer
import uvm.refimpl.UvmRefImplException
import uvm.utils.HexDump
import scala.collection.mutable.HashSet
import com.typesafe.scalalogging.LazyLogging

object SimpleImmixCollector {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

private class MarkClearCheck extends LazyLogging {
  val markedRefs = new HashSet[Word]()
  val clearedRefs = new HashSet[Word]()
  val diffRefs = new HashSet[Word]()
  
  def beforeMarking(): Unit = {
    markedRefs.clear
  }
  
  def marked(objRef: Word): Unit = {
    markedRefs.add(objRef)
  }
  
  def beforeClearing(): Unit = {
    clearedRefs.clear
  }
  
  def cleared(objRef: Word): Unit = {
    clearedRefs.add(objRef)
  }
  
  def debugPrintStat(phase: String): Unit = {
    diffRefs.clear()
    for (r <- markedRefs if !clearedRefs.contains(r)) {
      diffRefs.add(r)
    }
    logger.debug("After phase %s: nmarked:%d, ncleared:%d".format(phase, markedRefs.size, clearedRefs.size))
    logger.debug("After phase %s: Refs marked but not cleared: \n".format(phase) + diffRefs.map(r => "0x%x\n".format(r)).mkString)
  }
}

class SimpleImmixCollector(val heap: SimpleImmixHeap, val space: SimpleImmixSpace, val los: LargeObjectSpace)(
  implicit microVM: MicroVM, memorySupport: MemorySupport)
    extends Collector with Runnable {

  import SimpleImmixCollector._

  // Mutator object for defrag. Created just before the defrag phase (s3).
  private var defragMutator: SimpleImmixDefragMutator = _

  // A flag to tell the markMover object that there is still (possibly) some space for defrag.  
  private var canDefrag: Boolean = _

  // Blocks with occupancy below this number of bytes are subject to defrag.
  // Calculated by collect() and read by the markMover object.
  private var threshold: Long = _

  // The number of times GC has run.
  private var gcCount: Int = 0
  
  private val MARK_CLEAR_DEBUG = false  // Set to true if you want to check if any references are marked but not cleared
  private val markClearCheck = if (MARK_CLEAR_DEBUG) Some(new MarkClearCheck()) else None

  protected override def collect() {
    gcCount += 1
    logger.debug(s"GC starts. gcCount=${gcCount}")
    
    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()

    logger.debug("Clearing stats...")
    space.clearStats()

    val weakRefs = new ArrayBuffer[Word]()
    
    markClearCheck.map(_.beforeMarking())

    logger.debug("Marking and getting statistics....")
    val s1 = new AllScanner(new RefFieldHandler() {
      override def boxToHeap(box: HasObjRef): Option[Word] = if (box.hasObjRef()) {
        maybeMarkAndStatIfNotNull(box.getObjRef())
      } else {
        None
      }

      override def boxToStack(box: BoxStack): Option[InterpreterStack] = {
        maybeMarkStackIfSome(box.stack)
      }

      override def memToHeap(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
        if (toObj != 0L) {
          if (isWeak) {
            logger.debug(s"Enqueued weak reference ${iRef} to ${toObj}")
            weakRefs.append(iRef)
            None
          } else {
            maybeMarkAndStat(toObj)
          }
        } else {
          None
        }
      }

      override def memToStack(objRef: Word, iRef: Word, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
        maybeMarkStackIfSome(toStack)
      }

      override def stackToStackMem(stack: InterpreterStack, toObj: Word): Option[Word] = maybeMarkAndStatIfNotNull(toObj)

      override def threadToStack(thread: InterpreterThread, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
        maybeMarkStackIfSome(toStack)
      }

      override def pinSetToMem(toObj: Word): Option[Word] = {
        maybeMarkAndStatIfNotNull(toObj)
      }
    })
    s1.scanAll()

    logger.debug("Visit and clear weak references...")
    for (iRefWR <- weakRefs) {
      val toObj = memorySupport.loadLong(iRefWR)
      val tag = HeaderUtils.getTag(toObj)
      val isMarked = (tag & MARK_MASK) != 0
      if (!isMarked) {
        logger.debug("WeakRef %d whose value was %d is zeroed.".format(iRefWR, toObj))
        memorySupport.storeLong(iRefWR, 0)
      } else {
        logger.debug("WeakRef %d whose value was %d is still marked. Do not zero.".format(iRefWR, toObj))
      }
    }

    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()

    markClearCheck.map(_.beforeClearing())
    
    logger.debug("Stat finished. Unmarking....")
    val s2 = new AllScanner(clearMarkHandler)
    s2.scanAll()
    
    markClearCheck.map(_.debugPrintStat("2"))

    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()

    val resvSpace = space.getTotalReserveSpace
    threshold = space.findThreshold(resvSpace)
    logger.debug("Making defrag mutator...")
    defragMutator = new SimpleImmixDefragMutator(heap, space)
    canDefrag = true

    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()
    
    markClearCheck.map(_.beforeMarking())

    logger.debug("Mark again, maybe move objects....")
    val s3 = new AllScanner(markMover)
    s3.scanAll()

    defragMutator.close()

    logger.debug("Marked. Collecting blocks....")
    val anyMemoryRecycled = collectBlocks()

    logger.debug("Killing unreachable stacks...")
    for (st <- microVM.threadStackManager.iterateAllLiveStacks) {
      if (!st.gcMark) {
        logger.debug("Killing stack %d...".format(st.id))
        st.kill()
      }
    }

    if (!anyMemoryRecycled && heap.getMustFreeSpace) {
      throw new UvmRefImplException("Out of memory because the GC failed to recycle any memory.")
    }

    notifyMovedObjectsToFutex()

    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()
    
    markClearCheck.map(_.beforeClearing())

    logger.debug("Blocks collected. Unmarking....")
    val s4 = new AllScanner(clearMarkHandler)
    s4.scanAll()

    if (logger.underlying.isDebugEnabled()) space.debugLogBlockStates()


    logger.debug("GC finished.")
    heap.untriggerGC()
  }

  private def maybeMarkAndStatIfNotNull(addr: Word): Option[Word] = {
    if (addr != 0L) maybeMarkAndStat(addr) else None
  }

  private def maybeMarkAndStat(addr: Word): Option[Word] = {
    assert(addr != 0L, "addr should be non-zero before calling this function")
    val oldHeader = HeaderUtils.getTag(addr)
    logger.debug("GC header of 0x%x is 0x%x".format(addr, oldHeader))
    val markBit = oldHeader & MARK_MASK
    val moveBit = oldHeader & MOVE_MASK
    val wasMarked = markBit != 0
    val wasMoved = moveBit != 0
    if (!wasMarked) {
      val newHeader = oldHeader | MARK_MASK
      HeaderUtils.setTag(addr, newHeader)
      markClearCheck.map(_.marked(addr))
      logger.debug("MarkStat: Newly marked 0x%x".format(addr))
      if (space.isInSpace(addr)) {
        //space.markBlockByObjRef(addr) // Unnecessary. Block mark is only cleared when collecting. Should do in phase3
        val tag = HeaderUtils.getTag(addr)
        val ty = HeaderUtils.getType(microVM, tag)
        val used = ty match {
          case h: TypeHybrid => {
            val varSize = HeaderUtils.getVarLength(addr)
            TypeSizes.hybridSizeOf(ty.asInstanceOf[TypeHybrid], varSize) + TypeSizes.GC_HEADER_SIZE_HYBRID
          }
          case _ => {
            TypeSizes.sizeOf(ty) + TypeSizes.GC_HEADER_SIZE_SCALAR
          }
        }
        val blockNum = space.objRefToBlockIndex(addr)
        space.incStat(blockNum, used)
      } else if (los.isInSpace(addr)) {
        //los.markBlockByObjRef(addr) // Unnecessary. Block mark is only cleared when collecting. Should do in phase3
      } else {
        throw new UvmRefImplException("Object ref %d not in any space".format(addr))
      }
      Some(addr)
    } else {
      None
    }
  }

  private def maybeMarkStackIfSome(maybeStack: Option[InterpreterStack]): Option[InterpreterStack] = {
    maybeStack.flatMap(maybeMarkStack)
  }

  private def maybeMarkStack(stack: InterpreterStack): Option[InterpreterStack] = {
    if (stack.state != FrameState.Dead) {
      if (!stack.gcMark) {
        stack.gcMark = true
        Some(stack)
      } else {
        None
      }
    } else {
      None
    }
  }

  private val markMover = new RefFieldHandler {

    override def boxToHeap(box: HasObjRef): Option[Word] = if (box.hasObjRef()) {
      maybeMoveIfNotNull(box.getObjRef, newObjRef => RefFieldUpdater.updateBoxToHeap(box, newObjRef))
    } else {
      None
    }
    override def boxToStack(box: BoxStack): Option[InterpreterStack] = {
      maybeMarkStackIfSome(box.stack)
    }
    override def memToHeap(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
      maybeMoveIfNotNull(toObj, newObjRef => RefFieldUpdater.updateMemToHeap(iRef, isTR64, newObjRef))
    }
    override def memToStack(objRef: Word, iRef: Word, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
      maybeMarkStackIfSome(toStack)
    }
    // Currently internally referenced objects (only the byte array for stacks) cannot move and does not transitively
    // refer to other objects.
    override def stackToStackMem(stack: InterpreterStack, toObj: Word): Option[Word] = {
      maybeMoveIfNotNull(toObj, _ => throw new UvmRefImplException("Stack memory cannot move."))
    }

    override def threadToStack(thread: InterpreterThread, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
      maybeMarkStackIfSome(toStack)
    }

    override def pinSetToMem(toObj: Word): Option[Word] = {
      if (space.isInSpace(toObj)) {
        logger.debug("Object 0x%x is in small object space and is pinned. Marking block %d".format(toObj, space.objRefToBlockIndex(toObj)))
        space.markBlockByObjRef(toObj, true)
      }
      maybeMoveIfNotNull(toObj, _ => throw new UvmRefImplException("Pinned object cannot move."))
    }

    private def maybeMoveIfNotNull(toObj: Word, updateFunc: Word => Unit): Option[Word] = {
      if (toObj != 0L) maybeMove(toObj, updateFunc) else None
    }

    private def maybeMove(toObj: Word, updateFunc: Word => Unit): Option[Word] = {
      val oldHeader = HeaderUtils.getTag(toObj)
      logger.debug("GC header of 0x%x is 0x%x".format(toObj, oldHeader))
      
      if (oldHeader == 0x0L) {
        logger.error("Header is zero! GC header of 0x%x is 0x%x".format(toObj, oldHeader))
        
        val plusMinus = 256L
        
        val memDump = HexDump.dumpMemory(toObj-plusMinus, plusMinus*2L)
        logger.error("Memory dump:\n"+memDump)
        
        throw new UvmRefImplException("Header is zero! obj: 0x%x, header: 0x%x".format(toObj, oldHeader))
      }
      
      val markBit = oldHeader & MARK_MASK
      val moveBit = oldHeader & MOVE_MASK
      val wasMarked = markBit != 0
      val wasMoved = moveBit != 0
      if (wasMoved) {
        val dest = HeaderUtils.getForwardedDest(oldHeader)
        updateFunc(dest)
        None
      } else {
        if (wasMarked) {
          None
        } else {
          val isInSmallObjectSpace = space.isInSpace(toObj)

          val isMovable = if (isInSmallObjectSpace) {
            val pageNum = space.objRefToBlockIndex(toObj)
            if (space.isPinned(pageNum)) {
              logger.debug("Object 0x%x is in pinned page %d, cannot move.".format(toObj, pageNum))
              false
            } else {
              val stat = space.getStat(pageNum)
              if (stat < threshold) true else false
            }
          } else {
            false
          }

          val actualObj = if (isMovable) {
            val newObjRef = evacuate(toObj)
            if (newObjRef != toObj) {
              updateFunc(newObjRef)
            }
            newObjRef
          } else {
            toObj
          }

          val newHeader = oldHeader | MARK_MASK
          HeaderUtils.setTag(actualObj, newHeader)
          markClearCheck.map(_.marked(actualObj))
          logger.debug("MarkMove: Newly marked 0x%x".format(actualObj))

          if (space.isInSpace(actualObj)) {
            space.markBlockByObjRef(actualObj)
          } else if (los.isInSpace(actualObj)) {
            los.markBlockByObjRef(actualObj)
          } else {
            throw new UvmRefImplException("Object ref %x not in any space".format(actualObj))
          }
          Some(actualObj)
        }
      }
    }

    /**
     * Evacuate an object to a new block when still has space. Return the new location of the object when moved, or the
     * old location when not moved.
     */
    private def evacuate(oldObjRef: Word): Word = {
      logger.debug("Evacuating object 0x%x".format(oldObjRef))
      if (!canDefrag) {
        logger.debug("No more reserved blocks.")
        oldObjRef
      } else {
        val tag = HeaderUtils.getTag(oldObjRef)
        val ty = HeaderUtils.getType(microVM, tag)

        try {
          val (newObjRef, oldSize, oldVarLen): (Long, Long, Option[Long]) = ty match {
            case htype: TypeHybrid => {
              val len = HeaderUtils.getVarLength(oldObjRef)
              val nor = defragMutator.newHybrid(htype, len)
              val os = TypeSizes.hybridSizeOf(htype, len)
              (nor, os, Some(len))
            }
            case _ => {
              val nor = defragMutator.newScalar(ty)
              val os = TypeSizes.sizeOf(ty)
              (nor, os, None)
            }
          }

          val alignedOldSize = TypeSizes.alignUp(oldSize, TypeSizes.WORD_SIZE_BYTES)

          logger.debug("Copying old object 0x%x to 0x%x, %d bytes (aligned up to %d bytes).".format(
            oldObjRef, newObjRef, oldSize, alignedOldSize))
          MemUtils.memcpy(oldObjRef, newObjRef, alignedOldSize)
          oldVarLen foreach { varLen => 
            logger.debug("Copying old variable part length %d 0x%x to objref 0x%x".format(varLen, varLen, newObjRef))
            HeaderUtils.setVarLength(newObjRef, varLen)
          }
          
          val newTag = newObjRef | MOVE_MASK
          HeaderUtils.setTag(oldObjRef, newTag)
          newObjRef

        } catch {
          case e: NoMoreDefragBlockException =>
            canDefrag = false
            logger.debug("No more reserved blocks and thus no more moving.")
            oldObjRef
        }
      }
    }
  }

  private def collectBlocks(): Boolean = {
    val spaceCollected = space.collectBlocks()
    val losCollected = los.collect()
    return spaceCollected || losCollected
  }

  private val clearMarkHandler = new RefFieldHandler() {
    override def boxToHeap(box: HasObjRef): Option[Word] = if (box.hasObjRef) {
      clearMarkIfNotNull(box.getObjRef())
    } else {
      None
    }

    override def boxToStack(box: BoxStack): Option[InterpreterStack] = {
      clearStackMarkIfSome(box.stack)
    }

    override def memToHeap(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
      clearMarkIfNotNull(toObj)
    }

    override def memToStack(objRef: Word, iRef: Word, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
      clearStackMarkIfSome(toStack)
    }

    override def stackToStackMem(stack: InterpreterStack, toObj: Word): Option[Word] = clearMarkIfNotNull(toObj)

    override def threadToStack(thread: InterpreterThread, toStack: Option[InterpreterStack]): Option[InterpreterStack] = {
      clearStackMarkIfSome(toStack)
    }

    override def pinSetToMem(toObj: Word): Option[Word] = {
      clearMarkIfNotNull(toObj)
    }
  }

  private def clearMarkIfNotNull(objRef: Long): Option[Word] = {
    if (objRef != 0L) clearMark(objRef) else None
  }

  private def clearMark(objRef: Long): Option[Word] = {
    val oldHeader = HeaderUtils.getTag(objRef)
    logger.debug("GC header of 0x%x is 0x%x".format(objRef, oldHeader))
    val markBit = oldHeader & MARK_MASK
    val moveBit = oldHeader & MOVE_MASK
    val wasMarked = markBit != 0
    val wasMoved = moveBit != 0
    if (wasMoved) {
      throw new UvmRefImplException("Should not point to tombstone when clearing marks. objRef: 0x%x".format(objRef))
    }
    if (wasMarked) {
      val newHeader = oldHeader & ~MARK_MASK
      HeaderUtils.setTag(objRef, newHeader)
      markClearCheck.map(_.cleared(objRef))
      logger.debug("MarkClear: Newly marked 0x%x (unmarking)".format(objRef))
      Some(objRef)
    } else {
      None
    }
  }

  private def clearStackMarkIfSome(maybeStack: Option[InterpreterStack]): Option[InterpreterStack] = {
    maybeStack.flatMap(clearStackMark)
  }

  private def clearStackMark(stack: InterpreterStack): Option[InterpreterStack] = {
    if (stack.gcMark) {
      stack.gcMark = false
      Some(stack)
    } else {
      None
    }
  }

  private def getMovement(objRef: Word): Option[Word] = {
    val tag = HeaderUtils.getTag(objRef)
    logger.debug("Inspecting header for Futex. Obj 0x%x, tag 0x%x".format(objRef, tag))

    val moveBit = tag & MOVE_MASK

    if (moveBit != 0) {
      val newAddr = HeaderUtils.getForwardedDest(tag)
      Some(newAddr)
    } else {
      None
    }
  }

  private def notifyMovedObjectsToFutex(): Unit = {
    microVM.threadStackManager.futexManager.afterGCAdjust(getMovement)
  }

}
