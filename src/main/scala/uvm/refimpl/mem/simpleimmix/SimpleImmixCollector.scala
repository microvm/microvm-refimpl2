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

object SimpleImmixCollector {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class SimpleImmixCollector(val heap: SimpleImmixHeap,
  val space: SimpleImmixSpace,
  val los: LargeObjectSpace,
  microVM: MicroVM) extends Collector with Runnable {

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

  protected override def collect() {
    gcCount += 1
    logger.debug(s"GC starts. gcCount=${gcCount}")

    logger.debug("Clearing stats...")
    space.clearStats()

    val weakRefs = new ArrayBuffer[Word]()

    logger.debug("Marking and getting statistics....")
    val s1 = new AllScanner(microVM, new RefFieldHandler() {
      override def fromBox(box: HasObjRef): Option[Word] = box match {
        case HasNonZeroRef(toObj) => maybeMarkAndStat(toObj)
        case _ => None
      }

      override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
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

      override def fromInternal(toObj: Word): Option[Word] = if (toObj != 0L) maybeMarkAndStat(toObj) else None

    })
    s1.scanAll()

    logger.debug("Visit and clear weak references...")
    for (iRefWR <- weakRefs) {
      val toObj = MemorySupport.loadLong(iRefWR)
      val tag = HeaderUtils.getTag(toObj)
      val isMarked = (tag & MARK_MASK) != 0
      if (!isMarked) {
        logger.debug("WeakRef %d whose value was %d is zeroed.".format(iRefWR, toObj))
        MemorySupport.storeLong(iRefWR, 0)
      } else {
        logger.debug("WeakRef %d whose value was %d is still marked. Do not zero.".format(iRefWR, toObj))
      }
    }

    logger.debug("Stat finished. Unmarking....")
    val s2 = new AllScanner(microVM, clearMarkHandler)
    s2.scanAll()

    val resvSpace = space.getTotalReserveSpace
    threshold = space.findThreshold(resvSpace)
    logger.debug("Making defrag mutator...")
    defragMutator = new SimpleImmixDefragMutator(heap, space)
    canDefrag = true

    logger.debug("Mark again, maybe move objects....")
    val s3 = new AllScanner(microVM, markMover)
    s3.scanAll()

    defragMutator.close()

    logger.debug("Marked. Collecting blocks....")
    val anyMemoryRecycled = collectBlocks()
    if (!anyMemoryRecycled && heap.getMustFreeSpace) {
      throw new UvmRefImplException("Out of memory because the GC failed to recycle any memory.")
    }

    notifyMovedObjectsToFutex()

    logger.debug("Blocks collected. Unmarking....")
    val s4 = new AllScanner(microVM, clearMarkHandler)
    s4.scanAll()

    logger.debug("GC finished.")
    heap.untriggerGC()
  }

  private def maybeMarkAndStat(addr: Word): Option[Word] = {
    assert(addr != 0L, "addr should be non-zero before calling this function")
    val oldHeader = HeaderUtils.getTag(addr)
    logger.debug("GC header of 0x%x is 0x%x".format(addr, oldHeader))
    val wasMarked = (oldHeader & MARK_MASK) != 0
    if (!wasMarked) {
      val newHeader = oldHeader | MARK_MASK
      HeaderUtils.setTag(addr, newHeader)
      logger.debug("Newly marked 0x%x".format(addr))
      if (space.isInSpace(addr)) {
        //space.markBlockByObjRef(addr)
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
        //los.markBlockByObjRef(addr)
      } else {
        throw new UvmRefImplException("Object ref %d not in any space".format(addr))
      }
      Some(addr)
    } else {
      None
    }
  }

  private val markMover = new RefFieldHandler {

    override def fromBox(box: HasObjRef): Option[Word] = box match {
      case HasNonZeroRef(toObj) => maybeMove(toObj, newObjRef => RefFieldUpdater.updateBox(box, newObjRef))
      case _ => None
    }

    override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
      if (toObj != 0L) {
        maybeMove(toObj, newObjRef => RefFieldUpdater.updateMemory(iRef, isTR64, newObjRef))
      } else {
        None
      }
    }

    // Currently internally referenced objects (only the byte array for stacks) cannot move and does not transitively
    // refer to other objects.
    override def fromInternal(toObj: Word): Option[Word] = None

    private def maybeMove(toObj: Word, updateFunc: Word => Unit): Option[Word] = {
      val oldHeader = HeaderUtils.getTag(toObj)
      logger.debug("GC header of 0x%x is 0x%x".format(toObj, oldHeader))
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
            val stat = space.getStat(pageNum)
            if (stat < threshold) true else false
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
          logger.debug("Newly marked 0x%x".format(actualObj))

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
          val (newObjRef, oldSize): (Long, Long) = ty match {
            case htype: TypeHybrid => {
              val len = HeaderUtils.getVarLength(oldObjRef)
              val nor = defragMutator.newHybrid(htype, len)
              val os = TypeSizes.hybridSizeOf(htype, len)
              (nor, os)
            }
            case _ => {
              val nor = defragMutator.newScalar(ty)
              val os = TypeSizes.sizeOf(ty)
              (nor, os)
            }
          }

          val alignedOldSize = TypeSizes.alignUp(oldSize, TypeSizes.WORD_SIZE_BYTES)
          logger.debug("Copying old object 0x%x to 0x%x, %d bytes (aligned up to %d bytes).".format(
            oldObjRef, newObjRef, oldSize, alignedOldSize))
          MemUtils.memcpy(oldObjRef, newObjRef, alignedOldSize)
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
    override def fromBox(box: HasObjRef): Option[Word] = box match {
      case HasNonZeroRef(toObj) => clearMark(toObj)
      case _ => None
    }

    override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word] = {
      if (toObj != 0L) clearMark(toObj) else None
    }

    override def fromInternal(toObj: Word): Option[Word] = if (toObj != 0L) clearMark(toObj) else None
  }

  private def clearMark(objRef: Long): Option[Word] = {
    val oldHeader = HeaderUtils.getTag(objRef)
    logger.debug("GC header of 0x%x is 0x%x".format(objRef, oldHeader))
    val markBit = oldHeader & MARK_MASK
    if (markBit != 0) {
      val newHeader = oldHeader & ~(MARK_MASK | MOVE_MASK)
      HeaderUtils.setTag(objRef, newHeader)
      Some(objRef)
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

  private object HasNonZeroRef {
    def unapply(obj: Any): Option[Word] = obj match {
      case hor: HasObjRef => if (hor.hasObjRef) {
        val toObj = hor.getObjRef
        if (toObj != 0L) Some(toObj)
        else None
      } else None

      case _ => None
    }
  }

}
