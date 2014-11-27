package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.MicroVM
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import uvm.refimpl.mem.scanning._
import TypeSizes.Word
import uvm.types._
import SimpleImmixCollector._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import scala.collection.mutable.ArrayBuffer

object SimpleImmixCollector {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  private val MARK_MASK = 0x4000000000000000L

  private val MOVE_MASK = 0x8000000000000000L
}

class SimpleImmixCollector(val heap: SimpleImmixHeap,
  val space: SimpleImmixSpace,
  val los: LargeObjectSpace,
  microVM: MicroVM) extends Collector with Runnable {

  import SimpleImmixCollector._

  private var defragMutator: SimpleImmixDefragMutator = _

  private var canDefrag: Boolean = _

  private var threshold: Long = _

  private var gcCount: Int = 0

  protected override def collect() {
    gcCount += 1
    logger.debug(s"GC starts. gcCount=${gcCount}")
    
    logger.debug("Clearing stats...")
    space.clearStats()
    
    val weakRefs = new ArrayBuffer[Word]()
    
    logger.debug("Marking and getting statistics....")
    val s1 = new AllScanner(microVM, new RefFieldHandler() {
      override def fromBox(box: HasObjRef): Boolean = {
        return maybeMarkAndStat(box.getObjRef)
      }

      override def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Boolean = {
        if (isWeak) {
          if (toObj != 0) {
            logger.debug(s"Enqueued weak reference ${iRef} to ${toObj}")
            weakRefs.append(iRef)
          }
          return false
        } else {
          return maybeMarkAndStat(toObj)
        }
      }
    })
    s1.scanAll()
    
    logger.debug("Stat finished. Unmarking....")
    val s2 = new AllScanner(new RefFieldHandler() {

      override def handle(fromClient: Boolean,
        fromBox: HasObjRef,
        fromObj: Long,
        fromIRef: Long,
        toObj: Long,
        isWeak: Boolean,
        isTR64: Boolean): Boolean = return clearMark(toObj)
    })
    s2.scanAll()
    val resvSpace = space.getTotalReserveSpace
    threshold = space.findThreshold(resvSpace)
    logger.format("Making defrag mutator...")
    defragMutator = new SimpleImmixDefragMutator(heap, space)
    canDefrag = true
    logger.format("Mark again, maybe move objects....")
    val s3 = new AllScanner(markMover)
    s3.scanAll()
    defragMutator.close()
    logger.format("Visit and clear weak references...")
    for (iRefWR <- weakRefs) {
      val toObj = MEMORY_SUPPORT.loadLong(iRefWR)
      val tag = HeaderUtils.getTag(toObj)
      val isMarked = (tag & MARK_MASK) != 0
      if (!isMarked) {
        logger.format("WeakRef %d whose value was %d is zeroed.", iRefWR, toObj)
        MEMORY_SUPPORT.storeLong(iRefWR, 0)
      } else {
        logger.format("WeakRef %d whose value was %d is still marked. Do not zero.", iRefWR, toObj)
      }
    }
    logger.format("Marked. Collecting blocks....")
    val anyMemoryRecycled = collectBlocks()
    if (!anyMemoryRecycled && heap.getMustFreeSpace) {
      uvmError("Out of memory because the GC failed to recycle any memory.")
      System.exit(1)
    }
    logger.format("Blocks collected. Unmarking....")
    val s4 = new AllScanner(new RefFieldHandler() {

      override def handle(fromClient: Boolean,
        fromBox: HasObjRef,
        fromObj: Long,
        fromIRef: Long,
        toObj: Long,
        isWeak: Boolean,
        isTR64: Boolean): Boolean = return clearMark(toObj)
    })
    s4.scanAll()
    logger.format("GC finished.")
    heap.untriggerGC()
  }

  private def maybeMarkAndStat(addr: Long): Boolean = {
    if (addr == 0) {
      return false
    }
    val oldHeader = HeaderUtils.getTag(addr)
    logger.format("GC header of %d is %x", addr, oldHeader)
    val wasMarked = (oldHeader & MARK_MASK) != 0
    if (!wasMarked) {
      val newHeader = oldHeader | MARK_MASK
      HeaderUtils.setTag(addr, newHeader)
      logger.format("Newly marked %d", addr)
      if (space.isInSpace(addr)) {
        space.markBlockByObjRef(addr)
        val tag = HeaderUtils.getTag(addr)
        val `type` = HeaderUtils.getType(microVM, tag)
        var used: Long = 0l
        if (`type`.isInstanceOf[Hybrid]) {
          val varSize = HeaderUtils.getVarLength(addr)
          used = TypeSizes.hybridSizeOf(`type`.asInstanceOf[Hybrid], varSize) +
            TypeSizes.GC_HEADER_SIZE_HYBRID
        } else {
          used = TypeSizes.sizeOf(`type`) + TypeSizes.GC_HEADER_SIZE_SCALAR
        }
        val blockNum = space.objRefToBlockIndex(addr)
        space.incStat(blockNum, used)
      } else if (los.isInSpace(addr)) {
        los.markBlockByObjRef(addr)
      } else {
        uvmError(String.format("Object ref %d not in any space", addr))
        return false
      }
      true
    } else {
      false
    }
  }

  private class MarkMover extends RefFieldHandler {

    override def handle(fromClient: Boolean,
      fromBox: HasObjRef,
      fromObj: Long,
      fromIRef: Long,
      toObj: Long,
      isWeak: Boolean,
      isTR64: Boolean): Boolean = {
      if (toObj == 0 || isWeak) {
        return false
      }
      val oldHeader = HeaderUtils.getTag(toObj)
      logger.format("GC header of %d is %x", toObj, oldHeader)
      val markBit = oldHeader & MARK_MASK
      val moveBit = oldHeader & MOVE_MASK
      val wasMarked = markBit != 0
      val wasMoved = moveBit != 0
      if (wasMoved) {
        val dest = HeaderUtils.getForwardedDest(oldHeader)
        updateSrcRef(fromClient, fromBox, fromIRef, dest, isTR64)
        false
      } else {
        if (wasMarked) {
          false
        } else {
          var isMovable: Boolean = false
          if (fromClient) {
            isMovable = false
          } else {
            val isInSmallObjectSpace = space.isInSpace(toObj)
            if (isInSmallObjectSpace) {
              val pageNum = space.objRefToBlockIndex(toObj)
              val stat = space.getStat(pageNum)
              isMovable = if (stat < threshold) true else false
            } else {
              isMovable = false
            }
          }
          var actualObj: Long = 0l
          if (isMovable) {
            actualObj = evacuate(toObj)
            if (actualObj != toObj) {
              updateSrcRef(fromClient, fromBox, fromIRef, actualObj, isTR64)
            }
          } else {
            actualObj = toObj
          }
          val newHeader = oldHeader | MARK_MASK
          HeaderUtils.setTag(actualObj, newHeader)
          logger.format("Newly marked %d", actualObj)
          if (space.isInSpace(actualObj)) {
            space.markBlockByObjRef(actualObj)
          } else if (los.isInSpace(actualObj)) {
            los.markBlockByObjRef(actualObj)
          } else {
            uvmError(String.format("Object ref %d not in any space", actualObj))
            return false
          }
          true
        }
      }
    }

    private def evacuate(oldObjRef: Long): Long = {
      logger.format("Evacuating object %d", oldObjRef)
      if (!canDefrag) {
        logger.format("No more reserved blocks.")
        oldObjRef
      } else {
        val tag = HeaderUtils.getTag(oldObjRef)
        val `type` = HeaderUtils.getType(microVM, tag)
        var newObjRef: Long = 0l
        var oldSize: Long = 0l
        if (`type`.isInstanceOf[Hybrid]) {
          val len = HeaderUtils.getVarLength(oldObjRef)
          val htype = `type`.asInstanceOf[Hybrid]
          newObjRef = defragMutator.newHybrid(htype, len)
          oldSize = TypeSizes.hybridSizeOf(htype, len)
        } else {
          newObjRef = defragMutator.newScalar(`type`)
          oldSize = TypeSizes.sizeOf(`type`)
        }
        if (newObjRef == 0) {
          canDefrag = false
          logger.format("No more reserved blocks and thus no more moving.")
          oldObjRef
        } else {
          oldSize = TypeSizes.alignUp(oldSize, MemConstants.WORD_SIZE_BYTES)
          logger.format("Copying old object %d to %d, %d bytes.", oldObjRef, newObjRef, oldSize)
          MemUtils.memcpy(oldObjRef, newObjRef, oldSize)
          val newTag = newObjRef | MOVE_MASK
          HeaderUtils.setTag(oldObjRef, newTag)
          newObjRef
        }
      }
    }

    private def updateSrcRef(fromClient: Boolean,
      fromBox: HasObjRef,
      fromIRef: Long,
      dest: Long,
      isTR64: Boolean) {
      if (fromClient) {
        return
      } else if (fromBox != null) {
        fromBox.setObjRef(dest)
      } else {
        if (isTR64) {
          val oldBits = MEMORY_SUPPORT.loadLong(fromIRef)
          val oldTag = OpHelper.tr64ToTag(oldBits)
          val newBits = OpHelper.refToTr64(dest, oldTag)
          MEMORY_SUPPORT.storeLong(fromIRef, newBits)
        } else {
          MEMORY_SUPPORT.storeLong(fromIRef, dest)
        }
      }
    }
  }

  private var markMover: MarkMover = new MarkMover()

  private def collectBlocks(): Boolean = {
    var anyMemoryRecycled = false
    anyMemoryRecycled = space.collectBlocks() || anyMemoryRecycled
    anyMemoryRecycled = los.collect() || anyMemoryRecycled
    anyMemoryRecycled
  }

  private def clearMark(objRef: Long): Boolean = {
    if (objRef == 0) {
      return false
    }
    val oldHeader = HeaderUtils.getTag(objRef)
    logger.format("GC header of %d is %x", objRef, oldHeader)
    val markBit = oldHeader & MARK_MASK
    if (markBit != 0) {
      val newHeader = oldHeader & ~(MARK_MASK | MOVE_MASK)
      HeaderUtils.setTag(objRef, newHeader)
      true
    } else {
      false
    }
  }

}
