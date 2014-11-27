package uvm.refimpl.mem.los

import uvm.platformsupport.Config._
import uvm.refimpl.mem.MemUtils
import uvm.refimpl.mem.Space
import uvm.refimpl.mem.TypeSizes
import uvm.refimpl.mem.simpleimmix.SimpleImmixHeap
import uvm.refimpl.mem.simpleimmix.SimpleImmixSpace
import uvm.util.ErrorUtils
import uvm.util.LogUtil
import uvm.util.Logger
import LargeObjectSpace._
//remove if not needed
import scala.collection.JavaConversions._

object LargeObjectSpace {

  private val logger = LogUtil.getLogger("LOS")

  val BLOCK_SIZE = SimpleImmixSpace.BLOCK_SIZE / 4

  private val OFFSET_PREV = 0

  private val OFFSET_NEXT = 8

  private val OFFSET_MARK = 16

  private val MARK_BIT = 0x1
}

class LargeObjectSpace(private var heap: SimpleImmixHeap, 
    name: String, 
    begin: Long, 
    extend: Long) extends Space(name, begin, extend) {

  private var freeList: FreeList = new FreeList((extend / BLOCK_SIZE).toInt)

  private var head: Long = 0

  ErrorUtils.uvmAssert(extend % BLOCK_SIZE == 0, String.format("extend %d should be a multiple of BLOCK_SIZE %d", 
    extend, BLOCK_SIZE))

  def alloc(size: Long, align: Long, headerSize: Long): Long = {
    val userStart = TypeSizes.alignUp(16 + headerSize, align)
    val totalSize = userStart + size
    val nBlocks = (totalSize - 1) / BLOCK_SIZE + 1
    if (nBlocks > 0xffffffffL) {
      ErrorUtils.uvmError("Object too large: " + totalSize)
      return 0
    }
    val iBlocks = nBlocks.toInt
    var blockIndex = -1
    for (tries <- 0.until(2)) {
      blockIndex = freeList.allocate(iBlocks)
      if (blockIndex == -1 && tries == 0) {
        heap.mutatorTriggerAndWaitForGCEnd(true)
      } else {
        //break
      }
    }
    if (blockIndex == -1) {
      ErrorUtils.uvmError("Out of memory when allocating large object of size: " + 
        totalSize)
      return 0
    }
    val blockAddr = blockIndexToBlockAddr(blockIndex)
    val regionSize = nBlocks * BLOCK_SIZE
    MemUtils.zeroRegion(blockAddr, regionSize)
    link(blockAddr)
    val objRef = blockAddr + userStart
    objRef
  }

  def markBlockByObjRef(objRef: Long) {
    val blockAddr = objRefToBlockAddr(objRef)
    logger.format("marking block addr %d for obj %d...", blockAddr, objRef)
    markBlock(blockAddr)
  }

  def collect(): Boolean = {
    logger.format("Start collecting...")
    if (head == 0) {
      logger.format("not iterating because head == 0")
      return false
    }
    var anyDeallocated = false
    var curBlock = head
    val lastBlock = getPrev(curBlock)
    var nextBlock = getNext(curBlock)
    logger.format("Begin iteration from %d to %d", curBlock, lastBlock)
    while (true) {
      logger.format("Visiting block %d..", curBlock)
      val mark = getBlockMark(curBlock)
      if (mark != MARK_BIT) {
        logger.format("Deallocating block addr %d...", curBlock)
        dealloc(curBlock)
        anyDeallocated = true
      } else {
        logger.format("Block addr %d contains live object.", curBlock)
        unmarkBlock(curBlock)
      }
      if (curBlock == lastBlock) {
        //break
      } else {
        curBlock = nextBlock
        nextBlock = getNext(curBlock)
      }
    }
    anyDeallocated
  }

  private def dealloc(blockAddr: Long) {
    val blockIndex = blockAddrToBlockIndex(blockAddr)
    freeList.deallocate(blockIndex)
    unlink(blockAddr)
  }

  def objRefToBlockIndex(objRef: Long): Int = {
    val blockAddr = objRefToBlockAddr(objRef)
    val blockIndex = blockAddrToBlockIndex(blockAddr)
    blockIndex
  }

  def objRefToBlockAddr(objRef: Long): Long = objRef & ~(BLOCK_SIZE - 1)

  def blockIndexToBlockAddr(blockIndex: Int): Long = begin + BLOCK_SIZE * blockIndex.toLong

  def blockAddrToBlockIndex(blockAddr: Long): Int = {
    ((blockAddr - begin) / BLOCK_SIZE).toInt
  }

  private def markBlock(blockAddr: Long) {
    MEMORY_SUPPORT.storeLong(blockAddr + OFFSET_MARK, MARK_BIT)
  }

  private def unmarkBlock(blockAddr: Long) {
    MEMORY_SUPPORT.storeLong(blockAddr + OFFSET_MARK, 0)
  }

  private def getBlockMark(blockAddr: Long): Long = {
    MEMORY_SUPPORT.loadLong(blockAddr + OFFSET_MARK)
  }

  private def link(blockAddr: Long) {
    if (head == 0) {
      head = blockAddr
      setPrev(blockAddr, blockAddr)
      setNext(blockAddr, blockAddr)
    } else {
      val last = getPrev(head)
      setPrev(blockAddr, last)
      setNext(blockAddr, head)
      setPrev(head, blockAddr)
      setNext(last, blockAddr)
    }
  }

  private def unlink(blockAddr: Long) {
    val next = getNext(blockAddr)
    if (next == blockAddr) {
      head = 0
    } else {
      val prev = getPrev(blockAddr)
      setNext(prev, next)
      setPrev(next, prev)
      head = next
    }
  }

  private def getPrev(blockAddr: Long): Long = {
    MEMORY_SUPPORT.loadLong(blockAddr + OFFSET_PREV)
  }

  private def getNext(blockAddr: Long): Long = {
    MEMORY_SUPPORT.loadLong(blockAddr + OFFSET_NEXT)
  }

  private def setPrev(blockAddr: Long, toBlock: Long) {
    MEMORY_SUPPORT.storeLong(blockAddr + OFFSET_PREV, toBlock)
  }

  private def setNext(blockAddr: Long, toBlock: Long) {
    MEMORY_SUPPORT.storeLong(blockAddr + OFFSET_NEXT, toBlock)
  }
}
