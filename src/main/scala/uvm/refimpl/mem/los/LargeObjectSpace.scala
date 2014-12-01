package uvm.refimpl.mem.los

import uvm.refimpl.mem._
import TypeSizes.Word
import uvm.refimpl.mem.simpleimmix._
import LargeObjectSpace._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.UvmRefImplException

object LargeObjectSpace {

  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val BLOCK_SIZE = SimpleImmixSpace.BLOCK_SIZE / 4

  private val OFFSET_PREV = 0

  private val OFFSET_NEXT = 8

  private val OFFSET_MARK = 16

  private val MARK_BIT = 0x1
}

/**
 * A mark-sweep freelist-based space to allocate large objects. An object always
 * occupies contiguous blocks and a block is used by at most one object at any
 * moment.
 * <p>
 * It has an extra header of three words before the GC header. The two words
 * form a doubly linked list of all live objects in the LOS. When sweeping, the
 * linked list is traversed to unlink all un-marked objects.
 * <p>
 * The third header is a mark bit for the block. It is used when traversing the
 * doubly linked list because the offset of the GC header relative to the block
 * start is not known (hybrids have one extra word of offset).
 * <p>
 * Objects in this space are never moved.
 */
class LargeObjectSpace(val heap: SimpleImmixHeap,
  name: String,
  begin: Long,
  extend: Word) extends Space(name, begin, extend) {

  if (extend % BLOCK_SIZE != 0)
    throw new UvmRefImplException("extend %d should be a multiple of BLOCK_SIZE %d".format(
      extend, BLOCK_SIZE))

  private val freeList: FreeList = new FreeList((extend / BLOCK_SIZE).toInt)

  /**
   * Head of the linked list of all live objects. 0 if there is no live object.
   */
  private var head: Word = 0

  def alloc(size: Word, align: Word, headerSize: Word): Word = {
    val userStart = TypeSizes.alignUp(16 + headerSize, align)
    val totalSize = userStart + size
    val nBlocks = (totalSize - 1) / BLOCK_SIZE + 1
    if (nBlocks > 0xffffffffL) {
      throw new UvmRefImplException("Object too large: " + totalSize)
    }
    val iBlocks = nBlocks.toInt

    def getBlockIndex: Int = {
      for (tries <- (0 until 2)) {
        val bi = freeList.allocate(iBlocks)
        if (bi == -1 && tries < 1) {
          heap.mutatorTriggerAndWaitForGCEnd(true)
        } else {
          return bi
        }
      }
      return -1
    }

    val blockIndex = getBlockIndex
    if (blockIndex == -1) {
      throw new UvmRefImplException("Out of memory when allocating large object of size: " +
        totalSize)
    }
    val blockAddr = blockIndexToBlockAddr(blockIndex)
    val regionSize = nBlocks * BLOCK_SIZE
    MemUtils.zeroRegion(blockAddr, regionSize)
    link(blockAddr)
    val objRef = blockAddr + userStart
    objRef
  }

  def markBlockByObjRef(objRef: Word) {
    val blockAddr = objRefToBlockAddr(objRef)
    logger.debug("marking block addr %d for obj %d...".format(blockAddr, objRef))
    markBlock(blockAddr)
  }

  def collect(): Boolean = {
    logger.debug("Start collecting...")
    if (head == 0) {
      logger.debug("not iterating because head == 0")
      return false
    }
    var anyDeallocated = false
    var curBlock = head
    val lastBlock = getPrev(curBlock)
    var nextBlock = getNext(curBlock)
    logger.debug("Begin iteration from %d to %d".format(curBlock, lastBlock))
    var finished = false
    while (!finished) {
      logger.debug("Visiting block %d..".format(curBlock))
      val mark = getBlockMark(curBlock)
      if (mark != MARK_BIT) {
        logger.debug("Deallocating block addr %d...".format(curBlock))
        dealloc(curBlock)
        anyDeallocated = true
      } else {
        logger.debug("Block addr %d contains live object.".format(curBlock))
        unmarkBlock(curBlock)
      }
      if (curBlock == lastBlock) {
        finished = true
      } else {
        curBlock = nextBlock
        nextBlock = getNext(curBlock)
      }
    }
    anyDeallocated
  }

  private def dealloc(blockAddr: Word) {
    val blockIndex = blockAddrToBlockIndex(blockAddr)
    freeList.deallocate(blockIndex)
    unlink(blockAddr)
  }

  def objRefToBlockIndex(objRef: Word): Int = {
    val blockAddr = objRefToBlockAddr(objRef)
    val blockIndex = blockAddrToBlockIndex(blockAddr)
    blockIndex
  }

  def objRefToBlockAddr(objRef: Word): Word = objRef & ~(BLOCK_SIZE - 1)

  def blockIndexToBlockAddr(blockIndex: Int): Word = begin + BLOCK_SIZE * blockIndex.toLong

  def blockAddrToBlockIndex(blockAddr: Word): Int = {
    ((blockAddr - begin) / BLOCK_SIZE).toInt
  }

  private def markBlock(blockAddr: Word) {
    MemorySupport.storeLong(blockAddr + OFFSET_MARK, MARK_BIT)
  }

  private def unmarkBlock(blockAddr: Word) {
    MemorySupport.storeLong(blockAddr + OFFSET_MARK, 0)
  }

  private def getBlockMark(blockAddr: Word): Word = {
    MemorySupport.loadLong(blockAddr + OFFSET_MARK)
  }

  private def link(blockAddr: Word) {
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

  private def unlink(blockAddr: Word) {
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

  private def getPrev(blockAddr: Word): Word = {
    MemorySupport.loadLong(blockAddr + OFFSET_PREV)
  }

  private def getNext(blockAddr: Word): Word = {
    MemorySupport.loadLong(blockAddr + OFFSET_NEXT)
  }

  private def setPrev(blockAddr: Word, toBlock: Word) {
    MemorySupport.storeLong(blockAddr + OFFSET_PREV, toBlock)
  }

  private def setNext(blockAddr: Word, toBlock: Word) {
    MemorySupport.storeLong(blockAddr + OFFSET_NEXT, toBlock)
  }
}
