package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import TypeSizes.Word
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.UvmRefImplException

object SimpleImmixSpace {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val BLOCK_SIZE = 32768L

  val BLOCK_MARKED = 0x1
  val BLOCK_RESERVED = 0x2

  private val LINE_SIZE = 128L

  private val N_BUCKETS = 256
}

class SimpleImmixSpace(val heap: SimpleImmixHeap, val name: String, val begin: Word, val extend: Word)
  extends Space(name, begin, extend) {

  import SimpleImmixSpace._

  val nBlocks: Int = (extend / BLOCK_SIZE).toInt

  /** Flag for each block */
  private val blockFlags: Array[Int] = new Array[Int](nBlocks)

  /** A list of free blocks */
  private val freeList: Array[Int] = new Array[Int](nBlocks)

  /** The number of valid entries in freeList */
  private var freeListValidCount: Int = nBlocks - nReserved

  /** The index of the next free block to allocate in freeList */
  private var nextFree: Int = 0

  /** For each block, count how many bytes are occupied. */
  private val blockUsedStats: Array[Word] = new Array[Word](nBlocks)

  /** The number of reserved blocks (for defrag). */
  private val nReserved: Int = nBlocks / 20

  /** A list of free blocks reserved for defrag. */
  private val defragResv: Array[Int] = new Array[Int](nReserved)

  /** The number of free blocks (valid entries) in defragResv. */
  private var defragResvFree: Int = _

  /** The index of the next free reserved block to allocate in defragResv. */
  private var nextResv: Int = 0

  /** A list of buckets, for statistics. Used by defrag. */
  private val buckets: Array[Word] = new Array[Word](N_BUCKETS)

  if (begin % BLOCK_SIZE != 0) {
    throw new UvmRefImplException("space should be aligned to BLOCK_SIZE " + BLOCK_SIZE)
  }

  if (extend % BLOCK_SIZE != 0) {
    throw new UvmRefImplException("space size should be a multiple of BLOCK_SIZE " + BLOCK_SIZE)
  }

  for (i <- 0 until nReserved) { // Block 0 to nReserved-1 are reserved
    defragResv(i) = i
    reserve(i) // Set the reserved flat
  }

  for (i <- nReserved until nBlocks) { // The rest of the blocks are free to allocate
    freeList(i - nReserved) = i
  }

  /**
   * Try to get a free block. If not available, return None. The old block, if present, is returned from reserving.
   */
  def tryGetBlock(oldBlockAddr: Option[Word]): Option[Word] = {
    oldBlockAddr.foreach(returnBlock)

    val myCursor = nextFree
    if (myCursor >= freeListValidCount) {
      return None
    }

    nextFree += 1

    val blockNum = freeList(myCursor)
    reserve(blockNum)

    val blockAddr = blockIndexToBlockAddr(blockNum)
    MemUtils.zeroRegion(blockAddr, BLOCK_SIZE)

    Some(blockAddr)
  }

  private def reserve(blockNum: Int) {
    blockFlags(blockNum) |= BLOCK_RESERVED
  }

  private def unreserve(blockNum: Int) {
    blockFlags(blockNum) &= ~BLOCK_RESERVED
  }

  def objRefToBlockIndex(objRef: Word): Int = {
    val blockAddr = objRef & ~(BLOCK_SIZE - 1)
    /*
     * NOTE: My SimpleImmixMutator refuses to fill up a block to exactly its
     * upper-bound, in which case if the last object is a "void", its header
     * will occupy the last word in the block, but the the objRef appears to
     * be the beginning of the next block. This has plagued Rifat, but I
     * cheated by avoiding the problem in the allocator.
     */

    val blockIndex = blockAddrToBlockIndex(blockAddr)
    blockIndex
  }

  def blockIndexToBlockAddr(blockIndex: Int): Word = begin + BLOCK_SIZE * blockIndex.toLong

  def blockAddrToBlockIndex(blockAddr: Word): Int = {
    ((blockAddr - begin) / BLOCK_SIZE).toInt
  }

  def markBlockByIndex(index: Int) {
    blockFlags(index) |= BLOCK_MARKED
  }

  def markBlockByObjRef(objRef: Word) {
    val blockIndex = objRefToBlockIndex(objRef)
    markBlockByIndex(blockIndex)
    logger.debug(s"Marked block ${blockIndex}")
  }

  def collectBlocks(): Boolean = {
    // Shift defrag reserved blocks to the beginning;
    for (i <- nextResv until defragResvFree) {
      defragResv(i - nextResv) = defragResv(i)
    }

    var newDefragResvFree = defragResvFree - nextResv
    var newNFree = 0
    for (i <- 0 until nBlocks) {
      var flag = blockFlags(i)
      val bits = (flag & (BLOCK_MARKED | BLOCK_RESERVED))
      if (bits == 0) {
        if (newDefragResvFree < nReserved) {
          defragResv(newDefragResvFree) = i
          newDefragResvFree += 1
          flag |= BLOCK_RESERVED
        } else {
          freeList(newNFree) = i
          newNFree += 1
          flag &= ~BLOCK_RESERVED
        }
      } else {
        logger.debug(s"Block ${i} is not freed because flag bits is ${bits}")
      }
      flag &= ~BLOCK_MARKED
      blockFlags(i) = flag
    }
    defragResvFree = newDefragResvFree
    freeListValidCount = newNFree
    if (logger.underlying.isDebugEnabled()) {
      val sb1 = new StringBuilder("New reserved freelist:")
      for (i <- 0 until defragResvFree) {
        sb1.append(" ").append(defragResv(i))
      }
      logger.debug(sb1.toString)
      val sb2 = new StringBuilder("New freelist:")
      for (i <- 0 until freeListValidCount) {
        sb2.append(" ").append(freeList(i))
      }
      logger.debug(sb2.toString)
      for (i <- 0 until nBlocks) {
        logger.debug(s"blockFlags[${i}] = ${blockFlags(i)}")
      }
    }
    nextResv = 0
    nextFree = 0
    return newNFree > 0
  }

  def returnBlock(blockAddr: Word) {
    val blockNum = blockAddrToBlockIndex(blockAddr)
    unreserve(blockNum)
  }

  // Statistics

  def clearStats() {
    for (i <- 0 until nBlocks) {
      blockUsedStats(i) = 0L
    }
  }

  def incStat(blockNum: Int, size: Word) {
    blockUsedStats(blockNum) += size
  }

  def getStat(pageNum: Int): Word = blockUsedStats(pageNum)

  def getTotalReserveSpace(): Word = defragResvFree * BLOCK_SIZE

  /**
   * Blocks whose used bytes larger than the returned threshold are subject to
   * defrag.
   */
  def findThreshold(avail: Word): Word = {
    if (logger.underlying.isDebugEnabled) {
      logger.debug(s"Finding threshold. avail = ${avail}")
      for (i <- 0 until nBlocks) {
        logger.debug(s"blockUsedStats[${i}] = ${blockUsedStats(i)}")
      }
    }
    for (i <- 0 until N_BUCKETS) {
      buckets(i) = 0
    }
    for (i <- 0 until nBlocks if (blockFlags(i) & BLOCK_MARKED) != 0) {
      val used = blockUsedStats(i)
      val bucket = (used / LINE_SIZE).toInt
      buckets(bucket) += used
    }
    if (logger.underlying.isDebugEnabled) {
      var accum: Word = 0
      for (i <- 0 until nBlocks) {
        accum += buckets(i)
        logger.debug(s"buckets[${i}] = ${buckets(i)}, accum: ${accum}")
      }
    }
    var curUsed: Word = 0
    var curBucket = 0
    while (curBucket < N_BUCKETS && curUsed <= avail) {
      curUsed += buckets(curBucket)
      if (curUsed <= avail) {
        curBucket += 1
      }
    }
    val threshold = curBucket.toLong * LINE_SIZE
    if (logger.underlying.isDebugEnabled) {
      logger.debug(s"threshold = ${threshold}")
    }
    threshold
  }

  // Defrag
  def getDefragBlock(oldBlockAddr: Option[Word]): Option[Word] = {
    oldBlockAddr.foreach(returnBlock)

    val myCursor = nextResv

    if (myCursor >= defragResvFree) {
      return None
    }
    
    nextResv += 1
    
    val blockNum = defragResv(myCursor)
    
    val blockAddr = blockIndexToBlockAddr(blockNum)
    MemUtils.zeroRegion(blockAddr, BLOCK_SIZE)
    
    return Some(blockAddr)
  }
}
