package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import TypeSizes.Word
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.UvmRefImplException

object SimpleImmixSpace {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val BLOCK_SIZE = 32768L
  
  val SOS_THRESHOLD = BLOCK_SIZE / 4L

  val BLOCK_MARKED = 0x1
  val BLOCK_RESERVED = 0x2
  val BLOCK_IN_MUTATOR = 0x4
  val BLOCK_PINNED = 0x8
  
  def prettyPrintFlags(flags: Int): String = {
    def optStr(flag: Int, str: String): String = if ((flags & flag) != 0) str else ""

    "0x%x (%s%s%s%s)".format(flags,
        optStr(BLOCK_MARKED, "M"),
        optStr(BLOCK_PINNED, "P"),
        optStr(BLOCK_RESERVED, "R"),
        optStr(BLOCK_IN_MUTATOR, "I"))
  }

  private val LINE_SIZE = 128L

  private val N_BUCKETS = 256
}

class SimpleImmixSpace(val heap: SimpleImmixHeap, name: String, begin: Word, extend: Word)(
  implicit memorySupport: MemorySupport)
    extends Space(name, begin, extend) {

  import SimpleImmixSpace._

  if (begin % BLOCK_SIZE != 0) {
    throw new UvmRefImplException("space should be aligned to BLOCK_SIZE " + BLOCK_SIZE)
  }

  if (extend % BLOCK_SIZE != 0) {
    throw new UvmRefImplException("space size should be a multiple of BLOCK_SIZE " + BLOCK_SIZE)
  }

  val nBlocks: Int = (extend / BLOCK_SIZE).toInt
  
  logger.debug {
    val sb = new StringBuilder("Blocks:\n")
    
    for (i <- 0L until nBlocks) {
      val blockBegin = begin + i*BLOCK_SIZE
      val blockEnd = begin + (i+1L)*BLOCK_SIZE
      sb ++= "  block[%d]: from %d 0x%x to %d 0x%x\n".format(i, blockBegin, blockBegin, blockEnd, blockEnd)
    }
    
    sb.toString
  }

  /** The number of reserved blocks (for defrag). */
  private val nReserved: Int = Math.max(nBlocks / 20, 1) // reserve at least one block

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

  /** A list of free blocks reserved for defrag. */
  private val defragResv: Array[Int] = new Array[Int](nReserved)

  /** The number of free blocks (valid entries) in defragResv. */
  private var defragResvFree: Int = nReserved

  /** The index of the next free reserved block to allocate in defragResv. */
  private var nextResv: Int = 0

  /** A list of buckets, for statistics. Used by defrag. */
  private val buckets: Array[Word] = new Array[Word](N_BUCKETS)
  
  /** Which mutator is using which block? */
  private val blockUser: Array[Option[Mutator]] = Array.fill(nBlocks)(None)

  for (i <- 0 until nReserved) { // Block 0 to nReserved-1 are reserved
    defragResv(i) = i
    blockFlags(i) |= BLOCK_RESERVED // Set the reserved flat
  }

  for (i <- nReserved until nBlocks) { // The rest of the blocks are free to allocate
    freeList(i - nReserved) = i
  }

  /**
   * Try to get a free block. If not available, return None. The old block, if present, is returned.
   */
  def tryGetBlock(oldBlockAddr: Option[Word], who: Mutator): Option[Word] = {
    oldBlockAddr.foreach(a => returnBlock(a, who))

    val myCursor = nextFree
    if (myCursor >= freeListValidCount) {
      return None
    }

    nextFree += 1

    val blockNum = freeList(myCursor)
    mutatorGetBlock(blockNum, who)

    logger.debug("Normal mutator %s got block %d".format(who.name, blockNum))

    val blockAddr = blockIndexToBlockAddr(blockNum)
    MemUtils.zeroRegion(blockAddr, BLOCK_SIZE)

    Some(blockAddr)
  }
  
  private def mutatorGetBlock(blockNum: Int, who: Mutator): Unit = {
    blockFlags(blockNum) |= BLOCK_IN_MUTATOR
    blockUser(blockNum) = Some(who)
  }

  private def mutatorReleaseBlock(blockNum: Int, who: Mutator): Unit = {
    val oldUser = blockUser(blockNum)
    if (oldUser != Some(who)) {
      throw new UvmRefImplException("Mutator %s returned a block %d which was reserved by %s".format(
        who.name, blockNum, oldUser.map(_.name).getOrElse("(nobody)")))
    }
    blockUser(blockNum) = None
    val flags = blockFlags(blockNum)
    assert((flags & BLOCK_IN_MUTATOR) != 0, "Block %d (to be returned) should have flags 0x%x. Actual flags: %s".format(
        blockNum, BLOCK_IN_MUTATOR, prettyPrintFlags(flags)))
    blockFlags(blockNum) &= ~BLOCK_IN_MUTATOR
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

  def markBlockByIndex(index: Int, pin: Boolean = false) {
    val addMark = if (pin) BLOCK_MARKED | BLOCK_PINNED else BLOCK_MARKED
    blockFlags(index) |= addMark
  }

  def markBlockByObjRef(objRef: Word, pin: Boolean = false) {
    val blockIndex = objRefToBlockIndex(objRef)
    markBlockByIndex(blockIndex, pin)
    logger.debug(s"Marked block ${blockIndex}. pin=${pin}")
  }
  
  def isPinned(pageNum: Int): Boolean = (blockFlags(pageNum) & BLOCK_PINNED) != 0

  def collectBlocks(): Boolean = {
    logger.debug("Before collecting blocks from SOS:")
    if (logger.underlying.isDebugEnabled()) {
      debugLogBlockStates()
    }
    
    for (i <- 0 until nBlocks) {
      val flags = blockFlags(i)
      if ((flags & BLOCK_RESERVED) != 0) {
        assert(flags == BLOCK_RESERVED, "Reserved block %d should have flags 0x%x. Actual flags: %s".format(
            i, BLOCK_RESERVED, prettyPrintFlags(flags)))
      }
    }

    // Shift defrag reserved blocks to the beginning;
    for (i <- nextResv until defragResvFree) {
      defragResv(i - nextResv) = defragResv(i)
    }
    
    var newDefragResvFree = defragResvFree - nextResv
    for (i <- 0 until newDefragResvFree) {
      val ind = defragResv(i)
      val flags = blockFlags(ind)
      assert(flags == BLOCK_RESERVED, "Block %d (from defrag freelist) should have flags 0x%x. Actual flags: %s".format(
          ind, BLOCK_RESERVED, prettyPrintFlags(flags)))
    }

    var newNFree = 0
    for (i <- 0 until nBlocks) {
      var flag = blockFlags(i)
      val bits = (flag & (BLOCK_MARKED | BLOCK_IN_MUTATOR | BLOCK_RESERVED | BLOCK_PINNED))
      if (bits == 0) {
        if (newDefragResvFree < nReserved) {
          defragResv(newDefragResvFree) = i
          newDefragResvFree += 1
          flag |= BLOCK_RESERVED
          logger.debug(s"Block ${i} added to defrag freelist")
        } else {
          freeList(newNFree) = i
          newNFree += 1
          logger.debug(s"Block ${i} added to normal freelist")
        }
      } else if ((bits & BLOCK_RESERVED) != 0) {
        logger.debug(s"Block ${i} is already reserved.")
      } else {
        logger.debug(s"Block ${i} is not freed because flag bits is ${bits}")
      }
      flag &= ~(BLOCK_MARKED | BLOCK_PINNED)  // The only place that unmarks the block
      blockFlags(i) = flag
    }
    defragResvFree = newDefragResvFree
    freeListValidCount = newNFree
    nextResv = 0
    nextFree = 0
    if (logger.underlying.isDebugEnabled()) {
      logger.debug("After collecting blocks from SOS:")
      debugLogBlockStates()
    }
    return newNFree > 0
  }

  def returnBlock(blockAddr: Word, who: Mutator) {
    val blockNum = blockAddrToBlockIndex(blockAddr)
    mutatorReleaseBlock(blockNum, who)
    logger.debug("Block %d returned to space by %s.".format(blockNum, who.name))
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
  def getDefragBlock(oldBlockAddr: Option[Word], who: SimpleImmixDefragMutator): Option[Word] = {
    oldBlockAddr.foreach(a => returnBlock(a, who))

    val myCursor = nextResv

    if (myCursor >= defragResvFree) {
      return None
    }

    nextResv += 1

    val blockNum = defragResv(myCursor)
    val flags = blockFlags(blockNum)
    assert(flags == BLOCK_RESERVED, "Defrag block %d (to be used) should have flags 0x%x. Actual flags: %s".format(
            blockNum, BLOCK_RESERVED, prettyPrintFlags(flags)))
    blockFlags(blockNum) &= ~BLOCK_RESERVED
    
    mutatorGetBlock(blockNum, who)

    logger.debug("Defrag mutator %s got block %d".format(who.name, blockNum))

    val blockAddr = blockIndexToBlockAddr(blockNum)
    MemUtils.zeroRegion(blockAddr, BLOCK_SIZE)

    return Some(blockAddr)
  }
  
  def returnDefragBlock(oldBlockAddr: Word, who: SimpleImmixDefragMutator): Unit = {
    returnBlock(oldBlockAddr, who)
  }

  // Debugging
  def debugLogBlockStates() {
    val sb1 = new StringBuilder("Reserved freelist:")
    for (i <- nextResv until defragResvFree) {
      sb1.append(" ").append(defragResv(i))
    }
    logger.debug(sb1.toString)
    val sb2 = new StringBuilder("Freelist:")
    for (i <- nextFree until freeListValidCount) {
      sb2.append(" ").append(freeList(i))
    }
    logger.debug(sb2.toString)
    for (i <- 0 until nBlocks) {
      val prettyFlags = prettyPrintFlags(blockFlags(i))
      val user = blockUser(i).map(m => "user: %s".format(m.name)).getOrElse("")
      logger.debug("block %d: flag=%s %s".format(i, prettyFlags, user))
    }
  }
}
