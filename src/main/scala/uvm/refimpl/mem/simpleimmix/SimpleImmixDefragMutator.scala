package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import TypeSizes.Word
import uvm.refimpl.UvmRefImplException
import uvm.utils.RetryUtils._
import uvm.utils.IDFactory

object SimpleImmixDefragMutator {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val defragMutatorIDFactory = new IDFactory(1)
}

class SimpleImmixDefragMutator(val heap: SimpleImmixHeap, val space: SimpleImmixSpace)(
  implicit memorySupport: MemorySupport)
    extends Mutator("defrag-" + SimpleImmixDefragMutator.defragMutatorIDFactory.getID()) with Allocator {

  import SimpleImmixDefragMutator._

  private var curBlockAddr: Option[Word] = None

  private var cursor: Word = _

  private var limit: Word = _

  getNewBlock()

  private def getNewBlock() {
    curBlockAddr = space.getDefragBlock(curBlockAddr, this)
    curBlockAddr match {
      case Some(addr) =>
        cursor = addr
        limit = addr + SimpleImmixSpace.BLOCK_SIZE
        logger.debug("Got block. cursor=0x%x limit=0x%x".format(cursor, limit))
      case None =>
    }
  }

  override def alloc(size: Word, align: Word, headerSize: Word): Word = {
    logger.debug(s"alloc(${size}, ${align}, ${headerSize})")
    if (curBlockAddr == None) {
      logger.debug("No more reserved blocks. Cannot defragment.")
      throw new NoMoreDefragBlockException("No more blocks for defrag.")
    }
    val actualAlign = if (align < TypeSizes.WORD_SIZE_BYTES) TypeSizes.WORD_SIZE_BYTES else align
    val result = tryTwice {
      val gcStart = TypeSizes.alignUp(cursor, align)
      val userStart = TypeSizes.alignUp(gcStart + headerSize, align)
      val userEnd = userStart + size
      if (userEnd >= limit) {
        if (userEnd - gcStart > SimpleImmixSpace.BLOCK_SIZE) {
          throw new UvmRefImplException("Defrag mutators should not be used to allocate large objects.")
        }
        logger.debug("Getting new reserved block...")
        getNewBlock
        logger.debug("got new reserved block.")
        if (curBlockAddr == None) {
          logger.debug("No more reserved blocks. Cannot defragment.")
          throw new NoMoreDefragBlockException("No more blocks for defrag.")
        } else {
          None
        }
      } else {
        cursor = userEnd
        Some(userStart)
      }
    }
    logger.debug("alloc(%d, %d, %d) = %d 0x%x".format(size, align, headerSize, result, result))
    result
  }

  def close() {
    logger.debug("Closing defrag mutator...")
    curBlockAddr.foreach(a => space.returnDefragBlock(a, this))
  }
}
