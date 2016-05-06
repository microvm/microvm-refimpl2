package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import TypeSizes.Word
import scala.annotation.tailrec
import uvm.utils.RetryUtils._

object SimpleImmixMutator {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class SimpleImmixMutator(val heap: SimpleImmixHeap, val space: SimpleImmixSpace, val los: LargeObjectSpace)(
  implicit memorySupport: MemorySupport)
    extends Mutator with Allocator {

  import SimpleImmixMutator._

  private var curBlockAddr: Option[Word] = None

  private var cursor: Word = _

  private var limit: Word = _

  getNewBlock()

  private def getNewBlock(): Unit = {
    val newAddr = tryRepeatedly {
      space.tryGetBlock(curBlockAddr).orElse {
        heap.mutatorTriggerAndWaitForGCEnd(true)
        None
      }
    }
    curBlockAddr = Some(newAddr)
    cursor = newAddr
    limit = newAddr + SimpleImmixSpace.BLOCK_SIZE
    logger.debug("Got block. cursor=0x%x limit=0x%x".format(cursor, limit))
  }

  override def alloc(size: Word, align: Word, headerSize: Word): Word = {
    logger.debug(s"alloc(${size}, ${align}, ${headerSize})")
    val actualAlign = if (align < TypeSizes.WORD_SIZE_BYTES) TypeSizes.WORD_SIZE_BYTES else align
    val result = tryRepeatedly { // Actually try at most twice.
      //If the first time failed, the second time cannot continue until a block is obtained. 
      val gcStart = TypeSizes.alignUp(cursor, align)
      val userStart = TypeSizes.alignUp(gcStart + headerSize, align)
      val userEnd = userStart + size
      if (userEnd >= limit) {
        if (userEnd - gcStart > SimpleImmixSpace.BLOCK_SIZE) {
          Some(los.alloc(size, align, headerSize))
        } else {
          logger.debug("Getting new block...")
          getNewBlock
          logger.debug("got new block.")
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
    logger.debug("Closing mutator...")
    curBlockAddr.foreach(space.returnBlock)
  }
}
