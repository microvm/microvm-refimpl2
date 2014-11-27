package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import TypeSizes.Word
import scala.annotation.tailrec

object SimpleImmixMutator {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class SimpleImmixMutator(val heap: SimpleImmixHeap, val space: SimpleImmixSpace, val los: LargeObjectSpace)
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
  }

  override def alloc(size: Word, align: Word, headerSize: Word): Word = {
    logger.debug(s"alloc(${size}, ${align}, ${headerSize})")
    val actualAlign = if (align < TypeSizes.WORD_SIZE_BYTES) TypeSizes.WORD_SIZE_BYTES else align
    tryRepeatedly { // Actually try at most twice.
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
  }

  /** Try a computation repeatedly until success. */
  @tailrec
  private def tryRepeatedly[T](body: => Option[T]): T = {
    val maybeResult = body
    maybeResult match {
      case None => tryRepeatedly(body)
      case Some(v) => v
    }
  }

  def close() {
    curBlockAddr.foreach(space.returnBlock)
  }
}
