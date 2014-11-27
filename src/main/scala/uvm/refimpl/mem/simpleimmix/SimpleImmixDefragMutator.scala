package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import TypeSizes.Word
import uvm.refimpl.UvmRefImplException

object SimpleImmixDefragMutator {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class SimpleImmixDefragMutator(val heap: SimpleImmixHeap, val space: SimpleImmixSpace, val los: LargeObjectSpace)
  extends Mutator with Allocator {

  import SimpleImmixDefragMutator._

  private var curBlockAddr: Option[Word] = None

  private var cursor: Word = _

  private var limit: Word = _

  getNewBlock()

  private def getNewBlock() {
    val newAddr = space.getDefragBlock(curBlockAddr)
    newAddr match {
      case Some(addr) =>
        cursor = addr
        limit = addr + SimpleImmixSpace.BLOCK_SIZE
      case None =>
    }
  }

  override def alloc(size: Word, align: Word, headerSize: Word): Word = {
    logger.debug(s"alloc(${size}, ${align}, ${headerSize})")
    if (curBlockAddr == 0) {
      logger.debug("No more reserved blocks. Cannot defragment.")
      return 0
    }
    val actualAlign = if (align < TypeSizes.WORD_SIZE_BYTES) TypeSizes.WORD_SIZE_BYTES else align
    tryTwice {
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
          return 0
        } else {
          None
        }
      } else {
        cursor = userEnd
        Some(userStart)
      }
    }
  }

  private def tryTwice[T](body: => Option[T]): T = {
    val maybeResult = body
    maybeResult match {
      case None => body.get
      case Some(v) => v
    }
  }

  def close() {
    curBlockAddr.foreach(space.returnBlock)
  }
}
