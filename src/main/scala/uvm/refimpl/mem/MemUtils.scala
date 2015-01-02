package uvm.refimpl.mem

import com.typesafe.scalalogging.StrictLogging
import TypeSizes._

object MemUtils extends StrictLogging {

  def zeroRegion(start: Word, length: Word) {
    val end = start + length
    logger.debug("Zeroing [0x%x -> 0x%x] %d bytes".format(start, end, length))
    var a = start
    while (a < end) {
      MemorySupport.storeLong(a, 0)
      a += WORD_SIZE_BYTES
    }
  }

  def memcpy(src: Word, dst: Word, length: Word) {
    logger.debug("Copying [0x%x -> 0x%x] %d bytes".format(src, dst, length))
    var a: Word = 0
    while (a < length) {
      val oldWord = MemorySupport.loadLong(src + a)
      MemorySupport.storeLong(dst + a, oldWord)
      a += WORD_SIZE_BYTES
    }
  }
}
