package uvm.refimpl.mem

import com.typesafe.scalalogging.StrictLogging
import TypeSizes._

object MemUtils extends StrictLogging {

  def zeroRegion(start: Word, length: Word) {
    val end = start + length
    logger.debug(s"Zeroing [${start} -> ${end}] ${length} bytes")
    var a = start
    while (a < end) {
      MemorySupport.storeLong(a, 0)
      a += WORD_SIZE_BYTES
    }
  }

  def memcpy(src: Long, dst: Word, length: Word) {
    logger.debug("Copying [${src} -> ${dst}] ${length} bytes")
    var a: Word = 0
    while (a < length) {
      val oldWord = MemorySupport.loadLong(src + a)
      MemorySupport.storeLong(dst + a, oldWord)
      a += WORD_SIZE_BYTES
    }
  }
}
