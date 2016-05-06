package uvm.refimpl.mem.simpleimmix

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.los.LargeObjectSpace

object SimpleImmixHeap {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class SimpleImmixHeap(val begin: Word, val sosSize: Word, val losSize: Word)(
  implicit microVM: MicroVM, memorySupport: MemorySupport)
    extends Heap {
  
  import SimpleImmixHeap._

  require(begin % 4096L == 0, "Heap beginning must be 4096 bytes aligned. actual beginning: %d".format(begin))
  
  val losBegin = begin + sosSize
  val losEnd = losBegin + losSize
  
  logger.debug("Small object space: %d 0x%x to %d 0x%x".format(begin, begin, losBegin, losBegin))
  logger.debug("Large object space: %d 0x%x to %d 0x%x".format(losBegin, losBegin, losEnd, losEnd))

  val space: SimpleImmixSpace = new SimpleImmixSpace(this, "SimpleImmixSpace", begin, sosSize)

  val los: LargeObjectSpace = new LargeObjectSpace(this, "Large object space", losBegin, losSize)

  val collector: SimpleImmixCollector = new SimpleImmixCollector(this, space, los)

  val collectorThread: Thread = new Thread(collector)

  collectorThread.setDaemon(true)

  collectorThread.start()

  override def makeMutator(): SimpleImmixMutator = {
    val mutator = new SimpleImmixMutator(this, space, los)
    mutator
  }

  def allocLargeObject(size: Word, align: Word, headerSize: Word): Word = los.alloc(size, align, headerSize)
}
