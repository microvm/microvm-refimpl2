package uvm.refimpl.mem.simpleimmix

import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.los.LargeObjectSpace
import TypeSizes._

class SimpleImmixHeap(val begin: Word, val size: Word, val microVM: MicroVM) extends Heap {

  val space: SimpleImmixSpace = new SimpleImmixSpace(this, "SimpleImmixSpace", begin, size / 2)

  val collector: SimpleImmixCollector = new SimpleImmixCollector(this, space, los, microVM)

  val collectorThread: Thread = new Thread(collector)

  val los: LargeObjectSpace = new LargeObjectSpace(this, "Large object space", mid, size / 2)

  val mid = begin + size / 2

  collectorThread.setDaemon(true)

  collectorThread.start()

  override def makeMutator(): SimpleImmixMutator = {
    val mutator = new SimpleImmixMutator(this, space, los)
    mutator
  }

  def allocLargeObject(size: Word, align: Word, headerSize: Word): Word = los.alloc(size, align, headerSize)
}
