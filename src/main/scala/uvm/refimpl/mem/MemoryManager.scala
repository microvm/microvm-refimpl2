package uvm.refimpl.mem

import uvm.refimpl._
import TypeSizes._
import uvm.refimpl.mem.simpleimmix._

class MemoryManager(val heapSize: Word, val globalSize: Word, val stackSize: Word)(implicit microVM: MicroVM) {
  
  val totalMemorySize = heapSize + globalSize
  
  implicit val memorySupport = new MemorySupport(totalMemorySize)  
  
  val memoryBegin = memorySupport.muMemoryBegin
  val heapBegin = TypeSizes.alignUp(memoryBegin, SimpleImmixSpace.BLOCK_SIZE)
  
  val heap = new SimpleImmixHeap(heapBegin, heapSize)
  val globalMemory = new GlobalMemory(heapBegin + heapSize, globalSize)

  def makeMutator(): Mutator = heap.makeMutator()

  def makeStackMemory(mutator: Mutator): StackMemory = {
    val objRef = mutator.newHybrid(InternalTypes.BYTE_ARRAY, stackSize)
    val stackMemory = new StackMemory(objRef, stackSize)
    stackMemory
  }
}
