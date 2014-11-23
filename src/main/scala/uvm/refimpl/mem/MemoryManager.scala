package uvm.refimpl.mem

import uvm.refimpl.MicroVM
//import uvm.refimpl.mem.MicroVMInternalTypes
//import uvm.refimpl.mem.simpleimmix.SimpleImmixHeap
import MemoryManager._

object MemoryManager {
  val MEMORY_BEGIN = 0x100000L
}

class MemoryManager(val heapSize: Long, 
    val globalSize: Long, 
    val stackSize: Long, 
    val microVM: MicroVM) {

//  val heap = new SimpleImmixHeap(MEMORY_BEGIN, heapSize, microVM)

  val globalMemory = new GlobalMemory(MEMORY_BEGIN + heapSize, globalSize, microVM)

//  private val stacks = new ArrayList[StackMemory]()

//  private val internalMutator = heap.makeMutator()

//  def makeMutator(): Mutator = heap.makeMutator()

//  def makeStackMemory(): StackMemory = {
//    val objRef = internalMutator.newHybrid(MicroVMInternalTypes.BYTE_ARRAY_TYPE, stackSize)
//    val stackMemory = new StackMemory(objRef, stackSize, microVM)
//    stackMemory
//  }
}
