package uvm.refimpl.mem

import uvm.refimpl._
import TypeSizes._
import uvm.refimpl.mem.simpleimmix._
import MemoryManager._

object MemoryManager {
  val MEMORY_BEGIN = 0x100000L
}

class MemoryManager(val heapSize: Word, val globalSize: Word, val stackSize: Word, microVM: MicroVM) {

  val heap = new SimpleImmixHeap(MEMORY_BEGIN, heapSize, microVM)

  val globalMemory = new GlobalMemory(MEMORY_BEGIN + heapSize, globalSize, microVM)

  private val internalMutator = heap.makeMutator()

  def makeMutator(): Mutator = heap.makeMutator()

  def makeStackMemory(): StackMemory = {
    val objRef = internalMutator.newHybrid(InternalTypes.BYTE_ARRAY, stackSize)
    val stackMemory = new StackMemory(objRef, stackSize, microVM)
    stackMemory
  }
}
