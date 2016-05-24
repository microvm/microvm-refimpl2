package uvm.refimpl.mem

import uvm.refimpl._
import TypeSizes._
import uvm.refimpl.mem.simpleimmix._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.mem.los.LargeObjectSpace

object MemoryManager {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class MemoryManager(val vmConf: VMConf)(implicit microVM: MicroVM) {
  import MemoryManager._
  
  logger.info("sosSize=%d, losSize=%d, globalSize=%d, stackSize=%d".format(
      vmConf.sosSize, vmConf.losSize, vmConf.globalSize, vmConf.stackSize))
      
  private val sosAlign = SimpleImmixSpace.BLOCK_SIZE
  private val losAlign = LargeObjectSpace.BLOCK_SIZE
  private val globalAlign = 4096L

  require(vmConf.sosSize % sosAlign == 0, "Small object space size must be a multiple of %d bytes. actual size: %d".format(
      sosAlign, vmConf.sosSize))
  require(vmConf.losSize % losAlign == 0, "Large object space size must a multiple of %d bytes. actual size: %d".format(
      losAlign, vmConf.losSize))
  require(vmConf.globalSize % globalAlign == 0, "Global space size must be a multiple of %d bytes. actual size: %d".format(
      globalAlign, vmConf.globalSize))

  val totalMemorySize = vmConf.sosSize.alignUpAndAdd(losAlign, vmConf.losSize).alignUpAndAdd(globalAlign, vmConf.globalSize)
  
  // Allocate slightly more memory to meet the SimpleImmixSpace's alignment requirement.
  implicit val memorySupport = new MemorySupport(totalMemorySize + SimpleImmixSpace.BLOCK_SIZE) 
  
  val memoryBegin = memorySupport.muMemoryBegin
  val heapBegin = TypeSizes.alignUp(memoryBegin, SimpleImmixSpace.BLOCK_SIZE)
  val globalBegin = heapBegin + vmConf.sosSize + vmConf.losSize
  val globalEnd = globalBegin + vmConf.globalSize
  
  logger.info(("Mu memory allocated.\n memoryBegin=%d 0x%x\n heapBegin=%d 0x%x\n" +
      " globalBegin=%d 0x%x\n memory end=%d 0x%x").format(
      memoryBegin, memoryBegin, heapBegin, heapBegin, globalBegin, globalBegin, globalEnd, globalEnd
      ))
  
  val heap = new SimpleImmixHeap(heapBegin, vmConf.sosSize, vmConf.losSize)
  val globalMemory = new GlobalMemory(globalBegin, vmConf.globalSize)

  def makeMutator(name: String): Mutator = heap.makeMutator(name)

  def makeStackMemory(mutator: Mutator): StackMemory = {
    val objRef = mutator.newHybrid(InternalTypes.BYTE_ARRAY, vmConf.stackSize)
    val stackMemory = new StackMemory(objRef, vmConf.stackSize)
    stackMemory
  }
}
