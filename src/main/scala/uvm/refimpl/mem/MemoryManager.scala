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

class MemoryManager(val gcConf: GCConf)(implicit microVM: MicroVM) {
  import MemoryManager._
  
  logger.info("sosSize=%d, losSize=%d, globalSize=%d, stackSize=%d".format(
      gcConf.sosSize, gcConf.losSize, gcConf.globalSize, gcConf.stackSize))

  require(gcConf.sosSize % SimpleImmixSpace.BLOCK_SIZE == 0, "Small object space size must be a multiple of %d bytes. actual size: %d".format(
      SimpleImmixSpace.BLOCK_SIZE, gcConf.sosSize))
  require(gcConf.losSize % LargeObjectSpace.BLOCK_SIZE == 0, "Large object space size must a multiple of %d bytes. actual size: %d".format(
      LargeObjectSpace.BLOCK_SIZE, gcConf.losSize))
  require(gcConf.globalSize % 4096L == 0, "Global space size must be 4096 bytes aligned. actual size: %d".format(gcConf.globalSize))

  val totalMemorySize = gcConf.sosSize + gcConf.losSize + gcConf.globalSize
  
  // Allocate slightly more memory to meet the SimpleImmixSpace's alignment requirement.
  implicit val memorySupport = new MemorySupport(totalMemorySize + SimpleImmixSpace.BLOCK_SIZE) 
  
  val memoryBegin = memorySupport.muMemoryBegin
  val heapBegin = TypeSizes.alignUp(memoryBegin, SimpleImmixSpace.BLOCK_SIZE)
  val globalBegin = heapBegin + gcConf.sosSize + gcConf.losSize
  val globalEnd = globalBegin + gcConf.globalSize
  
  logger.info(("Mu memory allocated.\n memoryBegin=%d 0x%x\n heapBegin=%d 0x%x\n" +
      " globalBegin=%d 0x%x\n memory end=%d 0x%x").format(
      memoryBegin, memoryBegin, heapBegin, heapBegin, globalBegin, globalBegin, globalEnd, globalEnd
      ))
  
  val heap = new SimpleImmixHeap(heapBegin, gcConf.sosSize, gcConf.losSize)
  val globalMemory = new GlobalMemory(globalBegin, gcConf.globalSize)

  def makeMutator(): Mutator = heap.makeMutator()

  def makeStackMemory(mutator: Mutator): StackMemory = {
    val objRef = mutator.newHybrid(InternalTypes.BYTE_ARRAY, gcConf.stackSize)
    val stackMemory = new StackMemory(objRef, gcConf.stackSize)
    stackMemory
  }
}
