package uvm.refimpl

import uvm._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.HashSet
import uvm.ir.textinput.UIRTextReader
import uvm.ir.textinput.IDFactory

object MicroVM {
  val DEFAULT_HEAP_SIZE: Word = 4L * 1024L * 1024L; // 4MiB
  val DEFAULT_GLOBAL_SIZE: Word = 1L * 1024L * 1024L; // 1MiB
  val DEFAULT_STACK_SIZE: Word = 63L * 1024L; // 60KiB per stack
}

class MicroVM(heapSize: Word = MicroVM.DEFAULT_HEAP_SIZE,
  globalSize: Word = MicroVM.DEFAULT_GLOBAL_SIZE,
  stackSize: Word = MicroVM.DEFAULT_STACK_SIZE) {

  val globalBundle = new Bundle()
  val constantPool = new ConstantPool(this)
  val memoryManager = new MemoryManager(heapSize, globalSize, stackSize, this)
  val threadStackManager = new ThreadStackManager(this)
  //val trapManager = new TrapManager(this)
  val clientAgents = new HashSet[ClientAgent]()
  
  val irReader = new UIRTextReader(new IDFactory())

  /**
   * Add things from a bundle to the Micro VM.
   */
  def addBundle(bundle: Bundle) {
    globalBundle.merge(bundle);

    for (gc <- bundle.globalCellNs.all) {
      memoryManager.globalMemory.addGlobalCell(gc)
    }
    for (g <- bundle.globalVarNs.all) {
      constantPool.addGlobalVar(g)
    }
  }
  
  def newClientAgent(): ClientAgent = new ClientAgent(this)

  def addClientAgent(ca: ClientAgent): Unit = {
    clientAgents.add(ca)
  }
  
  def removeClientAgent(ca: ClientAgent): Unit = {
    clientAgents.remove(ca)
  }
}