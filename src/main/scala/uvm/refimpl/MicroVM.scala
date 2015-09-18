package uvm.refimpl

import uvm._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.HashSet
import uvm.ir.textinput.UIRTextReader
import uvm.ir.textinput.IDFactory
import uvm.refimpl.nat.NativeCallHelper

object MicroVM {
  val DEFAULT_HEAP_SIZE: Word = 4L * 1024L * 1024L; // 4MiB
  val DEFAULT_GLOBAL_SIZE: Word = 1L * 1024L * 1024L; // 1MiB
  val DEFAULT_STACK_SIZE: Word = 63L * 1024L; // 60KiB per stack
}

class MicroVM(heapSize: Word = MicroVM.DEFAULT_HEAP_SIZE,
              globalSize: Word = MicroVM.DEFAULT_GLOBAL_SIZE,
              stackSize: Word = MicroVM.DEFAULT_STACK_SIZE) {

  // implicitly injected resources
  private implicit val microVM = this

  val globalBundle = new Bundle()
  val constantPool = new ConstantPool()
  val memoryManager = new MemoryManager(heapSize, globalSize, stackSize)

  private implicit val memorySupport = memoryManager.memorySupport
  
  val nativeCallHelper = new NativeCallHelper()

  val threadStackManager = new ThreadStackManager()
  val trapManager = new TrapManager()
  val clientAgents = new HashSet[ClientAgent]()

  val irReader = new UIRTextReader(new IDFactory())

  {
    // The micro VM allocates stacks on the heap in the large object space. It is represented as a bug chunk of byte array.
    // So the GC must know about this type because the GC looks up the globalBundle for types.
    globalBundle.allNs.add(InternalTypes.VOID)
    globalBundle.typeNs.add(InternalTypes.VOID)
    globalBundle.allNs.add(InternalTypes.BYTE)
    globalBundle.typeNs.add(InternalTypes.BYTE)
    globalBundle.allNs.add(InternalTypes.BYTE_ARRAY)
    globalBundle.typeNs.add(InternalTypes.BYTE_ARRAY)
  }

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

  /**
   * Create a new ClientAgent.
   */
  def newClientAgent(): ClientAgent = {
    val mutator = microVM.memoryManager.heap.makeMutator() // This may trigger GC
    val ca = new ClientAgent(mutator)
    clientAgents.add(ca)
    ca
  }
  /**
   * Given a name, get the ID of an identified entity.
   */
  def idOf(name: String): Int = globalBundle.allNs(name).id

  /**
   * Given an ID, get the name of an identified entity.
   */
  def nameOf(id: Int): String = globalBundle.allNs(id).name.get
  
  /**
   * Execute. This is the external pusher of the execution.
   */
  def execute(): Unit = {
    threadStackManager.execute()
  }
}