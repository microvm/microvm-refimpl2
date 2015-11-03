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

  val globalBundle = new GlobalBundle()
  val constantPool = new ConstantPool()
  val memoryManager = new MemoryManager(heapSize, globalSize, stackSize)

  private implicit val memorySupport = memoryManager.memorySupport

  implicit val nativeCallHelper = new NativeCallHelper()

  val threadStackManager = new ThreadStackManager()
  val trapManager = new TrapManager()
  val contexts = new HashSet[MuCtx]()

  val irReader = new UIRTextReader(new IDFactory())

  {
    // The micro VM allocates stacks on the heap in the large object space. It is represented as a bug chunk of byte array.
    // So the GC must know about this type because the GC looks up the globalBundle for types.
    globalBundle.typeNs.add(InternalTypes.VOID)
    globalBundle.typeNs.add(InternalTypes.BYTE)
    globalBundle.typeNs.add(InternalTypes.BYTE_ARRAY)
    
    // Types for the @uvm.meta.* common instructions.
    globalBundle.typeNs.add(InternalTypes.BYTES)
    globalBundle.typeNs.add(InternalTypes.BYTES_R)
    globalBundle.typeNs.add(InternalTypes.REFS)
    globalBundle.typeNs.add(InternalTypes.REFS_R)
  }

  /**
   * Add things from a bundle to the Micro VM.
   */
  def addBundle(bundle: TrantientBundle) {
    globalBundle.merge(bundle);

    for (gc <- bundle.globalCellNs.all) {
      memoryManager.globalMemory.addGlobalCell(gc)
    }
    for (ef <- bundle.expFuncNs.all) {
      nativeCallHelper.exposeFuncStatic(ef)
    }
    // Must allocate the memory and expose the functions before making constants.
    for (g <- bundle.globalVarNs.all) {
      constantPool.addGlobalVar(g)
    }
  }

  /**
   * Create a new MuCtx. Part of the API.
   */
  def newContext(): MuCtx = {
    val mutator = microVM.memoryManager.heap.makeMutator() // This may trigger GC
    val ca = new MuCtx(mutator)
    contexts.add(ca)
    ca
  }
  /**
   * Given a name, get the ID of an identified entity.
   */
  def idOf(name: String): Int = try {
    globalBundle.allNs(name).id
  } catch {
    case e: NoSuchElementException => throw new UvmRefImplException("No Mu entity has name %s".format(name), e)
  }

  /**
   * Given an ID, get the name of an identified entity.
   */
  def nameOf(id: Int): String = try {
    globalBundle.allNs(id).name.get
  } catch {
    case e: NoSuchElementException => throw new UvmRefImplException("No Mu entity has ID %d".format(id), e)
  }
  
  /**
   * Set the trap handler.
   */
  def setTrapHandler(trapHandler: TrapHandler): Unit = {
    trapManager.trapHandler = trapHandler
  }

  /**
   * Execute. This is the external pusher of the execution.
   */
  def execute(): Unit = {
    threadStackManager.execute()
  }
}