package uvm.refimpl

import scala.collection.mutable.HashSet

import uvm._
import uvm.utils.IDFactory
import uvm.ir.textinput.UIRTextReader
import uvm.refimpl.hail.HailScriptLoader
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.refimpl.nat.NativeCallHelper
import uvm.staticanalysis.StaticAnalyzer
import uvm.utils.IDFactory
import uvm.ir.irbuilder.IRBuilder

object MicroVM {
  val DEFAULT_SOS_SIZE: Word = 2L * 1024L * 1024L; // 2MiB
  val DEFAULT_LOS_SIZE: Word = 2L * 1024L * 1024L; // 2MiB
  val DEFAULT_GLOBAL_SIZE: Word = 1L * 1024L * 1024L; // 1MiB
  val DEFAULT_STACK_SIZE: Word = 63L * 1024L; // 60KiB per stack

  val FIRST_CLIENT_USABLE_ID: Int = 65536

  def apply(): MicroVM = {
    val vmConf = new VMConf()
    new MicroVM(vmConf)
  }
  
  def apply(confStr: String): MicroVM = {
    val vmConf = VMConf(confStr)
    new MicroVM(vmConf)
  }
}

class MicroVM(vmConf: VMConf) {
  // implicitly injected resources
  private implicit val microVM = this

  val globalBundle = new GlobalBundle()
  val constantPool = new ConstantPool()
  val memoryManager = new MemoryManager(vmConf)

  private implicit val memorySupport = memoryManager.memorySupport

  implicit val nativeCallHelper = new NativeCallHelper()

  val threadStackManager = new ThreadStackManager()

  val trapManager = new TrapManager()
  val contexts = new HashSet[MuCtx]()

  val idFactory = new IDFactory(MicroVM.FIRST_CLIENT_USABLE_ID)
  val irBuilder = new IRBuilder(globalBundle, idFactory)
  val irReader = new UIRTextReader(idFactory, recordSourceInfo=vmConf.sourceInfo)
  val hailScriptLoader = new HailScriptLoader(recordSourceInfo=vmConf.sourceInfo)
  val staticAnalyzer = new StaticAnalyzer()

  {
    // VOID, BYTE, BYTE_ARRAY: The micro VM allocates stacks on the heap in the large object space.
    // It is represented as a big chunk of byte array.
    // So the GC must know about this type because the GC looks up the globalBundle for types.

    // BYTES, BYTES_R, REFS, REFS_R: Types for the @uvm.meta.* common instructions.

    import InternalTypes._
    for (ty <- Seq(VOID, BYTE, BYTE_ARRAY, BYTES, BYTES_R, REFS, REFS_R)) {
      globalBundle.typeNs.add(ty)
    }

    // Some internal constants needed by the HAIL loader

    for (c <- Seq(NULL_REF_VOID, NULL_IREF_VOID, NULL_FUNCREF_VV, NULL_THREADREF, NULL_STACKREF)) {
      globalBundle.constantNs.add(c)
      constantPool.addGlobalVar(c)
    }
  }

  /**
   * Add things from a bundle to the Micro VM.
   */
  def addBundle(bundle: TrantientBundle) {
    if (vmConf.staticCheck) {
      staticAnalyzer.checkBundle(bundle, Some(globalBundle))
    }

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
  
  private val contextIDFactory = new IDFactory(1)

  /**
   * Create a new MuCtx. Part of the API.
   */
  def newContext(): MuCtx = newContext("user")

  /**
   * Create a new MuCtx. Extended to add a name for debugging.
   */
  def newContext(name: String): MuCtx = {
    val id = contextIDFactory.getID()
    val mutator = microVM.memoryManager.heap.makeMutator("MuCtx-%d-%s".format(id, name)) // This may trigger GC
    val ca = new MuCtx(id, mutator)
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