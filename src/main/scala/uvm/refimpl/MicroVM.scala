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

object GCConf {
  val ReComment = """^\s*#.*$""".r
  val ReBlank = """^\s*$""".r
  val ReConf = """^\s*(\w+)=(\w+)\s*$""".r

  def apply(confStr: String): GCConf = {
    var sosSize = MicroVM.DEFAULT_SOS_SIZE
    var losSize = MicroVM.DEFAULT_LOS_SIZE
    var globalSize = MicroVM.DEFAULT_GLOBAL_SIZE
    var stackSize = MicroVM.DEFAULT_GLOBAL_SIZE
    confStr.lines foreach {
      case ReComment() => 
      case ReBlank() =>
      case ReConf(key, value) => {
        key match {
          case "sosSize" => sosSize = value.toLong
          case "losSize" => losSize = value.toLong
          case "globalSize" => globalSize = value.toLong
          case "stackSize" => stackSize = value.toLong
          case _ => throw new UvmRefImplException("Unrecognized option %s".format(key))
        }
      }
    }
    new GCConf(sosSize, losSize, globalSize, stackSize)
  }
}

class GCConf(
  val sosSize: Word = MicroVM.DEFAULT_SOS_SIZE,
  val losSize: Word = MicroVM.DEFAULT_LOS_SIZE,
  val globalSize: Word = MicroVM.DEFAULT_GLOBAL_SIZE,
  val stackSize: Word = MicroVM.DEFAULT_STACK_SIZE)

object MicroVM {
  val DEFAULT_SOS_SIZE: Word = 2L * 1024L * 1024L; // 2MiB
  val DEFAULT_LOS_SIZE: Word = 2L * 1024L * 1024L; // 2MiB
  val DEFAULT_GLOBAL_SIZE: Word = 1L * 1024L * 1024L; // 1MiB
  val DEFAULT_STACK_SIZE: Word = 63L * 1024L; // 60KiB per stack

  val FIRST_CLIENT_USABLE_ID: Int = 65536

  def apply(): MicroVM = {
    val gcConf = new GCConf()
    new MicroVM(gcConf)
  }
  
  def apply(confStr: String): MicroVM = {
    val gcConf = GCConf(confStr)
    new MicroVM(gcConf)
  }
}

class MicroVM(gcConf: GCConf) {
  // implicitly injected resources
  private implicit val microVM = this

  val globalBundle = new GlobalBundle()
  val constantPool = new ConstantPool()
  val memoryManager = new MemoryManager(gcConf)

  private implicit val memorySupport = memoryManager.memorySupport

  implicit val nativeCallHelper = new NativeCallHelper()

  val threadStackManager = new ThreadStackManager()

  val trapManager = new TrapManager()
  val contexts = new HashSet[MuCtx]()

  val irReader = new UIRTextReader(new IDFactory(MicroVM.FIRST_CLIENT_USABLE_ID))
  val hailScriptLoader = new HailScriptLoader()
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
    staticAnalyzer.checkBundle(bundle, Some(globalBundle))

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