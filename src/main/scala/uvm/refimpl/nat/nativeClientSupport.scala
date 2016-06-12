package uvm.refimpl.nat

import java.nio.charset.StandardCharsets
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.{ universe => ru }
import org.slf4j.LoggerFactory
import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import com.typesafe.scalalogging.Logger
import NativeSupport._
import PlatformConstants._
import jnr.ffi.ObjectReferenceManager
import jnr.ffi.Pointer
import uvm.refimpl._
import uvm.ssavariables._
import com.kenai.jffi.Invoker
import com.kenai.jffi.CallContext
import com.kenai.jffi.HeapInvocationBuffer
import NativeClientSupport._

import uvm.ir.irbuilder.DestKind

object NativeTrapHandler {
  val jffiInvoker = Invoker.getInstance
  val trapHandlerCallContext = new CallContext(
    JType.VOID, // return value
    // input params
    JType.POINTER, //MuCtx *ctx, 
    JType.POINTER, //MuThreadRefValue thread,
    JType.POINTER, //MuStackRefValue stack,
    JType.SINT32, //int wpid,
    // output params
    JType.POINTER, //MuTrapHandlerResult *result,
    JType.POINTER, //MuStackRefValue *new_stack,
    JType.POINTER, //MuValue **values,
    JType.POINTER, //int *nvalues,
    JType.POINTER, //MuValuesFreer *freer, 
    JType.POINTER, //MuCPtr *freerdata, 
    JType.POINTER, //MuRefValue *exception,
    // user data
    JType.POINTER //MuCPtr userdata
    );

  val freerCallContext = new CallContext(
    JType.VOID, // return value
    // input params
    JType.POINTER, // MuValue *values,
    JType.POINTER //MuCPtr freerdata
    )

  def callFreer(freerFP: MuValuesFreerFP, valuesPtr: MuValueFakArrayPtr, freerData: MuCPtr): Unit = {
    val freerFunc = new com.kenai.jffi.Function(freerFP, freerCallContext)
    logger.debug("Calling freer: 0x%x with args (0x%x, 0x%x)".format(freerFP, valuesPtr, freerData))
    jffiInvoker.invokeLLrL(freerFunc, valuesPtr, freerData)
    logger.debug("Returned from freer")
  }

}

/**
 * This object calls a C function to handle the trap.
 */
class NativeTrapHandler(val funcAddr: MuTrapHandlerFP, val userdata: MuCPtr) extends TrapHandler {
  import NativeTrapHandler._

  val jffiFunction = new com.kenai.jffi.Function(funcAddr, trapHandlerCallContext)

  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
    logger.debug("Trapped. Prepare to call native trap handler: %d 0x%x".format(funcAddr, funcAddr))

    val hib = new HeapInvocationBuffer(jffiFunction)

    // input args
    val ctxFak = exposeMuCtx(ctx) // It is a fresh context.
    hib.putAddress(ctxFak)

    val threadFak = exposeMuValue(ctx, thread) // It is a fresh handle, too.
    hib.putAddress(threadFak)

    val stackFak = exposeMuValue(ctx, stack) // It is a fresh handle, too.
    hib.putAddress(stackFak)

    hib.putInt(watchPointID)

    // output args
    val nOutArgs = 7
    val outBuf = jnrMemoryManager.allocateDirect((7L * WORD_SIZE_BYTES).toInt, true) // output values are received here.
    val outBufAddr = outBuf.address()
    for (i <- 0 until nOutArgs) {
      val outAddr = outBufAddr + i * WORD_SIZE_BYTES
      logger.debug("The %d-th out arg: 0x%x".format(i, outAddr))
      hib.putAddress(outAddr)
    }

    // user data
    hib.putAddress(userdata)

    // Call
    logger.debug("Calling native trap handler: 0x%x".format(funcAddr))
    jffiInvoker.invokeLong(jffiFunction, hib)
    logger.debug("Returned from native trap handler: 0x%x".format(funcAddr))

    // Inspect return value
    val result = outBuf.getInt(0L)

    val scalaResult: TrapHandlerResult = result match {
      case CDefs.MU_THREAD_EXIT => TrapHandlerResult.ThreadExit()
      case CDefs.MU_REBIND_PASS_VALUES => {
        val nsFak = outBuf.getAddress(1 * WORD_SIZE_BYTES)
        val newStack = getMuValueNotNull(nsFak).asInstanceOf[MuStackRefValue]
        val valuesPtr = outBuf.getAddress(2 * WORD_SIZE_BYTES)
        val nValues = outBuf.getInt(3 * WORD_SIZE_BYTES)
        val valueFaks = for (i <- 0 until nValues) yield {
          theMemory.getAddress(valuesPtr + i * WORD_SIZE_BYTES)
        }

        val freerFP = outBuf.getAddress(4 * WORD_SIZE_BYTES)
        if (freerFP != 0L) {
          val freerData = outBuf.getAddress(5 * WORD_SIZE_BYTES)
          callFreer(freerFP, valuesPtr, freerData)
        }

        val values = valueFaks.map(getMuValueNotNull)

        TrapHandlerResult.Rebind(newStack, HowToResume.PassValues(values))
      }
      case CDefs.MU_REBIND_THROW_EXC => {
        val nsFak = outBuf.getAddress(1 * WORD_SIZE_BYTES)
        val newStack = getMuValueNotNull(nsFak).asInstanceOf[MuStackRefValue]
        val excFak = outBuf.getAddress(6 * WORD_SIZE_BYTES)
        val excVal = getMuValueNotNull(excFak).asInstanceOf[MuRefValue]

        TrapHandlerResult.Rebind(newStack, HowToResume.ThrowExc(excVal))
      }
    }

    unexposeMuCtx(ctxFak) // Mu will close the context, but not un-expose it.

    scalaResult
  }
}

/**
 * Proprietary interface. Set an error number (not the global errno!) when an
 * exception is thrown in Mu before returning
 */
object MuErrorNumber {
  val MU_NATIVE_ERRNO = 6481626 // muErrno is set to this number if an exception is thrown

  val muErrorPtr = jnrMemoryManager.allocateDirect(16)

  def getMuError(): Int = muErrorPtr.getInt(0)
  def setMuError(errno: Int): Unit = muErrorPtr.putInt(0, errno)
}

/**
 * Expose a class (MicroVM and MuCtx) as a function table.
 *
 * @param funcPtrs A list of function pointers.
 */
class ClientAccessibleClassExposer(funcPtrs: Seq[Word]) {
  /**
   * Allocate and populate a native function table,
   * i.e. the in-memory structure of struct MuVM and struct MuCtx in the client interface.
   *
   * @param objAddr The "address" of the object, created by the ObjectReferenceManager.
   * This will be the first field of the table.
   *
   * @return The address of the function table.
   */
  def makeFuncTable(objAddr: Word): FuncTablePtr = {
    val nMethods = funcPtrs.size
    val structSizeWords = nMethods + 1
    val structSizeBytes = structSizeWords * WORD_SIZE_BYTES.toInt
    val s = jnrMemoryManager.allocateDirect(structSizeBytes)
    s.putLong(0L, objAddr)
    for ((ptr, i) <- funcPtrs.zipWithIndex) {
      val offset = (i + 1) * WORD_SIZE_BYTES
      s.putLong(offset, ptr)
    }
    val funcTableAddr = s.address()
    NativeClientSupport.funcTableToPointer(funcTableAddr) = s
    funcTableAddr
  }
}

/**
 * Support for native clients (such as C programs).
 * <p>
 * MicroVM, MuCtx and MuValue instances can be exposed to native programs.
 * <p>
 * An exposed MicroVM has a function table (the MuVM struct in C) and a fake pointer (managed by the
 * ObjectReferenceManager) referring to the Scala MicroVM instance. The former is kept in the funcTableToPtr map,
 * and the latter is kept in the microVMs ORM. The fake pointer is stored in the header field of the MuVM struct.
 * <p>
 * An exposed MuCtx has a function table, a fake pointer, and a hash set containing all exposed MuValues derived from
 * that MuCtx, held in muCtxToMuValue(theCtx). Unexposing a MuCtx automatically unexposes all of its MuValues. The fake
 * pointer is the header of the MuCtx struct.
 * <p>
 * An exposed MuValue only has a fake pointer, held in muValues. The pointer is also stored in muCtxToMuValues(ctx) for
 * its context ctx.
 */
object NativeClientSupport {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  // Syntax sugar for C-level pointer types.
  // "Fak" means "fake pointer", i.e. ObjectReferenceManager-generated "pointer".
  type FuncTablePtr = Word
  type MuVMFak = FuncTablePtr
  type MuCtxFak = FuncTablePtr
  type MuValueFak = Word
  type MuTrapHandlerFP = Word
  type IntPtr = Word
  type LongPtr = Word
  type CharArrayPtr = Word
  type MuValueFakPtr = Word
  type MuValueFakArrayPtr = Word
  type MuValuesFreerFP = Word
  type MuFlag = Int
  type MuFlagPtr = Word
  type MuFlagArrayPtr = Word

  // Mu API C-level types.
  type MuID = Int
  type MuName = String
  type MuCPtr = Word
  type MuCFP = Word
  type MuMemOrd = Int
  type MuAtomicRMWOptr = Int
  type MuHowToResume = Int
  type MuHowToResumePtr = IntPtr
  type MuCallConv = Int
  type MuBinOptr = Int
  type MuCmpOptr = Int
  type MuConvOptr = Int
  type MuDestKind = Int
  type MuWPID = Int
  type MuCommInst = Int

  // Give exposed objects a random "memory address" so native programs can pass them back to Mu as parameters.
  val microVMs = jnrRuntime.newObjectReferenceManager[MicroVM]()
  val muCtxs = jnrRuntime.newObjectReferenceManager[MuCtx]()
  val muValues = jnrRuntime.newObjectReferenceManager[MuValue]()

  /** Map each MuCtx to all of its current MuValues. This is needed when closing a MuCtx. */
  val muCtxToMuValueFaks = HashMap[MuCtx, HashSet[MuValueFak]]()

  def getMicroVM(funcTableAddr: FuncTablePtr): MicroVM = getObjFromFuncTableAddrNotNull(funcTableAddr, microVMs)
  def getMuCtx(funcTableAddr: FuncTablePtr): MuCtx = getObjFromFuncTableAddrNotNull(funcTableAddr, muCtxs)

  /** Given a function table pointer, get an object from a ObjectReferenceManager. Assert it is not null. */
  def getObjFromFuncTableAddrNotNull[T](funcTableAddr: FuncTablePtr, orm: ObjectReferenceManager[T]): T = {
    val objFak = theMemory.getAddress(funcTableAddr)
    val obj = orm.get(jnrMemoryManager.newPointer(objFak))
    if (obj == null) {
      throw new UvmRefImplException("Exposed object not found. Fake address: %d 0x%x".format(objFak, objFak))
    }
    obj
  }

  /** Get the MuValue instance form a C MuValue (a pointer). */
  def getMuValueNotNull(fak: MuValueFak): MuValue = {
    val muVal = muValues.get(jnrMemoryManager.newPointer(fak))
    if (muVal == null) {
      throw new UvmRefImplException("Exposed MuValue not found. Fake address: %d 0x%x".format(fak, fak))
    }
    muVal
  }

  /** Get the MuValue instance form a C MuValue (a pointer). */
  def getMuValueNullable(fak: MuValueFak): Option[MuValue] = {
    val muVal = muValues.get(jnrMemoryManager.newPointer(fak))
    if (muVal == null) {
      None
    } else {
      Some(muVal)
    }
  }

  /**
   * Map function table addresses to Pointer so they can be closed. JFFI uses the TrantientNativeMemory (wrapped
   * in the Pointer object) to keep the allocated native memory alive.
   */
  val funcTableToPointer = HashMap[FuncTablePtr, Pointer]()

  /**
   * Map Java strings (currently only names of Identified objects in MU) to Pointers to keep the allocated strings
   * alive so that the native program can access them.
   */
  val stringPool = HashMap[String, Pointer]()

  // These "exposer" can repeatedly generate function tables.
  logger.debug("Creating Mu VM Exposer...")
  val muVMExposer = new ClientAccessibleClassExposer(CDefs.stubsOfMuVM)
  logger.debug("Creating Mu Ctx Exposer...")
  val muCtxExposer = new ClientAccessibleClassExposer(CDefs.stubsOfMuCtx)

  // Expose and unexpose objects

  def exposeString(str: String): Word = {
    val ptr = NativeClientSupport.stringPool.getOrElseUpdate(str, {
      val bytes = str.getBytes(StandardCharsets.US_ASCII)
      val newPtr = jnrMemoryManager.allocateDirect(bytes.size + 1)
      newPtr.put(0L, bytes, 0, bytes.length)
      newPtr.putByte(bytes.length, 0)
      newPtr
    })
    ptr.address()
  }

  /** Expose a MicroVM. Return a pointer to the C MuVM structure. */
  def exposeMicroVM(microVM: MicroVM): FuncTablePtr = {
    val objFak = microVMs.add(microVM).address()
    val funcTableAddr = muVMExposer.makeFuncTable(objFak)
    funcTableAddr
  }

  /** Expose a MuCtx. Return a pointer to the C MuCtx structure. */
  def exposeMuCtx(muCtx: MuCtx): FuncTablePtr = {
    require(!muCtxToMuValueFaks.contains(muCtx), "Context %s is already exposed.".format(muCtx))
    muCtxToMuValueFaks(muCtx) = HashSet()
    val objFak = muCtxs.add(muCtx).address()
    val funcTableAddr = muCtxExposer.makeFuncTable(objFak)
    funcTableAddr
  }

  /** Expose a MuValue. Return an "object pointer", i.e. faked by ObjectReferenceManager. */
  def exposeMuValue(muCtx: MuCtx, muValue: MuValue): MuValueFak = {
    val fak = muValues.add(muValue).address()
    muCtxToMuValueFaks(muCtx).add(fak)
    fak
  }

  /** Unexpose a MicroVM. Remove the faked pointer and deallocate the function table (by removing the Pointer). */
  def unexposeMicroVM(funcTableAddr: FuncTablePtr): Unit = {
    require(funcTableToPointer.contains(funcTableAddr),
      "MuVM struct pointer %d 0x%x does not exist".format(funcTableAddr, funcTableAddr))
    val objPtr = theMemory.getPointer(funcTableAddr)
    microVMs.remove(objPtr).ensuring(_ == true)
    funcTableToPointer.remove(funcTableAddr)
  }

  /**
   * Unexpose a MuCtx. Remove the faked pointer and deallocate the function table (by removing the Pointer).
   *  Also unexpose all MuValues held by the MuCtx.
   */
  def unexposeMuCtx(funcTableAddr: FuncTablePtr): Unit = {
    require(funcTableToPointer.contains(funcTableAddr),
      "MuCtx struct pointer %d 0x%x does not exist".format(funcTableAddr, funcTableAddr))
    val objPtr = theMemory.getPointer(funcTableAddr)
    val muCtx = muCtxs.get(objPtr).ensuring { _ != null }
    for (muValueAddr <- muCtxToMuValueFaks.remove(muCtx).ensuring(_.isDefined).get) {
      // Don't remove it from muCtxToMuValues(muCtx) because the whole HashSet has just been detached.  
      muValues.remove(jnrMemoryManager.newPointer(muValueAddr)).ensuring(_ == true)
    }
    muCtxs.remove(objPtr)
    funcTableToPointer.remove(funcTableAddr)
  }

  /** Unexpose a MuValue by removing it from the ORM. */
  def unexposeMuValue(muCtx: MuCtx, addr: MuValueFak): Unit = {
    muCtxToMuValueFaks(muCtx).remove(addr)
    muValues.remove(jnrMemoryManager.newPointer(addr)).ensuring(_ == true)
  }
}