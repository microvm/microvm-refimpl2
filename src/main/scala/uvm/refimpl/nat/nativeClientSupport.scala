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
import PlatformConstants.WORD_SIZE_BYTES
import PlatformConstants.Word
import jnr.ffi.ObjectReferenceManager
import jnr.ffi.Pointer
import uvm.refimpl._

/**
 * This object calls a C function to handle the trap.
 */
class NativeTrapHandler(val funcAddr: Long, val userdata: Long) extends TrapHandler {
  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
    ???
  }
}

/**
 * This object routes calls from native clients to the MicroVM object.
 */
object NativeMuVM {
  import NativeMuHelpers._
  import NativeClientSupport._

  def new_context(microVM: MicroVM): MuCtxPtr = {
    val ctx = microVM.newContext()
    val ptr = exposeMuCtx(ctx)
    ptr
  }
  def id_of(microVM: MicroVM, name: MuName): MuID = microVM.idOf(name)
  def name_of(microVM: MicroVM, id: MuID): MuName = microVM.nameOf(id)
  def set_trap_handler(microVM: MicroVM, trap_handler: Word, userdata: Word): Unit = {
    microVM.setTrapHandler(new NativeTrapHandler(trap_handler, userdata))
  }
}

/**
 * This object routes calls from native clients to a MuCtx object.
 * <p>
 * Parameters are usually auto-converted from low-level types (pointers to function tables or C MuValue pointer), but
 * return values are usually explicitly converted to pointers by the "expose*" methods, such as exposeMuValue.
 */
object NativeMuCtx {
  import NativeMuHelpers._
  import NativeClientSupport._

  def id_of(ctx: MuCtx, name: MuName): MuID = ctx.idOf(name)
  def name_of(ctx: MuCtx, id: MuID): MuName = ctx.nameOf(id)

  /**
   * NOTE: Should have taken MuCtx as the first parameter, but we need to de-allocate
   * the function table, too.
   */
  def close_context(funcTableAddr: Word): Unit = {
    val ctx = getObjFromFuncTableAddrNotNull(funcTableAddr, NativeClientSupport.muCtxs)
    unexposeMuCtx(funcTableAddr)
    ctx.closeContext()
  }

  def load_bundle(ctx: MuCtx, buf: Word, sz: Int): Unit = {
    val str = theMemory.getString(buf, sz, StandardCharsets.US_ASCII)
    ctx.loadBundle(str)
  }

  def load_hail(ctx: MuCtx, buf: Word, sz: Int): Unit = {
    val str = theMemory.getString(buf, sz, StandardCharsets.US_ASCII)
    ctx.loadHail(str)
  }

  def handle_from_sint8(ctx: MuCtx, num: Byte, len: Int): MuValuePtr = handleFromInt(ctx, num, len)
  def handle_from_uint8(ctx: MuCtx, num: Byte, len: Int): MuValuePtr = handleFromInt(ctx, trunc(num, 8), len)
  def handle_from_sint16(ctx: MuCtx, num: Short, len: Int): MuValuePtr = handleFromInt(ctx, num, len)
  def handle_from_uint16(ctx: MuCtx, num: Short, len: Int): MuValuePtr = handleFromInt(ctx, trunc(num, 16), len)
  def handle_from_sint32(ctx: MuCtx, num: Int, len: Int): MuValuePtr = handleFromInt(ctx, num, len)
  def handle_from_uint32(ctx: MuCtx, num: Int, len: Int): MuValuePtr = handleFromInt(ctx, trunc(num, 32), len)
  def handle_from_sint64(ctx: MuCtx, num: Long, len: Int): MuValuePtr = handleFromInt(ctx, num, len)
  def handle_from_uint64(ctx: MuCtx, num: Long, len: Int): MuValuePtr = handleFromInt(ctx, trunc(num, 64), len)
  def handle_from_float(ctx: MuCtx, num: Float): MuValuePtr = exposeMuValue(ctx, ctx.handleFromFloat(num))
  def handle_from_double(ctx: MuCtx, num: Double): MuValuePtr = exposeMuValue(ctx, ctx.handleFromDouble(num))
  def handle_from_ptr(ctx: MuCtx, mu_type: MuID, ptr: MuCPtr): MuValuePtr = exposeMuValue(ctx, ctx.handleFromPtr(mu_type, ptr))
  def handle_from_fp(ctx: MuCtx, mu_type: MuID, fp: MuCFP): MuValuePtr = exposeMuValue(ctx, ctx.handleFromFP(mu_type, fp))

  def handle_to_sint8(ctx: MuCtx, opnd: MuIntValue): Byte = ctx.handleToSInt(opnd).toByte
  def handle_to_uint8(ctx: MuCtx, opnd: MuIntValue): Byte = ctx.handleToUInt(opnd).toByte
  def handle_to_sint16(ctx: MuCtx, opnd: MuIntValue): Short = ctx.handleToSInt(opnd).toShort
  def handle_to_uint16(ctx: MuCtx, opnd: MuIntValue): Short = ctx.handleToUInt(opnd).toShort
  def handle_to_sint32(ctx: MuCtx, opnd: MuIntValue): Int = ctx.handleToSInt(opnd).toInt
  def handle_to_uint32(ctx: MuCtx, opnd: MuIntValue): Int = ctx.handleToUInt(opnd).toInt
  def handle_to_sint64(ctx: MuCtx, opnd: MuIntValue): Long = ctx.handleToSInt(opnd).toLong
  def handle_to_uint64(ctx: MuCtx, opnd: MuIntValue): Long = ctx.handleToUInt(opnd).toLong
  def handle_to_float(ctx: MuCtx, opnd: MuFloatValue): Float = ctx.handleToFloat(opnd)
  def handle_to_double(ctx: MuCtx, opnd: MuDoubleValue): Double = ctx.handleToDouble(opnd)
  def handle_to_ptr(ctx: MuCtx, opnd: MuUPtrValue): MuCPtr = ctx.handleToPtr(opnd)
  def handle_to_fp(ctx: MuCtx, opnd: MuUFPValue): MuCFP = ctx.handleToFP(opnd)

  def handle_from_const(ctx: MuCtx, id: MuID): MuValuePtr = exposeMuValue(ctx, ctx.handleFromConst(id))
  def handle_from_global(ctx: MuCtx, id: MuID): MuValuePtr = exposeMuValue(ctx, ctx.handleFromGlobal(id))
  def handle_from_func(ctx: MuCtx, id: MuID): MuValuePtr = exposeMuValue(ctx, ctx.handleFromFunc(id))
  def handle_from_expose(ctx: MuCtx, id: MuID): MuValuePtr = exposeMuValue(ctx, ctx.handleFromExpose(id))

  /** Need to dispose the value manually. */
  def delete_value(ctx: MuCtx, opnd: MuValuePtr): Unit = {
    logger.debug("Deleting %d 0x%x".format(opnd, opnd))
    val muVal = getMuValueNotNull(opnd)
    unexposeMuValue(ctx, opnd)
    ctx.deleteValue(muVal)
  }

}

/**
 * These functions are not exposed.
 */
object NativeMuHelpers {
  import NativeClientSupport._

  val ONE = BigInt(1)

  /**
   * Convert unsigned integer to BigInt.
   * <p>
   * NOTE: Scala implicitly converts Byte, Short, Int or Long to BigInt using sign-extension.
   * But when zero-extension is desired (unsigned native arguments), we must truncate the BigInt.
   *
   * @param len The number of bits in the INPUT number (not the output Mu int).
   */
  def trunc(num: BigInt, len: Int): BigInt = num & ((ONE << len) - ONE)

  def handleFromInt(ctx: MuCtx, num: BigInt, len: Int): MuValuePtr = {
    exposeMuValue(ctx, ctx.handleFromInt(num, len))
  }
}

object ClientAccessibleClassExposer {
  val MAX_NAME_SIZE = 65536

  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  // Type objects for common types.
  val TUnit = ru.typeTag[Unit].tpe
  val TByte = ru.typeTag[Byte].tpe
  val TShort = ru.typeTag[Short].tpe
  val TInt = ru.typeTag[Int].tpe
  val TLong = ru.typeTag[Long].tpe
  val TFloat = ru.typeTag[Float].tpe
  val TDouble = ru.typeTag[Double].tpe
  val TString = ru.typeTag[String].tpe
  val TMicroVM = ru.typeTag[MicroVM].tpe
  val TMuCtx = ru.typeTag[MuCtx].tpe
  val TMuValue = ru.typeTag[MuValue].tpe

  // com.kenai.jffi.Closure.Buffer param getters and return value setters.
  // These are partially-applied functions, and will be called in closures (callback from C).
  def paramByte(index: Int)(buffer: Buffer): Any = buffer.getByte(index)
  def paramShort(index: Int)(buffer: Buffer): Any = buffer.getShort(index)
  def paramInt(index: Int)(buffer: Buffer): Any = buffer.getInt(index)
  def paramLong(index: Int)(buffer: Buffer): Any = buffer.getLong(index)
  def paramFloat(index: Int)(buffer: Buffer): Any = buffer.getFloat(index)
  def paramDouble(index: Int)(buffer: Buffer): Any = buffer.getDouble(index)
  def paramString(index: Int)(buffer: Buffer): Any = getStr(buffer, index)
  def paramMicroVM(index: Int)(buffer: Buffer): Any = getObjFromFuncTableAddr(buffer, index, NativeClientSupport.microVMs)
  def paramMuCtx(index: Int)(buffer: Buffer): Any = getObjFromFuncTableAddr(buffer, index, NativeClientSupport.muCtxs)
  def paramMuValue(index: Int)(buffer: Buffer): Any = getMuValue(buffer, index)

  def retVoid(buffer: Buffer, v: Any): Unit = {}
  def retByte(buffer: Buffer, v: Any): Unit = buffer.setByteReturn(v.asInstanceOf[Byte])
  def retShort(buffer: Buffer, v: Any): Unit = buffer.setShortReturn(v.asInstanceOf[Short])
  def retInt(buffer: Buffer, v: Any): Unit = buffer.setIntReturn(v.asInstanceOf[Int])
  def retLong(buffer: Buffer, v: Any): Unit = buffer.setLongReturn(v.asInstanceOf[Long])
  def retFloat(buffer: Buffer, v: Any): Unit = buffer.setFloatReturn(v.asInstanceOf[Float])
  def retDouble(buffer: Buffer, v: Any): Unit = buffer.setDoubleReturn(v.asInstanceOf[Double])
  def retString(buffer: Buffer, v: Any): Unit = buffer.setLongReturn(exposeStr(v.asInstanceOf[String]))

  private def getStr(buffer: Buffer, index: Int): String = {
    val addr = buffer.getAddress(index)
    val str = theMemory.getString(addr, MAX_NAME_SIZE, StandardCharsets.US_ASCII)
    str
  }

  private def getMuValue(buffer: Buffer, index: Int): MuValue = {
    val addr = buffer.getAddress(index)
    NativeClientSupport.getMuValueNotNull(addr)
  }

  private def exposeStr(str: String): Word = {
    val ptr = NativeClientSupport.stringPool.getOrElseUpdate(str, {
      val bytes = str.getBytes(StandardCharsets.US_ASCII)
      val newPtr = jnrMemoryManager.allocateDirect(bytes.size + 1)
      newPtr.put(0L, bytes, 0, bytes.length)
      newPtr.putByte(bytes.length, 0)
      newPtr
    })
    ptr.address()
  }

  private def getObjFromFuncTableAddr[T](buffer: Buffer, index: Int, orm: ObjectReferenceManager[T]): T = {
    val funcTableAddr = buffer.getLong(index)
    NativeClientSupport.getObjFromFuncTableAddrNotNull(funcTableAddr, orm)
  }

  // Reflection utils.
  val mirror = ru.runtimeMirror(getClass.getClassLoader)

  // Convert to JFFI native types.
  def toJFFIType(t: ru.Type): JType = t match {
    case t if t =:= TUnit    => JType.VOID
    case t if t =:= TByte    => JType.SINT8
    case t if t =:= TShort   => JType.SINT16
    case t if t =:= TInt     => JType.SINT32
    case t if t =:= TLong    => JType.SINT64
    case t if t =:= TFloat   => JType.FLOAT
    case t if t =:= TDouble  => JType.DOUBLE
    case t if t =:= TString  => JType.POINTER
    case t if t =:= TMicroVM => JType.POINTER
    case t if t =:= TMuCtx   => JType.POINTER
    case t if t <:< TMuValue => JType.POINTER
  }

  /**
   * Let a native program call a reflectively-exposed method.
   */
  class ExposedMethodClosure(method: ru.MethodMirror, paramGetters: Seq[Buffer => Any], returnSetter: (Buffer, Any) => Unit) extends Closure {
    def invoke(buffer: Buffer): Unit = {
      try {
        val params = paramGetters.map(_(buffer))
        val rv = method.apply(params: _*)
        returnSetter(buffer, rv)
      } catch {
        case e: Throwable => {
          logger.error("Exception thrown before returning to native. This is fatal", e)
          throw e
        }
      }
    }
  }
}

/**
 * Expose a class (NativeMuVM and NativeMuCtx) as a function table.
 */
class ClientAccessibleClassExposer[T: ru.TypeTag: ClassTag](obj: T) {
  import ClientAccessibleClassExposer._
  import NativeSupport._

  /**
   * A list of JFFI closure handles, one for each declared method in obj, in the declared order.
   */
  val closureHandles: IndexedSeq[Closure.Handle] = {
    val tt = ru.typeTag[T]
    val iter = for (m <- tt.tpe.decls if m.isMethod && !m.isConstructor) yield {
      val meth = m.asMethod
      logger.debug("Exposing method: %s".format(meth))

      val paramTypes = meth.paramLists(0).map(_.asTerm.typeSignature)
      val returnType = meth.returnType

      val paramGetters = for ((ty, i) <- paramTypes.zipWithIndex) yield ty match {
        case t if t =:= TByte    => paramByte(i) _
        case t if t =:= TShort   => paramShort(i) _
        case t if t =:= TInt     => paramInt(i) _
        case t if t =:= TLong    => paramLong(i) _
        case t if t =:= TFloat   => paramFloat(i) _
        case t if t =:= TDouble  => paramDouble(i) _
        case t if t =:= TString  => paramString(i) _
        case t if t =:= TMicroVM => paramMicroVM(i) _
        case t if t =:= TMuCtx   => paramMuCtx(i) _
        case t if t <:< TMuValue => paramMuValue(i) _
      }

      val returnSetter = returnType match {
        case t if t =:= TUnit   => retVoid _
        case t if t =:= TByte   => retByte _
        case t if t =:= TShort  => retShort _
        case t if t =:= TInt    => retInt _
        case t if t =:= TLong   => retLong _
        case t if t =:= TFloat  => retFloat _
        case t if t =:= TDouble => retDouble _
        case t if t =:= TString => retString _
      }

      val paramNativeTypes = for (ty <- paramTypes) yield toJFFIType(ty)
      val returnNativeType = toJFFIType(returnType)

      val objMirror = mirror.reflect(obj)
      val methMirror = objMirror.reflectMethod(meth)

      val closure = new ExposedMethodClosure(methMirror, paramGetters, returnSetter)

      val closureHandle = jffiClosureManager.newClosure(closure, returnNativeType, paramNativeTypes.toArray, CallingConvention.DEFAULT)

      closureHandle
    }

    iter.toIndexedSeq
  }

  /**
   * Allocate and populate a native function table,
   * i.e. the in-memory structure of struct MuVM and struct MuCtx in the client interface.
   *
   * @param objAddr The "address" of the object, created by the ObjectReferenceManager.
   * This will be the first field of the table.
   *
   * @return The address of the function table.
   */
  def makeFuncTable(objAddr: Word): Word = {
    val nMethods = closureHandles.size
    val structSizeWords = nMethods + 1
    val structSizeBytes = structSizeWords * WORD_SIZE_BYTES.toInt
    val s = jnrMemoryManager.allocateDirect(structSizeBytes)
    s.putLong(0L, objAddr)
    for ((handle, i) <- closureHandles.zipWithIndex) {
      val offset = (i + 1) * WORD_SIZE_BYTES
      val funcAddr = handle.getAddress()
      s.putLong(offset, funcAddr)
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

  // Syntax sugar for MuCtx and MuValue return values, which are pointers
  type FuncTablePtr = Word
  type MuVMPtr = FuncTablePtr
  type MuCtxPtr = FuncTablePtr
  type MuValuePtr = Word

  type MuID = Int
  type MuName = String
  type MuCPtr = Word
  type MuCFP = Word

  // Give exposed objects a random "memory address" so native programs can pass them back to Mu as parameters.
  val microVMs = jnrRuntime.newObjectReferenceManager[MicroVM]()
  val muCtxs = jnrRuntime.newObjectReferenceManager[MuCtx]()
  val muValues = jnrRuntime.newObjectReferenceManager[MuValue]()

  /** Map each MuCtx to all of its current MuValues. This is needed when closing a MuCtx. */
  val muCtxToMuValues = HashMap[MuCtx, HashSet[MuValuePtr]]()

  /** Given a function table pointer, get an object from a ObjectReferenceManager. Assert it is not null. */
  def getObjFromFuncTableAddrNotNull[T](funcTableAddr: FuncTablePtr, orm: ObjectReferenceManager[T]): T = {
    val objAddr = theMemory.getAddress(funcTableAddr)
    val obj = orm.get(jnrMemoryManager.newPointer(objAddr))
    if (obj == null) {
      throw new UvmRefImplException("Exposed object not found. Address: %d 0x%x".format(objAddr, objAddr))
    }
    obj
  }

  /** Get the MuValue instance form a C MuValue (a pointer). */
  def getMuValueNotNull(addr: MuValuePtr): MuValue = {
    val muVal = muValues.get(jnrMemoryManager.newPointer(addr))
    if (muVal == null) {
      throw new UvmRefImplException("Exposed MuValue not found. Address: %d 0x%x".format(addr, addr))
    }
    muVal
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
  val muVMExposer = new ClientAccessibleClassExposer(NativeMuVM)
  val muCtxExposer = new ClientAccessibleClassExposer(NativeMuCtx)

  // Expose and unexpose objects

  /** Expose a MicroVM. Return a pointer to the C MuVM structure. */
  def exposeMicroVM(microVM: MicroVM): Word = {
    val objAddr = microVMs.add(microVM).address()
    val funcTableAddr = muVMExposer.makeFuncTable(objAddr)
    funcTableAddr
  }

  /** Expose a MuCtx. Return a pointer to the C MuCtx structure. */
  def exposeMuCtx(muCtx: MuCtx): Word = {
    require(!muCtxToMuValues.contains(muCtx), "Context %s is already exposed.".format(muCtx))
    muCtxToMuValues(muCtx) = HashSet()
    val objAddr = muCtxs.add(muCtx).address()
    val funcTableAddr = muCtxExposer.makeFuncTable(objAddr)
    funcTableAddr
  }

  /** Expose a MuValue. Return an "object pointer", i.e. faked by ObjectReferenceManager. */
  def exposeMuValue(muCtx: MuCtx, muValue: MuValue): Word = {
    val addr = muValues.add(muValue).address()
    muCtxToMuValues(muCtx).add(addr)
    addr
  }

  /** Unexpose a MicroVM. Remove the faked pointer and deallocate the function table (by removing the Pointer). */
  def unexposeMicroVM(funcTableAddr: Word): Unit = {
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
  def unexposeMuCtx(funcTableAddr: Long): Unit = {
    require(funcTableToPointer.contains(funcTableAddr),
      "MuCtx struct pointer %d 0x%x does not exist".format(funcTableAddr, funcTableAddr))
    val objPtr = theMemory.getPointer(funcTableAddr)
    val muCtx = muCtxs.get(objPtr).ensuring { _ != null }
    for (muValueAddr <- muCtxToMuValues.remove(muCtx).ensuring(_.isDefined).get) {
      // Don't remove it from muCtxToMuValues(muCtx) because the whole HashSet has just been detached.  
      muValues.remove(jnrMemoryManager.newPointer(muValueAddr)).ensuring(_ == true)
    }
    muCtxs.remove(objPtr)
    funcTableToPointer.remove(funcTableAddr)
  }

  /** Unexpose a MuValue by removing it from the ORM. */
  def unexposeMuValue(muCtx: MuCtx, addr: Long): Unit = {
    muCtxToMuValues(muCtx).remove(addr)
    muValues.remove(jnrMemoryManager.newPointer(addr)).ensuring(_ == true)
  }
}