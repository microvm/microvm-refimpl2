package uvm.refimpl.nat

import java.nio.charset.StandardCharsets

import scala.collection.mutable.HashMap
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
  // Syntax sugar for MuCtx and MuValue return values, which are pointers
  type MuCtxPtr = Word
  type MuValuePtr = Word

  def new_context(microVM: MicroVM): MuCtxPtr = {
    val ctx = microVM.newContext()
    val ptr = NativeClientSupport.muCtxs.add(ctx)
    ptr.address()
  }
  def id_of(microVM: MicroVM, name: String): Int = microVM.idOf(name)
  def name_of(microVM: MicroVM, id: Int): String = microVM.nameOf(id)
  def set_trap_handler(microVM: MicroVM, trap_handler: Word, userdata: Word): Unit = {
    microVM.setTrapHandler(new NativeTrapHandler(trap_handler, userdata))
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
  def paramMicroVM(index: Int)(buffer: Buffer): Any = getObj(buffer, index, NativeClientSupport.microVMs)
  def paramMuCtx(index: Int)(buffer: Buffer): Any = getObj(buffer, index, NativeClientSupport.muCtxs)
  def paramMuValue(index: Int)(buffer: Buffer): Any = getObj(buffer, index, NativeClientSupport.muValues)

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

  private def exposeStr(str: String): Word = {
    val ptr = NativeClientSupport.stringPool.getOrElseUpdate(str, {
      val bytes = str.getBytes(StandardCharsets.US_ASCII)
      val newPtr = jnrMemoryManager.allocateDirect(bytes.size + 1)
      newPtr.put(0L, bytes, 0, 0)
      newPtr
    })
    ptr.address()
  }

  private def getObj[T](buffer: Buffer, index: Int, orm: ObjectReferenceManager[T]): T = {
    val addr = buffer.getLong(index)
    orm.get(jnrMemoryManager.newPointer(addr))
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
    case t if t =:= TMuValue => JType.POINTER
  }

  /**
   * Let a native program call a reflectively-exposed method.
   */
  class ExposedMethodClosure(method: ru.MethodMirror, paramGetters: Seq[Buffer => Any], returnSetter: (Buffer, Any) => Unit) extends Closure {
    def invoke(buffer: Buffer): Unit = {
      val params = paramGetters.map(_(buffer))
      val rv = try {
        method.apply(params: _*)
      } catch {
        case e: Throwable => {
          logger.error("Exception thrown before returning to native. This is fatal", e)
          throw e
        }
      }
      returnSetter(buffer, rv)
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
        case t if t =:= TMuValue => paramMuValue(i) _
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
   */
  def makeFuncTable(instance: Word): Word = {
    val nMethods = closureHandles.size
    val structSizeWords = nMethods + 1
    val structSizeBytes = structSizeWords * WORD_SIZE_BYTES.toInt
    val s = jnrMemoryManager.allocateDirect(structSizeBytes)
    s.putLong(0L, instance)
    for ((handle, i) <- closureHandles.zipWithIndex) {
      val offset = (i + 1) * WORD_SIZE_BYTES
      val funcAddr = handle.getAddress()
      s.putLong(offset, funcAddr)
    }
    val addr = s.address()
    NativeClientSupport.funcTableToPointer(addr) = s
    addr
  }
}

/**
 * Support for native clients (such as C programs).
 */
object NativeClientSupport {
  // Give exposed objects a random "memory address" so native programs can pass them back to Mu as parameters.
  val microVMs = jnrRuntime.newObjectReferenceManager[MicroVM]()
  val muCtxs = jnrRuntime.newObjectReferenceManager[MuCtx]()
  val muValues = jnrRuntime.newObjectReferenceManager[MuValue]()

  /**
   * Map function table addresses to Pointer so they can be closed. JFFI uses the TrantientNativeMemory (wrapped
   * in the Pointer object) to keep the allocated native memory alive.
   */
  val funcTableToPointer = HashMap[Word, Pointer]()

  /**
   * Map Java strings (currently only names of Identified objects in MU) to Pointers to keep the allocated strings
   * alive so that the native program can access them.
   */
  val stringPool = HashMap[String, Pointer]()
}