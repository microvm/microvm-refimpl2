package uvm.refimpl.nat

import java.nio.ByteBuffer
import java.nio.ByteOrder
import org.slf4j.LoggerFactory
import com.kenai.jffi.{ Type => JType, Struct => JStruct, Function => JFunction, HeapInvocationBuffer, Invoker }
import com.typesafe.scalalogging.Logger
import uvm.FuncSig
import uvm.refimpl.UvmRefImplException
import uvm.refimpl.itpr._
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes
import uvm.refimpl.mem.TypeSizes.Word
import uvm.{ Function => MFunc }
import uvm.types._
import uvm.types.{ Type => MType }
import uvm.utils.LazyPool
import uvm.utils.HexDump
import scala.collection.mutable.HashMap
import com.kenai.jffi.Closure
import com.kenai.jffi.ClosureManager
import com.kenai.jffi.CallingConvention
import uvm.refimpl.UvmRuntimeException
import uvm.refimpl.MicroVM

object NativeCallHelper {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

/**
 * Helps calling native functions and supports callbacks from native. Based on JFFI.
 */
class NativeCallHelper {
  import NativeCallHelper._

  /** A mapping of Mu types to JFFI types. Cached for struct types. */
  val jffiTypePool: LazyPool[MType, JType] = LazyPool {
    case TypeVoid()       => JType.VOID
    case TypeInt(8)       => JType.SINT8
    case TypeInt(16)      => JType.SINT16
    case TypeInt(32)      => JType.SINT32
    case TypeInt(64)      => JType.SINT64
    case TypeFloat()      => JType.FLOAT
    case TypeDouble()     => JType.DOUBLE
    case TypeVector(_, _) => throw new UvmRefImplException("Vectors are not implemented in native calls.")
    case TypeStruct(fields) => {
      val fieldsNativeTypes: Seq[JType] = fields.map(jffiTypePool.apply)
      val strType = JStruct.newStruct(fieldsNativeTypes: _*)
      strType
    }
    case _: AbstractPointerType => JType.POINTER
    case t                      => throw new UvmRefImplException("Type %s cannot be used in native calls.".format(t.repr))
  }

  /** A mapping from referenced C functions (signature, function pointer) to JFFI functions. Cached. */
  val jffiFuncPool = LazyPool[(FuncSig, Word), JFunction] {
    case (sig, funcAddr) => {
      val jParamTypes = sig.paramTy.map(jffiTypePool.apply)
      val jRetTy = jffiTypePool(sig.retTy)
      new JFunction(funcAddr, jRetTy, jParamTypes: _*)
    }
  }

  /**
   * A dynamically-exposed Mu function. A Mu function may be exposed many times. Each DynExpFunc corresponds to one
   * such callable instance.
   * <p>
   * A ".expose" definition will permanently create an instance.
   * <p>
   * The "@uvm.native.expose" instruction will also create one such instance. Such instances can be removed later by
   * "@uvm.native.unexpose". The equivalent API calls do the same.
   */
  class DynExpFunc(val muFunc: MFunc, val cookie: Long, val closure: MuCallbackClosure, val closureHandle: Closure.Handle)

  /**
   * Map each address of closure handle to the DynExpFunc record so that the closure handle can be disposed.
   */
  val exposedFuncs = new HashMap[Word, DynExpFunc]()

  /**
   * The current NativeStackKeeper instance that makes the native call.
   * <p>
   * It is set just before entering native, by calling a native function, or returning to a native function which called
   * back to Mu.
   * <p>
   * It is cleared after returning to JVM, either when returning from a native function, or when the native calls back
   * to Mu (to JVM).
   */
  val currentNativeStackKeeper = new ThreadLocal[NativeStackKeeper]()

  /**
   * Call a native function. Must be called by a NativeStackKeeper.Slave thread.
   */
  def callNative(nsk: NativeStackKeeper, sig: FuncSig, func: Word, args: Seq[ValueBox], retBox: ValueBox): Unit = {
    val jFunc = jffiFuncPool((sig, func))

    val hib = new HeapInvocationBuffer(jFunc)

    for ((mty, vb) <- (sig.paramTy zip args)) {
      putArg(hib, mty, vb)
    }

    currentNativeStackKeeper.set(nsk)
    assert(currentNativeStackKeeper.get() == nsk)

    val inv = Invoker.getInstance

    sig.retTy match {
      case TypeVoid() => {
        inv.invokeLong(jFunc, hib)
      }
      case TypeInt(8) => {
        val rv = inv.invokeInt(jFunc, hib).toByte
        retBox.asInstanceOf[BoxInt].value = OpHelper.trunc(BigInt(rv), 8)
      }
      case TypeInt(16) => {
        val rv = inv.invokeInt(jFunc, hib).toShort
        retBox.asInstanceOf[BoxInt].value = OpHelper.trunc(BigInt(rv), 16)
      }
      case TypeInt(32) => {
        val rv = inv.invokeInt(jFunc, hib)
        retBox.asInstanceOf[BoxInt].value = OpHelper.trunc(BigInt(rv), 32)
      }
      case TypeInt(64) => {
        val rv = inv.invokeLong(jFunc, hib)
        retBox.asInstanceOf[BoxInt].value = OpHelper.trunc(BigInt(rv), 64)
      }
      case TypeFloat() => {
        val rv = inv.invokeFloat(jFunc, hib)
        retBox.asInstanceOf[BoxFloat].value = rv
      }
      case TypeDouble() => {
        val rv = inv.invokeDouble(jFunc, hib)
        retBox.asInstanceOf[BoxDouble].value = rv
      }
      case TypeStruct(flds) => {
        val rv = inv.invokeStruct(jFunc, hib)
        val buf = ByteBuffer.wrap(rv).order(ByteOrder.LITTLE_ENDIAN)
        logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(buf))
        getArgFromBuf(buf, 0, sig.retTy, retBox)
      }
      case _: AbstractPointerType => {
        val rv = inv.invokeAddress(jFunc, hib)
        retBox.asInstanceOf[BoxPointer].addr = rv
      }
    }
    currentNativeStackKeeper.remove()
  }

  private def putArgToBuf(buf: ByteBuffer, off: Int, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => buf.put(off, vb.asInstanceOf[BoxInt].value.toByte)
      case TypeInt(16)  => buf.putShort(off, vb.asInstanceOf[BoxInt].value.toShort)
      case TypeInt(32)  => buf.putInt(off, vb.asInstanceOf[BoxInt].value.toInt)
      case TypeInt(64)  => buf.putLong(off, vb.asInstanceOf[BoxInt].value.toLong)
      case TypeFloat()  => buf.putFloat(off, vb.asInstanceOf[BoxFloat].value)
      case TypeDouble() => buf.putDouble(off, vb.asInstanceOf[BoxDouble].value)
      case s @ TypeStruct(flds) => {
        val fldvbs = vb.asInstanceOf[BoxStruct].values
        for (((fty, fvb), i) <- (flds zip fldvbs).zipWithIndex) {
          val off2 = TypeSizes.fieldOffsetOf(s, i)
          putArgToBuf(buf, off + off2.toInt, fty, fvb)
        }
      }
      case _: AbstractPointerType => buf.putLong(off, vb.asInstanceOf[BoxPointer].addr)
    }
  }

  private val FORCE_ALIGN_UP = 16L

  private def putArg(hib: HeapInvocationBuffer, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => hib.putByte(vb.asInstanceOf[BoxInt].value.toByte)
      case TypeInt(16)  => hib.putShort(vb.asInstanceOf[BoxInt].value.toShort)
      case TypeInt(32)  => hib.putInt(vb.asInstanceOf[BoxInt].value.toInt)
      case TypeInt(64)  => hib.putLong(vb.asInstanceOf[BoxInt].value.toLong)
      case TypeFloat()  => hib.putFloat(vb.asInstanceOf[BoxFloat].value)
      case TypeDouble() => hib.putDouble(vb.asInstanceOf[BoxDouble].value)
      case TypeStruct(flds) => {
        // Always allocate more space so that C may access the word that contains the byte instead of just the byte.
        val buf = ByteBuffer.allocate(TypeSizes.alignUp(TypeSizes.sizeOf(mty), FORCE_ALIGN_UP).intValue())
        buf.order(ByteOrder.LITTLE_ENDIAN)
        putArgToBuf(buf, 0, mty, vb)
        logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(buf))
        hib.putStruct(buf.array(), buf.arrayOffset())
      }
      case _: AbstractPointerType => hib.putAddress(vb.asInstanceOf[BoxPointer].addr)
    }
  }

  private def getArgFromBuf(buf: ByteBuffer, off: Int, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(buf.get(off), 8)
      case TypeInt(16)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(buf.getShort(off), 16)
      case TypeInt(32)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(buf.getInt(off), 32)
      case TypeInt(64)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(buf.getLong(off), 64)
      case TypeFloat()  => vb.asInstanceOf[BoxFloat].value = buf.getFloat(off)
      case TypeDouble() => vb.asInstanceOf[BoxDouble].value = buf.getDouble(off)
      case s @ TypeStruct(flds) => {
        val fldvbs = vb.asInstanceOf[BoxStruct].values
        for (((fty, fvb), i) <- (flds zip fldvbs).zipWithIndex) {
          val off2 = TypeSizes.fieldOffsetOf(s, i)
          getArgFromBuf(buf, off + off2.toInt, fty, fvb)
        }
      }
      case _: AbstractPointerType => vb.asInstanceOf[BoxPointer].addr = buf.getLong(off)
    }
  }

  /**
   * Expose a Mu function.
   *
   * @return the address of the exposed function (i.e. of the closure handle)
   */
  def exposeFunc(muFunc: MFunc, cookie: Long): Word = {
    val sig = muFunc.sig
    val jParamTypes = sig.paramTy.map(jffiTypePool.apply)
    val jRetTy = jffiTypePool(sig.retTy)

    val clos = new MuCallbackClosure(muFunc, cookie)
    val handle = NativeSupport.closureManager.newClosure(clos, jRetTy, jParamTypes.toArray, CallingConvention.DEFAULT)
    val addr = handle.getAddress

    val dynExpFunc = new DynExpFunc(muFunc, cookie, clos, handle)

    exposedFuncs(addr) = dynExpFunc

    addr
  }

  def unexposeFunc(addr: Word): Unit = {
    val dynExpFunc = exposedFuncs.remove(addr).getOrElse {
      throw new UvmRuntimeException("Attempt to unexpose function %d (0x%x) which has not been exposed.".format(addr, addr))
    }

    dynExpFunc.closureHandle.dispose()
  }

  /** Handles calling back from C */
  class MuCallbackClosure(val muFunc: MFunc, val cookie: Long) extends Closure {
    def invoke(buf: Closure.Buffer): Unit = {
      val nsk = currentNativeStackKeeper.get()
      logger.debug("nsk = %s, currentThread = %s".format(nsk, Thread.currentThread()))
      assert(nsk != null)
      currentNativeStackKeeper.remove()

      if (nsk == null) {
        throw new UvmNativeCallException(s"Native calls Mu function ${muFunc.repr} with cookie ${cookie}, but Mu did not call native.")
      }

      val sig = muFunc.sig

      val paramBoxes = for ((paramTy, i) <- sig.paramTy.zipWithIndex) yield {
        makeBoxForParam(buf, paramTy, i)
      }

      val rvBox = ValueBox.makeBoxForType(sig.retTy)

      nsk.slave.onCallBack(muFunc, cookie, paramBoxes, rvBox)
      
      logger.debug("Back from onCallBack. Returning to native...")

      putRvToBuf(buf, sig.retTy, rvBox)

      currentNativeStackKeeper.set(nsk)
    }

    def makeBoxForParam(buf: Closure.Buffer, ty: MType, index: Int): ValueBox = ty match {
      case TypeInt(8)   => BoxInt(OpHelper.trunc(buf.getByte(index), 8))
      case TypeInt(16)  => BoxInt(OpHelper.trunc(buf.getShort(index), 16))
      case TypeInt(32)  => BoxInt(OpHelper.trunc(buf.getInt(index), 32))
      case TypeInt(64)  => BoxInt(OpHelper.trunc(buf.getLong(index), 64))
      case TypeFloat()  => BoxFloat(buf.getFloat(index))
      case TypeDouble() => BoxDouble(buf.getDouble(index))
      case s @ TypeStruct(flds) => {
        val mem = buf.getStruct(index)
        makeBoxFromMemory(ty, mem)
      }
      case _: AbstractPointerType => BoxPointer(buf.getAddress(index))
    }

    def makeBoxFromMemory(ty: MType, addr: Word): ValueBox = ty match {
      case TypeInt(8)   => BoxInt(OpHelper.trunc(NativeSupport.theMemory.getByte(addr), 8))
      case TypeInt(16)  => BoxInt(OpHelper.trunc(NativeSupport.theMemory.getShort(addr), 16))
      case TypeInt(32)  => BoxInt(OpHelper.trunc(NativeSupport.theMemory.getInt(addr), 32))
      case TypeInt(64)  => BoxInt(OpHelper.trunc(NativeSupport.theMemory.getLong(addr), 64))
      case TypeFloat()  => BoxFloat(NativeSupport.theMemory.getFloat(addr))
      case TypeDouble() => BoxDouble(NativeSupport.theMemory.getDouble(addr))
      case s @ TypeStruct(flds) => {
        val fldvbs = for ((fty, i) <- flds.zipWithIndex) yield {
          val off = TypeSizes.fieldOffsetOf(s, i)
          makeBoxFromMemory(fty, addr + off.toInt)
        }
        BoxStruct(fldvbs)
      }
      case _: AbstractPointerType => BoxPointer(NativeSupport.theMemory.getAddress(addr))
    }

    def putRvToBuf(buf: Closure.Buffer, ty: MType, vb: ValueBox): Unit = ty match {
      case TypeInt(8)   => buf.setByteReturn(vb.asInstanceOf[BoxInt].value.toByte)
      case TypeInt(16)  => buf.setShortReturn(vb.asInstanceOf[BoxInt].value.toShort)
      case TypeInt(32)  => buf.setIntReturn(vb.asInstanceOf[BoxInt].value.toInt)
      case TypeInt(64)  => buf.setLongReturn(vb.asInstanceOf[BoxInt].value.toLong)
      case TypeFloat()  => buf.setFloatReturn(vb.asInstanceOf[BoxFloat].value)
      case TypeDouble() => buf.setDoubleReturn(vb.asInstanceOf[BoxDouble].value)
      case s @ TypeStruct(flds) => {
        // Always allocate more space so that C may access the word that contains the byte instead of just the byte.
        val bbuf = ByteBuffer.allocate(TypeSizes.alignUp(TypeSizes.sizeOf(ty), FORCE_ALIGN_UP).intValue())
        bbuf.order(ByteOrder.LITTLE_ENDIAN)
        putArgToBuf(bbuf, 0, ty, vb)
        logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(bbuf))
        buf.setStructReturn(bbuf.array(), bbuf.arrayOffset())
      }
      case _: AbstractPointerType => buf.setAddressReturn(vb.asInstanceOf[BoxPointer].addr)
    }
  }
}