package uvm.refimpl.nat

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.HashMap
import org.slf4j.LoggerFactory
import com.kenai.jffi.{ Type => JType, Struct => JStruct, Function => JFunction, HeapInvocationBuffer, Invoker }
import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.typesafe.scalalogging.Logger
import uvm.FuncSig
import uvm.{ Function => MFunc }
import uvm.refimpl.UvmRefImplException
import uvm.refimpl.UvmRuntimeException
import uvm.refimpl.itpr._
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables.ExposedFunc
import uvm.types._
import uvm.types.{ Type => MType }
import uvm.utils.HexDump
import uvm.utils.LazyPool
import uvm.refimpl.UvmRefImplException
import jnr.ffi.Pointer

object NativeCallHelper {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  private def storeBoxToPtr(ptr: Pointer, off: Int, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => ptr.putByte(off, vb.asInstanceOf[BoxInt].value.toByte)
      case TypeInt(16)  => ptr.putShort(off, vb.asInstanceOf[BoxInt].value.toShort)
      case TypeInt(32)  => ptr.putInt(off, vb.asInstanceOf[BoxInt].value.toInt)
      case TypeInt(64)  => ptr.putLong(off, vb.asInstanceOf[BoxInt].value.toLong)
      case TypeFloat()  => ptr.putFloat(off, vb.asInstanceOf[BoxFloat].value)
      case TypeDouble() => ptr.putDouble(off, vb.asInstanceOf[BoxDouble].value)
      case s @ TypeStruct(flds) => {
        val fldvbs = vb.asInstanceOf[BoxSeq].values
        for (((fty, fvb), i) <- (flds zip fldvbs).zipWithIndex) {
          val off2 = TypeSizes.fieldOffsetOf(s, i)
          storeBoxToPtr(ptr, off + off2.toInt, fty, fvb)
        }
      }
      case _: AbstractPointerType => ptr.putLong(off, vb.asInstanceOf[BoxPointer].addr)
    }
  }

  private def loadBoxFromPtr(ptr: Pointer, off: Long, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(ptr.getByte(off), 8)
      case TypeInt(16)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(ptr.getShort(off), 16)
      case TypeInt(32)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(ptr.getInt(off), 32)
      case TypeInt(64)  => vb.asInstanceOf[BoxInt].value = OpHelper.trunc(ptr.getLong(off), 64)
      case TypeFloat()  => vb.asInstanceOf[BoxFloat].value = ptr.getFloat(off)
      case TypeDouble() => vb.asInstanceOf[BoxDouble].value = ptr.getDouble(off)
      case s @ TypeStruct(flds) => {
        val fldvbs = vb.asInstanceOf[BoxSeq].values
        for (((fty, fvb), i) <- (flds zip fldvbs).zipWithIndex) {
          val off2 = TypeSizes.fieldOffsetOf(s, i)
          loadBoxFromPtr(ptr, off + off2, fty, fvb)
        }
      }
      case _: AbstractPointerType => vb.asInstanceOf[BoxPointer].addr = ptr.getAddress(off)
    }
  }

  private def makeBoxFromPtr(ptr: Pointer, off: Long, mty: MType): ValueBox = mty match {
    case TypeInt(8)   => BoxInt(OpHelper.trunc(ptr.getByte(off), 8))
    case TypeInt(16)  => BoxInt(OpHelper.trunc(ptr.getShort(off), 16))
    case TypeInt(32)  => BoxInt(OpHelper.trunc(ptr.getInt(off), 32))
    case TypeInt(64)  => BoxInt(OpHelper.trunc(ptr.getLong(off), 64))
    case TypeFloat()  => BoxFloat(ptr.getFloat(off))
    case TypeDouble() => BoxDouble(ptr.getDouble(off))
    case s @ TypeStruct(flds) => {
      val fldvbs = for ((fty, i) <- flds.zipWithIndex) yield {
        val off2 = TypeSizes.fieldOffsetOf(s, i)
        makeBoxFromPtr(ptr, off + off2, fty)
      }
      BoxSeq(fldvbs)
    }
    case _: AbstractPointerType => BoxPointer(ptr.getAddress(off))
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
        val sz = TypeSizes.alignUp(TypeSizes.sizeOf(mty), FORCE_ALIGN_UP).intValue()
        val buf = ByteBuffer.allocate(sz).order(ByteOrder.LITTLE_ENDIAN)
        val ptr = Pointer.wrap(NativeSupport.jnrRuntime, buf)
        storeBoxToPtr(ptr, 0, mty, vb)
        logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(buf))
        hib.putStruct(buf.array(), buf.arrayOffset())
      }
      case _: AbstractPointerType => hib.putAddress(vb.asInstanceOf[BoxPointer].addr)
    }
  }

  def makeBoxFromClosureBufParam(cbuf: Closure.Buffer, index: Int, mty: MType): ValueBox = mty match {
    case TypeInt(8)   => BoxInt(OpHelper.trunc(cbuf.getByte(index), 8))
    case TypeInt(16)  => BoxInt(OpHelper.trunc(cbuf.getShort(index), 16))
    case TypeInt(32)  => BoxInt(OpHelper.trunc(cbuf.getInt(index), 32))
    case TypeInt(64)  => BoxInt(OpHelper.trunc(cbuf.getLong(index), 64))
    case TypeFloat()  => BoxFloat(cbuf.getFloat(index))
    case TypeDouble() => BoxDouble(cbuf.getDouble(index))
    case s @ TypeStruct(flds) => {
      val mem = cbuf.getStruct(index)
      makeBoxFromPtr(NativeSupport.theMemory, mem, mty)
    }
    case _: AbstractPointerType => BoxPointer(cbuf.getAddress(index))
  }

  def putBoxToClosureBufRv(cbuf: Closure.Buffer, mty: MType, vb: ValueBox): Unit = mty match {
    case TypeInt(8)   => cbuf.setByteReturn(vb.asInstanceOf[BoxInt].value.toByte)
    case TypeInt(16)  => cbuf.setShortReturn(vb.asInstanceOf[BoxInt].value.toShort)
    case TypeInt(32)  => cbuf.setIntReturn(vb.asInstanceOf[BoxInt].value.toInt)
    case TypeInt(64)  => cbuf.setLongReturn(vb.asInstanceOf[BoxInt].value.toLong)
    case TypeFloat()  => cbuf.setFloatReturn(vb.asInstanceOf[BoxFloat].value)
    case TypeDouble() => cbuf.setDoubleReturn(vb.asInstanceOf[BoxDouble].value)
    case s @ TypeStruct(flds) => {
      // Always allocate more space so that C may access the word that contains the byte instead of just the byte.
      val sz = TypeSizes.alignUp(TypeSizes.sizeOf(mty), FORCE_ALIGN_UP).intValue()
      val buf = ByteBuffer.allocate(sz).order(ByteOrder.LITTLE_ENDIAN)
      val ptr = Pointer.wrap(NativeSupport.jnrRuntime, buf)
      storeBoxToPtr(ptr, 0, mty, vb)
      logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(buf))
      cbuf.setStructReturn(buf.array(), buf.arrayOffset())
    }
    case _: AbstractPointerType => cbuf.setAddressReturn(vb.asInstanceOf[BoxPointer].addr)
  }

}

/**
 * Helps calling native functions and supports callbacks from native. Based on JFFI.
 */
class NativeCallHelper {
  import NativeCallHelper._

  /** A mapping of Mu types to JFFI types. Cached for struct types. */
  val jffiTypePool: LazyPool[MType, JType] = LazyPool {
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

  /** Map Mu-style multi-return types to C-style single return type */
  def getNativeReturnType(retTys: Seq[MType]): JType = {
    retTys match {
      case Seq()  => JType.VOID
      case Seq(t) => jffiTypePool(t)
      case ts     => throw new UvmRefImplException("Multiple return types %s cannot be used in native calls.".format(ts.map(_.repr).mkString(" ")))
    }
  }

  /** A mapping from referenced C functions (signature, function pointer) to JFFI functions. Cached. */
  val jffiFuncPool = LazyPool[(FuncSig, Word), JFunction] {
    case (sig, funcAddr) => {
      val jParamTypes = sig.paramTys.map(jffiTypePool.apply)
      val jRetTy = getNativeReturnType(sig.retTys)
      new JFunction(funcAddr, jRetTy, jParamTypes: _*)
    }
  }

  /**
   * A run-time record of an exposed Mu function. A Mu function may be exposed many times. Each ExpFuncRecord
   * corresponds to one such callable instance.
   * <p>
   * A ".expose" definition will permanently create an instance.
   * <p>
   * The "@uvm.native.expose" instruction will also create one such instance. Such instances can be removed later by
   * "@uvm.native.unexpose". The equivalent API calls do the same.
   *
   * @param isDynamic true if it is exposed via the "@uvm.native.expose" instruction or the equivalent API call. false
   * if it is exposed by the ".expose" top-level definintion of the Mu IR.
   */
  class ExpFuncRec(val muFunc: MFunc, val cookie: Long, val closure: MuCallbackClosure, val closureHandle: Closure.Handle, val isDynamic: Boolean)

  /**
   * Map each address of closure handle to the DynExpFunc record so that the closure handle can be disposed.
   */
  val addrToRec = new HashMap[Word, ExpFuncRec]()

  /**
   * Map each uvm.ssavariables.ExpFunc instance to the DynExpFunc record.
   */
  val expFuncToRec = new HashMap[ExposedFunc, ExpFuncRec]()

  /**
   * Get the address to the statically exposed function (.expose) by the ExposedFunc instance.
   */
  def getStaticExpFuncAddr(expFunc: ExposedFunc): Word = {
    expFuncToRec(expFunc).closureHandle.getAddress()
  }

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
  def callNative(nsk: NativeStackKeeper, sig: FuncSig, func: Word, args: Seq[ValueBox]): Option[ValueBox] = {
    assert(Thread.currentThread() == nsk.slaveThread)

    val jFunc = jffiFuncPool((sig, func))

    val hib = new HeapInvocationBuffer(jFunc)

    for ((mty, vb) <- (sig.paramTys zip args)) {
      putArg(hib, mty, vb)
    }

    currentNativeStackKeeper.set(nsk)
    assert(currentNativeStackKeeper.get() == nsk)

    val inv = Invoker.getInstance

    val maybeRvb = sig.retTys match {
      case Seq() => {
        inv.invokeLong(jFunc, hib)
        None
      }
      case Seq(t) => {
        val b = t match {
          case TypeInt(8) => {
            val rv = inv.invokeInt(jFunc, hib).toByte
            BoxInt(OpHelper.trunc(BigInt(rv), 8))
          }
          case TypeInt(16) => {
            val rv = inv.invokeInt(jFunc, hib).toShort
            BoxInt(OpHelper.trunc(BigInt(rv), 16))
          }
          case TypeInt(32) => {
            val rv = inv.invokeInt(jFunc, hib)
            BoxInt(OpHelper.trunc(BigInt(rv), 32))
          }
          case TypeInt(64) => {
            val rv = inv.invokeLong(jFunc, hib)
            BoxInt(OpHelper.trunc(BigInt(rv), 64))
          }
          case TypeFloat() => {
            val rv = inv.invokeFloat(jFunc, hib)
            BoxFloat(rv)
          }
          case TypeDouble() => {
            val rv = inv.invokeDouble(jFunc, hib)
            BoxDouble(rv)
          }
          case TypeStruct(flds) => {
            val rv = inv.invokeStruct(jFunc, hib)
            val buf = ByteBuffer.wrap(rv).order(ByteOrder.LITTLE_ENDIAN)
            logger.debug("Hexdump:\n" + HexDump.dumpByteBuffer(buf))
            val ptr = Pointer.wrap(NativeSupport.jnrRuntime, buf)
            makeBoxFromPtr(ptr, 0, t)
          }
          case _: AbstractPointerType => {
            val rv = inv.invokeAddress(jFunc, hib)
            BoxPointer(rv)
          }
        }
        Some(b)
      }
    }
    currentNativeStackKeeper.remove()

    maybeRvb
  }

  def exposeFuncStatic(expFunc: ExposedFunc): Word = {
    val efr = exposeFunc(expFunc.func, expFunc.cookie.num.toLong, false)
    expFuncToRec(expFunc) = efr
    efr.closureHandle.getAddress
  }

  def exposeFuncDynamic(muFunc: MFunc, cookie: Long): Word = {
    val efr = exposeFunc(muFunc, cookie, true)
    efr.closureHandle.getAddress
  }

  /**
   * Expose a Mu function.
   *
   * @return the address of the exposed function (i.e. of the closure handle)
   */
  private def exposeFunc(muFunc: MFunc, cookie: Long, isDynamic: Boolean): ExpFuncRec = {
    val sig = muFunc.sig
    val jParamTypes = sig.paramTys.map(jffiTypePool.apply)
    val jRetTy = getNativeReturnType(sig.retTys)

    val clos = new MuCallbackClosure(muFunc, cookie)
    val handle = NativeSupport.jffiClosureManager.newClosure(clos, jRetTy, jParamTypes.toArray, CallingConvention.DEFAULT)
    val addr = handle.getAddress

    val efr = new ExpFuncRec(muFunc, cookie, clos, handle, isDynamic)

    addrToRec(addr) = efr

    efr
  }

  def unexposeFunc(addr: Word): Unit = {
    val efr = addrToRec.get(addr).getOrElse {
      throw new UvmRuntimeException("Attempt to unexpose function %d (0x%x) which has not been exposed.".format(addr, addr))
    }

    if (!efr.isDynamic) {
      throw new UvmRuntimeException("Attempt to unexpose a function %d (0x%x) exposed via the '.expose' top-level definition.".format(addr, addr))
    }

    addrToRec.remove(addr)

    efr.closureHandle.dispose()
  }

  /** Handles calling back from C */
  class MuCallbackClosure(val muFunc: MFunc, val cookie: Long) extends Closure {
    def invoke(buf: Closure.Buffer): Unit = {
      try {
        val nsk = currentNativeStackKeeper.get()
        logger.debug("Called back. nsk = %s, currentThread = %s, muFunc = %s".format(nsk, Thread.currentThread(), muFunc.repr))
        assert(nsk != null, s"Native calls Mu function ${muFunc.repr} with cookie ${cookie}, but Mu did not call native.")
        assert(Thread.currentThread() == nsk.slaveThread)
        currentNativeStackKeeper.remove()

        val sig = muFunc.sig

        val paramBoxes = for ((paramTy, i) <- sig.paramTys.zipWithIndex) yield {
          makeBoxFromClosureBufParam(buf, i, paramTy)
        }

        val maybeRetTy = sig.retTys match {
          case Seq()  => None
          case Seq(t) => Some(t)
          case ts     => throw new UvmRefImplException("Multiple return types %s cannot be used in native calls.".format(ts.map(_.repr).mkString(" ")))
        }

        val maybeRetBox = maybeRetTy.map(ValueBox.makeBoxForType)

        logger.debug("Calling to Mu nsk.slave...")

        val maybeRvb = nsk.slave.onCallBack(muFunc, cookie, paramBoxes)

        (maybeRetBox, maybeRvb) match {
          case (None, None)           =>
          case (Some(dst), Some(src)) => dst.copyFrom(src)
          case _                      => throw new Error("This is impossible")
        }

        logger.debug("Back from nsk.slave. Returning to native...")

        maybeRetBox.foreach { rvBox =>
          putBoxToClosureBufRv(buf, maybeRetTy.get, rvBox)
        }

        currentNativeStackKeeper.set(nsk)
      } catch {
        case e: Throwable =>
          logger.debug("Exception occured in the slave thread when there are native threads alive. " +
            "Prepare for undefined behaviours in native frames (or JVM frames if the native calls back again).", e)
          throw e

      }
    }
  }
}