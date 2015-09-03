package uvm.refimpl.nat

import com.kenai.jffi.{ Type => JType, Struct => JStruct, Function => JFunction, HeapInvocationBuffer, Invoker }
import uvm.FuncSig
import uvm.refimpl.UvmRefImplException
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes.Word
import uvm.types._
import uvm.types.{ Type => MType }
import uvm.utils.LazyPool
import javax.vecmath.Tuple2d
import uvm.refimpl.itpr._
import java.nio.ByteBuffer
import uvm.refimpl.mem.TypeSizes

/**
 * Helps calling native functions. Based on JFFI.
 */
class NativeHelper {
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

  val jffiFuncPool = LazyPool[(FuncSig, Word), JFunction] {
    case (sig, funcAddr) => {
      val jParamTypes = sig.paramTy.map(jffiTypePool.apply)
      val jRetTy = jffiTypePool(sig.retTy)
      new JFunction(funcAddr, jRetTy, jParamTypes: _*)
    }
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
          putArgToBuf(buf, off + off2.toInt, mty, vb)
        }
      }
      case _: AbstractPointerType => buf.putLong(off, vb.asInstanceOf[BoxPointer].addr)
    }
  }

  private def putArg(hib: HeapInvocationBuffer, mty: MType, vb: ValueBox): Unit = {
    mty match {
      case TypeInt(8)   => hib.putByte(vb.asInstanceOf[BoxInt].value.toByte)
      case TypeInt(16)  => hib.putShort(vb.asInstanceOf[BoxInt].value.toShort)
      case TypeInt(32)  => hib.putInt(vb.asInstanceOf[BoxInt].value.toInt)
      case TypeInt(64)  => hib.putLong(vb.asInstanceOf[BoxInt].value.toLong)
      case TypeFloat()  => hib.putFloat(vb.asInstanceOf[BoxFloat].value)
      case TypeDouble() => hib.putDouble(vb.asInstanceOf[BoxDouble].value)
      case TypeStruct(flds) => {
        val buf = ByteBuffer.allocate(TypeSizes.sizeOf(mty).toInt)
        putArgToBuf(buf, 0, mty, vb)
        hib.putStruct(buf.array(), buf.arrayOffset())
      }
      case _: AbstractPointerType => hib.putAddress(vb.asInstanceOf[BoxPointer].addr)
    }
  }

  def callNative(sig: FuncSig, func: Word, args: Seq[ValueBox], retBox: ValueBox): Unit = {
    val jFunc = jffiFuncPool((sig, func))

    val hib = new HeapInvocationBuffer(jFunc)

    for ((mty, vb) <- (sig.paramTy zip args)) {
      putArg(hib, mty, vb)
    }

    val inv = Invoker.getInstance

    sig.retTy match {
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
        val buf = ByteBuffer.wrap(rv)
        getArgFromBuf(buf, 0, sig.retTy, retBox)
      }
      case _: AbstractPointerType => {
        val rv = inv.invokeAddress(jFunc, hib)
        retBox.asInstanceOf[BoxPointer].addr = rv
      }
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
          getArgFromBuf(buf, off + off2.toInt, mty, vb)
        }
      }
      case _: AbstractPointerType => vb.asInstanceOf[BoxPointer].addr = buf.getLong(off)
    }
  }
}