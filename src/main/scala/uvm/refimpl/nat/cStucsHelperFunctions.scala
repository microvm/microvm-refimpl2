package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._
import PlatformConstants._
import uvm.refimpl._
import uvm.ssavariables.{ BinOptr, CmpOptr, ConvOptr, MemoryOrder, AtomicRMWOptr }
import uvm.ir.irbuilder.DestKind

import CDefsHelperFunctions._
import uvm.refimpl.MicroVM
import uvm.ssavariables.Flag
import java.nio.charset.StandardCharsets

class ExposedMethod(jRetTy: JType, jParamTys: Array[JType], invokeFunc: Buffer => Unit) {
  val closure = new SimpleClosure(invokeFunc)
  val handle = jffiClosureManager.newClosure(closure, jRetTy, jParamTys, CallingConvention.DEFAULT)
  def address = handle.getAddress()
}

class SimpleClosure(f: Buffer => Unit) extends Closure {
  def invoke(buffer: Buffer): Unit = f(buffer)
}

private object CDefsHelperFunctions {
  import NativeClientSupport._

  def exposedMethod(jRetTy: JType, jParamTys: Array[JType])(invokeFunc: Buffer => Unit) = {
    new ExposedMethod(jRetTy, jParamTys, invokeFunc)
  }

  def readIntArray(base: Long, len: Long): IndexedSeq[Int] = {
    if (base == 0L) {
      IndexedSeq[Int]()
    } else {
      for (i <- 0L until len) yield {
        val addr = base + i * 4L
        val v = theMemory.getInt(addr)
        v
      }
    }
  }

  def readLongArray(base: Long, len: Long): IndexedSeq[Long] = {
    if (base == 0L) {
      IndexedSeq[Long]()
    } else {
      for (i <- 0L until len) yield {
        val addr = base + i * 8L
        val v = theMemory.getLong(addr)
        v
      }
    }
  }

  def readMuValueArray(base: Long, len: Long): IndexedSeq[MuValue] = {
    readLongArray(base, len).map(getMuValueNotNull)
  }

  def readFlagArray(base: Long, len: Long): IndexedSeq[Flag] = {
    readIntArray(base, len).map(toFlag)
  }

  def toFlag(cval: Int): Flag = cval match {
    case 0x00 => Flag("#DEFAULT")
    case _    => throw new IllegalArgumentException("Unknown calling convention %d (0x%x)".format(cval, cval))
  }

  val MAX_NAME_SIZE = 65536

  def readCString(addr: Long): String = {
    val str = theMemory.getString(addr, MAX_NAME_SIZE, StandardCharsets.US_ASCII)
    str
  }

  def readCharArray(base: Long, len: Int): String = {
    val str = theMemory.getString(base, len, StandardCharsets.US_ASCII)
    str
  }
  
  def unsignedLongSeqToBigInt(nums: Seq[Long]): BigInt = {
      var bigNum = BigInt(0)
      for (num <- nums) {
        bigNum = (bigNum << 64) | (BigInt(num) & 0xffffffffffffffffL)
      }
      bigNum
  }

  implicit class RichMuVM(val mvm: MicroVM) extends AnyVal {
    def setTrapHandler(trap_handler: MuTrapHandlerFP, userdata: MuCPtr): Unit = {
      mvm.setTrapHandler(new NativeTrapHandler(trap_handler, userdata))
    }

    def getMuErrorPtr(): MuCPtr = ClientAccessibleClassExposer.muErrorPtr.address()
  }

  implicit class RichMuCtx(val ctx: MuCtx) extends AnyVal {
    def handleFromSInt8(num: Byte, len: Int): MuIntValue = ctx.handleFromInt(num, len)
    def handleFromUInt8(num: Byte, len: Int): MuIntValue = ctx.handleFromInt(BigInt(num) & 0xff, len)
    def handleFromSInt16(num: Short, len: Int): MuIntValue = ctx.handleFromInt(num, len)
    def handleFromUInt16(num: Short, len: Int): MuIntValue = ctx.handleFromInt(BigInt(num) & 0xffff, len)
    def handleFromSInt32(num: Int, len: Int): MuIntValue = ctx.handleFromInt(num, len)
    def handleFromUInt32(num: Int, len: Int): MuIntValue = ctx.handleFromInt(BigInt(num) & 0xffffffff, len)
    def handleFromSInt64(num: Long, len: Int): MuIntValue = ctx.handleFromInt(num, len)
    def handleFromUInt64(num: Long, len: Int): MuIntValue = ctx.handleFromInt(BigInt(num) & 0xffffffffffffffffL, len)
    def handleFromUInt64s(nums: IndexedSeq[Long], len: Int): MuIntValue = {
      val bigNum = unsignedLongSeqToBigInt(nums)
      ctx.handleFromInt(bigNum, len)
    }
    def handleToSInt8(h: MuIntValue): Byte = ctx.handleToSInt(h).toByte
    def handleToUInt8(h: MuIntValue): Byte = ctx.handleToUInt(h).toByte
    def handleToSInt16(h: MuIntValue): Short = ctx.handleToSInt(h).toShort
    def handleToUInt16(h: MuIntValue): Short = ctx.handleToUInt(h).toShort
    def handleToSInt32(h: MuIntValue): Int = ctx.handleToSInt(h).toInt
    def handleToUInt32(h: MuIntValue): Int = ctx.handleToUInt(h).toInt
    def handleToSInt64(h: MuIntValue): Long = ctx.handleToSInt(h).toLong
    def handleToUInt64(h: MuIntValue): Long = ctx.handleToUInt(h).toLong

    def cmpxchg(ordSucc: MemoryOrder.Value, ordFail: MemoryOrder.Value, weak: Boolean,
      loc: MuIRefValue, expected: MuValue, desired: MuValue, is_succ: IntPtr): MuValue = {
      val (v, s) = ctx.cmpXchg(ordSucc, ordFail, weak, loc, expected, desired)
      val sInt = if (s) 1 else 0
      theMemory.putInt(is_succ, sInt)
      v
    }

    def atomicrmw(ord: MemoryOrder.Value, op: AtomicRMWOptr.Value, loc: MuIRefValue, opnd: MuValue): MuValue = {
      ctx.atomicRMW(ord, op, loc, opnd)
    }

    def newThreadNor(stack: MuStackRefValue, threadLocal: Option[MuRefValue], vals: Seq[MuValue]): MuThreadRefValue = {
      ctx.newThread(stack, threadLocal, HowToResume.PassValues(vals))
    }

    def newThreadExc(stack: MuStackRefValue, threadLocal: Option[MuRefValue], exc: MuRefValue): MuThreadRefValue = {
      ctx.newThread(stack, threadLocal, HowToResume.ThrowExc(exc))
    }

    def dumpKeepalives(cursor: MuFCRefValue, results: MuValueFakArrayPtr): Unit = {
      val kas = ctx.dumpKeepalives(cursor)
      for ((ka, i) <- kas.zipWithIndex) {
        val fak = exposeMuValue(ctx, ka)
        val addr = results + i * WORD_SIZE_BYTES
        theMemory.putAddress(addr, fak)
      }
    }
    
    def newConstIntEx(b: MuBundleNode, ty: MuTypeNode, values: Seq[Long]): MuConstNode = {
      val bigNum = unsignedLongSeqToBigInt(values)
      ctx.newConstInt(b, ty, bigNum)
    }
  }
  
  implicit def makeMuValueSeqCovariant[T <: MuValue, U <: T](seq: Seq[T]): Seq[U] = seq.asInstanceOf[Seq[U]]

}