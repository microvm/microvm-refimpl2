package uvm.refimpl.mem

import uvm.refimpl.mem.TypeSizes.Word
import java.nio.ByteBuffer
import uvm.ssavariables.AtomicRMWOptr._

object MemorySupport {
  val MEMORY_SIZE: Word = 1024L * 1024L * 1024L

  val bb: ByteBuffer = ByteBuffer.allocateDirect(MEMORY_SIZE.toInt)
  bb.order(java.nio.ByteOrder.LITTLE_ENDIAN)

  def loadByte(loc: Word): Byte = bb.get(loc.toInt)
  def loadShort(loc: Word): Short = bb.getShort(loc.toInt)
  def loadInt(loc: Word): Int = bb.getInt(loc.toInt)
  def loadLong(loc: Word): Long = bb.getLong(loc.toInt)
  def loadI128(loc: Word): (Long, Long) = (bb.getLong(loc.toInt), bb.getLong(loc.toInt + 8))
  def loadFloat(loc: Word): Float = bb.getFloat(loc.toInt)
  def loadDouble(loc: Word): Double = bb.getDouble(loc.toInt)

  def storeByte(loc: Word, v: Byte): Unit = bb.put(loc.toInt, v)
  def storeShort(loc: Word, v: Short): Unit = bb.putShort(loc.toInt, v)
  def storeInt(loc: Word, v: Int): Unit = bb.putInt(loc.toInt, v)
  def storeLong(loc: Word, v: Long): Unit = bb.putLong(loc.toInt, v)
  def storeI128(loc: Word, v: (Long, Long)): Unit = { val (low, high) = v; bb.putLong(loc.toInt, low); bb.putLong(loc.toInt + 8, high) }
  def storeFloat(loc: Word, v: Float): Unit = bb.putFloat(loc.toInt, v)
  def storeDouble(loc: Word, v: Double): Unit = bb.putDouble(loc.toInt, v)

  def cmpXchgInt(loc: Word, expected: Int, desired: Int): (Boolean, Int) = {
    val oldVal = loadInt(loc)
    if (oldVal == expected) {
      storeInt(loc, desired)
      return (true, oldVal)
    } else {
      return (false, oldVal)
    }
  }

  def cmpXchgLong(loc: Word, expected: Long, desired: Long): (Boolean, Long) = {
    val oldVal = loadLong(loc)
    if (oldVal == expected) {
      storeLong(loc, desired)
      return (true, oldVal)
    } else {
      return (false, oldVal)
    }
  }

  def cmpXchgI128(loc: Word, expected: (Long, Long), desired: (Long, Long)): (Boolean, (Long, Long)) = {
    val oldVal = loadI128(loc)
    if (oldVal == expected) {
      storeI128(loc, desired)
      return (true, oldVal)
    } else {
      return (false, oldVal)
    }
  }

  def atomicRMWInt(optr: AtomicRMWOptr, loc: Word, opnd: Int): Int = {
    val oldVal = loadInt(loc)
    val newVal = optr match {
      case XCHG => opnd
      case ADD => oldVal + opnd
      case SUB => oldVal - opnd
      case AND => oldVal & opnd
      case NAND => ~(oldVal & opnd)
      case OR => oldVal | opnd
      case XOR => oldVal ^ opnd
      case MAX => Math.max(oldVal, opnd)
      case MIN => Math.min(oldVal, opnd)
      case UMAX => Math.max(oldVal - Int.MinValue, opnd - Int.MinValue) + Int.MinValue
      case UMIN => Math.min(oldVal - Int.MinValue, opnd - Int.MinValue) + Int.MinValue
    }
    storeInt(loc, newVal)
    return oldVal
  }

  def atomicRMWLong(optr: AtomicRMWOptr, loc: Word, opnd: Long): Long = {
    val oldVal = loadLong(loc)
    val newVal = optr match {
      case XCHG => opnd
      case ADD => oldVal + opnd
      case SUB => oldVal - opnd
      case AND => oldVal & opnd
      case NAND => ~(oldVal & opnd)
      case OR => oldVal | opnd
      case XOR => oldVal ^ opnd
      case MAX => Math.max(oldVal, opnd)
      case MIN => Math.min(oldVal, opnd)
      case UMAX => Math.max(oldVal - Long.MinValue, opnd - Long.MinValue) + Long.MinValue
      case UMIN => Math.min(oldVal - Long.MinValue, opnd - Long.MinValue) + Long.MinValue
    }
    storeLong(loc, newVal)
    return oldVal
  }

  def xchgI128(loc: Word, desired: (Long, Long)): (Long, Long) = {
    val oldVal = loadI128(loc)
    storeI128(loc, desired)
    return oldVal
  }
}