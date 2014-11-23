package uvm.refimpl.itpr

object OpHelper {

  val ONE = BigInt(1)

  def mask(n: Int): BigInt = {
    (ONE << n) - ONE
  }

  def truncFromBigInt(n: BigInt, len: Int): BigInt = n & mask(len)

  def zextToBigInt(n: BigInt, len: Int): BigInt = n & mask(len)

  def sextToBigInt(n: BigInt, len: Int): BigInt = {
    val bit = n.testBit(len - 1)
    if (bit) {
      n | (~mask(len - 1))
    } else {
      n & (mask(len - 1))
    }
  }

  def prepareUnsigned(n: BigInt, len: Int): BigInt = truncFromBigInt(n, len)

  def prepareSigned(n: BigInt, len: Int): BigInt = {
    sextToBigInt(truncFromBigInt(n, len), len)
  }

  def unprepare(n: BigInt, len: Int): BigInt = truncFromBigInt(n, len)

  def trunc(n: BigInt, toLen: Int): BigInt = truncFromBigInt(n, toLen)

  def zext(n: BigInt, fromLen: Int, toLen: Int): BigInt = truncFromBigInt(n, fromLen)

  def sext(n: BigInt, fromLen: Int, toLen: Int): BigInt = {
    truncFromBigInt(sextToBigInt(n, fromLen), toLen)
  }

  def tr64IsInt(opnd: Long): Boolean = {
    (opnd & 0x7ff0000000000001L) == 0x7ff0000000000001L
  }

  def tr64IsFp(opnd: Long): Boolean = {
    (opnd & 0x7ff0000000000001L) != 0x7ff0000000000001L &&
      (opnd & 0x7ff0000000000003L) != 0x7ff0000000000002L
  }

  def tr64IsRef(opnd: Long): Boolean = {
    (opnd & 0x7ff0000000000003L) == 0x7ff0000000000002L
  }

  def intToTr64(opnd: Long): Long = {
    (0x7ff0000000000001L | ((opnd & 0x7ffffffffffffL) << 1) |
      ((opnd & 0x8000000000000L) << 12))
  }

  def fpToTr64(opnd: Double): Long = {
    var bits = java.lang.Double.doubleToRawLongBits(opnd)
    if (java.lang.Double.isNaN(opnd)) {
      bits = bits & 0xfff8000000000000L | 0x0000000000000008L
    }
    bits
  }

  def refToTr64(opnd: Long, tag: Long): Long = {
    (0x7ff0000000000002L | (opnd & 0x7ffffffffff8L) | ((opnd & 0x800000000000L) << 16) |
      ((tag & 0x3eL) << 46) |
      ((tag & 0x1) << 2))
  }

  def tr64ToInt(opnd: Long): Long = {
    (((opnd & 0xffffffffffffeL) >> 1) | ((opnd & 0x8000000000000000L) >> 12) & (1L << 51))
  }

  def tr64ToFp(opnd: Long): Double = java.lang.Double.longBitsToDouble(opnd)

  def tr64ToRef(opnd: Long): Long = {
    ((opnd & 0x7ffffffffff8L) |
      (((~(((opnd & 0x8000000000000000L) << 1) - 1)) >> 17) &
        0xffff800000000000L))
  }

  def tr64ToTag(opnd: Long): Long = {
    (((opnd & 0x000f800000000000L) >> 46) | ((opnd & 0x4) >> 2))
  }
}
