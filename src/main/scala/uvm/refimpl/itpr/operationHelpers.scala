package uvm.refimpl.itpr

import uvm.ssavariables._
import uvm.refimpl._

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

  def maxSInt(l: Int): BigInt = (BigInt(1) << (l - 1)) - 1
  def minSIntAbs(l: Int): BigInt = BigInt(1) << (l - 1)
  def maxUInt(l: Int): BigInt = (BigInt(1) << l) - 1

  /**
   * Convert a float to an integer. The result only has the lowest iLen binary digits.
   */
  def floatToI(n: Float, iLen: Int, signed: Boolean): BigInt = {
    val lExp = Math.getExponent(n)
    val rExp = lExp - 23
    val frac = (java.lang.Float.floatToRawIntBits(n) & 0x7fffff) | 0x800000;

    if (java.lang.Float.isNaN(n)) 0
    else if (signed) {
      if (java.lang.Float.isInfinite(n)) { if (n > 0.0F) maxSInt(iLen) else minSIntAbs(iLen) }
      else {
        if (lExp >= (iLen - 1)) if (n > 0.0F) maxSInt(iLen) else minSIntAbs(iLen)
        else if (lExp < 0) 0
        else {
          val abs = BigInt(frac) << rExp
          unprepare(if (n < 0.0F) -abs else abs, iLen)
        }
      }
    } else {
      if (n < 0.0F) 0
      else if (java.lang.Float.isInfinite(n)) maxUInt(iLen)
      else {
        if (lExp >= iLen) maxUInt(iLen)
        else if (lExp < 0) 0
        else unprepare(BigInt(frac) << rExp, iLen)
      }
    }
  }

  /**
   * Convert a float to an integer. The result only has the lowest iLen binary digits.
   */
  def doubleToI(n: Double, iLen: Int, signed: Boolean): BigInt = {
    val lExp = Math.getExponent(n)
    val rExp = lExp - 52
    val frac = (java.lang.Double.doubleToRawLongBits(n) & 0xfffffffffffffL) | 0x10000000000000L;
    
    if (java.lang.Double.isNaN(n)) 0
    else if (signed) {
      if (java.lang.Double.isInfinite(n)) { if (n > 0.0D) maxSInt(iLen) else minSIntAbs(iLen) }
      else {
        if (lExp >= (iLen - 1)) if (n > 0.0D) maxSInt(iLen) else minSIntAbs(iLen)
        else if (lExp < 0) 0
        else {
          val abs = BigInt(frac) << rExp
          unprepare(if (n < 0.0D) -abs else abs, iLen)
        }
      }
    } else {
      if (n < 0.0D) 0
      else if (java.lang.Double.isInfinite(n)) maxUInt(iLen)
      else {
        if (lExp >= iLen) maxUInt(iLen)
        else if (lExp < 0) 0
        else unprepare(BigInt(frac) << rExp, iLen)
      }
    }
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

object PrimOpHelpers {
  @throws(classOf[UvmDivisionByZeroException])
  def intBinOp(op: BinOptr.BinOptr, l: Int, op1v: BigInt, op2v: BigInt, ctx: => String): BigInt = {
    def pu(v: BigInt): BigInt = OpHelper.prepareUnsigned(v, l)
    def ps(v: BigInt): BigInt = OpHelper.prepareSigned(v, l)
    def up(v: BigInt): BigInt = OpHelper.unprepare(v, l)
    def shiftMask = {
      var i = 1
      while (i < l) { i <<= 1 }
      i - 1
    }
    def checkDivByZero(): Unit = {
      if (op2v == 0) throw new UvmDivisionByZeroException(ctx + "Division by zero.")
    }

    up(op match {
      case BinOptr.ADD  => pu(op1v) + pu(op2v)
      case BinOptr.SUB  => pu(op1v) - pu(op2v)
      case BinOptr.MUL  => pu(op1v) * pu(op2v)
      case BinOptr.UDIV => { checkDivByZero(); pu(op1v) / pu(op2v) }
      case BinOptr.SDIV => { checkDivByZero(); ps(op1v) / ps(op2v) }
      case BinOptr.UREM => { checkDivByZero(); pu(op1v) % pu(op2v) }
      case BinOptr.SREM => { checkDivByZero(); ps(op1v) % ps(op2v) }
      case BinOptr.SHL  => pu(op1v) << (op2v.intValue & shiftMask)
      case BinOptr.LSHR => pu(op1v) >> (op2v.intValue & shiftMask)
      case BinOptr.ASHR => ps(op1v) >> (op2v.intValue & shiftMask)
      case BinOptr.AND  => pu(op1v) & pu(op2v)
      case BinOptr.OR   => pu(op1v) | pu(op2v)
      case BinOptr.XOR  => pu(op1v) ^ pu(op2v)
      case _            => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for integer.".format(op))
    })
  }

  def floatBinOp(op: BinOptr.BinOptr, op1v: Float, op2v: Float, ctx: => String): Float = {
    op match {
      case BinOptr.FADD => op1v + op2v
      case BinOptr.FSUB => op1v - op2v
      case BinOptr.FMUL => op1v * op2v
      case BinOptr.FDIV => op1v / op2v
      case BinOptr.FREM => Math.IEEEremainder(op1v, op2v).toFloat
      case _            => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for float.".format(op))
    }
  }

  def doubleBinOp(op: BinOptr.BinOptr, op1v: Double, op2v: Double, ctx: => String): Double = {
    op match {
      case BinOptr.FADD => op1v + op2v
      case BinOptr.FSUB => op1v - op2v
      case BinOptr.FMUL => op1v * op2v
      case BinOptr.FDIV => op1v / op2v
      case BinOptr.FREM => Math.IEEEremainder(op1v, op2v)
      case _            => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for double.".format(op))
    }
  }

  def intCmp(op: CmpOptr.CmpOptr, l: Int, op1v: BigInt, op2v: BigInt, ctx: => String): Boolean = {
    def pu(v: BigInt): BigInt = OpHelper.prepareUnsigned(v, l)
    def ps(v: BigInt): BigInt = OpHelper.prepareSigned(v, l)

    op match {
      case CmpOptr.EQ  => pu(op1v) == pu(op2v)
      case CmpOptr.NE  => pu(op1v) != pu(op2v)
      case CmpOptr.UGT => pu(op1v) > pu(op2v)
      case CmpOptr.UGE => pu(op1v) >= pu(op2v)
      case CmpOptr.ULT => pu(op1v) < pu(op2v)
      case CmpOptr.ULE => pu(op1v) <= pu(op2v)
      case CmpOptr.SGT => ps(op1v) > ps(op2v)
      case CmpOptr.SGE => ps(op1v) >= ps(op2v)
      case CmpOptr.SLT => ps(op1v) < ps(op2v)
      case CmpOptr.SLE => ps(op1v) <= ps(op2v)
      case _           => throw new UvmRuntimeException(ctx + "Comparison operator %s not suitable for integers".format(op))
    }
  }

  def floatCmp(op: CmpOptr.CmpOptr, op1v: Float, op2v: Float, ctx: => String): Boolean = {
    import java.lang.Float.isNaN
    def ord = !isNaN(op1v) && !isNaN(op2v)
    def uno = isNaN(op1v) || isNaN(op2v)
    op match {
      case CmpOptr.FTRUE  => true
      case CmpOptr.FFALSE => false
      case CmpOptr.FOEQ   => ord && op1v == op2v
      case CmpOptr.FONE   => ord && op1v != op2v
      case CmpOptr.FOGT   => ord && op1v > op2v
      case CmpOptr.FOGE   => ord && op1v >= op2v
      case CmpOptr.FOLT   => ord && op1v < op2v
      case CmpOptr.FOLE   => ord && op1v <= op2v
      case CmpOptr.FORD   => ord
      case CmpOptr.FUEQ   => uno || op1v == op2v
      case CmpOptr.FUNE   => uno || op1v != op2v
      case CmpOptr.FUGT   => uno || op1v > op2v
      case CmpOptr.FUGE   => uno || op1v >= op2v
      case CmpOptr.FULT   => uno || op1v < op2v
      case CmpOptr.FULE   => uno || op1v <= op2v
      case CmpOptr.FUNO   => uno
      case _              => throw new UvmRuntimeException(ctx + "Comparison operator %s is not suitable for float.".format(op))
    }
  }

  def doubleCmp(op: CmpOptr.CmpOptr, op1v: Double, op2v: Double, ctx: => String): Boolean = {
    import java.lang.Double.isNaN
    def ord = !isNaN(op1v) && !isNaN(op2v)
    def uno = isNaN(op1v) || isNaN(op2v)
    op match {
      case CmpOptr.FTRUE  => true
      case CmpOptr.FFALSE => false
      case CmpOptr.FOEQ   => ord && op1v == op2v
      case CmpOptr.FONE   => ord && op1v != op2v
      case CmpOptr.FOGT   => ord && op1v > op2v
      case CmpOptr.FOGE   => ord && op1v >= op2v
      case CmpOptr.FOLT   => ord && op1v < op2v
      case CmpOptr.FOLE   => ord && op1v <= op2v
      case CmpOptr.FORD   => ord
      case CmpOptr.FUEQ   => uno || op1v == op2v
      case CmpOptr.FUNE   => uno || op1v != op2v
      case CmpOptr.FUGT   => uno || op1v > op2v
      case CmpOptr.FUGE   => uno || op1v >= op2v
      case CmpOptr.FULT   => uno || op1v < op2v
      case CmpOptr.FULE   => uno || op1v <= op2v
      case CmpOptr.FUNO   => uno
      case _              => throw new UvmRuntimeException(ctx + "Comparison operator %s is not suitable for double.".format(op))
    }
  }
}
