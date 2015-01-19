package uvm.refimpl.itpr

import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.MemorySupport
import AtomicRMWOptr._

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
      case BinOptr.ADD => pu(op1v) + pu(op2v)
      case BinOptr.SUB => pu(op1v) - pu(op2v)
      case BinOptr.MUL => pu(op1v) * pu(op2v)
      case BinOptr.UDIV => { checkDivByZero(); pu(op1v) / pu(op2v) }
      case BinOptr.SDIV => { checkDivByZero(); ps(op1v) / ps(op2v) }
      case BinOptr.UREM => { checkDivByZero(); pu(op1v) % pu(op2v) }
      case BinOptr.SREM => { checkDivByZero(); ps(op1v) % ps(op2v) }
      case BinOptr.SHL => pu(op1v) << (op2v.intValue & shiftMask)
      case BinOptr.LSHR => pu(op1v) >> (op2v.intValue & shiftMask)
      case BinOptr.ASHR => ps(op1v) >> (op2v.intValue & shiftMask)
      case BinOptr.AND => pu(op1v) & pu(op2v)
      case BinOptr.OR => pu(op1v) | pu(op2v)
      case BinOptr.XOR => pu(op1v) ^ pu(op2v)
      case _ => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for integer.".format(op))
    })
  }

  def floatBinOp(op: BinOptr.BinOptr, op1v: Float, op2v: Float, ctx: => String): Float = {
    op match {
      case BinOptr.FADD => op1v + op2v
      case BinOptr.FSUB => op1v - op2v
      case BinOptr.FMUL => op1v * op2v
      case BinOptr.FDIV => op1v / op2v
      case BinOptr.FREM => Math.IEEEremainder(op1v, op2v).toFloat
      case _ => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for float.".format(op))
    }
  }

  def doubleBinOp(op: BinOptr.BinOptr, op1v: Double, op2v: Double, ctx: => String): Double = {
    op match {
      case BinOptr.FADD => op1v + op2v
      case BinOptr.FSUB => op1v - op2v
      case BinOptr.FMUL => op1v * op2v
      case BinOptr.FDIV => op1v / op2v
      case BinOptr.FREM => Math.IEEEremainder(op1v, op2v)
      case _ => throw new UvmRuntimeException(ctx + "Binary operator %s is not suitable for double.".format(op))
    }
  }

  def intCmp(op: CmpOptr.CmpOptr, l: Int, op1v: BigInt, op2v: BigInt, ctx: => String): Boolean = {
    def pu(v: BigInt): BigInt = OpHelper.prepareUnsigned(v, l)
    def ps(v: BigInt): BigInt = OpHelper.prepareSigned(v, l)

    op match {
      case CmpOptr.EQ => pu(op1v) == pu(op2v)
      case CmpOptr.NE => pu(op1v) != pu(op2v)
      case CmpOptr.UGT => pu(op1v) > pu(op2v)
      case CmpOptr.UGE => pu(op1v) >= pu(op2v)
      case CmpOptr.ULT => pu(op1v) < pu(op2v)
      case CmpOptr.ULE => pu(op1v) <= pu(op2v)
      case CmpOptr.SGT => ps(op1v) > ps(op2v)
      case CmpOptr.SGE => ps(op1v) >= ps(op2v)
      case CmpOptr.SLT => ps(op1v) < ps(op2v)
      case CmpOptr.SLE => ps(op1v) <= ps(op2v)
      case _ => throw new UvmRuntimeException(ctx + "Comparison operator %s not suitable for integers".format(op))
    }
  }

  def floatCmp(op: CmpOptr.CmpOptr, op1v: Float, op2v: Float, ctx: => String): Boolean = {
    import java.lang.Float.isNaN
    def ord = !isNaN(op1v) && !isNaN(op2v)
    def uno = isNaN(op1v) || isNaN(op2v)
    op match {
      case CmpOptr.FTRUE => true
      case CmpOptr.FFALSE => false
      case CmpOptr.FOEQ => ord && op1v == op2v
      case CmpOptr.FONE => ord && op1v != op2v
      case CmpOptr.FOGT => ord && op1v > op2v
      case CmpOptr.FOGE => ord && op1v >= op2v
      case CmpOptr.FOLT => ord && op1v < op2v
      case CmpOptr.FOLE => ord && op1v <= op2v
      case CmpOptr.FORD => ord
      case CmpOptr.FUEQ => uno || op1v == op2v
      case CmpOptr.FUNE => uno || op1v != op2v
      case CmpOptr.FUGT => uno || op1v > op2v
      case CmpOptr.FUGE => uno || op1v >= op2v
      case CmpOptr.FULT => uno || op1v < op2v
      case CmpOptr.FULE => uno || op1v <= op2v
      case CmpOptr.FUNO => uno
      case _ => throw new UvmRuntimeException(ctx + "Comparison operator %s is not suitable for float.".format(op))
    }
  }

  def doubleCmp(op: CmpOptr.CmpOptr, op1v: Double, op2v: Double, ctx: => String): Boolean = {
    import java.lang.Double.isNaN
    def ord = !isNaN(op1v) && !isNaN(op2v)
    def uno = isNaN(op1v) || isNaN(op2v)
    op match {
      case CmpOptr.FTRUE => true
      case CmpOptr.FFALSE => false
      case CmpOptr.FOEQ => ord && op1v == op2v
      case CmpOptr.FONE => ord && op1v != op2v
      case CmpOptr.FOGT => ord && op1v > op2v
      case CmpOptr.FOGE => ord && op1v >= op2v
      case CmpOptr.FOLT => ord && op1v < op2v
      case CmpOptr.FOLE => ord && op1v <= op2v
      case CmpOptr.FORD => ord
      case CmpOptr.FUEQ => uno || op1v == op2v
      case CmpOptr.FUNE => uno || op1v != op2v
      case CmpOptr.FUGT => uno || op1v > op2v
      case CmpOptr.FUGE => uno || op1v >= op2v
      case CmpOptr.FULT => uno || op1v < op2v
      case CmpOptr.FULE => uno || op1v <= op2v
      case CmpOptr.FUNO => uno
      case _ => throw new UvmRuntimeException(ctx + "Comparison operator %s is not suitable for double.".format(op))
    }
  }
}

object MemoryOperations {
  def load(ty: Type, loc: Word, br: ValueBox, microVM: MicroVM): Unit = {
    def loadScalar(ty: Type, loc: Word, br: ValueBox): Unit = ty match {
      case TypeInt(l) =>
        val bi: BigInt = l match {
          case 8 => MemorySupport.loadByte(loc)
          case 16 => MemorySupport.loadShort(loc)
          case 32 => MemorySupport.loadInt(loc)
          case 64 => MemorySupport.loadLong(loc)
          case _ => throw new UnimplementedOprationException("Loading int of length %d is not supported".format(l))
        }
        br.asInstanceOf[BoxInt].value = OpHelper.unprepare(bi, l)
      case _: TypeFloat =>
        val fv = MemorySupport.loadFloat(loc)
        br.asInstanceOf[BoxFloat].value = fv
      case _: TypeDouble =>
        val dv = MemorySupport.loadDouble(loc)
        br.asInstanceOf[BoxDouble].value = dv
      case _: TypeRef =>
        val addr = MemorySupport.loadLong(loc)
        br.asInstanceOf[BoxRef].objRef = addr
      case _: TypeIRef =>
        val base = MemorySupport.loadLong(loc)
        val offset = MemorySupport.loadLong(loc + WORD_SIZE_BYTES)
        br.asInstanceOf[BoxIRef].oo = (base, offset)
      case _: TypeFunc =>
        val fid = MemorySupport.loadLong(loc).toInt
        val func = microVM.globalBundle.funcNs.get(fid)
        br.asInstanceOf[BoxFunc].func = func
      case _: TypeThread =>
        val tid = MemorySupport.loadLong(loc).toInt
        val thr = microVM.threadStackManager.getThreadByID(tid)
        br.asInstanceOf[BoxThread].thread = thr
      case _: TypeStack =>
        val sid = MemorySupport.loadLong(loc).toInt
        val sta = microVM.threadStackManager.getStackByID(sid)
        br.asInstanceOf[BoxStack].stack = sta
      case _: TypeTagRef64 =>
        val raw = MemorySupport.loadLong(loc)
        br.asInstanceOf[BoxTagRef64].raw = raw
      case _ => throw new UnimplementedOprationException("Loading of type %s is not supporing".format(ty.getClass.getName))
    }

    ty match {
      case TypeVector(ety, len) =>
        val brs = br.asInstanceOf[BoxVector].values
        val elemSkip = alignUp(sizeOf(ety), alignOf(ety))
        for ((brElem, i) <- brs.zipWithIndex) {
          loadScalar(ety, loc + elemSkip * i, brElem)
        }
      case sty => loadScalar(sty, loc, br)
    }
  }

  def store(ty: Type, loc: Word, nvb: ValueBox, br: ValueBox, microVM: MicroVM): Unit = {
    def storeScalar(ty: Type, loc: Word, nvb: ValueBox, br: ValueBox): Unit = ty match {
      case TypeInt(l) =>
        val bi = nvb.asInstanceOf[BoxInt].value
        l match {
          case 8 => MemorySupport.storeByte(loc, bi.byteValue)
          case 16 => MemorySupport.storeShort(loc, bi.shortValue)
          case 32 => MemorySupport.storeInt(loc, bi.intValue)
          case 64 => MemorySupport.storeLong(loc, bi.longValue)
          case _ => throw new UnimplementedOprationException("Storing int of length %d is not supported".format(l))
        }
      case _: TypeFloat =>
        val fv = nvb.asInstanceOf[BoxFloat].value
        MemorySupport.storeFloat(loc, fv)
      case _: TypeDouble =>
        val dv = nvb.asInstanceOf[BoxDouble].value
        MemorySupport.storeDouble(loc, dv)
      case _: TypeRef =>
        val addr = nvb.asInstanceOf[BoxRef].objRef
        MemorySupport.storeLong(loc, addr)
      case _: TypeIRef =>
        val BoxIRef(base, offset) = nvb.asInstanceOf[BoxIRef]
        MemorySupport.storeLong(loc, base)
        MemorySupport.storeLong(loc + WORD_SIZE_BYTES, offset)
      case _: TypeFunc =>
        val fid = nvb.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0)
        MemorySupport.storeLong(loc, fid.toLong & 0xFFFFFFFFL)
      case _: TypeThread =>
        val tid = nvb.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0)
        MemorySupport.storeLong(loc, tid.toLong & 0xFFFFFFFFL)
      case _: TypeStack =>
        val sid = nvb.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0)
        MemorySupport.storeLong(loc, sid.toLong & 0xFFFFFFFFL)
      case _: TypeTagRef64 =>
        val raw = nvb.asInstanceOf[BoxTagRef64].raw
        MemorySupport.storeLong(loc, raw)
      case _ => throw new UnimplementedOprationException("Storing of type %s is not supporing".format(ty.getClass.getName))
    }

    ty match {
      case TypeVector(ety, len) =>
        val nvbs = nvb.asInstanceOf[BoxVector].values
        val brs = br.asInstanceOf[BoxVector].values
        val elemSkip = alignUp(sizeOf(ety), alignOf(ety))
        for (((brElem, nvbElem), i) <- (brs zip nvbs).zipWithIndex) {
          storeScalar(ety, loc + elemSkip * i, nvbElem, brElem)
        }
      case sty => storeScalar(sty, loc, nvb, br)
    }
  }

  /**
   * Compare exchange. The result (the old value) is written into br. Return true if successful, false otherwise.
   */
  def cmpXchg(ty: Type, loc: Word, eb: ValueBox, db: ValueBox, br: ValueBox, microVM: MicroVM): Boolean = {
    ty match {
      case TypeInt(l) =>
        val ebi = eb.asInstanceOf[BoxInt].value
        val dbi = db.asInstanceOf[BoxInt].value
        val (succ, rbi) = l match {
          case 32 => {
            val (succ2, rv) = MemorySupport.cmpXchgInt(loc, ebi.intValue, dbi.intValue)
            (succ2, BigInt(rv))
          }
          case 64 => {
            val (succ2, rv) = MemorySupport.cmpXchgLong(loc, ebi.longValue, dbi.longValue)
            (succ2, BigInt(rv))
          }
          case _ => throw new UnimplementedOprationException("CmpXchg on int of length %d is not supported".format(l))
        }
        br.asInstanceOf[BoxInt].value = OpHelper.unprepare(rbi, l)
        succ
      case _: TypeRef =>
        val el = eb.asInstanceOf[BoxRef].objRef
        val dl = db.asInstanceOf[BoxRef].objRef
        val (succ, rl) = MemorySupport.cmpXchgLong(loc, el, dl)
        br.asInstanceOf[BoxRef].objRef = rl
        succ
      case _: TypeIRef =>
        val BoxIRef(el, eh) = eb.asInstanceOf[BoxIRef]
        val BoxIRef(dl, dh) = db.asInstanceOf[BoxIRef]
        val (succ, (rl, rh)) = MemorySupport.cmpXchgI128(loc, (el, eh), (dl, dh))
        br.asInstanceOf[BoxIRef].oo = (rl, rh)
        succ
      case _: TypeFunc =>
        val el = eb.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(loc, el, dl)
        val rf = microVM.globalBundle.funcNs.get(rl.toInt)
        br.asInstanceOf[BoxFunc].func = rf
        succ
      case _: TypeThread =>
        val el = eb.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(loc, el, dl)
        val rt = microVM.threadStackManager.getThreadByID(rl.toInt)
        br.asInstanceOf[BoxThread].thread = rt
        succ
      case _: TypeStack =>
        val el = eb.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(loc, el, dl)
        val rs = microVM.threadStackManager.getStackByID(rl.toInt)
        br.asInstanceOf[BoxStack].stack = rs
        succ
      case _ => throw new UnimplementedOprationException("CmpXchg of type %s is not supporing".format(ty.getClass.getName))
    }
  }

  def atomicRMW(ty: Type, op: AtomicRMWOptr, loc: Word, ob: ValueBox, br: ValueBox, microVM: MicroVM): Unit = {
    ty match {
      case TypeInt(l) =>
        val obi = ob.asInstanceOf[BoxInt].value
        val rbi: BigInt = l match {
          case 32 => MemorySupport.atomicRMWInt(op, loc, obi.intValue)
          case 64 => MemorySupport.atomicRMWLong(op, loc, obi.longValue)
          case _ => throw new UnimplementedOprationException("AtomicRMW on int of length %d is not supported".format(l))
        }
        br.asInstanceOf[BoxInt].value = OpHelper.unprepare(rbi, l)
      case _ =>
        if (op != XCHG) {
          throw new UnimplementedOprationException("AtomicRMW operation other than XCHG only supports int. %s found.".format(ty.getClass.getName))
        } else {
          ty match {
            case _: TypeRef =>
              val ol = ob.asInstanceOf[BoxRef].objRef
              val rl = MemorySupport.atomicRMWLong(op, loc, ol)
              br.asInstanceOf[BoxRef].objRef = rl
            case _: TypeIRef =>
              val BoxIRef(ol, oh) = ob.asInstanceOf[BoxIRef]
              val (rl, rh) = MemorySupport.xchgI128(loc, (ol, oh))
              br.asInstanceOf[BoxIRef].oo = (rl, rh)
            case _: TypeFunc =>
              val ol = ob.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, loc, ol)
              val rf = microVM.globalBundle.funcNs.get(rl.toInt)
              br.asInstanceOf[BoxFunc].func = rf
            case _: TypeThread =>
              val ol = ob.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, loc, ol)
              val rt = microVM.threadStackManager.getThreadByID(rl.toInt)
              br.asInstanceOf[BoxThread].thread = rt
            case _: TypeStack =>
              val ol = ob.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, loc, ol)
              val rs = microVM.threadStackManager.getStackByID(rl.toInt)
              br.asInstanceOf[BoxStack].stack = rs
            case _: TypeTagRef64 =>
              val ol = ob.asInstanceOf[BoxTagRef64].raw
              val rl = MemorySupport.atomicRMWLong(op, loc, ol)
              br.asInstanceOf[BoxTagRef64].raw = rl
            case _ =>
              throw new UnimplementedOprationException("AtomicRMW XCHG of type %s is not supporing".format(ty.getClass.getName))
          }
        }
    }
  }

  /**
   * Check if a memory location still holds a particular value. Used by futex.
   */
  def cmpInt(len: Int, loc: Word, expected: BoxInt): Boolean = len match {
    case 64 => {
      val expNum = OpHelper.prepareSigned(expected.value, len).longValue
      val actualNum = MemorySupport.loadLong(loc)
      expNum == actualNum
    }
    case 32 => {
      val expNum = OpHelper.prepareSigned(expected.value, len).intValue
      val actualNum = MemorySupport.loadInt(loc)
      expNum == actualNum
    }
    case _ => throw new UnimplementedOprationException("Futex of %d bit int is not supported".format(len))
  }
}

