package uvm.refimpl.mem

import uvm.types._
import uvm.refimpl.UvmRefImplException

/**
 * Responsible for object layout.
 * <p>
 * Scalar object:
 *
 * <pre>
 *     8 bytes
 * +----------------+----------------
 * | tag            | payload...
 * +----------------+----------------
 * ^objref-8bytes   ^ objref
 * </pre>
 *
 * Hybrid:
 *
 * <pre>
 *    8 bytes           8 bytes
 * +-----------------+-----------------+------------+--------------
 * | var part length | tag             | fixed part | var part
 * +-----------------+-----------------+------------+--------------
 * ^objref-16bytes   ^objref-8bytes    ^ objref
 * </pre>
 *
 * tag: (not forwarded)
 *
 * <pre>
 *   1bit         1bit       30bit            32bit
 * +-----------+--------+------------------+---------+
 * | moved = 0 | marked | (unused bits...) | type id |
 * +-----------+--------+------------------+---------+
 * 64          63       62                 32        0
 * </pre>
 * (forwarded)
 * <pre>
 *   1bit            15bit               48bits
 * +-----------+------------------+------------------+
 * | moved = 1 | (unused bits...) | destination addr |
 * +-----------+------------------+------------------+
 * 64          63                 48                 0
 * </pre>
 */
object TypeSizes {
  type Word = Long

  val WORD_SIZE_LOG: Word = 6L
  val WORD_SIZE_BITS: Word = 1L << WORD_SIZE_LOG
  val WORD_SIZE_BYTES: Word = 1L << (WORD_SIZE_LOG - 3L)

  val GC_HEADER_SIZE_SCALAR: Word = 8L;
  val GC_HEADER_SIZE_HYBRID: Word = 16L;

  val GC_HEADER_OFFSET_TAG: Word = -8L;
  val GC_HEADER_OFFSET_HYBRID_LENGTH: Word = -16L;

  def sizeOf(ty: Type): Word = ty match {
    case TypeInt(l) => intBitsToBytes(l)
    case _:TypeFloat => 4L
    case _:TypeDouble => 8L
    case _:TypeRef => WORD_SIZE_BYTES
    case _:TypeIRef => 2L * WORD_SIZE_BYTES
    case _:TypeWeakRef => WORD_SIZE_BYTES
    case t @ TypeStruct(ftys) => structPrefixSizeOf(t, ftys.size)
    case t @ TypeArray(et,l) => seqPrefixSizeOf(t, l)
    case _:TypeHybrid => throw new IllegalArgumentException("Hybrid should use hybridSizeOf to probe size")
    case _:TypeVoid => 0L
    case _:TypeFunc => WORD_SIZE_BYTES
    case _:TypeThread => WORD_SIZE_BYTES
    case _:TypeStack => WORD_SIZE_BYTES
    case _:TypeTagRef64 => 8L
    case t @ TypeVector(et,l) => seqPrefixSizeOf(t, l)
  }

  def alignOf(ty: Type): Word = ty match {
    case TypeStruct(ftys) => ftys.map(sizeOf).max
    case TypeArray(et,_) => alignOf(et)
    case _:TypeHybrid => throw new IllegalArgumentException("Hybrid should use hybridAlignOf to probe alignment")
    case _:TypeVoid => 1L
    case _ => sizeOf(ty)
  }

  def hybridSizeOf(ty: TypeHybrid, len: Word): Word = {
    val fixedSize = sizeOf(ty.fixedTy)
    val varAlign = alignOf(ty.varTy)
    val varSize = shiftOffsetOf(ty.varTy, len)
    val size = alignUp(fixedSize, varAlign) + varSize
    return size
  }

  def hybridAlignOf(ty: TypeHybrid, len: Word): Word = {
    val fixedAlign = alignOf(ty.fixedTy)
    val varAlign = alignOf(ty.varTy)
    val align = Math.max(fixedAlign, varAlign)
    return align
  }

  def fieldOffsetOf(ty: TypeStruct, index: Int): Word = {
    val fieldType = ty.fieldTy(index)
    val fieldAlign = alignOf(fieldType)
    val prefixSize = structPrefixSizeOf(ty, index)
    val offset = alignUp(prefixSize, fieldAlign)
    return offset
  }

  def structPrefixSizeOf(ty: TypeStruct, prefixLen: Int): Word = {
    val sz = ty.fieldTy.foldLeft(0L) { (oldSz, nextTy) =>
      alignUp(oldSz, alignOf(nextTy)) + sizeOf(nextTy)
    }
    return sz
  }

  def seqPrefixSizeOf(ty: AbstractSeqType, length: Word): Word = {
    return shiftOffsetOf(ty.elemTy, length)
  }

  def shiftOffsetOf(elemType: Type, index: Word): Word = {
    return alignUp(sizeOf(elemType), alignOf(elemType)) * index
  }

  def nextPowOfTwo(n: Word): Word = {
    var i = 1L
    while (i < n) {
      i <<= 1L
    }
    return i
  }

  def intBitsToBytes(n: Word): Word = {
    val p2 = nextPowOfTwo(n)
    if (p2 < 8L) {
      return 1L;
    } else {
      return p2 / 8L;
    }
  }

  def alignUp(n: Word, alignment: Word): Word = {
    return ((n - 1) & ~(alignment - 1)) + alignment;
  }

  def alignDown(n: Word, alignment: Word): Word = {
    return n & ~(alignment - 1);
  }
}