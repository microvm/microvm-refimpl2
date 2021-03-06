package uvm.refimpl.mem

import uvm.types._
import uvm.refimpl.UvmRefImplException
import uvm.refimpl.nat.PlatformConstants

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
  type Word = PlatformConstants.Word

  val WORD_SIZE_LOG: Word = PlatformConstants.WORD_SIZE_LOG
  val WORD_SIZE_BITS: Word = PlatformConstants.WORD_SIZE_BITS
  val WORD_SIZE_BYTES: Word = PlatformConstants.WORD_SIZE_BYTES

  val GC_HEADER_SIZE_SCALAR: Word = 8L;
  val GC_HEADER_SIZE_HYBRID: Word = 16L;

  val GC_HEADER_OFFSET_TAG: Word = -8L;
  val GC_HEADER_OFFSET_HYBRID_LENGTH: Word = -16L;

  val MARK_MASK = 0x4000000000000000L

  val MOVE_MASK = 0x8000000000000000L

  def sizeOf(ty: Type): Word = ty match {
    case TypeInt(l)            => intBitsToBytes(l)
    case _: TypeFloat          => 4L
    case _: TypeDouble         => 8L
    case _: TypeRef            => WORD_SIZE_BYTES
    case _: TypeIRef           => 2L * WORD_SIZE_BYTES
    case _: TypeWeakRef        => WORD_SIZE_BYTES
    case t @ TypeStruct(ftys)  => structPrefixSizeOf(t, ftys.size)
    case t @ TypeArray(et, l)  => seqPrefixSizeOf(t, l)
    case _: TypeHybrid         => throw new IllegalArgumentException("Hybrid should use hybridSizeOf to probe size")
    case _: TypeVoid           => 0L
    case _: TypeFuncRef        => WORD_SIZE_BYTES
    case _: TypeThreadRef      => WORD_SIZE_BYTES
    case _: TypeStackRef       => WORD_SIZE_BYTES
    case _: TypeTagRef64       => 8L
    case t @ TypeVector(et, l) => seqPrefixSizeOf(t, l)
    case _: TypeUPtr           => WORD_SIZE_BYTES
    case _: TypeUFuncPtr       => WORD_SIZE_BYTES
  }

  def alignOf(ty: Type): Word = ty match {
    case TypeStruct(ftys) => ftys.map(alignOf).max
    case TypeArray(et, _) => alignOf(et)
    case _: TypeHybrid    => throw new IllegalArgumentException("Hybrid should use hybridAlignOf to probe alignment")
    case _: TypeVoid      => 1L
    case _                => sizeOf(ty)
  }

  def hybridSizeOf(ty: TypeHybrid, len: Word): Word = {
    val fixedSize = structPrefixSizeOf(ty, ty.fieldTys.size)
    val varAlign = alignOf(ty.varTy)
    val varSize = shiftOffsetOf(ty.varTy, len)
    val size = alignUp(fixedSize, varAlign) + varSize
    return size
  }

  def hybridAlignOf(ty: TypeHybrid, len: Word): Word = {
    val fieldAligns = ty.fieldTys.map(alignOf)
    val varAlign = alignOf(ty.varTy)
    val align = fieldAligns.foldLeft(varAlign)(Math.max)
    return align
  }

  def fieldOffsetOf(ty: AbstractStructType, index: Int): Word = {
    val fieldType = ty.fieldTys(index)
    val fieldAlign = alignOf(fieldType)
    val prefixSize = structPrefixSizeOf(ty, index)
    val offset = alignUp(prefixSize, fieldAlign)
    return offset
  }

  def elemOffsetOf(ty: AbstractSeqType, length: Word): Word = {
    return seqPrefixSizeOf(ty, length)
  }

  def shiftOffsetOf(ty: Type, index: Word): Word = {
    return alignUp(sizeOf(ty), alignOf(ty)) * index
  }

  def varPartOffsetOf(ty: TypeHybrid): Word = {
    return alignUp(structPrefixSizeOf(ty, ty.fieldTys.length), alignOf(ty.varTy))
  }

  def structPrefixSizeOf(ty: AbstractStructType, prefixLen: Int): Word = {
    val sz = ty.fieldTys.take(prefixLen).foldLeft(0L) { (oldSz, nextTy) =>
      alignUp(oldSz, alignOf(nextTy)) + sizeOf(nextTy)
    }
    return sz
  }

  def seqPrefixSizeOf(ty: AbstractSeqType, length: Word): Word = {
    return shiftOffsetOf(ty.elemTy, length)
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

  implicit class MagicalWord(val word: Word) extends AnyVal {
    def alignUpAndAdd(align: Word, size: Word): Word = {
      return alignUp(word, align) + size
    }
  }
}