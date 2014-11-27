package uvm.refimpl.mem

import uvm.types._
import TypeSizes._

abstract class Mutator {

  def alloc(size: Word, align: Word, headerSize: Word): Word

  def newScalar(ty: Type): Word = {
    val tag = ty.id
    val size = sizeOf(ty)
    val align = alignOf(ty)
    val objAddr = alloc(size, align, GC_HEADER_SIZE_SCALAR)
    HeaderUtils.postAllocScalar(objAddr, tag)
    objAddr
  }

  def newHybrid(ty: TypeHybrid, len: Word): Word = {
    val tag = ty.id
    val size = hybridSizeOf(ty, len)
    val align = hybridAlignOf(ty, len)
    val objAddr = alloc(size, align, GC_HEADER_SIZE_HYBRID)
    HeaderUtils.postAllocHybrid(objAddr, tag, len)
    objAddr
  }

  def allocaScalar(sm: StackMemory, ty: Type): Word = {
    val tag = ty.id
    val size = sizeOf(ty)
    val align = alignOf(ty)
    val objAddr = sm.alloc(size, align, GC_HEADER_SIZE_SCALAR)
    HeaderUtils.postAllocScalar(objAddr, tag)
    objAddr
  }

  def allocaHybrid(sm: StackMemory, ty: TypeHybrid, len: Word): Word = {
    val tag = ty.id
    val size = hybridSizeOf(ty, len)
    val align = hybridAlignOf(ty, len)
    val objAddr = sm.alloc(size, align, GC_HEADER_SIZE_HYBRID)
    HeaderUtils.postAllocHybrid(objAddr, tag, len)
    objAddr
  }

  def close(): Unit
}
