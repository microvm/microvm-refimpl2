package uvm.refimpl.mem

import uvm.types._
import TypeSizes._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object Mutator {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

abstract class Mutator(implicit memorySupport: MemorySupport) {
  import Mutator._
  
  def alloc(size: Word, align: Word, headerSize: Word): Word

  def newScalar(ty: Type): Word = {
    val tag = ty.id
    val size = sizeOf(ty)
    val align = alignOf(ty)
    val objAddr = alloc(size, align, GC_HEADER_SIZE_SCALAR)
    HeaderUtils.postAllocScalar(objAddr, tag)
    logger.debug("newScalar: objAddr=%d 0x%x, ty=%s".format(objAddr, objAddr, ty))
    objAddr
  }

  def newHybrid(ty: TypeHybrid, len: Word): Word = {
    val tag = ty.id
    val size = hybridSizeOf(ty, len)
    val align = hybridAlignOf(ty, len)
    val objAddr = alloc(size, align, GC_HEADER_SIZE_HYBRID)
    HeaderUtils.postAllocHybrid(objAddr, tag, len)
    logger.debug("newHybrid: objAddr=%d 0x%x, len=%d 0x%x, ty=%s".format(objAddr, objAddr, len, len, ty))
    objAddr
  }

  def allocaScalar(sm: StackMemory, ty: Type): Word = {
    val tag = ty.id
    val size = sizeOf(ty)
    val align = alignOf(ty)
    val objAddr = sm.alloc(size, align, GC_HEADER_SIZE_SCALAR)
    HeaderUtils.postAllocScalar(objAddr, tag)
    logger.debug("allocaScalar: objAddr=%d 0x%x, ty=%s".format(objAddr, objAddr, ty))
    objAddr
  }

  def allocaHybrid(sm: StackMemory, ty: TypeHybrid, len: Word): Word = {
    val tag = ty.id
    val size = hybridSizeOf(ty, len)
    val align = hybridAlignOf(ty, len)
    val objAddr = sm.alloc(size, align, GC_HEADER_SIZE_HYBRID)
    HeaderUtils.postAllocHybrid(objAddr, tag, len)
    logger.debug("allocaHybrid: objAddr=%d 0x%x, len=%d 0x%x, ty=%s".format(objAddr, objAddr, len, len, ty))
    objAddr
  }

  def close(): Unit
}
