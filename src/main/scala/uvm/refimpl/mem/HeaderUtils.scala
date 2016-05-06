package uvm.refimpl.mem

import uvm.types.Type
import uvm.refimpl.MicroVM
import uvm.refimpl.mem.TypeSizes.Word
import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory

/**
 * Accessors to object headers. Require MemorySupport. 
 */
object HeaderUtils extends StrictLogging {

  def postAllocScalar(addr: Word, tag: Word)(implicit memorySupport: MemorySupport) {
    setTag(addr, tag)
  }

  def postAllocHybrid(addr: Word, tag: Word, len: Word)(implicit memorySupport: MemorySupport) {
    postAllocScalar(addr, tag)
    setVarLength(addr, len)
  }

  def getTag(objRef: Word)(implicit memorySupport: MemorySupport): Word = {
    memorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG)
  }

  def getVarLength(objRef: Word)(implicit memorySupport: MemorySupport): Word = {
    memorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH)
  }

  def setTag(objRef: Word, tag: Word)(implicit memorySupport: MemorySupport) {
    logger.debug("Storing tag 0x%x at addr 0x%x".format(tag, objRef + TypeSizes.GC_HEADER_OFFSET_TAG))
    memorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG, tag)
  }

  def setVarLength(objRef: Word, len: Word)(implicit memorySupport: MemorySupport) {
    logger.debug("Storing varLength 0x%x at addr 0x%x".format(len, objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH))
    memorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH, len)
  }

  def getTypeID(tag: Word): Int = (tag & 0x00000000ffffffffL).toInt

  def getType(microVM: MicroVM, tag: Word): Type = {
    val typeID = getTypeID(tag)
    microVM.globalBundle.typeNs(typeID)
  }

  def getForwardedDest(oldHeader: Word): Word = oldHeader & 0x0000ffffffffffffL
}
