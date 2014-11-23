package uvm.refimpl.mem

import uvm.types.Type
import uvm.refimpl.MicroVM
import uvm.refimpl.mem.TypeSizes.Word
import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory

object HeaderUtils extends StrictLogging {

  def postAllocScalar(addr: Word, tag: Long) {
    setTag(addr, tag)
  }

  def postAllocHybrid(addr: Word, tag: Long, len: Long) {
    postAllocScalar(addr, tag)
    setVarLength(addr, len)
  }

  def getTag(objRef: Word): Long = {
    MemorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG)
  }

  def getVarLength(objRef: Word): Long = {
    MemorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH)
  }

  def setTag(objRef: Word, tag: Long) {
    logger.debug(s"Storing tag ${tag} at addr ${TypeSizes.GC_HEADER_OFFSET_TAG}")
    MemorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG, tag)
  }

  def setVarLength(objRef: Word, len: Long) {
    MemorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH, len)
  }

  def getTypeID(tag: Long): Int = (tag & 0x00000000ffffffffL).toInt

  def getType(microVM: MicroVM, tag: Long): Type = {
    val typeID = getTypeID(tag)
    microVM.globalBundle.typeNs(typeID)
  }

  def getForwardedDest(oldHeader: Long): Long = oldHeader & 0x0000ffffffffffffL
}
