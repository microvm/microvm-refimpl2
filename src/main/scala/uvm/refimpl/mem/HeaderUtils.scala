package uvm.refimpl.mem

import uvm.types.Type
import uvm.refimpl.MicroVM
import uvm.refimpl.mem.TypeSizes.Word
import com.typesafe.scalalogging._
import org.slf4j.LoggerFactory

object HeaderUtils extends StrictLogging {

  def postAllocScalar(addr: Word, tag: Word) {
    setTag(addr, tag)
  }

  def postAllocHybrid(addr: Word, tag: Word, len: Word) {
    postAllocScalar(addr, tag)
    setVarLength(addr, len)
  }

  def getTag(objRef: Word): Word = {
    MemorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG)
  }

  def getVarLength(objRef: Word): Word = {
    MemorySupport.loadLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH)
  }

  def setTag(objRef: Word, tag: Word) {
    logger.debug(s"Storing tag ${tag} at addr ${TypeSizes.GC_HEADER_OFFSET_TAG}")
    MemorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_TAG, tag)
  }

  def setVarLength(objRef: Word, len: Word) {
    MemorySupport.storeLong(objRef + TypeSizes.GC_HEADER_OFFSET_HYBRID_LENGTH, len)
  }

  def getTypeID(tag: Word): Int = (tag & 0x00000000ffffffffL).toInt

  def getType(microVM: MicroVM, tag: Word): Type = {
    val typeID = getTypeID(tag)
    microVM.globalBundle.typeNs(typeID)
  }

  def getForwardedDest(oldHeader: Word): Word = oldHeader & 0x0000ffffffffffffL
}
