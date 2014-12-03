package uvm.refimpl.mem.bumppointer

import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.scanning._
import uvm.types._
import uvm.refimpl.mem.TypeSizes._
import com.typesafe.scalalogging.StrictLogging
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.mem.scanning.RefFieldHandler

object RewindableBumpPointerAllocator {
  val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class RewindableBumpPointerAllocator(val begin: Word, val extend: Word, val microVM: MicroVM)
  extends Allocator {
  import RewindableBumpPointerAllocator._

  var top: Word = begin

  override def alloc(size: Word, align: Word, headerSize: Word): Word = {
    val dataStart = top + WORD_SIZE_BYTES
    val iRef = dataStart + headerSize
    val dataEnd = iRef + size
    val nextTop = TypeSizes.alignUp(dataEnd, WORD_SIZE_BYTES)
    if (nextTop >= begin + extend) {
      throw new UvmRefImplException("Stack overflow or insufficient global memory.")
    }
    logger.debug(s"alloc(${size}, ${align}, ${headerSize}) top=${top} iRef=${iRef} nextTop=${nextTop}")
    MemUtils.zeroRegion(dataStart, nextTop - dataStart)
    MemorySupport.storeLong(nextTop, iRef)
    top = nextTop
    iRef
  }

  def rewind(newTop: Word) {
    top = newTop
  }

  def traverseFields(handler: RefFieldHandler) {
    logger.debug("Traversing a RewindableBumpPointerAllocator")
    var curTopLoc = top
    var reachBottom = false
    while (!reachBottom) {
      logger.debug("curTopLoc is 0x%x".format(curTopLoc))
      val iRef = MemorySupport.loadLong(curTopLoc)
      logger.debug("iRef is 0x%x".format(iRef))
      if (iRef != 0) {
        val hdr = HeaderUtils.getTag(iRef)
        val typeID = (hdr & 0xffffffffL).toInt
        logger.debug("hdr=0x%x, typeID=0x%x".format(hdr, typeID))
        val ty = microVM.globalBundle.typeNs(typeID)
        logger.debug("type=%s: %s".format(ty.repr, ty.toString))
        MemoryDataScanner.scanField(ty, 0, iRef, handler)
        var prevTopLoc: Word = 0L
        prevTopLoc = if (ty.isInstanceOf[TypeHybrid]) {
          iRef - TypeSizes.GC_HEADER_SIZE_HYBRID - WORD_SIZE_BYTES
        } else {
          iRef - TypeSizes.GC_HEADER_SIZE_SCALAR - WORD_SIZE_BYTES
        }
        curTopLoc = prevTopLoc
      } else {
        reachBottom = true
      }
    }
  }
}
