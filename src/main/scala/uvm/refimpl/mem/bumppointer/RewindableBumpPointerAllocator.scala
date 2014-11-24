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

class RewindableBumpPointerAllocator(val begin: Long, val extend: Long, val microVM: MicroVM)
  extends Allocator {
  import RewindableBumpPointerAllocator._

  var top: Long = begin

  override def alloc(size: Word, align: Word, headerSize: Word): Long = {
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

  def rewind(newTop: Long) {
    top = newTop
  }

  def traverseFields(handler: RefFieldHandler) {
    logger.debug("Traversing a RewindableBumpPointerAllocator")
    var curTopLoc = top
    var reachBottom = false
    while (!reachBottom) {
      logger.debug(s"curTopLoc is ${curTopLoc}")
      val iRef = MemorySupport.loadLong(curTopLoc)
      logger.debug(s"iRef is ${iRef}")
      if (iRef != 0) {
        val hdr = HeaderUtils.getTag(iRef)
        val typeID = (hdr & 0xffffffffL).toInt
        logger.debug(s"hdr=${hdr}, typeID=${typeID}")
        val ty = microVM.globalBundle.typeNs(typeID)
        logger.debug(s"type=${ty.repr}: ${ty.toString}")
        MemoryDataScanner.scanField(ty, 0, iRef, handler)
        var prevTopLoc: Long = 0L
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
