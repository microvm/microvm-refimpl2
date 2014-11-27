package uvm.refimpl.mem.los

import uvm.util.ErrorUtils
import uvm.util.LogUtil
import uvm.util.Logger
import FreeList._
//remove if not needed
import scala.collection.JavaConversions._

object FreeList {

  private val logger = LogUtil.getLogger("FreeList")
}

class FreeList(private var nUnits: Int) {

  private var isUsed: Boolean = new Boolean(nUnits)

  private var isMulti: Boolean = new Boolean(nUnits)

  private var prev: Int = new Int(nUnits)

  private var next: Int = new Int(nUnits)

  private var size: Int = new Int(nUnits)

  private var head: Int = -1

  ErrorUtils.uvmAssert(nUnits > 1, "Why use such a small \"large\" object space?")

  setSizeAndIsUsed(0, nUnits, false)

  link(0)

  def allocate(requiredSize: Int): Int = {
    val region = firstFit(requiredSize)
    if (region != -1) {
      allocInto(region, requiredSize)
      region
    } else {
      -1
    }
  }

  def deallocate(region: Int) {
    deallocAndMerge(region)
  }

  private def firstFit(requiredSize: Int): Int = {
    if (head == -1) {
      -1
    } else {
      var cur = head
      do {
        if (getSize(cur) >= requiredSize) {
          return cur
        }
        cur = next(cur)
      } while (cur != head);
      -1
    }
  }

  private def allocInto(freeStart: Int, allocSize: Int) {
    val thisStart = freeStart
    logger.format("Allocate %d units into %d...", allocSize, thisStart)
    val thisPrev = prev(thisStart)
    val thisNext = next(thisStart)
    val thisSize = getSize(thisStart)
    logger.format("thisPrev = %d", thisPrev)
    logger.format("thisNext = %d", thisNext)
    logger.format("thisSize = %d", thisSize)
    unlink(thisStart)
    setSizeAndIsUsed(thisStart, allocSize, true)
    val newFreeSize = thisSize - allocSize
    if (newFreeSize > 0) {
      val newFreeStart = thisStart + allocSize
      setSizeAndIsUsed(newFreeStart, newFreeSize, false)
      link(newFreeStart)
    }
  }

  private def deallocAndMerge(usedStart: Int) {
    val thisStart = usedStart
    val thisSize = getSize(thisStart)
    val thisLeft = thisStart - 1
    val thisRight = thisStart + thisSize
    var newStart: Int = 0
    var newEnd: Int = 0
    if (thisLeft == -1 || isUsed(thisLeft)) {
      newStart = thisStart
    } else {
      newStart = getStartFromLast(thisLeft)
      unlink(newStart)
    }
    if (thisRight == nUnits || isUsed(thisRight)) {
      newEnd = thisRight
    } else {
      newEnd = thisRight + getSize(thisRight)
      unlink(thisRight)
    }
    val newSize = newEnd - newStart
    setSizeAndIsUsed(newStart, newSize, false)
    link(newStart)
  }

  private def link(st: Int) {
    if (head == -1) {
      head = st
      prev(st) = next(st) = st
    } else {
      val last = prev(head)
      prev(st) = last
      next(st) = head
      next(last) = st
      prev(head) = st
    }
  }

  private def unlink(st: Int) {
    val nxt = next(st)
    if (nxt == st) {
      head = -1
    } else {
      val prv = prev(st)
      next(prv) = nxt
      prev(nxt) = prv
      head = nxt
    }
  }

  private def getStartFromLast(last: Int): Int = last - getSize(last) + 1

  private def getSize(regionStartOrLast: Int): Int = {
    val thisMulti = isMulti(regionStartOrLast)
    val thisSize = if (thisMulti) size(regionStartOrLast) else 1
    thisSize
  }

  private def setSizeAndIsUsed(regionStart: Int, sz: Int, used: Boolean) {
    if (sz == 1) {
      isUsed(regionStart) = used
      isMulti(regionStart) = false
    } else {
      isUsed(regionStart) = used
      isMulti(regionStart) = true
      size(regionStart) = sz
      val regionLast = regionStart + sz - 1
      isUsed(regionLast) = used
      isMulti(regionLast) = true
      size(regionLast) = sz
    }
  }

  def debugPrintList() {
    System.out.format("head=%d", head)
    var multiSkipTo = 0
    for (i <- 0 until nUnits) {
      var multiSkip: String = null
      multiSkip = if (i < multiSkipTo - 1) " SKIPPED" else ""
      System.out.format("%d [%s%s] %d (%d %d)%s", i, if (isUsed(i)) "U" else " ", if (isMulti(i)) "m" else " ", 
        size(i), prev(i), next(i), multiSkip)
      if (i >= multiSkipTo && isMulti(i)) {
        multiSkipTo = i + size(i)
      }
    }
  }
}
