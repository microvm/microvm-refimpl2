package uvm.refimpl.mem.los

import FreeList._
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object FreeList {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class FreeList(val nUnits: Int) {

  private val isUsed = new Array[Boolean](nUnits)

  private val isMulti = new Array[Boolean](nUnits)

  private val prev = new Array[Int](nUnits)

  private val next = new Array[Int](nUnits)

  private val size = new Array[Int](nUnits)

  private var head: Int = -1 // point to any free region

  assert(nUnits > 1, "Why use such a small \"large\" object space?")

  setSizeAndIsUsed(0, nUnits, false)

  link(0)

  /**
   * Allocate requiredSize contiguous units of resources.
   *
   * @param requiredSize
   *            the required size (in units)
   * @return the index of the first unit of the allocated region, or -1 if not
   *         available.
   */
  def allocate(requiredSize: Int): Int = {
    val region = firstFit(requiredSize)
    if (region != -1) {
      allocInto(region, requiredSize)
      region
    } else {
      -1
    }
  }

  /**
   * Deallocate a previously allocated region.
   *
   * @param region
   *            the index of the first block of the region.
   */
  def deallocate(region: Int) {
    deallocAndMerge(region)
  }

    /**
     * Find the first region that has 'size' units available.
     * 
     * @param requiredSize
     *            the required size (in units)
     * @return the index of the first unit of the region, or -1 if not found.
     */
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

    /**
     * Allocate some units into the beginning of a free region. Shrink it and
     * relink its neighbours.
     * 
     * @param freeUnit
     *            the first unit of the region to allocate in
     * @param allocSize
     *            size (in units) of allocation
     */
  private def allocInto(freeStart: Int, allocSize: Int) {
    val thisStart = freeStart
    logger.debug("Allocate %d units into %d...".format(allocSize, thisStart))
    val thisPrev = prev(thisStart)
    val thisNext = next(thisStart)
    val thisSize = getSize(thisStart)
    logger.debug("thisPrev = %d".format(thisPrev))
    logger.debug("thisNext = %d".format(thisNext))
    logger.debug("thisSize = %d".format(thisSize))
    unlink(thisStart)
    setSizeAndIsUsed(thisStart, allocSize, true)
    val newFreeSize = thisSize - allocSize
    if (newFreeSize > 0) {
      val newFreeStart = thisStart + allocSize
      setSizeAndIsUsed(newFreeStart, newFreeSize, false)
      link(newFreeStart)
    }
  }

    /**
     * Deallocate a used region and join the newly freed region with its
     * neighbours if there are any.
     * 
     * @param usedStart
     *            the first unit of the used region.
     */
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
      prev(st) = st
      next(st) = st
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

    /**
     * Get the size of a region
     * 
     * @param regionStartOrLast
     *            the first or the last block of a region
     * @return the size of the region
     */
   private def getSize(regionStartOrLast: Int): Int = {
    val thisMulti = isMulti(regionStartOrLast)
    val thisSize = if (thisMulti) size(regionStartOrLast) else 1
    thisSize
  }

    /**
     * Set the size of a region. Updates both isMulti and size and updates both
     * ends if multi.
     * 
     * @param regionStart
     *            The first block of a region
     * @param sz
     *            the size of the region.
     */
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
    System.out.println("head=%d".format(head))
    var multiSkipTo = 0
    for (i <- 0 until nUnits) {
      var multiSkip: String = null
      multiSkip = if (i < multiSkipTo - 1) " SKIPPED" else ""
      System.out.println("%d [%s%s] %d (%d %d)%s".format(i, if (isUsed(i)) "U" else " ", if (isMulti(i)) "m" else " ",
        size(i), prev(i), next(i), multiSkip))
      if (i >= multiSkipTo && isMulti(i)) {
        multiSkipTo = i + size(i)
      }
    }
  }
}
