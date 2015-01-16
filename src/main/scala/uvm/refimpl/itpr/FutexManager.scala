package uvm.refimpl.itpr

import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.{ HashSet, HashMap, MultiMap, ListBuffer, Set, TreeSet }
import scala.math.Ordering

/**
 * This class manages Futexes.
 */
class FutexManager {
  case class WaitRecord(val objRef: Word, val offset: Word, val loc: Word, val thread: InterpreterThread, val autoWakeup: Option[Long])

  type WaitingQueueType = ListBuffer[WaitRecord]
  type WaitingQueuesType = HashMap[Word, WaitingQueueType]

  /** One waiting queue for each memory location (address). */
  private val waitingQueues = new WaitingQueuesType()

  /** Maps each object address to all its internal locations. Non-heap locations has objRef==0. */
  private val objIndex = new HashMap[Word, Set[Word]] with MultiMap[Word, Word]

  /** Waiting threads with timeout */
  private val timeoutSet = new TreeSet[WaitRecord]()(Ordering.by(_.autoWakeup.get))

  def futexWaitNoCheck(objRef: Word, offset: Word, thread: InterpreterThread, maybeTimeout: Option[Long] = None): Unit = {
    thread.isFutexWaiting = true

    val loc = objRef + offset
    val autoWakeup = maybeTimeout.map { timeout =>
      val now = System.currentTimeMillis()
      timeout + now * 1000000L
    }
    val wr = WaitRecord(objRef, offset, loc, thread, autoWakeup)

    val q = waitingQueues.getOrElseUpdate(loc, new WaitingQueueType)
    q.append(wr)

    objIndex.addBinding(objRef, loc)

    if (autoWakeup.isDefined) {
      timeoutSet.add(wr)
    }
  }

  def futexWake(objRef: Word, offset: Word, nThread: Int): Unit = {
    val loc = objRef + offset

    waitingQueues.get(loc).foreach { q =>
      val wrs = q.take(nThread)
      for (wr <- wrs) {
        wr.thread.isFutexWaiting = false
        timeoutSet.remove(wr)
      }
      if (wrs.size <= q.size) {
        waitingQueues.remove(loc)
        objIndex.removeBinding(objRef, loc)
      } else {
        q.remove(0, wrs.size)
      }
    }
  }

  def futexWakeTimeout(): Unit = {
    val now = System.currentTimeMillis() * 1000000L

    while (!timeoutSet.isEmpty) {
      val first = timeoutSet.firstKey
      if (first.autoWakeup.get <= now) {
        first.thread.isFutexWaiting = false
        timeoutSet.remove(first)
      }
    }
  }

  def futexRequeue(objRefSrc: Word, offsetSrc: Word, objRefDst: Word, offsetDst: Word, nThread: Int): Unit = {
    val locSrc = objRefSrc + offsetSrc
    val locDst = objRefDst + offsetDst

    waitingQueues.get(locSrc).foreach { q =>
      waitingQueues.remove(locSrc)
      objIndex.removeBinding(objRefSrc, locSrc)

      val wakes = q.take(nThread)
      for (wr <- wakes) {
        wr.thread.isFutexWaiting = false
        timeoutSet.remove(wr)
      }

      val moves = q.drop(nThread)
      if (!moves.isEmpty) {
        objIndex.addBinding(objRefDst, locDst)
        waitingQueues.get(locDst) match {
          case None => {
            waitingQueues(locDst) = moves
          }
          case Some(qDst) => {
            qDst.appendAll(moves)
          }
        }
      }
    }
  }

}