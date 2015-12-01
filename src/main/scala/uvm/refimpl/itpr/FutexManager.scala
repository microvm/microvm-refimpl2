package uvm.refimpl.itpr

import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.{ HashSet, HashMap, MultiMap, ListBuffer, Set, TreeSet }
import scala.math.Ordering
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object FutexManager {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}
/**
 * This class manages Futexes.
 */
class FutexManager {
  import FutexManager._
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
      val now = System.nanoTime()
      timeout + now
    }
    val wr = WaitRecord(objRef, offset, loc, thread, autoWakeup)

    val q = waitingQueues.getOrElseUpdate(loc, new WaitingQueueType)
    q.append(wr)

    objIndex.addBinding(objRef, loc)

    if (autoWakeup.isDefined) {
      timeoutSet.add(wr)
    }
  }

  def futexWake(objRef: Word, offset: Word, nThread: Int): Int = {
    val loc = objRef + offset

    waitingQueues.get(loc) match {
      case Some(q) => {
        val wrs = q.take(nThread)
        for (wr <- wrs) {
          wakeThread(wr, 0)
        }
        if (wrs.size <= q.size) {
          waitingQueues.remove(loc)
          objIndex.removeBinding(objRef, loc)
        } else {
          q.remove(0, wrs.size)
        }
        wrs.size
      }
      case None => 0
    }
  }

  def futexRequeue(objRefSrc: Word, offsetSrc: Word, objRefDst: Word, offsetDst: Word, nThread: Int): Int = {
    val locSrc = objRefSrc + offsetSrc
    val locDst = objRefDst + offsetDst

    waitingQueues.get(locSrc) match {
      case Some(q) => {
        waitingQueues.remove(locSrc)
        objIndex.removeBinding(objRefSrc, locSrc)

        val wakes = q.take(nThread)
        for (wr <- wakes) {
          wakeThread(wr, 0)
        }

        val moves = q.drop(nThread)
        if (!moves.isEmpty) {
          val newMoves = moves.map {
            case WaitRecord(or, of, lo, th, aw) => WaitRecord(objRefDst, of, locDst, th, aw)
          }
          moves.foreach { wr => if (wr.autoWakeup.isDefined) timeoutSet.remove(wr) }
          newMoves.foreach { wr => if (wr.autoWakeup.isDefined) timeoutSet.add(wr) }

          objIndex.addBinding(objRefDst, locDst)
          waitingQueues.get(locDst) match {
            case None => {
              waitingQueues(locDst) = newMoves
            }
            case Some(qDst) => {
              qDst.appendAll(newMoves)
            }
          }
        }

        wakes.length
      }
      case None => 0
    }
  }

  /** Wake up all threads with their timeout expired. */
  def futexWakeTimeout(): Unit = {
    val now = System.nanoTime()

    for (wr <- timeoutSet.takeWhile(_.autoWakeup.get <= now)) {
      wakeThread(wr, -3)
      waitingQueues(wr.loc) -= wr
    }
  }

  /** Return the time the next thread wakes up (in the unit of nanoTime) */
  def nextWakeup(): Option[Long] = {
    if (!timeoutSet.isEmpty) {
      Some(timeoutSet.firstKey.autoWakeup.get)
    } else {
      None
    }
  }

  /**
   * Adjust waiting queues affected by garbage collection.
   *  <p>
   *  @param getMovement A function that maps an object address to its new address if it is moved. Return None otherwise.
   *  It assumes the new address is not previously occupied. (This shows why a micro virtual machine must be tightly coupled.)
   */
  def afterGCAdjust(getMovement: Word => Option[Word]): Unit = {
    val oldKeys = objIndex.keySet.toList

    for (oor <- oldKeys) {
      getMovement(oor) match {
        case None => {
          logger.debug("Object 0x%x is not moved.".format(oor))
        }
        case Some(nor) => {
          logger.debug("Object 0x%x is moved to 0x%x.".format(oor, nor))
          val delta = nor - oor
          val locs = objIndex(oor)
          val nlocs = for (loc <- locs) yield {
            val nloc = loc + delta
            logger.debug("Moving queue 0x%x to 0x%x".format(loc, nloc))
            val q = waitingQueues(loc)
            val nq = q.map {
              case WaitRecord(or, of, lo, th, aw) => WaitRecord(nor, of, nloc, th, aw)
            }
            waitingQueues.remove(loc)
            waitingQueues(nloc) = nq
            nloc
          }
          objIndex.remove(oor)
          objIndex(nor) = nlocs
        }
      }
    }
  }

  private def wakeThread(wr: WaitRecord, rv: Int): Unit = {
    wr.thread.futexReturn(rv)
    wr.thread.isFutexWaiting = false
    timeoutSet.remove(wr)
  }
}