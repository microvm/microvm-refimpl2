package uvm.refimpl.mem

import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object Collector {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

abstract class Collector extends Runnable() {
  import Collector._

  override def run() {
    try {
      while (true) {
        park()
        collect()
      }
    } catch {
      case e: Exception => {
        logger.error("Collector throws an exception.", e)
        heap.gcError(e)
      }
    }
  }

  private def park() {
    heap.collectorWaitForGCStart()
  }

  protected def heap: Heap

  protected def collect(): Unit
}
