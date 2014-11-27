package uvm.refimpl.mem

abstract class Collector extends Runnable() {

  override def run() {
    try {
      while (true) {
        park()
        collect()
      }
    } catch {
      case e: Exception => {
        System.err.println("Error thrown from collection thread.")
        e.printStackTrace()
        System.exit(1)
      }
    }
  }

  private def park() {
    getHeap.collectorWaitForGCStart()
  }

  protected def getHeap(): Heap

  protected def collect(): Unit
}
