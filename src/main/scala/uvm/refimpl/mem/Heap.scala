package uvm.refimpl.mem

import java.util.concurrent.locks._
import Heap._
import uvm.refimpl.UvmRefImplException

object Heap {
  protected val MUTATOR_RUNNING = 0
  protected val DOING_GC = 1
  protected val GC_ERROR = 2  // Set if an exception is thrown in GC. In this case, an exception is thrown to the (only) mutator thread.
}

abstract class Heap {
  protected val lock: Lock = new ReentrantLock()

  protected val gcCanStart: Condition = lock.newCondition()

  protected val gcFinished: Condition = lock.newCondition()

  protected var gcState: Int = _

  protected var mustFreeSpace: Boolean = _
  
  protected var gcException: Exception = null

  def mutatorTriggerAndWaitForGCEnd(mustFreeSpace: Boolean) {
    lock.lock()
    try {
      triggerGC(mustFreeSpace)
      mutatorWaitForGCEnd()
    } finally {
      lock.unlock()
    }
  }

  private def triggerGC(mustFreeSpace: Boolean) {
    lock.lock()
    try {
      assert((gcState == MUTATOR_RUNNING))
      gcState = DOING_GC
      this.mustFreeSpace = mustFreeSpace
      gcCanStart.signalAll()
    } finally {
      lock.unlock()
    }
  }

  private def mutatorWaitForGCEnd() {
    lock.lock()
    try {
      while (gcState == DOING_GC) {
        try {
          gcFinished.await()
        } catch {
          case e: InterruptedException => throw new UvmRefImplException("Interrupted while waiting for GC. Stop.")
        }
      }
    } finally {
      lock.unlock()
    }
    
    if (gcState == GC_ERROR) {
      throw new UvmRefImplException("Exception thrown in the GC thread.", gcException)
    }
  }

  def untriggerGC() {
    lock.lock()
    try {
      assert((gcState == DOING_GC))
      gcState = MUTATOR_RUNNING
      gcFinished.signalAll()
    } finally {
      lock.unlock()
    }
  }
  
  def gcError(e: Exception) {
    lock.lock()
    try {
      assert((gcState == DOING_GC))
      gcState = GC_ERROR
      gcException = e
      gcFinished.signalAll()
    } finally {
      lock.unlock()
    }
  }
  def collectorWaitForGCStart() {
    lock.lock()
    while (gcState != DOING_GC) {
      try {
        gcCanStart.await()
      } catch {
        case e: InterruptedException => throw new UvmRefImplException("GC thread is interrupted.")
      }
    }
    lock.unlock()
  }

  /** @param who A string that identifies a mutator. Used for debug purpose. */
  def makeMutator(who: String): Mutator

  def getMustFreeSpace(): Boolean = mustFreeSpace
}
