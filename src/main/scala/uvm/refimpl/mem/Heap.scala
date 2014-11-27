package uvm.refimpl.mem

import java.util.concurrent.locks._
import Heap._
import uvm.refimpl.UvmRefImplException

object Heap {
  protected val MUTATOR_RUNNING = 0
  protected val DOING_GC = 1
}

abstract class Heap {
  protected val lock: Lock = new ReentrantLock()

  protected val gcCanStart: Condition = lock.newCondition()

  protected val gcFinished: Condition = lock.newCondition()

  protected var gcState: Int = _

  protected var mustFreeSpace: Boolean = _

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
      while (gcState != MUTATOR_RUNNING) {
        try {
          gcFinished.await()
        } catch {
          case e: InterruptedException => throw new UvmRefImplException("Interrupted while waiting for GC. Stop.")
        }
      }
    } finally {
      lock.unlock()
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

  def makeMutator(): Mutator

  def getMustFreeSpace(): Boolean = mustFreeSpace
}
