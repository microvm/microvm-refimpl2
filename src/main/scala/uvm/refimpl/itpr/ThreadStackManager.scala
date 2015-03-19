package uvm.refimpl.itpr

import uvm.FuncVer
import uvm.refimpl.MicroVM
import uvm.refimpl.mem._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object ThreadStackManager {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class ThreadStackManager(microVM: MicroVM) {
  import ThreadStackManager._

  private val stackRegistry = new HashMap[Int, InterpreterStack]()

  private val threadRegistry = new HashMap[Int, InterpreterThread]()

  def getStackByID(id: Int): Option[InterpreterStack] = stackRegistry.get(id)

  def getThreadByID(id: Int): Option[InterpreterThread] = threadRegistry.get(id)

  def iterateAllLiveStacks: Iterable[InterpreterStack] = stackRegistry.values.filter(_.state != StackState.Dead)

  private var nextStackID: Int = 1

  private def makeStackID(): Int = { val id = nextStackID; nextStackID += 1; id }

  private var nextThreadID: Int = 1

  private def makeThreadID(): Int = { val id = nextThreadID; nextThreadID += 1; id }

  val futexManager = new FutexManager

  /**
   * Create a new stack with function and args as the stack-bottom function and its arguments.
   * <p>
   * About mutator: "Bring your own mutator!" A mutator object is needed to allocate the stack memory. This means all
   * callers of the newStack function must have a mutator. Currently they are either ClientAgents which can create stack
   * via the "new_stack" message or micro VM threads (the InterpreterThread class) which can execute the NEWSTACK
   * instruction.
   */
  def newStack(funcVer: FuncVer, args: Seq[ValueBox], mutator: Mutator): InterpreterStack = {
    val stackMemory = microVM.memoryManager.makeStackMemory(mutator)
    val id = makeStackID()
    val sta = new InterpreterStack(id, stackMemory, funcVer, args)
    stackRegistry.put(id, sta)
    sta
  }

  /**
   * Create a new thread, bind to a given stack.
   */
  def newThread(stack: InterpreterStack): InterpreterThread = {
    val mutator = microVM.memoryManager.makeMutator()
    val id = makeThreadID()
    val thr = new InterpreterThread(id, microVM, stack, mutator)
    threadRegistry.put(id, thr)
    thr
  }

  def joinAll() {
    var someRunning: Boolean = false
    var someWaiting: Boolean = false

    var continue: Boolean = false

    do {
      futexManager.futexWakeTimeout()

      someRunning = false
      someWaiting = false
      val curThreads = threadRegistry.values.toList
      for (thr2 <- curThreads) {
        if (thr2.isRunning)
          if (thr2.isFutexWaiting) {
            someWaiting = thr2.isFutexWaiting || someWaiting
          } else {
            thr2.step()
            someRunning = thr2.isRunning || someRunning
          }
      }

      continue = if (someRunning) {
        true
      } else {
        if (someWaiting) {
          futexManager.nextWakeup match {
            case Some(nextWakeup) => {
              val now = System.currentTimeMillis() * 1000000L
              val sleep = nextWakeup - now
              val sleepMillis = sleep / 1000000L
              val sleepNanos = sleep % 1000000L
              logger.debug("Waiting for futex. Now: %d, next wake up: %d, sleep: %d".format(now, nextWakeup, sleep))
              Thread.sleep(sleepMillis, sleepNanos.toInt)
              true
            }
            case None => {
              logger.error("No threads are running. No threads are waiting for futex with timer. This is a deadlock situation.")
              false
            }
          }
        } else {
          false
        }
      }
    } while (continue)
  }

  def joinThread(thr: InterpreterThread) {
    while (thr.isRunning) {
      val curThreads = threadRegistry.values.toList
      for (thr2 <- curThreads) {
        thr2.step()
      }
    }
  }
}
