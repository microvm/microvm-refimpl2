package uvm.refimpl.itpr

import uvm.Function
import uvm.refimpl.MicroVM
import uvm.refimpl.mem._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.nat.NativeCallHelper
import uvm.utils.IDFactory
import uvm.refimpl.UvmRuntimeException

/**
 * An object with this trait can be uniquely identified within a MicroVM using an Int ID.
 * <p>
 * This is different from Identified because the ID in this context is used as opaque references to these objects. Currently, these
 * objects are threads, stacks and frame cursors. These are referred by opaque references which, in the memory are represented as numerical
 * IDs.
 */
trait HasID {
  def id: Int
}

/**
 * Keep HasID objects and allow looking them up by their IDs.
 *
 * @param kind A name that indicates what kind of object it holds. For debugging.
 */
class IDObjectKeeper[T <: HasID](val kind: String) {
  val registry = new HashMap[Int, T]()
  val idFactory = new IDFactory(1)

  /** Get an object by its ID. */
  def apply(id: Int) = registry.apply(id)

  /** Get an object by its ID, handle non-existing cases. */
  def get(id: Int) = registry.get(id)

  /** Add an object to the registry. */
  def put(obj: T): Unit = {
    if (registry.put(obj.id, obj).isDefined) {
      throw new UvmRuntimeException("%s of ID %s already exists.".format(kind, obj.id))
    }
  }

  /** Iterate through all objects. */
  def values: Iterable[T] = registry.values

  /** Remove an object from the registry. */
  def remove(obj: T): Unit = {
    val old = registry.remove(obj.id)
    if (old.isDefined && !(old.get eq obj)) {
      throw new UvmRuntimeException("The %s removed is not the same object as the argument. ID: %d".format(kind, obj.id))
    }
  }

  /** Create an ID for a new object of this type. */
  def getID() = idFactory.getID()
}

object ThreadStackManager {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

/**
 * The manager of all Mu threads and stacks. Also responsible for the actual execution of Mu IR code, i.e. as the "boss"
 * of all InterpreterThread instances.
 */
class ThreadStackManager(implicit microVM: MicroVM, nativeCallHelper: NativeCallHelper) {
  import ThreadStackManager._

  val stackRegistry = new IDObjectKeeper[InterpreterStack]("stack")
  val threadRegistry = new IDObjectKeeper[InterpreterThread]("thread")
  val frameCursorRegistry = new IDObjectKeeper[FrameCursor]("framecursor")

  def iterateAllLiveStacks: Iterable[InterpreterStack] = stackRegistry.values.filter(_.state != FrameState.Dead)
  def iterateAllLiveThreads: Iterable[InterpreterThread] = threadRegistry.values.filter(_.isRunning)

  val futexManager = new FutexManager

  /**
   * The current Mu thread that is calling a native function via CCALL.
   */
  var threadCallingNative: Option[InterpreterThread] = None

  /**
   * Create a new stack with function and args as the stack-bottom function and its arguments.
   * <p>
   * About mutator: "Bring your own mutator!" A mutator object is needed to allocate the stack memory. This means all
   * callers of the newStack function must have a mutator. Currently they are either ClientAgents which can create stack
   * via the "new_stack" message or micro VM threads (the InterpreterThread class) which can execute the NEWSTACK
   * instruction.
   */
  def newStack(func: Function, mutator: Mutator): InterpreterStack = {
    val stackMemory = microVM.memoryManager.makeStackMemory(mutator)
    val id = stackRegistry.getID()
    val sta = new InterpreterStack(id, stackMemory, func)
    stackRegistry.put(sta)
    sta
  }

  /**
   * Create a new thread, bind to a given stack.
   */
  def newThread(stack: InterpreterStack, htr: HowToResume): InterpreterThread = {
    val mutator = microVM.memoryManager.makeMutator()
    val id = threadRegistry.getID()
    val thr = new InterpreterThread(id, stack, mutator, htr)
    threadRegistry.put(thr)
    thr
  }

  //// Frame cursors related operations

  private def createAndAddFrameCursor(id: Int, stack: InterpreterStack, frame: InterpreterFrame): FrameCursor = {
    val fc = new FrameCursor(id, stack, frame)
    frameCursorRegistry.put(fc)
    stack.frameCursors.add(fc)
    fc

  }

  /**
   * Create a new frame cursor for a stack.
   */
  def newFrameCursor(stack: InterpreterStack): FrameCursor = {
    val id = frameCursorRegistry.getID()
    val frame = stack.top
    createAndAddFrameCursor(id, stack, frame)
  }

  /**
   * Copy a frame cursor.
   */
  def copyCursor(cursor: FrameCursor): FrameCursor = {
    val id = frameCursorRegistry.getID()
    val stack = cursor.stack
    val frame = cursor.frame
    createAndAddFrameCursor(id, stack, frame)
  }

  /**
   * Copy a frame cursor.
   */
  def closeCursor(cursor: FrameCursor): Unit = {
    cursor.stack.frameCursors.remove(cursor)
    frameCursorRegistry.remove(cursor)
  }
  
  //// Execution

  /**
   * Execute one instruction in each currently executable thread.
   */
  def roundRobin(): Boolean = {

    var someRunning: Boolean = false
    var someWaiting: Boolean = false
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

    val shouldContinue = if (someRunning) {
      true
    } else {
      if (someWaiting) {
        futexManager.nextWakeup match {
          case Some(nextWakeup) => {
            val now = System.nanoTime()
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
    shouldContinue
  }

  /**
   * Execute until all threads stopped.
   */
  def execute() {
    var shouldContinue: Boolean = false

    do {
      shouldContinue = roundRobin()
    } while (shouldContinue)
  }
}
