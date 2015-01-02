package uvm.refimpl.itpr

import uvm.Function
import uvm.refimpl.MicroVM
import uvm.refimpl.mem._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class ThreadStackManager(microVM: MicroVM) {

  private val stackRegistry = new HashMap[Int, InterpreterStack]()

  private val threadRegistry = new HashMap[Int, InterpreterThread]()

  def getStackByID(id: Int): Option[InterpreterStack] = stackRegistry.get(id)

  def getThreadByID(id: Int): Option[InterpreterThread] = threadRegistry.get(id)
  
  def iterateAllLiveStacks: Iterable[InterpreterStack] = stackRegistry.values.filter(_.state != StackState.Dead)

  private var nextStackID: Int = 1

  private def makeStackID(): Int = { val id = nextStackID; nextStackID += 1; id }

  private var nextThreadID: Int = 1

  private def makeThreadID(): Int = { val id = nextThreadID; nextThreadID += 1; id }

  /**
   * Create a new stack with function and args as the stack-bottom function and its arguments.
   * <p>
   * About mutator: "Bring your own mutator!" A mutator object is needed to allocate the stack memory. This means all 
   * callers of the newStack function must have a mutator. Currently they are either ClientAgents which can create stack
   * via the "new_stack" message or µVM threads (the InterpreterThread class) which can execute the NEWSTACK
   * instruction.
   */
  def newStack(function: Function, args: Seq[ValueBox], mutator: Mutator): InterpreterStack = {
    val stackMemory = microVM.memoryManager.makeStackMemory(mutator)
    val id = makeStackID()
    val sta = new InterpreterStack(id, stackMemory, function.versions.head, args)
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
    do {
      someRunning = false
      val curThreads = threadRegistry.values.toList
      for (thr2 <- curThreads if thr2.isRunning) {
        thr2.step()
        someRunning = thr2.isRunning || someRunning
      }
    } while (someRunning)
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
