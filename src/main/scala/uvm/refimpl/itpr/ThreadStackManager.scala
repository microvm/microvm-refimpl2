package uvm.refimpl.itpr

import uvm.Function
import uvm.refimpl.MicroVM
import uvm.refimpl.mem._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

class ThreadStackManager(microVM: MicroVM) {

  val stackRegistry = new HashMap[Int, InterpreterStack]()

  val threadRegistry = new HashMap[Int, InterpreterThread]()

  def getStackByID(id: Int): Option[InterpreterStack] = stackRegistry.get(id)

  def getThreadByID(id: Int): Option[InterpreterThread] = threadRegistry.get(id)

  private var nextStackID: Int = 1

  private def makeStackID(): Int = {val id = nextStackID; nextStackID += 1; id}

  private var nextThreadID: Int = 1

  private def makeThreadID(): Int = {val id = nextThreadID; nextThreadID += 1; id}

  def newStack(function: Function, args: Seq[ValueBox], mutator: Mutator): InterpreterStack = {
    val stackMemory = microVM.memoryManager.makeStackMemory(mutator)
    val id = makeStackID()
    val sta = new InterpreterStack(id, stackMemory, function.versions.head, args)
    stackRegistry.put(id, sta)
    sta
  }

  def newThread(stack: InterpreterStack): InterpreterThread = {
    val mutator = microVM.memoryManager.makeMutator()
    val id = makeThreadID()
    val thr = new InterpreterThread(id, microVM, stack, mutator)
    threadRegistry.put(id, thr)
    thr.start()
    thr
  }

  def joinAll() {
    var someRunning: Boolean = false
    do {
      someRunning = false
      val curThreads = threadRegistry.values.toList
      for (thr2 <- curThreads) {
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
