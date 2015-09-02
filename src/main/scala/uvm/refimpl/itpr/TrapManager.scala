package uvm.refimpl.itpr

import uvm.refimpl._
import scala.collection.mutable.HashSet

class TrapManager(implicit microVM: MicroVM) {
  var trapHandler: TrapHandler = DefaultTrapHandler
  var undefinedFunctionHandler: UndefinedFunctionHandler = DefaultUndefinedFunctionHandler

  private val enabledWatchPoints = new HashSet[Int]()

  def isWatchPointEnabled(wpID: Int): Boolean = enabledWatchPoints.contains(wpID)

  def enableWatchPoint(wpID: Int): Unit = enabledWatchPoints.add(wpID)

  def disableWatchPoint(wpID: Int): Unit = enabledWatchPoints.remove(wpID)

  object DefaultTrapHandler extends TrapHandler {
    def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      val thr = thread.vb.asInstanceOf[BoxThread].thread.get
      val thrID = thr.id
      val funcVerID = ca.currentFuncVer(stack, 0)
      val funcVer = microVM.globalBundle.funcVerNs(funcVerID)
      val instID = ca.currentInstruction(stack, 0)
      val inst = microVM.globalBundle.varNs(instID)
      throw new UvmRuntimeException("Unhandled trap. Thread %d, funcver %s, trap inst %s, watch point ID %d".format(
        thr.id, funcVer.repr, inst.repr, watchPointID))
    }
  }

  object DefaultUndefinedFunctionHandler extends UndefinedFunctionHandler {
    def handleUndefinedFunction(functionID: Int): Unit = {
      val func = microVM.globalBundle.funcNs(functionID)
      throw new UvmRuntimeException("Unhandled undefined function. Function %s.".format(func.repr))
    }
  }
}
