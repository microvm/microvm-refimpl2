package uvm.refimpl.itpr

import uvm.refimpl._
import scala.collection.mutable.HashSet

class TrapManager(implicit microVM: MicroVM) {
  var trapHandler: TrapHandler = DefaultTrapHandler

  private val enabledWatchPoints = new HashSet[Int]()

  def isWatchPointEnabled(wpID: Int): Boolean = enabledWatchPoints.contains(wpID)

  def enableWatchPoint(wpID: Int): Unit = enabledWatchPoints.add(wpID)

  def disableWatchPoint(wpID: Int): Unit = enabledWatchPoints.remove(wpID)

  object DefaultTrapHandler extends TrapHandler {
    def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
      val thrID = thread.vb.asInstanceOf[BoxThread].thread.get.id
      val staID = stack.vb.asInstanceOf[BoxStack].stack.get.id
      val cursor = ctx.newCursor(stack)
      val curFuncID = ctx.curFunc(cursor)
      val curFuncVerID = ctx.curFuncVer(cursor)
      val curInstID = ctx.curInst(cursor)
      ctx.closeCursor(cursor)
      throw new UvmRuntimeException("Unhandled trap. thread %d, stack: %d, func: %d, funcver: %d, inst %d, watch point ID %d".format(
        thrID, staID, curFuncID, curFuncVerID, curInstID, watchPointID))
    }
  }

}
