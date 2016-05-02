package junks

import uvm.refimpl._
import uvm.refimpl.TrapHandlerResult.Rebind
import uvm.refimpl.HowToResume.PassValues

object FactorialFromRPython extends App {
  import uvm.refimpl.RichMuCtx._
  val microVM = new MicroVM()

  val ctx = microVM.newContext()

  val r = new java.io.FileReader("tests/extra-progs/factorial.uir")
  ctx.loadBundle(r)
  r.close()

  val m = ctx.handleFromFunc(microVM.idOf("@main"))

  microVM.setTrapHandler(new TrapHandler {
    override def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
      val fc = ctx.newCursor(stack)
      val curInst = ctx.curInst(fc)
      val trapName = microVM.nameOf(curInst)

      if (trapName == "@main_v1.entry.main_trap") {
        val kas = ctx.dumpKeepalives(fc)
        val Seq(rv) = kas

        val i = ctx.handleToSInt(rv.asInstanceOf[MuIntValue])

        println(i)
      } else {
        throw new RuntimeException("Hit the wrong trap: " + trapName)
      }

      ctx.closeCursor(fc)
      Rebind(stack, PassValues(Seq())) // continue

    }
  })

  val sta = ctx.newStack(m)
  val thr = ctx.newThread(sta, None, PassValues(Seq()))

  microVM.execute() // run until all threads stop

  ctx.closeContext()
}