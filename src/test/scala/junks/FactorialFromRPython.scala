package junks

import uvm.refimpl._

object FactorialFromRPython extends App {
  val microVM = new MicroVM()

  val ca = microVM.newClientAgent()

  val r = new java.io.FileReader("tests/extra-progs/factorial.uir")
  ca.loadBundle(r)
  r.close()

  val m = ca.putFunction(microVM.idOf("@main"))

  microVM.trapManager.trapHandler = new TrapHandler {
    override def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      val curInst = ca.currentInstruction(stack, 0)
      val trapName = microVM.nameOf(curInst)

      if (trapName == "@main_v1.main_trap") {
        val kas = ca.dumpKeepalives(stack, 0)
        val Seq(rv) = kas

        val i = ca.toInt(rv, signExt = true)

        println(i)
      } else {
        throw new RuntimeException("Hit the wrong trap: " + trapName)
      }

      TrapRebindPassVoid(stack) // continue
    }
  }

  val sta = ca.newStack(m, Seq())
  val thr = ca.newThread(sta)

  microVM.threadStackManager.joinAll() // run until all threads stop

  ca.close()
}