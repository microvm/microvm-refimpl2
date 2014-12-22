package junks

import uvm.refimpl._

object FactorialFromRPython extends App {
  val microVM = new MicroVM()

  val ca = microVM.newClientAgent()

  val r = new java.io.FileReader("tests/extra-progs/factorial.uir")
  ca.loadBundle(r)
  r.close()

  // Magical trick. Theoretically the client would publish bundles as binary and knows all the IDs. But in this version
  // only the text form is supported and IDs are automatically generated. So we look into the globalBundle itself.
  val m = ca.putFunction(microVM.globalBundle.funcNs("@main").id)

  microVM.trapManager.trapHandler = new TrapHandler {
    override def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      val curInst = ca.currentInstruction(stack, 0)
      val trapName = microVM.globalBundle.varNs(curInst).name.get

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