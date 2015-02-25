package tutorial

import uvm.refimpl._

object Interact extends App {
  val microVM = new MicroVM()
  
  implicit def idOf(name: String) = microVM.idOf(name)

  val ca = microVM.newClientAgent()

  val reader = new java.io.FileReader("tests/tutorial/interact.uir")
  ca.loadBundle(reader)
  reader.close()
  
  object MyTrapHandler extends TrapHandler {
    def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      val curInstID = ca.currentInstruction(stack, 0)
      microVM.nameOf(curInstID) match {
        case "@main_v1.the_trap" => {
          val Seq(result) = ca.dumpKeepalives(stack, 0)
          val resultInt = ca.toInt(result, signExt = true)
          printf("The result is %d.\n", resultInt)
          TrapRebindPassVoid(st)
        }
      }
    }
  }
  
  microVM.trapManager.trapHandler = MyTrapHandler

  val mainFunc = ca.putFunction("@main")
  val st = ca.newStack(mainFunc, Seq())
  
  val th = ca.newThread(st)
  
  ca.close()
  
  microVM.threadStackManager.joinAll()
}