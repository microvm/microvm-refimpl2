package tutorial

import uvm.refimpl._

object Interact extends App {
  val microVM = new MicroVM()

  val ca = microVM.newClientAgent()

  val reader = new java.io.FileReader("tests/tutorial/interact.uir")
  ca.loadBundle(reader)
  reader.close()
  
  implicit def idOf(name: String) = microVM.idOf(name)

  val mainFunc = ca.putFunction("@main")
  val st = ca.newStack(mainFunc, Seq())
  
  val th = ca.newThread(st)
  
  ca.close()
  
  microVM.threadStackManager.joinAll()
}