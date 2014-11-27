package uvm.refimpl.itpr

import uvm._
import uvm.refimpl._
import uvm.refimpl.mem._

class InterpreterThread(val id: Int, microVM: MicroVM, initialStack: InterpreterStack, val mutator: Mutator) {
  var stack: Option[InterpreterStack] = Some(initialStack)
  throw new UvmRefImplException("Not implemented")

  def start(): Unit = {
    throw new UvmRefImplException("Not implemented")
  }

  def isRunning: Boolean = {
    throw new UvmRefImplException("Not implemented")
  }

  def step(): Unit = {
    throw new UvmRefImplException("Not implemented")
  }

}
