package uvm.ifuncs

import uvm._
import uvm.types._

import uvm.types.CommonTypes._

case class IFunc(val id: Int, val name: Option[String], val sig: FuncSig) extends Identified {
  def ty = sig.retTy
}

object IFuncs extends SimpleNamespace[IFunc] {

  private def addIFunc(id: Int, name: String, retTy: Type, paramTy: Seq[Type]): IFunc = {
    val iFunc = IFunc(id, Some(name), FuncSig(retTy, paramTy))
    add(iFunc)
    return iFunc
  }

  val UVM_NEW_THREAD = addIFunc(0x201, "@uvm.new_thread", VOID, Seq(THREAD))
  val UVM_SWAP_STACK = addIFunc(0x202, "@uvm.swap_stack", VOID, Seq(STACK))
  val UVM_KILL_STACK = addIFunc(0x203, "@uvm.kill_stack", VOID, Seq(STACK))
  val UVM_SWAP_AND_KILL = addIFunc(0x204, "@uvm.swap_and_kill", VOID, Seq(STACK))
  val UVM_THREAD_EXIT = addIFunc(0x205, "@uvm.thread_exit", VOID, Seq(THREAD))

  val UVM_MATH_SIN = addIFunc(0x101, "@uvm.math.sin", DOUBLE, Seq(DOUBLE))

}