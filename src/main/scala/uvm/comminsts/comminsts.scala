package uvm.comminsts

import uvm._

case class CommInst(val id: Int, val name: Option[String]) extends Identified

object CommInsts extends SimpleNamespace[CommInst] {

  private def commInst(id: Int, name: String) {
    val ci = CommInst(id, Some(name))
    add(ci)
  }

  commInst(0x201, "@uvm.new_stack")
  commInst(0x202, "@uvm.kill_stack")
  commInst(0x203, "@uvm.thread_exit")
  commInst(0x204, "@uvm.current_stack")

  commInst(0x211, "@uvm.tr64.is_fp")
  commInst(0x212, "@uvm.tr64.is_int")
  commInst(0x213, "@uvm.tr64.is_ref")
  commInst(0x214, "@uvm.tr64.from_fp")
  commInst(0x215, "@uvm.tr64.from_int")
  commInst(0x216, "@uvm.tr64.from_ref")
  commInst(0x217, "@uvm.tr64.to_fp")
  commInst(0x218, "@uvm.tr64.to_int")
  commInst(0x219, "@uvm.tr64.to_ref")
  commInst(0x21a, "@uvm.tr64.to_tag")

  commInst(0x220, "@uvm.futex.wait")
  commInst(0x221, "@uvm.futex.wait_timeout")
  commInst(0x222, "@uvm.futex.wake")
  commInst(0x223, "@uvm.futex.cmp_requeue")

  commInst(0x230, "@uvm.kill_dependency")
  
  commInst(0x240, "@uvm.native.pin")
  commInst(0x241, "@uvm.native.unpin")
  commInst(0x242, "@uvm.native.expose")
  commInst(0x243, "@uvm.native.unexpose")
  commInst(0x244, "@uvm.native.get_cookie")
}