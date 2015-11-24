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
  
  commInst(0x250, "@uvm.meta.id_of")
  commInst(0x251, "@uvm.meta.name_of")
  commInst(0x252, "@uvm.meta.load_bundle")
  commInst(0x253, "@uvm.meta.load_hail")
  
  commInst(0x254, "@uvm.meta.new_cursor")
  commInst(0x255, "@uvm.meta.next_frame")
  commInst(0x256, "@uvm.meta.copy_cursor")
  commInst(0x257, "@uvm.meta.close_cursor")
  
  commInst(0x258, "@uvm.meta.cur_func")
  commInst(0x259, "@uvm.meta.cur_func_ver")
  commInst(0x25a, "@uvm.meta.cur_inst")
  commInst(0x25b, "@uvm.meta.dump_keepalives")
  
  commInst(0x25c, "@uvm.meta.pop_frames_to")
  commInst(0x25d, "@uvm.meta.push_frame")
  
  commInst(0x25e, "@uvm.meta.enable_watchpoint")
  commInst(0x25f, "@uvm.meta.disable_watchpoint")
  
  commInst(0x260, "@uvm.meta.set_trap_handler")

}