package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._

class ExposedMethod(jRetTy: JType, jParamTys: Array[JType], invokeFunc: Buffer => Unit) {
  val closure = new SimpleClosure(invokeFunc)
  val handle = jffiClosureManager.newClosure(closure, jRetTy, jParamTys, CallingConvention.DEFAULT)
  def address = handle.getAddress()
}

class SimpleClosure(f: Buffer => Unit) extends Closure {
  def invoke(buffer: Buffer): Unit = f(buffer)
}

object CDefs {
  def exposedMethod(jRetTy: JType, jParamTys: Array[JType])(invokeFunc: Buffer => Unit) = {
    new ExposedMethod(jRetTy, jParamTys, invokeFunc)
  }
  
  exposedMethod(JType.VOID, Array(JType.SINT, JType.UINT32)) { jffiBuffer =>
    val i = jffiBuffer.getInt(0)
    jffiBuffer.setIntReturn(i)
  }

  /// SCRIPT: GENERATED CODE BEGIN
// id_of
// name_of
// close_context
// load_bundle
// load_hail
// handle_from_sint8
// handle_from_uint8
// handle_from_sint16
// handle_from_uint16
// handle_from_sint32
// handle_from_uint32
// handle_from_sint64
// handle_from_uint64
// handle_from_uint64s
// handle_from_float
// handle_from_double
// handle_from_ptr
// handle_from_fp
// handle_to_sint8
// handle_to_uint8
// handle_to_sint16
// handle_to_uint16
// handle_to_sint32
// handle_to_uint32
// handle_to_sint64
// handle_to_uint64
// handle_to_float
// handle_to_double
// handle_to_ptr
// handle_to_fp
// handle_from_const
// handle_from_global
// handle_from_func
// handle_from_expose
// delete_value
// ref_eq
// ref_ult
// extract_value
// insert_value
// extract_element
// insert_element
// new_fixed
// new_hybrid
// refcast
// get_iref
// get_field_iref
// get_elem_iref
// shift_iref
// get_var_part_iref
// load
// store
// cmpxchg
// atomicrmw
// fence
// new_stack
// new_thread_nor
// new_thread_exc
// kill_stack
// set_threadlocal
// get_threadlocal
// new_cursor
// next_frame
// copy_cursor
// close_cursor
// cur_func
// cur_func_ver
// cur_inst
// dump_keepalives
// pop_frames_to
// push_frame
// tr64_is_fp
// tr64_is_int
// tr64_is_ref
// tr64_to_fp
// tr64_to_int
// tr64_to_ref
// tr64_to_tag
// tr64_from_fp
// tr64_from_int
// tr64_from_ref
// enable_watchpoint
// disable_watchpoint
// pin
// unpin
// expose
// unexpose
// new_bundle
// load_bundle_from_node
// abort_bundle_node
// get_node
// get_id
// set_name
// new_type_int
// new_type_float
// new_type_double
// new_type_uptr
// set_type_uptr
// new_type_ufuncptr
// set_type_ufuncptr
// new_type_struct
// new_type_hybrid
// new_type_array
// new_type_vector
// new_type_void
// new_type_ref
// set_type_ref
// new_type_iref
// set_type_iref
// new_type_weakref
// set_type_weakref
// new_type_funcref
// set_type_funcref
// new_type_tagref64
// new_type_threadref
// new_type_stackref
// new_type_framecursorref
// new_type_irnoderef
// new_funcsig
// new_const_int
// new_const_int_ex
// new_const_float
// new_const_double
// new_const_null
// new_const_seq
// new_global_cell
// new_func
// new_func_ver
// new_exp_func
// new_bb
// new_nor_param
// new_exc_param
// new_inst_res
// add_dest
// add_keepalives
// new_binop
// new_cmp
// new_conv
// new_select
// new_branch
// new_branch2
// new_switch
// add_switch_dest
// new_call
// new_tailcall
// new_ret
// new_throw
// new_extractvalue
// new_insertvalue
// new_extractelement
// new_insertelement
// new_shufflevector
// new_new
// new_newhybrid
// new_alloca
// new_allocahybrid
// new_getiref
// new_getfieldiref
// new_getelemiref
// new_shiftiref
// new_getvarpartiref
// new_load
// new_store
// new_cmpxchg
// new_atomicrmw
// new_fence
// new_trap
// new_watchpoint
// new_wpbranch
// new_ccall
// new_newthread
// new_swapstack_ret
// new_swapstack_kill
// set_newstack_pass_values
// set_newstack_throw_exc
// new_comminst
  /// SCRIPT: GENERATED CODE END
}