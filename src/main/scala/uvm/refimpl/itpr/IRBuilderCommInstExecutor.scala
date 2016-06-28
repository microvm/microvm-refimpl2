package uvm.refimpl.itpr
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm._
import uvm.comminsts._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables._
import uvm.types._
import uvm.refimpl.nat.NativeCallResult
/**
 * A part of the InterpreterThread that interprets common instructions
 */
trait IRBuilderCommInstExecutor extends InterpreterActions with ObjectPinner {
  import InterpreterThread.logger

  implicit protected def mutator: Mutator
  implicit protected def memorySupport: MemorySupport
  
  protected def irBuilder = microVM.irBuilder

  def interpretCurrentIRBuilderCommonInstruction(): Unit = {
    assert(curInst.isInstanceOf[InstCommInst])
    val InstCommInst(ci, flagList, typeList, sigList, argList, excClause, keepalives) = curInst
    
    assert(ci.name.get.startsWith("@uvm.irbuilder"))

    ci.name.get match {
      /// GEN:BEGIN:IRBUILDER_IMPL
      case "@uvm.irbuilder.new_bundle" => ???
      case "@uvm.irbuilder.load_bundle_from_node" => ???
      case "@uvm.irbuilder.abort_bundle_node" => ???
      case "@uvm.irbuilder.get_node" => ???
      case "@uvm.irbuilder.get_id" => ???
      case "@uvm.irbuilder.set_name" => ???
      case "@uvm.irbuilder.new_type_int" => ???
      case "@uvm.irbuilder.new_type_float" => ???
      case "@uvm.irbuilder.new_type_double" => ???
      case "@uvm.irbuilder.new_type_uptr" => ???
      case "@uvm.irbuilder.set_type_uptr" => ???
      case "@uvm.irbuilder.new_type_ufuncptr" => ???
      case "@uvm.irbuilder.set_type_ufuncptr" => ???
      case "@uvm.irbuilder.new_type_struct" => ???
      case "@uvm.irbuilder.new_type_hybrid" => ???
      case "@uvm.irbuilder.new_type_array" => ???
      case "@uvm.irbuilder.new_type_vector" => ???
      case "@uvm.irbuilder.new_type_void" => ???
      case "@uvm.irbuilder.new_type_ref" => ???
      case "@uvm.irbuilder.set_type_ref" => ???
      case "@uvm.irbuilder.new_type_iref" => ???
      case "@uvm.irbuilder.set_type_iref" => ???
      case "@uvm.irbuilder.new_type_weakref" => ???
      case "@uvm.irbuilder.set_type_weakref" => ???
      case "@uvm.irbuilder.new_type_funcref" => ???
      case "@uvm.irbuilder.set_type_funcref" => ???
      case "@uvm.irbuilder.new_type_tagref64" => ???
      case "@uvm.irbuilder.new_type_threadref" => ???
      case "@uvm.irbuilder.new_type_stackref" => ???
      case "@uvm.irbuilder.new_type_framecursorref" => ???
      case "@uvm.irbuilder.new_type_irnoderef" => ???
      case "@uvm.irbuilder.new_funcsig" => ???
      case "@uvm.irbuilder.new_const_int" => ???
      case "@uvm.irbuilder.new_const_int_ex" => ???
      case "@uvm.irbuilder.new_const_float" => ???
      case "@uvm.irbuilder.new_const_double" => ???
      case "@uvm.irbuilder.new_const_null" => ???
      case "@uvm.irbuilder.new_const_seq" => ???
      case "@uvm.irbuilder.new_global_cell" => ???
      case "@uvm.irbuilder.new_func" => ???
      case "@uvm.irbuilder.new_func_ver" => ???
      case "@uvm.irbuilder.new_exp_func" => ???
      case "@uvm.irbuilder.new_bb" => ???
      case "@uvm.irbuilder.new_nor_param" => ???
      case "@uvm.irbuilder.new_exc_param" => ???
      case "@uvm.irbuilder.new_inst_res" => ???
      case "@uvm.irbuilder.add_dest" => ???
      case "@uvm.irbuilder.add_keepalives" => ???
      case "@uvm.irbuilder.new_binop" => ???
      case "@uvm.irbuilder.new_cmp" => ???
      case "@uvm.irbuilder.new_conv" => ???
      case "@uvm.irbuilder.new_select" => ???
      case "@uvm.irbuilder.new_branch" => ???
      case "@uvm.irbuilder.new_branch2" => ???
      case "@uvm.irbuilder.new_switch" => ???
      case "@uvm.irbuilder.add_switch_dest" => ???
      case "@uvm.irbuilder.new_call" => ???
      case "@uvm.irbuilder.new_tailcall" => ???
      case "@uvm.irbuilder.new_ret" => ???
      case "@uvm.irbuilder.new_throw" => ???
      case "@uvm.irbuilder.new_extractvalue" => ???
      case "@uvm.irbuilder.new_insertvalue" => ???
      case "@uvm.irbuilder.new_extractelement" => ???
      case "@uvm.irbuilder.new_insertelement" => ???
      case "@uvm.irbuilder.new_shufflevector" => ???
      case "@uvm.irbuilder.new_new" => ???
      case "@uvm.irbuilder.new_newhybrid" => ???
      case "@uvm.irbuilder.new_alloca" => ???
      case "@uvm.irbuilder.new_allocahybrid" => ???
      case "@uvm.irbuilder.new_getiref" => ???
      case "@uvm.irbuilder.new_getfieldiref" => ???
      case "@uvm.irbuilder.new_getelemiref" => ???
      case "@uvm.irbuilder.new_shiftiref" => ???
      case "@uvm.irbuilder.new_getvarpartiref" => ???
      case "@uvm.irbuilder.new_load" => ???
      case "@uvm.irbuilder.new_store" => ???
      case "@uvm.irbuilder.new_cmpxchg" => ???
      case "@uvm.irbuilder.new_atomicrmw" => ???
      case "@uvm.irbuilder.new_fence" => ???
      case "@uvm.irbuilder.new_trap" => ???
      case "@uvm.irbuilder.new_watchpoint" => ???
      case "@uvm.irbuilder.new_wpbranch" => ???
      case "@uvm.irbuilder.new_ccall" => ???
      case "@uvm.irbuilder.new_newthread" => ???
      case "@uvm.irbuilder.new_swapstack_ret" => ???
      case "@uvm.irbuilder.new_swapstack_kill" => ???
      case "@uvm.irbuilder.set_newstack_pass_values" => ???
      case "@uvm.irbuilder.set_newstack_throw_exc" => ???
      case "@uvm.irbuilder.new_comminst" => ???
      /// GEN:END:IRBUILDER_IMPL

      case ciName => {
        throw new UvmRefImplException("Unimplemented IR builder common instruction %s".format(ciName))
      }

    }

  }
}
