package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._
import PlatformConstants._
import uvm.refimpl._

class ExposedMethod(jRetTy: JType, jParamTys: Array[JType], invokeFunc: Buffer => Unit) {
  val closure = new SimpleClosure(invokeFunc)
  val handle = jffiClosureManager.newClosure(closure, jRetTy, jParamTys, CallingConvention.DEFAULT)
  def address = handle.getAddress()
}

class SimpleClosure(f: Buffer => Unit) extends Closure {
  def invoke(buffer: Buffer): Unit = f(buffer)
}

private object CDefsHelperFunctions {

  def exposedMethod(jRetTy: JType, jParamTys: Array[JType])(invokeFunc: Buffer => Unit) = {
    new ExposedMethod(jRetTy, jParamTys, invokeFunc)
  }
  
  def getMuVM(ptr: Long): MicroVM = NativeClientSupport.getMicroVM(ptr)
  def getMuCtx(ptr: Long): MuCtx = NativeClientSupport.getMuCtx(ptr)
  
}

import CDefsHelperFunctions._
import uvm.refimpl.MicroVM

object CDefs {
  
  exposedMethod(JType.VOID, Array(JType.SINT, JType.UINT32)) { jffiBuffer =>
    val i = jffiBuffer.getInt(0)
    jffiBuffer.setIntReturn(i)
  }

  // generated from migrate_scripts/muapitocstubs.py
  /// SCRIPT: GENERATED CODE BEGIN
val MUVM__NEW_CONTEXT = exposedMethod(JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val MUVM__ID_OF = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val MUVM__NAME_OF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val MUVM__SET_TRAP_HANDLER = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val MUVM__EXECUTE = exposedMethod(JType.VOID, Array(JType.POINTER)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val MUVM__GET_MU_ERROR_PTR = exposedMethod(JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val mvm = _jffiBuffer.getAddress(0)
val _mvm_obj = getMuVM(mvm)
}
val stubsOfMuVM = Array[Word](6)
stubsOfMuVM(0) = MUVM__NEW_CONTEXT.address
stubsOfMuVM(1) = MUVM__ID_OF.address
stubsOfMuVM(2) = MUVM__NAME_OF.address
stubsOfMuVM(3) = MUVM__SET_TRAP_HANDLER.address
stubsOfMuVM(4) = MUVM__EXECUTE.address
stubsOfMuVM(5) = MUVM__GET_MU_ERROR_PTR.address
val MUCTX__ID_OF = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NAME_OF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CLOSE_CONTEXT = exposedMethod(JType.VOID, Array(JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__LOAD_BUNDLE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__LOAD_HAIL = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_SINT8 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.SINT8, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_UINT8 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT8, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_SINT16 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.SINT16, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_UINT16 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT16, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_SINT32 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.SINT32, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_UINT32 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_SINT64 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.SINT64, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_UINT64 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT64, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_UINT64S = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_FLOAT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.FLOAT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_DOUBLE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.DOUBLE)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_PTR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_FP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_SINT8 = exposedMethod(JType.SINT8, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_UINT8 = exposedMethod(JType.UINT8, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_SINT16 = exposedMethod(JType.SINT16, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_UINT16 = exposedMethod(JType.UINT16, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_SINT32 = exposedMethod(JType.SINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_UINT32 = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_SINT64 = exposedMethod(JType.SINT64, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_UINT64 = exposedMethod(JType.UINT64, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_FLOAT = exposedMethod(JType.FLOAT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_DOUBLE = exposedMethod(JType.DOUBLE, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_PTR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_TO_FP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_CONST = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_GLOBAL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_FUNC = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__HANDLE_FROM_EXPOSE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__DELETE_VALUE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__REF_EQ = exposedMethod(JType.SINT, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__REF_ULT = exposedMethod(JType.SINT, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__EXTRACT_VALUE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__INSERT_VALUE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__EXTRACT_ELEMENT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__INSERT_ELEMENT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_FIXED = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_HYBRID = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__REFCAST = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_FIELD_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_ELEM_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SHIFT_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_VAR_PART_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__LOAD = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__STORE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CMPXCHG = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.UINT32, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ATOMICRMW = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__FENCE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_STACK = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_THREAD_NOR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_THREAD_EXC = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__KILL_STACK = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_THREADLOCAL = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_THREADLOCAL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CURSOR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEXT_FRAME = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__COPY_CURSOR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CLOSE_CURSOR = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CUR_FUNC = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CUR_FUNC_VER = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__CUR_INST = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__DUMP_KEEPALIVES = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__POP_FRAMES_TO = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__PUSH_FRAME = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_IS_FP = exposedMethod(JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_IS_INT = exposedMethod(JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_IS_REF = exposedMethod(JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_TO_FP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_TO_INT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_TO_REF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_TO_TAG = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_FROM_FP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_FROM_INT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__TR64_FROM_REF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ENABLE_WATCHPOINT = exposedMethod(JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__DISABLE_WATCHPOINT = exposedMethod(JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__PIN = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__UNPIN = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__EXPOSE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__UNEXPOSE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_BUNDLE = exposedMethod(JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__LOAD_BUNDLE_FROM_NODE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ABORT_BUNDLE_NODE = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_NODE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__GET_ID = exposedMethod(JType.UINT32, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_NAME = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_INT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_FLOAT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_DOUBLE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_UPTR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_UPTR = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_UFUNCPTR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_UFUNCPTR = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_STRUCT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_HYBRID = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_ARRAY = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_VECTOR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_VOID = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_REF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_REF = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_IREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_IREF = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_WEAKREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_WEAKREF = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_FUNCREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_TYPE_FUNCREF = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_TAGREF64 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_THREADREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_STACKREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_FRAMECURSORREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TYPE_IRNODEREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_FUNCSIG = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_INT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_INT_EX = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_FLOAT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.FLOAT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_DOUBLE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.DOUBLE)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_NULL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONST_SEQ = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_GLOBAL_CELL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_FUNC = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_FUNC_VER = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_EXP_FUNC = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_BB = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_NOR_PARAM = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_EXC_PARAM = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_INST_RES = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ADD_DEST = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ADD_KEEPALIVES = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_BINOP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CMP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CONV = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SELECT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_BRANCH = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_BRANCH2 = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SWITCH = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__ADD_SWITCH_DEST = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CALL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TAILCALL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_RET = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_THROW = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_EXTRACTVALUE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_INSERTVALUE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_EXTRACTELEMENT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_INSERTELEMENT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SHUFFLEVECTOR = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_NEW = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_NEWHYBRID = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_ALLOCA = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_ALLOCAHYBRID = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_GETIREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_GETFIELDIREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_GETELEMIREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SHIFTIREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_GETVARPARTIREF = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_LOAD = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_STORE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CMPXCHG = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.SINT, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_ATOMICRMW = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_FENCE = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_TRAP = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_WATCHPOINT = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_WPBRANCH = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_CCALL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_NEWTHREAD = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SWAPSTACK_RET = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_SWAPSTACK_KILL = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_NEWSTACK_PASS_VALUES = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__SET_NEWSTACK_THROW_EXC = exposedMethod(JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val MUCTX__NEW_COMMINST = exposedMethod(JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val ctx = _jffiBuffer.getAddress(0)
val _ctx_obj = getMuCtx(ctx)
}
val stubsOfMuCtx = Array[Word](175)
stubsOfMuCtx(0) = MUCTX__ID_OF.address
stubsOfMuCtx(1) = MUCTX__NAME_OF.address
stubsOfMuCtx(2) = MUCTX__CLOSE_CONTEXT.address
stubsOfMuCtx(3) = MUCTX__LOAD_BUNDLE.address
stubsOfMuCtx(4) = MUCTX__LOAD_HAIL.address
stubsOfMuCtx(5) = MUCTX__HANDLE_FROM_SINT8.address
stubsOfMuCtx(6) = MUCTX__HANDLE_FROM_UINT8.address
stubsOfMuCtx(7) = MUCTX__HANDLE_FROM_SINT16.address
stubsOfMuCtx(8) = MUCTX__HANDLE_FROM_UINT16.address
stubsOfMuCtx(9) = MUCTX__HANDLE_FROM_SINT32.address
stubsOfMuCtx(10) = MUCTX__HANDLE_FROM_UINT32.address
stubsOfMuCtx(11) = MUCTX__HANDLE_FROM_SINT64.address
stubsOfMuCtx(12) = MUCTX__HANDLE_FROM_UINT64.address
stubsOfMuCtx(13) = MUCTX__HANDLE_FROM_UINT64S.address
stubsOfMuCtx(14) = MUCTX__HANDLE_FROM_FLOAT.address
stubsOfMuCtx(15) = MUCTX__HANDLE_FROM_DOUBLE.address
stubsOfMuCtx(16) = MUCTX__HANDLE_FROM_PTR.address
stubsOfMuCtx(17) = MUCTX__HANDLE_FROM_FP.address
stubsOfMuCtx(18) = MUCTX__HANDLE_TO_SINT8.address
stubsOfMuCtx(19) = MUCTX__HANDLE_TO_UINT8.address
stubsOfMuCtx(20) = MUCTX__HANDLE_TO_SINT16.address
stubsOfMuCtx(21) = MUCTX__HANDLE_TO_UINT16.address
stubsOfMuCtx(22) = MUCTX__HANDLE_TO_SINT32.address
stubsOfMuCtx(23) = MUCTX__HANDLE_TO_UINT32.address
stubsOfMuCtx(24) = MUCTX__HANDLE_TO_SINT64.address
stubsOfMuCtx(25) = MUCTX__HANDLE_TO_UINT64.address
stubsOfMuCtx(26) = MUCTX__HANDLE_TO_FLOAT.address
stubsOfMuCtx(27) = MUCTX__HANDLE_TO_DOUBLE.address
stubsOfMuCtx(28) = MUCTX__HANDLE_TO_PTR.address
stubsOfMuCtx(29) = MUCTX__HANDLE_TO_FP.address
stubsOfMuCtx(30) = MUCTX__HANDLE_FROM_CONST.address
stubsOfMuCtx(31) = MUCTX__HANDLE_FROM_GLOBAL.address
stubsOfMuCtx(32) = MUCTX__HANDLE_FROM_FUNC.address
stubsOfMuCtx(33) = MUCTX__HANDLE_FROM_EXPOSE.address
stubsOfMuCtx(34) = MUCTX__DELETE_VALUE.address
stubsOfMuCtx(35) = MUCTX__REF_EQ.address
stubsOfMuCtx(36) = MUCTX__REF_ULT.address
stubsOfMuCtx(37) = MUCTX__EXTRACT_VALUE.address
stubsOfMuCtx(38) = MUCTX__INSERT_VALUE.address
stubsOfMuCtx(39) = MUCTX__EXTRACT_ELEMENT.address
stubsOfMuCtx(40) = MUCTX__INSERT_ELEMENT.address
stubsOfMuCtx(41) = MUCTX__NEW_FIXED.address
stubsOfMuCtx(42) = MUCTX__NEW_HYBRID.address
stubsOfMuCtx(43) = MUCTX__REFCAST.address
stubsOfMuCtx(44) = MUCTX__GET_IREF.address
stubsOfMuCtx(45) = MUCTX__GET_FIELD_IREF.address
stubsOfMuCtx(46) = MUCTX__GET_ELEM_IREF.address
stubsOfMuCtx(47) = MUCTX__SHIFT_IREF.address
stubsOfMuCtx(48) = MUCTX__GET_VAR_PART_IREF.address
stubsOfMuCtx(49) = MUCTX__LOAD.address
stubsOfMuCtx(50) = MUCTX__STORE.address
stubsOfMuCtx(51) = MUCTX__CMPXCHG.address
stubsOfMuCtx(52) = MUCTX__ATOMICRMW.address
stubsOfMuCtx(53) = MUCTX__FENCE.address
stubsOfMuCtx(54) = MUCTX__NEW_STACK.address
stubsOfMuCtx(55) = MUCTX__NEW_THREAD_NOR.address
stubsOfMuCtx(56) = MUCTX__NEW_THREAD_EXC.address
stubsOfMuCtx(57) = MUCTX__KILL_STACK.address
stubsOfMuCtx(58) = MUCTX__SET_THREADLOCAL.address
stubsOfMuCtx(59) = MUCTX__GET_THREADLOCAL.address
stubsOfMuCtx(60) = MUCTX__NEW_CURSOR.address
stubsOfMuCtx(61) = MUCTX__NEXT_FRAME.address
stubsOfMuCtx(62) = MUCTX__COPY_CURSOR.address
stubsOfMuCtx(63) = MUCTX__CLOSE_CURSOR.address
stubsOfMuCtx(64) = MUCTX__CUR_FUNC.address
stubsOfMuCtx(65) = MUCTX__CUR_FUNC_VER.address
stubsOfMuCtx(66) = MUCTX__CUR_INST.address
stubsOfMuCtx(67) = MUCTX__DUMP_KEEPALIVES.address
stubsOfMuCtx(68) = MUCTX__POP_FRAMES_TO.address
stubsOfMuCtx(69) = MUCTX__PUSH_FRAME.address
stubsOfMuCtx(70) = MUCTX__TR64_IS_FP.address
stubsOfMuCtx(71) = MUCTX__TR64_IS_INT.address
stubsOfMuCtx(72) = MUCTX__TR64_IS_REF.address
stubsOfMuCtx(73) = MUCTX__TR64_TO_FP.address
stubsOfMuCtx(74) = MUCTX__TR64_TO_INT.address
stubsOfMuCtx(75) = MUCTX__TR64_TO_REF.address
stubsOfMuCtx(76) = MUCTX__TR64_TO_TAG.address
stubsOfMuCtx(77) = MUCTX__TR64_FROM_FP.address
stubsOfMuCtx(78) = MUCTX__TR64_FROM_INT.address
stubsOfMuCtx(79) = MUCTX__TR64_FROM_REF.address
stubsOfMuCtx(80) = MUCTX__ENABLE_WATCHPOINT.address
stubsOfMuCtx(81) = MUCTX__DISABLE_WATCHPOINT.address
stubsOfMuCtx(82) = MUCTX__PIN.address
stubsOfMuCtx(83) = MUCTX__UNPIN.address
stubsOfMuCtx(84) = MUCTX__EXPOSE.address
stubsOfMuCtx(85) = MUCTX__UNEXPOSE.address
stubsOfMuCtx(86) = MUCTX__NEW_BUNDLE.address
stubsOfMuCtx(87) = MUCTX__LOAD_BUNDLE_FROM_NODE.address
stubsOfMuCtx(88) = MUCTX__ABORT_BUNDLE_NODE.address
stubsOfMuCtx(89) = MUCTX__GET_NODE.address
stubsOfMuCtx(90) = MUCTX__GET_ID.address
stubsOfMuCtx(91) = MUCTX__SET_NAME.address
stubsOfMuCtx(92) = MUCTX__NEW_TYPE_INT.address
stubsOfMuCtx(93) = MUCTX__NEW_TYPE_FLOAT.address
stubsOfMuCtx(94) = MUCTX__NEW_TYPE_DOUBLE.address
stubsOfMuCtx(95) = MUCTX__NEW_TYPE_UPTR.address
stubsOfMuCtx(96) = MUCTX__SET_TYPE_UPTR.address
stubsOfMuCtx(97) = MUCTX__NEW_TYPE_UFUNCPTR.address
stubsOfMuCtx(98) = MUCTX__SET_TYPE_UFUNCPTR.address
stubsOfMuCtx(99) = MUCTX__NEW_TYPE_STRUCT.address
stubsOfMuCtx(100) = MUCTX__NEW_TYPE_HYBRID.address
stubsOfMuCtx(101) = MUCTX__NEW_TYPE_ARRAY.address
stubsOfMuCtx(102) = MUCTX__NEW_TYPE_VECTOR.address
stubsOfMuCtx(103) = MUCTX__NEW_TYPE_VOID.address
stubsOfMuCtx(104) = MUCTX__NEW_TYPE_REF.address
stubsOfMuCtx(105) = MUCTX__SET_TYPE_REF.address
stubsOfMuCtx(106) = MUCTX__NEW_TYPE_IREF.address
stubsOfMuCtx(107) = MUCTX__SET_TYPE_IREF.address
stubsOfMuCtx(108) = MUCTX__NEW_TYPE_WEAKREF.address
stubsOfMuCtx(109) = MUCTX__SET_TYPE_WEAKREF.address
stubsOfMuCtx(110) = MUCTX__NEW_TYPE_FUNCREF.address
stubsOfMuCtx(111) = MUCTX__SET_TYPE_FUNCREF.address
stubsOfMuCtx(112) = MUCTX__NEW_TYPE_TAGREF64.address
stubsOfMuCtx(113) = MUCTX__NEW_TYPE_THREADREF.address
stubsOfMuCtx(114) = MUCTX__NEW_TYPE_STACKREF.address
stubsOfMuCtx(115) = MUCTX__NEW_TYPE_FRAMECURSORREF.address
stubsOfMuCtx(116) = MUCTX__NEW_TYPE_IRNODEREF.address
stubsOfMuCtx(117) = MUCTX__NEW_FUNCSIG.address
stubsOfMuCtx(118) = MUCTX__NEW_CONST_INT.address
stubsOfMuCtx(119) = MUCTX__NEW_CONST_INT_EX.address
stubsOfMuCtx(120) = MUCTX__NEW_CONST_FLOAT.address
stubsOfMuCtx(121) = MUCTX__NEW_CONST_DOUBLE.address
stubsOfMuCtx(122) = MUCTX__NEW_CONST_NULL.address
stubsOfMuCtx(123) = MUCTX__NEW_CONST_SEQ.address
stubsOfMuCtx(124) = MUCTX__NEW_GLOBAL_CELL.address
stubsOfMuCtx(125) = MUCTX__NEW_FUNC.address
stubsOfMuCtx(126) = MUCTX__NEW_FUNC_VER.address
stubsOfMuCtx(127) = MUCTX__NEW_EXP_FUNC.address
stubsOfMuCtx(128) = MUCTX__NEW_BB.address
stubsOfMuCtx(129) = MUCTX__NEW_NOR_PARAM.address
stubsOfMuCtx(130) = MUCTX__NEW_EXC_PARAM.address
stubsOfMuCtx(131) = MUCTX__NEW_INST_RES.address
stubsOfMuCtx(132) = MUCTX__ADD_DEST.address
stubsOfMuCtx(133) = MUCTX__ADD_KEEPALIVES.address
stubsOfMuCtx(134) = MUCTX__NEW_BINOP.address
stubsOfMuCtx(135) = MUCTX__NEW_CMP.address
stubsOfMuCtx(136) = MUCTX__NEW_CONV.address
stubsOfMuCtx(137) = MUCTX__NEW_SELECT.address
stubsOfMuCtx(138) = MUCTX__NEW_BRANCH.address
stubsOfMuCtx(139) = MUCTX__NEW_BRANCH2.address
stubsOfMuCtx(140) = MUCTX__NEW_SWITCH.address
stubsOfMuCtx(141) = MUCTX__ADD_SWITCH_DEST.address
stubsOfMuCtx(142) = MUCTX__NEW_CALL.address
stubsOfMuCtx(143) = MUCTX__NEW_TAILCALL.address
stubsOfMuCtx(144) = MUCTX__NEW_RET.address
stubsOfMuCtx(145) = MUCTX__NEW_THROW.address
stubsOfMuCtx(146) = MUCTX__NEW_EXTRACTVALUE.address
stubsOfMuCtx(147) = MUCTX__NEW_INSERTVALUE.address
stubsOfMuCtx(148) = MUCTX__NEW_EXTRACTELEMENT.address
stubsOfMuCtx(149) = MUCTX__NEW_INSERTELEMENT.address
stubsOfMuCtx(150) = MUCTX__NEW_SHUFFLEVECTOR.address
stubsOfMuCtx(151) = MUCTX__NEW_NEW.address
stubsOfMuCtx(152) = MUCTX__NEW_NEWHYBRID.address
stubsOfMuCtx(153) = MUCTX__NEW_ALLOCA.address
stubsOfMuCtx(154) = MUCTX__NEW_ALLOCAHYBRID.address
stubsOfMuCtx(155) = MUCTX__NEW_GETIREF.address
stubsOfMuCtx(156) = MUCTX__NEW_GETFIELDIREF.address
stubsOfMuCtx(157) = MUCTX__NEW_GETELEMIREF.address
stubsOfMuCtx(158) = MUCTX__NEW_SHIFTIREF.address
stubsOfMuCtx(159) = MUCTX__NEW_GETVARPARTIREF.address
stubsOfMuCtx(160) = MUCTX__NEW_LOAD.address
stubsOfMuCtx(161) = MUCTX__NEW_STORE.address
stubsOfMuCtx(162) = MUCTX__NEW_CMPXCHG.address
stubsOfMuCtx(163) = MUCTX__NEW_ATOMICRMW.address
stubsOfMuCtx(164) = MUCTX__NEW_FENCE.address
stubsOfMuCtx(165) = MUCTX__NEW_TRAP.address
stubsOfMuCtx(166) = MUCTX__NEW_WATCHPOINT.address
stubsOfMuCtx(167) = MUCTX__NEW_WPBRANCH.address
stubsOfMuCtx(168) = MUCTX__NEW_CCALL.address
stubsOfMuCtx(169) = MUCTX__NEW_NEWTHREAD.address
stubsOfMuCtx(170) = MUCTX__NEW_SWAPSTACK_RET.address
stubsOfMuCtx(171) = MUCTX__NEW_SWAPSTACK_KILL.address
stubsOfMuCtx(172) = MUCTX__SET_NEWSTACK_PASS_VALUES.address
stubsOfMuCtx(173) = MUCTX__SET_NEWSTACK_THROW_EXC.address
stubsOfMuCtx(174) = MUCTX__NEW_COMMINST.address
  /// SCRIPT: GENERATED CODE END
}