package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._
import PlatformConstants._
import uvm.refimpl._
import uvm.ssavariables.{BinOptr, CmpOptr, ConvOptr, MemoryOrder, AtomicRMWOptr}
import uvm.ir.irbuilder.DestKind

import uvm.refimpl.MicroVM

object CDefs {
  import CDefsHelperFunctions._
  import NativeClientSupport._

  // generated from migrate_scripts/muapitocstubs.py
  /// GEN:BEGIN:STUBS
val MUVM__NEW_CONTEXT = exposedMethod("MuVM.new_context", JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val mvm = getMicroVM(_raw_mvm)
val _RV = mvm.newContext()
val _RV_FAK = exposeMuCtx(_RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUVM__ID_OF = exposedMethod("MuVM.id_of", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val _raw_name = _jffiBuffer.getAddress(1)
val mvm = getMicroVM(_raw_mvm)
val name = readCString(_raw_name)
val _RV = mvm.idOf(name)
_jffiBuffer.setIntReturn(_RV)
}
val MUVM__NAME_OF = exposedMethod("MuVM.name_of", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val mvm = getMicroVM(_raw_mvm)
val id = _raw_id
val _RV = mvm.nameOf(id)
val _RV_FAK = exposeString(_RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUVM__SET_TRAP_HANDLER = exposedMethod("MuVM.set_trap_handler", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val _raw_trap_handler = _jffiBuffer.getAddress(1)
val _raw_userdata = _jffiBuffer.getAddress(2)
val mvm = getMicroVM(_raw_mvm)
val trap_handler = _raw_trap_handler
val userdata = _raw_userdata
val _RV = mvm.setTrapHandler(trap_handler, userdata)
}
val MUVM__EXECUTE = exposedMethod("MuVM.execute", JType.VOID, Array(JType.POINTER)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val mvm = getMicroVM(_raw_mvm)
val _RV = mvm.execute()
}
val MUVM__GET_MU_ERROR_PTR = exposedMethod("MuVM.get_mu_error_ptr", JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val _raw_mvm = _jffiBuffer.getAddress(0)
val mvm = getMicroVM(_raw_mvm)
val _RV = mvm.getMuErrorPtr()
_jffiBuffer.setAddressReturn(_RV)
}
val stubsOfMuVM = new Array[Word](6)
stubsOfMuVM(0) = MUVM__NEW_CONTEXT.address
stubsOfMuVM(1) = MUVM__ID_OF.address
stubsOfMuVM(2) = MUVM__NAME_OF.address
stubsOfMuVM(3) = MUVM__SET_TRAP_HANDLER.address
stubsOfMuVM(4) = MUVM__EXECUTE.address
stubsOfMuVM(5) = MUVM__GET_MU_ERROR_PTR.address
val MUCTX__ID_OF = exposedMethod("MuCtx.id_of", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_name = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val name = readCString(_raw_name)
val _RV = ctx.idOf(name)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__NAME_OF = exposedMethod("MuCtx.name_of", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val id = _raw_id
val _RV = ctx.nameOf(id)
val _RV_FAK = exposeString(_RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__CLOSE_CONTEXT = exposedMethod("MuCtx.close_context", JType.VOID, Array(JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val ctx = getMuCtx(_raw_ctx)
val _RV = ctx.closeContext()
}
val MUCTX__LOAD_BUNDLE = exposedMethod("MuCtx.load_bundle", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_buf = _jffiBuffer.getAddress(1)
val _raw_sz = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val buf = readCharArray(_raw_buf, _raw_sz)
val _RV = ctx.loadBundle(buf)
}
val MUCTX__LOAD_HAIL = exposedMethod("MuCtx.load_hail", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_buf = _jffiBuffer.getAddress(1)
val _raw_sz = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val buf = readCharArray(_raw_buf, _raw_sz)
val _RV = ctx.loadHail(buf)
}
val MUCTX__HANDLE_FROM_SINT8 = exposedMethod("MuCtx.handle_from_sint8", JType.POINTER, Array(JType.POINTER, JType.SINT8, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getByte(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromSInt8(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_UINT8 = exposedMethod("MuCtx.handle_from_uint8", JType.POINTER, Array(JType.POINTER, JType.UINT8, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getByte(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromUInt8(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_SINT16 = exposedMethod("MuCtx.handle_from_sint16", JType.POINTER, Array(JType.POINTER, JType.SINT16, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getShort(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromSInt16(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_UINT16 = exposedMethod("MuCtx.handle_from_uint16", JType.POINTER, Array(JType.POINTER, JType.UINT16, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getShort(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromUInt16(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_SINT32 = exposedMethod("MuCtx.handle_from_sint32", JType.POINTER, Array(JType.POINTER, JType.SINT32, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getInt(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromSInt32(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_UINT32 = exposedMethod("MuCtx.handle_from_uint32", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getInt(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromUInt32(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_SINT64 = exposedMethod("MuCtx.handle_from_sint64", JType.POINTER, Array(JType.POINTER, JType.SINT64, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getLong(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromSInt64(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_UINT64 = exposedMethod("MuCtx.handle_from_uint64", JType.POINTER, Array(JType.POINTER, JType.UINT64, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getLong(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val len = _raw_len
val _RV = ctx.handleFromUInt64(num, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_UINT64S = exposedMethod("MuCtx.handle_from_uint64s", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_nums = _jffiBuffer.getAddress(1)
val _raw_nnums = _jffiBuffer.getAddress(2)
val _raw_len = _jffiBuffer.getInt(3)
val ctx = getMuCtx(_raw_ctx)
val nums = readLongArray(_raw_nums, _raw_nnums)
val len = _raw_len
val _RV = ctx.handleFromUInt64s(nums, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_FLOAT = exposedMethod("MuCtx.handle_from_float", JType.POINTER, Array(JType.POINTER, JType.FLOAT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getFloat(1)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val _RV = ctx.handleFromFloat(num)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_DOUBLE = exposedMethod("MuCtx.handle_from_double", JType.POINTER, Array(JType.POINTER, JType.DOUBLE)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_num = _jffiBuffer.getDouble(1)
val ctx = getMuCtx(_raw_ctx)
val num = _raw_num
val _RV = ctx.handleFromDouble(num)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_PTR = exposedMethod("MuCtx.handle_from_ptr", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_mu_type = _jffiBuffer.getInt(1)
val _raw_ptr = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val mu_type = _raw_mu_type
val ptr = _raw_ptr
val _RV = ctx.handleFromPtr(mu_type, ptr)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_FP = exposedMethod("MuCtx.handle_from_fp", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_mu_type = _jffiBuffer.getInt(1)
val _raw_fp = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val mu_type = _raw_mu_type
val fp = _raw_fp
val _RV = ctx.handleFromFP(mu_type, fp)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_TO_SINT8 = exposedMethod("MuCtx.handle_to_sint8", JType.SINT8, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToSInt8(opnd)
_jffiBuffer.setByteReturn(_RV)
}
val MUCTX__HANDLE_TO_UINT8 = exposedMethod("MuCtx.handle_to_uint8", JType.UINT8, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToUInt8(opnd)
_jffiBuffer.setByteReturn(_RV)
}
val MUCTX__HANDLE_TO_SINT16 = exposedMethod("MuCtx.handle_to_sint16", JType.SINT16, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToSInt16(opnd)
_jffiBuffer.setShortReturn(_RV)
}
val MUCTX__HANDLE_TO_UINT16 = exposedMethod("MuCtx.handle_to_uint16", JType.UINT16, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToUInt16(opnd)
_jffiBuffer.setShortReturn(_RV)
}
val MUCTX__HANDLE_TO_SINT32 = exposedMethod("MuCtx.handle_to_sint32", JType.SINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToSInt32(opnd)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__HANDLE_TO_UINT32 = exposedMethod("MuCtx.handle_to_uint32", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToUInt32(opnd)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__HANDLE_TO_SINT64 = exposedMethod("MuCtx.handle_to_sint64", JType.SINT64, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToSInt64(opnd)
_jffiBuffer.setLongReturn(_RV)
}
val MUCTX__HANDLE_TO_UINT64 = exposedMethod("MuCtx.handle_to_uint64", JType.UINT64, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIntValue]
val _RV = ctx.handleToUInt64(opnd)
_jffiBuffer.setLongReturn(_RV)
}
val MUCTX__HANDLE_TO_FLOAT = exposedMethod("MuCtx.handle_to_float", JType.FLOAT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuFloatValue]
val _RV = ctx.handleToFloat(opnd)
_jffiBuffer.setFloatReturn(_RV)
}
val MUCTX__HANDLE_TO_DOUBLE = exposedMethod("MuCtx.handle_to_double", JType.DOUBLE, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuDoubleValue]
val _RV = ctx.handleToDouble(opnd)
_jffiBuffer.setDoubleReturn(_RV)
}
val MUCTX__HANDLE_TO_PTR = exposedMethod("MuCtx.handle_to_ptr", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuUPtrValue]
val _RV = ctx.handleToPtr(opnd)
_jffiBuffer.setAddressReturn(_RV)
}
val MUCTX__HANDLE_TO_FP = exposedMethod("MuCtx.handle_to_fp", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuUFPValue]
val _RV = ctx.handleToFP(opnd)
_jffiBuffer.setAddressReturn(_RV)
}
val MUCTX__HANDLE_FROM_CONST = exposedMethod("MuCtx.handle_from_const", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val id = _raw_id
val _RV = ctx.handleFromConst(id)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_GLOBAL = exposedMethod("MuCtx.handle_from_global", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val id = _raw_id
val _RV = ctx.handleFromGlobal(id)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_FUNC = exposedMethod("MuCtx.handle_from_func", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val id = _raw_id
val _RV = ctx.handleFromFunc(id)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__HANDLE_FROM_EXPOSE = exposedMethod("MuCtx.handle_from_expose", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_id = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val id = _raw_id
val _RV = ctx.handleFromExpose(id)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__DELETE_VALUE = exposedMethod("MuCtx.delete_value", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuValue]
val _RV = ctx.deleteValue(opnd)
}
val MUCTX__REF_EQ = exposedMethod("MuCtx.ref_eq", JType.SINT, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_lhs = _jffiBuffer.getAddress(1)
val _raw_rhs = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val lhs = getMuValueNotNull(_raw_lhs).asInstanceOf[MuGenRefValue]
val rhs = getMuValueNotNull(_raw_rhs).asInstanceOf[MuGenRefValue]
val _RV = ctx.refEq(lhs, rhs)
val _RV_FAK = booleanToInt(_RV)
_jffiBuffer.setIntReturn(_RV_FAK)
}
val MUCTX__REF_ULT = exposedMethod("MuCtx.ref_ult", JType.SINT, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_lhs = _jffiBuffer.getAddress(1)
val _raw_rhs = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val lhs = getMuValueNotNull(_raw_lhs).asInstanceOf[MuIRefValue]
val rhs = getMuValueNotNull(_raw_rhs).asInstanceOf[MuIRefValue]
val _RV = ctx.refUlt(lhs, rhs)
val _RV_FAK = booleanToInt(_RV)
_jffiBuffer.setIntReturn(_RV_FAK)
}
val MUCTX__EXTRACT_VALUE = exposedMethod("MuCtx.extract_value", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_str = _jffiBuffer.getAddress(1)
val _raw_index = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val str = getMuValueNotNull(_raw_str).asInstanceOf[MuStructValue]
val index = _raw_index
val _RV = ctx.extractValue(str, index)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__INSERT_VALUE = exposedMethod("MuCtx.insert_value", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_str = _jffiBuffer.getAddress(1)
val _raw_index = _jffiBuffer.getInt(2)
val _raw_newval = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val str = getMuValueNotNull(_raw_str).asInstanceOf[MuStructValue]
val index = _raw_index
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuValue]
val _RV = ctx.insertValue(str, index, newval)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__EXTRACT_ELEMENT = exposedMethod("MuCtx.extract_element", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_str = _jffiBuffer.getAddress(1)
val _raw_index = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val str = getMuValueNotNull(_raw_str).asInstanceOf[MuSeqValue]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuIntValue]
val _RV = ctx.extractElement(str, index)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__INSERT_ELEMENT = exposedMethod("MuCtx.insert_element", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_str = _jffiBuffer.getAddress(1)
val _raw_index = _jffiBuffer.getAddress(2)
val _raw_newval = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val str = getMuValueNotNull(_raw_str).asInstanceOf[MuSeqValue]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuIntValue]
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuValue]
val _RV = ctx.insertElement(str, index, newval)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_FIXED = exposedMethod("MuCtx.new_fixed", JType.POINTER, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_mu_type = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val mu_type = _raw_mu_type
val _RV = ctx.newFixed(mu_type)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_HYBRID = exposedMethod("MuCtx.new_hybrid", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_mu_type = _jffiBuffer.getInt(1)
val _raw_length = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val mu_type = _raw_mu_type
val length = getMuValueNotNull(_raw_length).asInstanceOf[MuIntValue]
val _RV = ctx.newHybrid(mu_type, length)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__REFCAST = exposedMethod("MuCtx.refcast", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val _raw_new_type = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuGenRefValue]
val new_type = _raw_new_type
val _RV = ctx.refcast(opnd, new_type)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__GET_IREF = exposedMethod("MuCtx.get_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuRefValue]
val _RV = ctx.getIRef(opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__GET_FIELD_IREF = exposedMethod("MuCtx.get_field_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val _raw_field = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIRefValue]
val field = _raw_field
val _RV = ctx.getFieldIRef(opnd, field)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__GET_ELEM_IREF = exposedMethod("MuCtx.get_elem_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val _raw_index = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIRefValue]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuIntValue]
val _RV = ctx.getElemIRef(opnd, index)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SHIFT_IREF = exposedMethod("MuCtx.shift_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val _raw_offset = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIRefValue]
val offset = getMuValueNotNull(_raw_offset).asInstanceOf[MuIntValue]
val _RV = ctx.shiftIRef(opnd, offset)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__GET_VAR_PART_IREF = exposedMethod("MuCtx.get_var_part_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_opnd = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuIRefValue]
val _RV = ctx.getVarPartIRef(opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__LOAD = exposedMethod("MuCtx.load", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ord = _jffiBuffer.getInt(1)
val _raw_loc = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val ord = toMemoryOrder(_raw_ord)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuIRefValue]
val _RV = ctx.load(ord, loc)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__STORE = exposedMethod("MuCtx.store", JType.VOID, Array(JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ord = _jffiBuffer.getInt(1)
val _raw_loc = _jffiBuffer.getAddress(2)
val _raw_newval = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val ord = toMemoryOrder(_raw_ord)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuIRefValue]
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuValue]
val _RV = ctx.store(ord, loc, newval)
}
val MUCTX__CMPXCHG = exposedMethod("MuCtx.cmpxchg", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.UINT32, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ord_succ = _jffiBuffer.getInt(1)
val _raw_ord_fail = _jffiBuffer.getInt(2)
val _raw_weak = _jffiBuffer.getInt(3)
val _raw_loc = _jffiBuffer.getAddress(4)
val _raw_expected = _jffiBuffer.getAddress(5)
val _raw_desired = _jffiBuffer.getAddress(6)
val _raw_is_succ = _jffiBuffer.getAddress(7)
val ctx = getMuCtx(_raw_ctx)
val ord_succ = toMemoryOrder(_raw_ord_succ)
val ord_fail = toMemoryOrder(_raw_ord_fail)
val weak = intToBoolean(_raw_weak)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuIRefValue]
val expected = getMuValueNotNull(_raw_expected).asInstanceOf[MuValue]
val desired = getMuValueNotNull(_raw_desired).asInstanceOf[MuValue]
val is_succ = _raw_is_succ
val _RV = ctx.cmpxchg(ord_succ, ord_fail, weak, loc, expected, desired, is_succ)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__ATOMICRMW = exposedMethod("MuCtx.atomicrmw", JType.POINTER, Array(JType.POINTER, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ord = _jffiBuffer.getInt(1)
val _raw_op = _jffiBuffer.getInt(2)
val _raw_loc = _jffiBuffer.getAddress(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val ord = toMemoryOrder(_raw_ord)
val op = toAtomicRMWOptr(_raw_op)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuIRefValue]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuValue]
val _RV = ctx.atomicrmw(ord, op, loc, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__FENCE = exposedMethod("MuCtx.fence", JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ord = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val ord = toMemoryOrder(_raw_ord)
val _RV = ctx.fence(ord)
}
val MUCTX__NEW_STACK = exposedMethod("MuCtx.new_stack", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_func = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val func = getMuValueNotNull(_raw_func).asInstanceOf[MuFuncRefValue]
val _RV = ctx.newStack(func)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_THREAD_NOR = exposedMethod("MuCtx.new_thread_nor", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_stack = _jffiBuffer.getAddress(1)
val _raw_threadlocal = _jffiBuffer.getAddress(2)
val _raw_vals = _jffiBuffer.getAddress(3)
val _raw_nvals = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuStackRefValue]
val threadlocal = getMuValueNullable(_raw_threadlocal).asInstanceOf[Option[MuRefValue]]
val vals = readMuValueArray(_raw_vals, _raw_nvals)
val _RV = ctx.newThreadNor(stack, threadlocal, vals)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_THREAD_EXC = exposedMethod("MuCtx.new_thread_exc", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_stack = _jffiBuffer.getAddress(1)
val _raw_threadlocal = _jffiBuffer.getAddress(2)
val _raw_exc = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuStackRefValue]
val threadlocal = getMuValueNullable(_raw_threadlocal).asInstanceOf[Option[MuRefValue]]
val exc = getMuValueNotNull(_raw_exc).asInstanceOf[MuRefValue]
val _RV = ctx.newThreadExc(stack, threadlocal, exc)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__KILL_STACK = exposedMethod("MuCtx.kill_stack", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_stack = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuStackRefValue]
val _RV = ctx.killStack(stack)
}
val MUCTX__SET_THREADLOCAL = exposedMethod("MuCtx.set_threadlocal", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_thread = _jffiBuffer.getAddress(1)
val _raw_threadlocal = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val thread = getMuValueNotNull(_raw_thread).asInstanceOf[MuThreadRefValue]
val threadlocal = getMuValueNotNull(_raw_threadlocal).asInstanceOf[MuRefValue]
val _RV = ctx.setThreadlocal(thread, threadlocal)
}
val MUCTX__GET_THREADLOCAL = exposedMethod("MuCtx.get_threadlocal", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_thread = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val thread = getMuValueNotNull(_raw_thread).asInstanceOf[MuThreadRefValue]
val _RV = ctx.getThreadlocal(thread)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CURSOR = exposedMethod("MuCtx.new_cursor", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_stack = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuStackRefValue]
val _RV = ctx.newCursor(stack)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEXT_FRAME = exposedMethod("MuCtx.next_frame", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.nextFrame(cursor)
}
val MUCTX__COPY_CURSOR = exposedMethod("MuCtx.copy_cursor", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.copyCursor(cursor)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__CLOSE_CURSOR = exposedMethod("MuCtx.close_cursor", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.closeCursor(cursor)
}
val MUCTX__CUR_FUNC = exposedMethod("MuCtx.cur_func", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.curFunc(cursor)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__CUR_FUNC_VER = exposedMethod("MuCtx.cur_func_ver", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.curFuncVer(cursor)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__CUR_INST = exposedMethod("MuCtx.cur_inst", JType.UINT32, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.curInst(cursor)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__DUMP_KEEPALIVES = exposedMethod("MuCtx.dump_keepalives", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val _raw_results = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val results = _raw_results
val _RV = ctx.dumpKeepalives(cursor, results)
}
val MUCTX__POP_FRAMES_TO = exposedMethod("MuCtx.pop_frames_to", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_cursor = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val cursor = getMuValueNotNull(_raw_cursor).asInstanceOf[MuFCRefValue]
val _RV = ctx.popFramesTo(cursor)
}
val MUCTX__PUSH_FRAME = exposedMethod("MuCtx.push_frame", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_stack = _jffiBuffer.getAddress(1)
val _raw_func = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuStackRefValue]
val func = getMuValueNotNull(_raw_func).asInstanceOf[MuFuncRefValue]
val _RV = ctx.pushFrame(stack, func)
}
val MUCTX__TR64_IS_FP = exposedMethod("MuCtx.tr64_is_fp", JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64IsFP(value)
val _RV_FAK = booleanToInt(_RV)
_jffiBuffer.setIntReturn(_RV_FAK)
}
val MUCTX__TR64_IS_INT = exposedMethod("MuCtx.tr64_is_int", JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64IsInt(value)
val _RV_FAK = booleanToInt(_RV)
_jffiBuffer.setIntReturn(_RV_FAK)
}
val MUCTX__TR64_IS_REF = exposedMethod("MuCtx.tr64_is_ref", JType.SINT, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64IsRef(value)
val _RV_FAK = booleanToInt(_RV)
_jffiBuffer.setIntReturn(_RV_FAK)
}
val MUCTX__TR64_TO_FP = exposedMethod("MuCtx.tr64_to_fp", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64ToFP(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_TO_INT = exposedMethod("MuCtx.tr64_to_int", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64ToInt(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_TO_REF = exposedMethod("MuCtx.tr64_to_ref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64ToRef(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_TO_TAG = exposedMethod("MuCtx.tr64_to_tag", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuTagRef64Value]
val _RV = ctx.tr64ToTag(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_FROM_FP = exposedMethod("MuCtx.tr64_from_fp", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuDoubleValue]
val _RV = ctx.tr64FromFP(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_FROM_INT = exposedMethod("MuCtx.tr64_from_int", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_value = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuIntValue]
val _RV = ctx.tr64FromInt(value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__TR64_FROM_REF = exposedMethod("MuCtx.tr64_from_ref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ref = _jffiBuffer.getAddress(1)
val _raw_tag = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val ref = getMuValueNotNull(_raw_ref).asInstanceOf[MuRefValue]
val tag = getMuValueNotNull(_raw_tag).asInstanceOf[MuIntValue]
val _RV = ctx.tr64FromRef(ref, tag)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__ENABLE_WATCHPOINT = exposedMethod("MuCtx.enable_watchpoint", JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_wpid = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val wpid = _raw_wpid
val _RV = ctx.enableWatchPoint(wpid)
}
val MUCTX__DISABLE_WATCHPOINT = exposedMethod("MuCtx.disable_watchpoint", JType.VOID, Array(JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_wpid = _jffiBuffer.getInt(1)
val ctx = getMuCtx(_raw_ctx)
val wpid = _raw_wpid
val _RV = ctx.disableWatchPoint(wpid)
}
val MUCTX__PIN = exposedMethod("MuCtx.pin", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_loc = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuValue]
val _RV = ctx.pin(loc)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__UNPIN = exposedMethod("MuCtx.unpin", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_loc = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuValue]
val _RV = ctx.unpin(loc)
}
val MUCTX__EXPOSE = exposedMethod("MuCtx.expose", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_func = _jffiBuffer.getAddress(1)
val _raw_call_conv = _jffiBuffer.getInt(2)
val _raw_cookie = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val func = getMuValueNotNull(_raw_func).asInstanceOf[MuFuncRefValue]
val call_conv = toFlag(_raw_call_conv)
val cookie = getMuValueNotNull(_raw_cookie).asInstanceOf[MuIntValue]
val _RV = ctx.expose(func, call_conv, cookie)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__UNEXPOSE = exposedMethod("MuCtx.unexpose", JType.VOID, Array(JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_call_conv = _jffiBuffer.getInt(1)
val _raw_value = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val call_conv = toFlag(_raw_call_conv)
val value = getMuValueNotNull(_raw_value).asInstanceOf[MuValue]
val _RV = ctx.unexpose(call_conv, value)
}
val MUCTX__NEW_BUNDLE = exposedMethod("MuCtx.new_bundle", JType.POINTER, Array(JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val ctx = getMuCtx(_raw_ctx)
val _RV = ctx.newBundle()
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__LOAD_BUNDLE_FROM_NODE = exposedMethod("MuCtx.load_bundle_from_node", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.loadBundleFromNode(b)
}
val MUCTX__ABORT_BUNDLE_NODE = exposedMethod("MuCtx.abort_bundle_node", JType.VOID, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.abortBundleNode(b)
}
val MUCTX__GET_NODE = exposedMethod("MuCtx.get_node", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_id = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val id = _raw_id
val _RV = ctx.getNode(b, id)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__GET_ID = exposedMethod("MuCtx.get_id", JType.UINT32, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_node = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val node = getMuValueNotNull(_raw_node).asInstanceOf[MuChildNode]
val _RV = ctx.getID(b, node)
_jffiBuffer.setIntReturn(_RV)
}
val MUCTX__SET_NAME = exposedMethod("MuCtx.set_name", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_node = _jffiBuffer.getAddress(2)
val _raw_name = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val node = getMuValueNotNull(_raw_node).asInstanceOf[MuChildNode]
val name = readCString(_raw_name)
val _RV = ctx.setName(b, node, name)
}
val MUCTX__NEW_TYPE_INT = exposedMethod("MuCtx.new_type_int", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_len = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val len = _raw_len
val _RV = ctx.newTypeInt(b, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_FLOAT = exposedMethod("MuCtx.new_type_float", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeFloat(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_DOUBLE = exposedMethod("MuCtx.new_type_double", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeDouble(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_UPTR = exposedMethod("MuCtx.new_type_uptr", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeUPtr(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_UPTR = exposedMethod("MuCtx.set_type_uptr", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_uptr = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val uptr = getMuValueNotNull(_raw_uptr).asInstanceOf[MuTypeNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.setTypeUPtr(uptr, ty)
}
val MUCTX__NEW_TYPE_UFUNCPTR = exposedMethod("MuCtx.new_type_ufuncptr", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeUFuncPtr(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_UFUNCPTR = exposedMethod("MuCtx.set_type_ufuncptr", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ufuncptr = _jffiBuffer.getAddress(1)
val _raw_sig = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val ufuncptr = getMuValueNotNull(_raw_ufuncptr).asInstanceOf[MuTypeNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val _RV = ctx.setTypeUFuncPtr(ufuncptr, sig)
}
val MUCTX__NEW_TYPE_STRUCT = exposedMethod("MuCtx.new_type_struct", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_fieldtys = _jffiBuffer.getAddress(2)
val _raw_nfieldtys = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val fieldtys = readMuValueArray(_raw_fieldtys, _raw_nfieldtys)
val _RV = ctx.newTypeStruct(b, fieldtys)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_HYBRID = exposedMethod("MuCtx.new_type_hybrid", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_fixedtys = _jffiBuffer.getAddress(2)
val _raw_nfixedtys = _jffiBuffer.getAddress(3)
val _raw_varty = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val fixedtys = readMuValueArray(_raw_fixedtys, _raw_nfixedtys)
val varty = getMuValueNotNull(_raw_varty).asInstanceOf[MuTypeNode]
val _RV = ctx.newTypeHybrid(b, fixedtys, varty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_ARRAY = exposedMethod("MuCtx.new_type_array", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_elemty = _jffiBuffer.getAddress(2)
val _raw_len = _jffiBuffer.getLong(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val elemty = getMuValueNotNull(_raw_elemty).asInstanceOf[MuTypeNode]
val len = _raw_len
val _RV = ctx.newTypeArray(b, elemty, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_VECTOR = exposedMethod("MuCtx.new_type_vector", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_elemty = _jffiBuffer.getAddress(2)
val _raw_len = _jffiBuffer.getLong(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val elemty = getMuValueNotNull(_raw_elemty).asInstanceOf[MuTypeNode]
val len = _raw_len
val _RV = ctx.newTypeVector(b, elemty, len)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_VOID = exposedMethod("MuCtx.new_type_void", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeVoid(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_REF = exposedMethod("MuCtx.new_type_ref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_REF = exposedMethod("MuCtx.set_type_ref", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_ref = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val ref = getMuValueNotNull(_raw_ref).asInstanceOf[MuTypeNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.setTypeRef(ref, ty)
}
val MUCTX__NEW_TYPE_IREF = exposedMethod("MuCtx.new_type_iref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeIRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_IREF = exposedMethod("MuCtx.set_type_iref", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_iref = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val iref = getMuValueNotNull(_raw_iref).asInstanceOf[MuTypeNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.setTypeIRef(iref, ty)
}
val MUCTX__NEW_TYPE_WEAKREF = exposedMethod("MuCtx.new_type_weakref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeWeakRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_WEAKREF = exposedMethod("MuCtx.set_type_weakref", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_weakref = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val weakref = getMuValueNotNull(_raw_weakref).asInstanceOf[MuTypeNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.setTypeWeakRef(weakref, ty)
}
val MUCTX__NEW_TYPE_FUNCREF = exposedMethod("MuCtx.new_type_funcref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeFuncRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_TYPE_FUNCREF = exposedMethod("MuCtx.set_type_funcref", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_funcref = _jffiBuffer.getAddress(1)
val _raw_sig = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val funcref = getMuValueNotNull(_raw_funcref).asInstanceOf[MuTypeNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val _RV = ctx.setTypeFuncRef(funcref, sig)
}
val MUCTX__NEW_TYPE_TAGREF64 = exposedMethod("MuCtx.new_type_tagref64", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeTagRef64(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_THREADREF = exposedMethod("MuCtx.new_type_threadref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeThreadRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_STACKREF = exposedMethod("MuCtx.new_type_stackref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeStackRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_FRAMECURSORREF = exposedMethod("MuCtx.new_type_framecursorref", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeFrameCursorRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TYPE_IRNODEREF = exposedMethod("MuCtx.new_type_irnoderef", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val _RV = ctx.newTypeIRNodeRef(b)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_FUNCSIG = exposedMethod("MuCtx.new_funcsig", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_paramtys = _jffiBuffer.getAddress(2)
val _raw_nparamtys = _jffiBuffer.getAddress(3)
val _raw_rettys = _jffiBuffer.getAddress(4)
val _raw_nrettys = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val paramtys = readMuValueArray(_raw_paramtys, _raw_nparamtys)
val rettys = readMuValueArray(_raw_rettys, _raw_nrettys)
val _RV = ctx.newFuncSig(b, paramtys, rettys)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_INT = exposedMethod("MuCtx.new_const_int", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT64)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val _raw_value = _jffiBuffer.getLong(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val value = _raw_value
val _RV = ctx.newConstInt(b, ty, value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_INT_EX = exposedMethod("MuCtx.new_const_int_ex", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val _raw_values = _jffiBuffer.getAddress(3)
val _raw_nvalues = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val values = readLongArray(_raw_values, _raw_nvalues)
val _RV = ctx.newConstIntEx(b, ty, values)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_FLOAT = exposedMethod("MuCtx.new_const_float", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.FLOAT)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val _raw_value = _jffiBuffer.getFloat(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val value = _raw_value
val _RV = ctx.newConstFloat(b, ty, value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_DOUBLE = exposedMethod("MuCtx.new_const_double", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.DOUBLE)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val _raw_value = _jffiBuffer.getDouble(3)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val value = _raw_value
val _RV = ctx.newConstDouble(b, ty, value)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_NULL = exposedMethod("MuCtx.new_const_null", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.newConstNull(b, ty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONST_SEQ = exposedMethod("MuCtx.new_const_seq", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val _raw_elems = _jffiBuffer.getAddress(3)
val _raw_nelems = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val elems = readMuValueArray(_raw_elems, _raw_nelems)
val _RV = ctx.newConstSeq(b, ty, elems)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_GLOBAL_CELL = exposedMethod("MuCtx.new_global_cell", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.newGlobalCell(b, ty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_FUNC = exposedMethod("MuCtx.new_func", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_sig = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val _RV = ctx.newFunc(b, sig)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_FUNC_VER = exposedMethod("MuCtx.new_func_ver", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_func = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val func = getMuValueNotNull(_raw_func).asInstanceOf[MuFuncNode]
val _RV = ctx.newFuncVer(b, func)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_EXP_FUNC = exposedMethod("MuCtx.new_exp_func", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_b = _jffiBuffer.getAddress(1)
val _raw_func = _jffiBuffer.getAddress(2)
val _raw_callconv = _jffiBuffer.getInt(3)
val _raw_cookie = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val b = getMuValueNotNull(_raw_b).asInstanceOf[MuBundleNode]
val func = getMuValueNotNull(_raw_func).asInstanceOf[MuFuncNode]
val callconv = toFlag(_raw_callconv)
val cookie = getMuValueNotNull(_raw_cookie).asInstanceOf[MuConstNode]
val _RV = ctx.newExpFunc(b, func, callconv, cookie)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_BB = exposedMethod("MuCtx.new_bb", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_fv = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val fv = getMuValueNotNull(_raw_fv).asInstanceOf[MuFuncVerNode]
val _RV = ctx.newBB(fv)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_NOR_PARAM = exposedMethod("MuCtx.new_nor_param", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_ty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val _RV = ctx.newNorParam(bb, ty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_EXC_PARAM = exposedMethod("MuCtx.new_exc_param", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val _RV = ctx.newExcParam(bb)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_INST_RES = exposedMethod("MuCtx.new_inst_res", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_inst = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val inst = getMuValueNotNull(_raw_inst).asInstanceOf[MuInstNode]
val _RV = ctx.newInstRes(inst)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__ADD_DEST = exposedMethod("MuCtx.add_dest", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_inst = _jffiBuffer.getAddress(1)
val _raw_kind = _jffiBuffer.getInt(2)
val _raw_dest = _jffiBuffer.getAddress(3)
val _raw_vars = _jffiBuffer.getAddress(4)
val _raw_nvars = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val inst = getMuValueNotNull(_raw_inst).asInstanceOf[MuInstNode]
val kind = toDestKind(_raw_kind)
val dest = getMuValueNotNull(_raw_dest).asInstanceOf[MuBBNode]
val vars = readMuValueArray(_raw_vars, _raw_nvars)
val _RV = ctx.addDest(inst, kind, dest, vars)
}
val MUCTX__ADD_KEEPALIVES = exposedMethod("MuCtx.add_keepalives", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_inst = _jffiBuffer.getAddress(1)
val _raw_vars = _jffiBuffer.getAddress(2)
val _raw_nvars = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val inst = getMuValueNotNull(_raw_inst).asInstanceOf[MuInstNode]
val vars = readMuValueArray(_raw_vars, _raw_nvars)
val _RV = ctx.addKeepalives(inst, vars)
}
val MUCTX__NEW_BINOP = exposedMethod("MuCtx.new_binop", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_optr = _jffiBuffer.getInt(2)
val _raw_ty = _jffiBuffer.getAddress(3)
val _raw_opnd1 = _jffiBuffer.getAddress(4)
val _raw_opnd2 = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val optr = toBinOptr(_raw_optr)
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val opnd1 = getMuValueNotNull(_raw_opnd1).asInstanceOf[MuVarNode]
val opnd2 = getMuValueNotNull(_raw_opnd2).asInstanceOf[MuVarNode]
val _RV = ctx.newBinOp(bb, optr, ty, opnd1, opnd2)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CMP = exposedMethod("MuCtx.new_cmp", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_optr = _jffiBuffer.getInt(2)
val _raw_ty = _jffiBuffer.getAddress(3)
val _raw_opnd1 = _jffiBuffer.getAddress(4)
val _raw_opnd2 = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val optr = toCmpOptr(_raw_optr)
val ty = getMuValueNotNull(_raw_ty).asInstanceOf[MuTypeNode]
val opnd1 = getMuValueNotNull(_raw_opnd1).asInstanceOf[MuVarNode]
val opnd2 = getMuValueNotNull(_raw_opnd2).asInstanceOf[MuVarNode]
val _RV = ctx.newCmp(bb, optr, ty, opnd1, opnd2)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CONV = exposedMethod("MuCtx.new_conv", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_optr = _jffiBuffer.getInt(2)
val _raw_from_ty = _jffiBuffer.getAddress(3)
val _raw_to_ty = _jffiBuffer.getAddress(4)
val _raw_opnd = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val optr = toConvOptr(_raw_optr)
val from_ty = getMuValueNotNull(_raw_from_ty).asInstanceOf[MuTypeNode]
val to_ty = getMuValueNotNull(_raw_to_ty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newConv(bb, optr, from_ty, to_ty, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SELECT = exposedMethod("MuCtx.new_select", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_cond_ty = _jffiBuffer.getAddress(2)
val _raw_opnd_ty = _jffiBuffer.getAddress(3)
val _raw_cond = _jffiBuffer.getAddress(4)
val _raw_if_true = _jffiBuffer.getAddress(5)
val _raw_if_false = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val cond_ty = getMuValueNotNull(_raw_cond_ty).asInstanceOf[MuTypeNode]
val opnd_ty = getMuValueNotNull(_raw_opnd_ty).asInstanceOf[MuTypeNode]
val cond = getMuValueNotNull(_raw_cond).asInstanceOf[MuVarNode]
val if_true = getMuValueNotNull(_raw_if_true).asInstanceOf[MuVarNode]
val if_false = getMuValueNotNull(_raw_if_false).asInstanceOf[MuVarNode]
val _RV = ctx.newSelect(bb, cond_ty, opnd_ty, cond, if_true, if_false)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_BRANCH = exposedMethod("MuCtx.new_branch", JType.POINTER, Array(JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val _RV = ctx.newBranch(bb)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_BRANCH2 = exposedMethod("MuCtx.new_branch2", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_cond = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val cond = getMuValueNotNull(_raw_cond).asInstanceOf[MuVarNode]
val _RV = ctx.newBranch2(bb, cond)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SWITCH = exposedMethod("MuCtx.new_switch", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_opnd_ty = _jffiBuffer.getAddress(2)
val _raw_opnd = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val opnd_ty = getMuValueNotNull(_raw_opnd_ty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newSwitch(bb, opnd_ty, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__ADD_SWITCH_DEST = exposedMethod("MuCtx.add_switch_dest", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_sw = _jffiBuffer.getAddress(1)
val _raw_key = _jffiBuffer.getAddress(2)
val _raw_dest = _jffiBuffer.getAddress(3)
val _raw_vars = _jffiBuffer.getAddress(4)
val _raw_nvars = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val sw = getMuValueNotNull(_raw_sw).asInstanceOf[MuInstNode]
val key = getMuValueNotNull(_raw_key).asInstanceOf[MuConstNode]
val dest = getMuValueNotNull(_raw_dest).asInstanceOf[MuBBNode]
val vars = readMuValueArray(_raw_vars, _raw_nvars)
val _RV = ctx.addSwitchDest(sw, key, dest, vars)
}
val MUCTX__NEW_CALL = exposedMethod("MuCtx.new_call", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_sig = _jffiBuffer.getAddress(2)
val _raw_callee = _jffiBuffer.getAddress(3)
val _raw_args = _jffiBuffer.getAddress(4)
val _raw_nargs = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val callee = getMuValueNotNull(_raw_callee).asInstanceOf[MuVarNode]
val args = readMuValueArray(_raw_args, _raw_nargs)
val _RV = ctx.newCall(bb, sig, callee, args)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TAILCALL = exposedMethod("MuCtx.new_tailcall", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_sig = _jffiBuffer.getAddress(2)
val _raw_callee = _jffiBuffer.getAddress(3)
val _raw_args = _jffiBuffer.getAddress(4)
val _raw_nargs = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val callee = getMuValueNotNull(_raw_callee).asInstanceOf[MuVarNode]
val args = readMuValueArray(_raw_args, _raw_nargs)
val _RV = ctx.newTailCall(bb, sig, callee, args)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_RET = exposedMethod("MuCtx.new_ret", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_rvs = _jffiBuffer.getAddress(2)
val _raw_nrvs = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val rvs = readMuValueArray(_raw_rvs, _raw_nrvs)
val _RV = ctx.newRet(bb, rvs)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_THROW = exposedMethod("MuCtx.new_throw", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_exc = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val exc = getMuValueNotNull(_raw_exc).asInstanceOf[MuVarNode]
val _RV = ctx.newThrow(bb, exc)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_EXTRACTVALUE = exposedMethod("MuCtx.new_extractvalue", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_strty = _jffiBuffer.getAddress(2)
val _raw_index = _jffiBuffer.getInt(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val strty = getMuValueNotNull(_raw_strty).asInstanceOf[MuTypeNode]
val index = _raw_index
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newExtractValue(bb, strty, index, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_INSERTVALUE = exposedMethod("MuCtx.new_insertvalue", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_strty = _jffiBuffer.getAddress(2)
val _raw_index = _jffiBuffer.getInt(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val _raw_newval = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val strty = getMuValueNotNull(_raw_strty).asInstanceOf[MuTypeNode]
val index = _raw_index
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuVarNode]
val _RV = ctx.newInsertValue(bb, strty, index, opnd, newval)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_EXTRACTELEMENT = exposedMethod("MuCtx.new_extractelement", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_seqty = _jffiBuffer.getAddress(2)
val _raw_indty = _jffiBuffer.getAddress(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val _raw_index = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val seqty = getMuValueNotNull(_raw_seqty).asInstanceOf[MuTypeNode]
val indty = getMuValueNotNull(_raw_indty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuVarNode]
val _RV = ctx.newExtractElement(bb, seqty, indty, opnd, index)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_INSERTELEMENT = exposedMethod("MuCtx.new_insertelement", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_seqty = _jffiBuffer.getAddress(2)
val _raw_indty = _jffiBuffer.getAddress(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val _raw_index = _jffiBuffer.getAddress(5)
val _raw_newval = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val seqty = getMuValueNotNull(_raw_seqty).asInstanceOf[MuTypeNode]
val indty = getMuValueNotNull(_raw_indty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuVarNode]
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuVarNode]
val _RV = ctx.newInsertElement(bb, seqty, indty, opnd, index, newval)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SHUFFLEVECTOR = exposedMethod("MuCtx.new_shufflevector", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_vecty = _jffiBuffer.getAddress(2)
val _raw_maskty = _jffiBuffer.getAddress(3)
val _raw_vec1 = _jffiBuffer.getAddress(4)
val _raw_vec2 = _jffiBuffer.getAddress(5)
val _raw_mask = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val vecty = getMuValueNotNull(_raw_vecty).asInstanceOf[MuTypeNode]
val maskty = getMuValueNotNull(_raw_maskty).asInstanceOf[MuTypeNode]
val vec1 = getMuValueNotNull(_raw_vec1).asInstanceOf[MuVarNode]
val vec2 = getMuValueNotNull(_raw_vec2).asInstanceOf[MuVarNode]
val mask = getMuValueNotNull(_raw_mask).asInstanceOf[MuVarNode]
val _RV = ctx.newShuffleVector(bb, vecty, maskty, vec1, vec2, mask)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_NEW = exposedMethod("MuCtx.new_new", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_allocty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val allocty = getMuValueNotNull(_raw_allocty).asInstanceOf[MuTypeNode]
val _RV = ctx.newNew(bb, allocty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_NEWHYBRID = exposedMethod("MuCtx.new_newhybrid", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_allocty = _jffiBuffer.getAddress(2)
val _raw_lenty = _jffiBuffer.getAddress(3)
val _raw_length = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val allocty = getMuValueNotNull(_raw_allocty).asInstanceOf[MuTypeNode]
val lenty = getMuValueNotNull(_raw_lenty).asInstanceOf[MuTypeNode]
val length = getMuValueNotNull(_raw_length).asInstanceOf[MuVarNode]
val _RV = ctx.newNewHybrid(bb, allocty, lenty, length)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_ALLOCA = exposedMethod("MuCtx.new_alloca", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_allocty = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val allocty = getMuValueNotNull(_raw_allocty).asInstanceOf[MuTypeNode]
val _RV = ctx.newAlloca(bb, allocty)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_ALLOCAHYBRID = exposedMethod("MuCtx.new_allocahybrid", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_allocty = _jffiBuffer.getAddress(2)
val _raw_lenty = _jffiBuffer.getAddress(3)
val _raw_length = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val allocty = getMuValueNotNull(_raw_allocty).asInstanceOf[MuTypeNode]
val lenty = getMuValueNotNull(_raw_lenty).asInstanceOf[MuTypeNode]
val length = getMuValueNotNull(_raw_length).asInstanceOf[MuVarNode]
val _RV = ctx.newAllocaHybrid(bb, allocty, lenty, length)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_GETIREF = exposedMethod("MuCtx.new_getiref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_refty = _jffiBuffer.getAddress(2)
val _raw_opnd = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newGetIRef(bb, refty, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_GETFIELDIREF = exposedMethod("MuCtx.new_getfieldiref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.SINT, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_refty = _jffiBuffer.getAddress(3)
val _raw_index = _jffiBuffer.getInt(4)
val _raw_opnd = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val index = _raw_index
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newGetFieldIRef(bb, is_ptr, refty, index, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_GETELEMIREF = exposedMethod("MuCtx.new_getelemiref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_refty = _jffiBuffer.getAddress(3)
val _raw_indty = _jffiBuffer.getAddress(4)
val _raw_opnd = _jffiBuffer.getAddress(5)
val _raw_index = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val indty = getMuValueNotNull(_raw_indty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val index = getMuValueNotNull(_raw_index).asInstanceOf[MuVarNode]
val _RV = ctx.newGetElemIRef(bb, is_ptr, refty, indty, opnd, index)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SHIFTIREF = exposedMethod("MuCtx.new_shiftiref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_refty = _jffiBuffer.getAddress(3)
val _raw_offty = _jffiBuffer.getAddress(4)
val _raw_opnd = _jffiBuffer.getAddress(5)
val _raw_offset = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val offty = getMuValueNotNull(_raw_offty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val offset = getMuValueNotNull(_raw_offset).asInstanceOf[MuVarNode]
val _RV = ctx.newShiftIRef(bb, is_ptr, refty, offty, opnd, offset)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_GETVARPARTIREF = exposedMethod("MuCtx.new_getvarpartiref", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_refty = _jffiBuffer.getAddress(3)
val _raw_opnd = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newGetVarPartIRef(bb, is_ptr, refty, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_LOAD = exposedMethod("MuCtx.new_load", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_ord = _jffiBuffer.getInt(3)
val _raw_refty = _jffiBuffer.getAddress(4)
val _raw_loc = _jffiBuffer.getAddress(5)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val ord = toMemoryOrder(_raw_ord)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuVarNode]
val _RV = ctx.newLoad(bb, is_ptr, ord, refty, loc)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_STORE = exposedMethod("MuCtx.new_store", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_ord = _jffiBuffer.getInt(3)
val _raw_refty = _jffiBuffer.getAddress(4)
val _raw_loc = _jffiBuffer.getAddress(5)
val _raw_newval = _jffiBuffer.getAddress(6)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val ord = toMemoryOrder(_raw_ord)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuVarNode]
val newval = getMuValueNotNull(_raw_newval).asInstanceOf[MuVarNode]
val _RV = ctx.newStore(bb, is_ptr, ord, refty, loc, newval)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CMPXCHG = exposedMethod("MuCtx.new_cmpxchg", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.SINT, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_is_weak = _jffiBuffer.getInt(3)
val _raw_ord_succ = _jffiBuffer.getInt(4)
val _raw_ord_fail = _jffiBuffer.getInt(5)
val _raw_refty = _jffiBuffer.getAddress(6)
val _raw_loc = _jffiBuffer.getAddress(7)
val _raw_expected = _jffiBuffer.getAddress(8)
val _raw_desired = _jffiBuffer.getAddress(9)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val is_weak = intToBoolean(_raw_is_weak)
val ord_succ = toMemoryOrder(_raw_ord_succ)
val ord_fail = toMemoryOrder(_raw_ord_fail)
val refty = getMuValueNotNull(_raw_refty).asInstanceOf[MuTypeNode]
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuVarNode]
val expected = getMuValueNotNull(_raw_expected).asInstanceOf[MuVarNode]
val desired = getMuValueNotNull(_raw_desired).asInstanceOf[MuVarNode]
val _RV = ctx.newCmpXchg(bb, is_ptr, is_weak, ord_succ, ord_fail, refty, loc, expected, desired)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_ATOMICRMW = exposedMethod("MuCtx.new_atomicrmw", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.SINT, JType.UINT32, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_is_ptr = _jffiBuffer.getInt(2)
val _raw_ord = _jffiBuffer.getInt(3)
val _raw_optr = _jffiBuffer.getInt(4)
val _raw_refTy = _jffiBuffer.getAddress(5)
val _raw_loc = _jffiBuffer.getAddress(6)
val _raw_opnd = _jffiBuffer.getAddress(7)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val is_ptr = intToBoolean(_raw_is_ptr)
val ord = toMemoryOrder(_raw_ord)
val optr = toAtomicRMWOptr(_raw_optr)
val refTy = getMuValueNotNull(_raw_refTy).asInstanceOf[MuTypeNode]
val loc = getMuValueNotNull(_raw_loc).asInstanceOf[MuVarNode]
val opnd = getMuValueNotNull(_raw_opnd).asInstanceOf[MuVarNode]
val _RV = ctx.newAtomicRMW(bb, is_ptr, ord, optr, refTy, loc, opnd)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_FENCE = exposedMethod("MuCtx.new_fence", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_ord = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val ord = toMemoryOrder(_raw_ord)
val _RV = ctx.newFence(bb, ord)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_TRAP = exposedMethod("MuCtx.new_trap", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_rettys = _jffiBuffer.getAddress(2)
val _raw_nrettys = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val rettys = readMuValueArray(_raw_rettys, _raw_nrettys)
val _RV = ctx.newTrap(bb, rettys)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_WATCHPOINT = exposedMethod("MuCtx.new_watchpoint", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_wpid = _jffiBuffer.getInt(2)
val _raw_rettys = _jffiBuffer.getAddress(3)
val _raw_nrettys = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val wpid = _raw_wpid
val rettys = readMuValueArray(_raw_rettys, _raw_nrettys)
val _RV = ctx.newWatchPoint(bb, wpid, rettys)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_WPBRANCH = exposedMethod("MuCtx.new_wpbranch", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_wpid = _jffiBuffer.getInt(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val wpid = _raw_wpid
val _RV = ctx.newWPBranch(bb, wpid)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_CCALL = exposedMethod("MuCtx.new_ccall", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_callconv = _jffiBuffer.getInt(2)
val _raw_callee_ty = _jffiBuffer.getAddress(3)
val _raw_sig = _jffiBuffer.getAddress(4)
val _raw_callee = _jffiBuffer.getAddress(5)
val _raw_args = _jffiBuffer.getAddress(6)
val _raw_nargs = _jffiBuffer.getAddress(7)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val callconv = toFlag(_raw_callconv)
val callee_ty = getMuValueNotNull(_raw_callee_ty).asInstanceOf[MuTypeNode]
val sig = getMuValueNotNull(_raw_sig).asInstanceOf[MuFuncSigNode]
val callee = getMuValueNotNull(_raw_callee).asInstanceOf[MuVarNode]
val args = readMuValueArray(_raw_args, _raw_nargs)
val _RV = ctx.newCCall(bb, callconv, callee_ty, sig, callee, args)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_NEWTHREAD = exposedMethod("MuCtx.new_newthread", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_stack = _jffiBuffer.getAddress(2)
val _raw_threadlocal = _jffiBuffer.getAddress(3)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val stack = getMuValueNotNull(_raw_stack).asInstanceOf[MuVarNode]
val threadlocal = getMuValueNotNull(_raw_threadlocal).asInstanceOf[MuVarNode]
val _RV = ctx.newNewThread(bb, stack, threadlocal)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SWAPSTACK_RET = exposedMethod("MuCtx.new_swapstack_ret", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_swappee = _jffiBuffer.getAddress(2)
val _raw_ret_tys = _jffiBuffer.getAddress(3)
val _raw_nret_tys = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val swappee = getMuValueNotNull(_raw_swappee).asInstanceOf[MuVarNode]
val ret_tys = readMuValueArray(_raw_ret_tys, _raw_nret_tys)
val _RV = ctx.newSwapStackRet(bb, swappee, ret_tys)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__NEW_SWAPSTACK_KILL = exposedMethod("MuCtx.new_swapstack_kill", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_swappee = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val swappee = getMuValueNotNull(_raw_swappee).asInstanceOf[MuVarNode]
val _RV = ctx.newSwapStackKill(bb, swappee)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val MUCTX__SET_NEWSTACK_PASS_VALUES = exposedMethod("MuCtx.set_newstack_pass_values", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_inst = _jffiBuffer.getAddress(1)
val _raw_tys = _jffiBuffer.getAddress(2)
val _raw_vars = _jffiBuffer.getAddress(3)
val _raw_nvars = _jffiBuffer.getAddress(4)
val ctx = getMuCtx(_raw_ctx)
val inst = getMuValueNotNull(_raw_inst).asInstanceOf[MuInstNode]
val tys = readMuValueArray(_raw_tys, _raw_nvars)
val vars = readMuValueArray(_raw_vars, _raw_nvars)
val _RV = ctx.setNewStackPassValues(inst, tys, vars)
}
val MUCTX__SET_NEWSTACK_THROW_EXC = exposedMethod("MuCtx.set_newstack_throw_exc", JType.VOID, Array(JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_inst = _jffiBuffer.getAddress(1)
val _raw_exc = _jffiBuffer.getAddress(2)
val ctx = getMuCtx(_raw_ctx)
val inst = getMuValueNotNull(_raw_inst).asInstanceOf[MuInstNode]
val exc = getMuValueNotNull(_raw_exc).asInstanceOf[MuVarNode]
val _RV = ctx.setNewStackThrowExc(inst, exc)
}
val MUCTX__NEW_COMMINST = exposedMethod("MuCtx.new_comminst", JType.POINTER, Array(JType.POINTER, JType.POINTER, JType.UINT32, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER, JType.POINTER)) { _jffiBuffer =>
val _raw_ctx = _jffiBuffer.getAddress(0)
val _raw_bb = _jffiBuffer.getAddress(1)
val _raw_opcode = _jffiBuffer.getInt(2)
val _raw_flags = _jffiBuffer.getAddress(3)
val _raw_nflags = _jffiBuffer.getAddress(4)
val _raw_tys = _jffiBuffer.getAddress(5)
val _raw_ntys = _jffiBuffer.getAddress(6)
val _raw_sigs = _jffiBuffer.getAddress(7)
val _raw_nsigs = _jffiBuffer.getAddress(8)
val _raw_args = _jffiBuffer.getAddress(9)
val _raw_nargs = _jffiBuffer.getAddress(10)
val ctx = getMuCtx(_raw_ctx)
val bb = getMuValueNotNull(_raw_bb).asInstanceOf[MuBBNode]
val opcode = _raw_opcode
val flags = readFlagArray(_raw_flags, _raw_nflags)
val tys = readMuValueArray(_raw_tys, _raw_ntys)
val sigs = readMuValueArray(_raw_sigs, _raw_nsigs)
val args = readMuValueArray(_raw_args, _raw_nargs)
val _RV = ctx.newCommInst(bb, opcode, flags, tys, sigs, args)
val _RV_FAK = exposeMuValue(ctx, _RV)
_jffiBuffer.setAddressReturn(_RV_FAK)
}
val stubsOfMuCtx = new Array[Word](175)
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
val MU_THREAD_EXIT = 0x00
val MU_REBIND_PASS_VALUES = 0x01
val MU_REBIND_THROW_EXC = 0x02
val MU_DEST_NORMAL = 0x01
val MU_DEST_EXCEPT = 0x02
val MU_DEST_TRUE = 0x03
val MU_DEST_FALSE = 0x04
val MU_DEST_DEFAULT = 0x05
val MU_DEST_DISABLED = 0x06
val MU_DEST_ENABLED = 0x07
val MU_BINOP_ADD = 0x01
val MU_BINOP_SUB = 0x02
val MU_BINOP_MUL = 0x03
val MU_BINOP_SDIV = 0x04
val MU_BINOP_SREM = 0x05
val MU_BINOP_UDIV = 0x06
val MU_BINOP_UREM = 0x07
val MU_BINOP_SHL = 0x08
val MU_BINOP_LSHR = 0x09
val MU_BINOP_ASHR = 0x0A
val MU_BINOP_AND = 0x0B
val MU_BINOP_OR = 0x0C
val MU_BINOP_XOR = 0x0D
val MU_BINOP_FADD = 0xB0
val MU_BINOP_FSUB = 0xB1
val MU_BINOP_FMUL = 0xB2
val MU_BINOP_FDIV = 0xB3
val MU_BINOP_FREM = 0xB4
val MU_CMP_EQ = 0x20
val MU_CMP_NE = 0x21
val MU_CMP_SGE = 0x22
val MU_CMP_SGT = 0x23
val MU_CMP_SLE = 0x24
val MU_CMP_SLT = 0x25
val MU_CMP_UGE = 0x26
val MU_CMP_UGT = 0x27
val MU_CMP_ULE = 0x28
val MU_CMP_ULT = 0x29
val MU_CMP_FFALSE = 0xC0
val MU_CMP_FTRUE = 0xC1
val MU_CMP_FUNO = 0xC2
val MU_CMP_FUEQ = 0xC3
val MU_CMP_FUNE = 0xC4
val MU_CMP_FUGT = 0xC5
val MU_CMP_FUGE = 0xC6
val MU_CMP_FULT = 0xC7
val MU_CMP_FULE = 0xC8
val MU_CMP_FORD = 0xC9
val MU_CMP_FOEQ = 0xCA
val MU_CMP_FONE = 0xCB
val MU_CMP_FOGT = 0xCC
val MU_CMP_FOGE = 0xCD
val MU_CMP_FOLT = 0xCE
val MU_CMP_FOLE = 0xCF
val MU_CONV_TRUNC = 0x30
val MU_CONV_ZEXT = 0x31
val MU_CONV_SEXT = 0x32
val MU_CONV_FPTRUNC = 0x33
val MU_CONV_FPEXT = 0x34
val MU_CONV_FPTOUI = 0x35
val MU_CONV_FPTOSI = 0x36
val MU_CONV_UITOFP = 0x37
val MU_CONV_SITOFP = 0x38
val MU_CONV_BITCAST = 0x39
val MU_CONV_REFCAST = 0x3A
val MU_CONV_PTRCAST = 0x3B
val MU_ORD_NOT_ATOMIC = 0x00
val MU_ORD_RELAXED = 0x01
val MU_ORD_CONSUME = 0x02
val MU_ORD_ACQUIRE = 0x03
val MU_ORD_RELEASE = 0x04
val MU_ORD_ACQ_REL = 0x05
val MU_ORD_SEQ_CST = 0x06
val MU_ARMW_XCHG = 0x00
val MU_ARMW_ADD = 0x01
val MU_ARMW_SUB = 0x02
val MU_ARMW_AND = 0x03
val MU_ARMW_NAND = 0x04
val MU_ARMW_OR = 0x05
val MU_ARMW_XOR = 0x06
val MU_ARMW_MAX = 0x07
val MU_ARMW_MIN = 0x08
val MU_ARMW_UMAX = 0x09
val MU_ARMW_UMIN = 0x0A
val MU_CC_DEFAULT = 0x00
val MU_CI_UVM_NEW_STACK = 0x201
val MU_CI_UVM_KILL_STACK = 0x202
val MU_CI_UVM_THREAD_EXIT = 0x203
val MU_CI_UVM_CURRENT_STACK = 0x204
val MU_CI_UVM_SET_THREADLOCAL = 0x205
val MU_CI_UVM_GET_THREADLOCAL = 0x206
val MU_CI_UVM_TR64_IS_FP = 0x211
val MU_CI_UVM_TR64_IS_INT = 0x212
val MU_CI_UVM_TR64_IS_REF = 0x213
val MU_CI_UVM_TR64_FROM_FP = 0x214
val MU_CI_UVM_TR64_FROM_INT = 0x215
val MU_CI_UVM_TR64_FROM_REF = 0x216
val MU_CI_UVM_TR64_TO_FP = 0x217
val MU_CI_UVM_TR64_TO_INT = 0x218
val MU_CI_UVM_TR64_TO_REF = 0x219
val MU_CI_UVM_TR64_TO_TAG = 0x21a
val MU_CI_UVM_FUTEX_WAIT = 0x220
val MU_CI_UVM_FUTEX_WAIT_TIMEOUT = 0x221
val MU_CI_UVM_FUTEX_WAKE = 0x222
val MU_CI_UVM_FUTEX_CMP_REQUEUE = 0x223
val MU_CI_UVM_KILL_DEPENDENCY = 0x230
val MU_CI_UVM_NATIVE_PIN = 0x240
val MU_CI_UVM_NATIVE_UNPIN = 0x241
val MU_CI_UVM_NATIVE_EXPOSE = 0x242
val MU_CI_UVM_NATIVE_UNEXPOSE = 0x243
val MU_CI_UVM_NATIVE_GET_COOKIE = 0x244
val MU_CI_UVM_META_ID_OF = 0x250
val MU_CI_UVM_META_NAME_OF = 0x251
val MU_CI_UVM_META_LOAD_BUNDLE = 0x252
val MU_CI_UVM_META_LOAD_HAIL = 0x253
val MU_CI_UVM_META_NEW_CURSOR = 0x254
val MU_CI_UVM_META_NEXT_FRAME = 0x255
val MU_CI_UVM_META_COPY_CURSOR = 0x256
val MU_CI_UVM_META_CLOSE_CURSOR = 0x257
val MU_CI_UVM_META_CUR_FUNC = 0x258
val MU_CI_UVM_META_CUR_FUNC_VER = 0x259
val MU_CI_UVM_META_CUR_INST = 0x25a
val MU_CI_UVM_META_DUMP_KEEPALIVES = 0x25b
val MU_CI_UVM_META_POP_FRAMES_TO = 0x25c
val MU_CI_UVM_META_PUSH_FRAME = 0x25d
val MU_CI_UVM_META_ENABLE_WATCHPOINT = 0x25e
val MU_CI_UVM_META_DISABLE_WATCHPOINT = 0x25f
val MU_CI_UVM_META_SET_TRAP_HANDLER = 0x260
val MU_CI_UVM_IRBUILDER_NEW_BUNDLE = 0x300
val MU_CI_UVM_IRBUILDER_LOAD_BUNDLE_FROM_NODE = 0x301
val MU_CI_UVM_IRBUILDER_ABORT_BUNDLE_NODE = 0x302
val MU_CI_UVM_IRBUILDER_GET_NODE = 0x303
val MU_CI_UVM_IRBUILDER_GET_ID = 0x304
val MU_CI_UVM_IRBUILDER_SET_NAME = 0x305
val MU_CI_UVM_IRBUILDER_NEW_TYPE_INT = 0x306
val MU_CI_UVM_IRBUILDER_NEW_TYPE_FLOAT = 0x307
val MU_CI_UVM_IRBUILDER_NEW_TYPE_DOUBLE = 0x308
val MU_CI_UVM_IRBUILDER_NEW_TYPE_UPTR = 0x309
val MU_CI_UVM_IRBUILDER_SET_TYPE_UPTR = 0x30a
val MU_CI_UVM_IRBUILDER_NEW_TYPE_UFUNCPTR = 0x30b
val MU_CI_UVM_IRBUILDER_SET_TYPE_UFUNCPTR = 0x30c
val MU_CI_UVM_IRBUILDER_NEW_TYPE_STRUCT = 0x30d
val MU_CI_UVM_IRBUILDER_NEW_TYPE_HYBRID = 0x30e
val MU_CI_UVM_IRBUILDER_NEW_TYPE_ARRAY = 0x30f
val MU_CI_UVM_IRBUILDER_NEW_TYPE_VECTOR = 0x310
val MU_CI_UVM_IRBUILDER_NEW_TYPE_VOID = 0x311
val MU_CI_UVM_IRBUILDER_NEW_TYPE_REF = 0x312
val MU_CI_UVM_IRBUILDER_SET_TYPE_REF = 0x313
val MU_CI_UVM_IRBUILDER_NEW_TYPE_IREF = 0x314
val MU_CI_UVM_IRBUILDER_SET_TYPE_IREF = 0x315
val MU_CI_UVM_IRBUILDER_NEW_TYPE_WEAKREF = 0x316
val MU_CI_UVM_IRBUILDER_SET_TYPE_WEAKREF = 0x317
val MU_CI_UVM_IRBUILDER_NEW_TYPE_FUNCREF = 0x318
val MU_CI_UVM_IRBUILDER_SET_TYPE_FUNCREF = 0x319
val MU_CI_UVM_IRBUILDER_NEW_TYPE_TAGREF64 = 0x31a
val MU_CI_UVM_IRBUILDER_NEW_TYPE_THREADREF = 0x31b
val MU_CI_UVM_IRBUILDER_NEW_TYPE_STACKREF = 0x31c
val MU_CI_UVM_IRBUILDER_NEW_TYPE_FRAMECURSORREF = 0x31d
val MU_CI_UVM_IRBUILDER_NEW_TYPE_IRNODEREF = 0x31e
val MU_CI_UVM_IRBUILDER_NEW_FUNCSIG = 0x31f
val MU_CI_UVM_IRBUILDER_NEW_CONST_INT = 0x320
val MU_CI_UVM_IRBUILDER_NEW_CONST_INT_EX = 0x321
val MU_CI_UVM_IRBUILDER_NEW_CONST_FLOAT = 0x322
val MU_CI_UVM_IRBUILDER_NEW_CONST_DOUBLE = 0x323
val MU_CI_UVM_IRBUILDER_NEW_CONST_NULL = 0x324
val MU_CI_UVM_IRBUILDER_NEW_CONST_SEQ = 0x325
val MU_CI_UVM_IRBUILDER_NEW_GLOBAL_CELL = 0x326
val MU_CI_UVM_IRBUILDER_NEW_FUNC = 0x327
val MU_CI_UVM_IRBUILDER_NEW_FUNC_VER = 0x328
val MU_CI_UVM_IRBUILDER_NEW_EXP_FUNC = 0x329
val MU_CI_UVM_IRBUILDER_NEW_BB = 0x32a
val MU_CI_UVM_IRBUILDER_NEW_NOR_PARAM = 0x32b
val MU_CI_UVM_IRBUILDER_NEW_EXC_PARAM = 0x32c
val MU_CI_UVM_IRBUILDER_NEW_INST_RES = 0x32d
val MU_CI_UVM_IRBUILDER_ADD_DEST = 0x32e
val MU_CI_UVM_IRBUILDER_ADD_KEEPALIVES = 0x32f
val MU_CI_UVM_IRBUILDER_NEW_BINOP = 0x330
val MU_CI_UVM_IRBUILDER_NEW_CMP = 0x331
val MU_CI_UVM_IRBUILDER_NEW_CONV = 0x332
val MU_CI_UVM_IRBUILDER_NEW_SELECT = 0x333
val MU_CI_UVM_IRBUILDER_NEW_BRANCH = 0x334
val MU_CI_UVM_IRBUILDER_NEW_BRANCH2 = 0x335
val MU_CI_UVM_IRBUILDER_NEW_SWITCH = 0x336
val MU_CI_UVM_IRBUILDER_ADD_SWITCH_DEST = 0x337
val MU_CI_UVM_IRBUILDER_NEW_CALL = 0x338
val MU_CI_UVM_IRBUILDER_NEW_TAILCALL = 0x339
val MU_CI_UVM_IRBUILDER_NEW_RET = 0x33a
val MU_CI_UVM_IRBUILDER_NEW_THROW = 0x33b
val MU_CI_UVM_IRBUILDER_NEW_EXTRACTVALUE = 0x33c
val MU_CI_UVM_IRBUILDER_NEW_INSERTVALUE = 0x33d
val MU_CI_UVM_IRBUILDER_NEW_EXTRACTELEMENT = 0x33e
val MU_CI_UVM_IRBUILDER_NEW_INSERTELEMENT = 0x33f
val MU_CI_UVM_IRBUILDER_NEW_SHUFFLEVECTOR = 0x340
val MU_CI_UVM_IRBUILDER_NEW_NEW = 0x341
val MU_CI_UVM_IRBUILDER_NEW_NEWHYBRID = 0x342
val MU_CI_UVM_IRBUILDER_NEW_ALLOCA = 0x343
val MU_CI_UVM_IRBUILDER_NEW_ALLOCAHYBRID = 0x344
val MU_CI_UVM_IRBUILDER_NEW_GETIREF = 0x345
val MU_CI_UVM_IRBUILDER_NEW_GETFIELDIREF = 0x346
val MU_CI_UVM_IRBUILDER_NEW_GETELEMIREF = 0x347
val MU_CI_UVM_IRBUILDER_NEW_SHIFTIREF = 0x348
val MU_CI_UVM_IRBUILDER_NEW_GETVARPARTIREF = 0x349
val MU_CI_UVM_IRBUILDER_NEW_LOAD = 0x34a
val MU_CI_UVM_IRBUILDER_NEW_STORE = 0x34b
val MU_CI_UVM_IRBUILDER_NEW_CMPXCHG = 0x34c
val MU_CI_UVM_IRBUILDER_NEW_ATOMICRMW = 0x34d
val MU_CI_UVM_IRBUILDER_NEW_FENCE = 0x34e
val MU_CI_UVM_IRBUILDER_NEW_TRAP = 0x34f
val MU_CI_UVM_IRBUILDER_NEW_WATCHPOINT = 0x350
val MU_CI_UVM_IRBUILDER_NEW_WPBRANCH = 0x351
val MU_CI_UVM_IRBUILDER_NEW_CCALL = 0x352
val MU_CI_UVM_IRBUILDER_NEW_NEWTHREAD = 0x353
val MU_CI_UVM_IRBUILDER_NEW_SWAPSTACK_RET = 0x354
val MU_CI_UVM_IRBUILDER_NEW_SWAPSTACK_KILL = 0x355
val MU_CI_UVM_IRBUILDER_SET_NEWSTACK_PASS_VALUES = 0x356
val MU_CI_UVM_IRBUILDER_SET_NEWSTACK_THROW_EXC = 0x357
val MU_CI_UVM_IRBUILDER_NEW_COMMINST = 0x358
def toDestKind(cval: MuDestKind): DestKind.Value = cval match {
  case 0x01 => DestKind.NORMAL
  case 0x02 => DestKind.EXCEPT
  case 0x03 => DestKind.TRUE
  case 0x04 => DestKind.FALSE
  case 0x05 => DestKind.DEFAULT
  case 0x06 => DestKind.DISABLED
  case 0x07 => DestKind.ENABLED
}
def toBinOptr(cval: MuBinOptr): BinOptr.Value = cval match {
  case 0x01 => BinOptr.ADD
  case 0x02 => BinOptr.SUB
  case 0x03 => BinOptr.MUL
  case 0x04 => BinOptr.SDIV
  case 0x05 => BinOptr.SREM
  case 0x06 => BinOptr.UDIV
  case 0x07 => BinOptr.UREM
  case 0x08 => BinOptr.SHL
  case 0x09 => BinOptr.LSHR
  case 0x0A => BinOptr.ASHR
  case 0x0B => BinOptr.AND
  case 0x0C => BinOptr.OR
  case 0x0D => BinOptr.XOR
  case 0xB0 => BinOptr.FADD
  case 0xB1 => BinOptr.FSUB
  case 0xB2 => BinOptr.FMUL
  case 0xB3 => BinOptr.FDIV
  case 0xB4 => BinOptr.FREM
}
def toCmpOptr(cval: MuCmpOptr): CmpOptr.Value = cval match {
  case 0x20 => CmpOptr.EQ
  case 0x21 => CmpOptr.NE
  case 0x22 => CmpOptr.SGE
  case 0x23 => CmpOptr.SGT
  case 0x24 => CmpOptr.SLE
  case 0x25 => CmpOptr.SLT
  case 0x26 => CmpOptr.UGE
  case 0x27 => CmpOptr.UGT
  case 0x28 => CmpOptr.ULE
  case 0x29 => CmpOptr.ULT
  case 0xC0 => CmpOptr.FFALSE
  case 0xC1 => CmpOptr.FTRUE
  case 0xC2 => CmpOptr.FUNO
  case 0xC3 => CmpOptr.FUEQ
  case 0xC4 => CmpOptr.FUNE
  case 0xC5 => CmpOptr.FUGT
  case 0xC6 => CmpOptr.FUGE
  case 0xC7 => CmpOptr.FULT
  case 0xC8 => CmpOptr.FULE
  case 0xC9 => CmpOptr.FORD
  case 0xCA => CmpOptr.FOEQ
  case 0xCB => CmpOptr.FONE
  case 0xCC => CmpOptr.FOGT
  case 0xCD => CmpOptr.FOGE
  case 0xCE => CmpOptr.FOLT
  case 0xCF => CmpOptr.FOLE
}
def toConvOptr(cval: MuConvOptr): ConvOptr.Value = cval match {
  case 0x30 => ConvOptr.TRUNC
  case 0x31 => ConvOptr.ZEXT
  case 0x32 => ConvOptr.SEXT
  case 0x33 => ConvOptr.FPTRUNC
  case 0x34 => ConvOptr.FPEXT
  case 0x35 => ConvOptr.FPTOUI
  case 0x36 => ConvOptr.FPTOSI
  case 0x37 => ConvOptr.UITOFP
  case 0x38 => ConvOptr.SITOFP
  case 0x39 => ConvOptr.BITCAST
  case 0x3A => ConvOptr.REFCAST
  case 0x3B => ConvOptr.PTRCAST
}
def toMemoryOrder(cval: MuMemOrd): MemoryOrder.Value = cval match {
  case 0x00 => MemoryOrder.NOT_ATOMIC
  case 0x01 => MemoryOrder.RELAXED
  case 0x02 => MemoryOrder.CONSUME
  case 0x03 => MemoryOrder.ACQUIRE
  case 0x04 => MemoryOrder.RELEASE
  case 0x05 => MemoryOrder.ACQ_REL
  case 0x06 => MemoryOrder.SEQ_CST
}
def toAtomicRMWOptr(cval: MuAtomicRMWOptr): AtomicRMWOptr.Value = cval match {
  case 0x00 => AtomicRMWOptr.XCHG
  case 0x01 => AtomicRMWOptr.ADD
  case 0x02 => AtomicRMWOptr.SUB
  case 0x03 => AtomicRMWOptr.AND
  case 0x04 => AtomicRMWOptr.NAND
  case 0x05 => AtomicRMWOptr.OR
  case 0x06 => AtomicRMWOptr.XOR
  case 0x07 => AtomicRMWOptr.MAX
  case 0x08 => AtomicRMWOptr.MIN
  case 0x09 => AtomicRMWOptr.UMAX
  case 0x0A => AtomicRMWOptr.UMIN
}
  /// GEN:END:STUBS
}
