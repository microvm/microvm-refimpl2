package uvm.refimpl.nat

import java.nio.charset.StandardCharsets
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.{ universe => ru }
import org.slf4j.LoggerFactory
import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import com.typesafe.scalalogging.Logger
import NativeSupport._
import PlatformConstants._
import jnr.ffi.ObjectReferenceManager
import jnr.ffi.Pointer
import uvm.refimpl._
import uvm.ssavariables._
import com.kenai.jffi.Invoker
import com.kenai.jffi.CallContext
import com.kenai.jffi.HeapInvocationBuffer
import NativeClientSupport._

import uvm.ir.irbuilder.DestKind

object NativeTrapHandler {
  val jffiInvoker = Invoker.getInstance
  val trapHandlerCallContext = new CallContext(
    JType.VOID, // return value
    // input params
    JType.POINTER, //MuCtx *ctx, 
    JType.POINTER, //MuThreadRefValue thread,
    JType.POINTER, //MuStackRefValue stack,
    JType.SINT32, //int wpid,
    // output params
    JType.POINTER, //MuTrapHandlerResult *result,
    JType.POINTER, //MuStackRefValue *new_stack,
    JType.POINTER, //MuValue **values,
    JType.POINTER, //int *nvalues,
    JType.POINTER, //MuValuesFreer *freer, 
    JType.POINTER, //MuCPtr *freerdata, 
    JType.POINTER, //MuRefValue *exception,
    // user data
    JType.POINTER //MuCPtr userdata
    );

  val freerCallContext = new CallContext(
    JType.VOID, // return value
    // input params
    JType.POINTER, // MuValue *values,
    JType.POINTER //MuCPtr freerdata
    )

  def callFreer(freerFP: MuValuesFreerFP, valuesPtr: MuValueFakArrayPtr, freerData: MuCPtr): Unit = {
    val freerFunc = new com.kenai.jffi.Function(freerFP, freerCallContext)
    logger.debug("Calling freer: 0x%x with args (0x%x, 0x%x)".format(freerFP, valuesPtr, freerData))
    jffiInvoker.invokeLLrL(freerFunc, valuesPtr, freerData)
    logger.debug("Returned from freer")
  }

}

/**
 * This object calls a C function to handle the trap.
 */
class NativeTrapHandler(val funcAddr: MuTrapHandlerFP, val userdata: MuCPtr) extends TrapHandler {
  import NativeTrapHandler._
  import NativeMuHelpers._

  val jffiFunction = new com.kenai.jffi.Function(funcAddr, trapHandlerCallContext)

  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
    logger.debug("Trapped. Prepare to call native trap handler: %d 0x%x".format(funcAddr, funcAddr))

    val hib = new HeapInvocationBuffer(jffiFunction)

    // input args
    val ctxFak = exposeMuCtx(ctx) // It is a fresh context.
    hib.putAddress(ctxFak)

    val threadFak = exposeMuValue(ctx, thread) // It is a fresh handle, too.
    hib.putAddress(threadFak)

    val stackFak = exposeMuValue(ctx, stack) // It is a fresh handle, too.
    hib.putAddress(stackFak)

    hib.putInt(watchPointID)

    // output args
    val nOutArgs = 7
    val outBuf = jnrMemoryManager.allocateDirect((7L * WORD_SIZE_BYTES).toInt, true) // output values are received here.
    val outBufAddr = outBuf.address()
    for (i <- 0 until nOutArgs) {
      val outAddr = outBufAddr + i * WORD_SIZE_BYTES
      logger.debug("The %d-th out arg: 0x%x".format(i, outAddr))
      hib.putAddress(outAddr)
    }

    // user data
    hib.putAddress(userdata)

    // Call
    logger.debug("Calling native trap handler: 0x%x".format(funcAddr))
    jffiInvoker.invokeLong(jffiFunction, hib)
    logger.debug("Returned from native trap handler: 0x%x".format(funcAddr))

    // Inspect return value
    val result = outBuf.getInt(0L)

    val scalaResult: TrapHandlerResult = result match {
      case MU_THREAD_EXIT => TrapHandlerResult.ThreadExit()
      case MU_REBIND_PASS_VALUES => {
        val nsFak = outBuf.getAddress(1 * WORD_SIZE_BYTES)
        val newStack = getMuValueNotNull(nsFak).asInstanceOf[MuStackRefValue]
        val valuesPtr = outBuf.getAddress(2 * WORD_SIZE_BYTES)
        val nValues = outBuf.getInt(3 * WORD_SIZE_BYTES)
        val valueFaks = for (i <- 0 until nValues) yield {
          theMemory.getAddress(valuesPtr + i * WORD_SIZE_BYTES)
        }

        val freerFP = outBuf.getAddress(4 * WORD_SIZE_BYTES)
        if (freerFP != 0L) {
          val freerData = outBuf.getAddress(5 * WORD_SIZE_BYTES)
          callFreer(freerFP, valuesPtr, freerData)
        }

        val values = valueFaks.map(getMuValueNotNull)

        TrapHandlerResult.Rebind(newStack, HowToResume.PassValues(values))
      }
      case MU_REBIND_THROW_EXC => {
        val nsFak = outBuf.getAddress(1 * WORD_SIZE_BYTES)
        val newStack = getMuValueNotNull(nsFak).asInstanceOf[MuStackRefValue]
        val excFak = outBuf.getAddress(6 * WORD_SIZE_BYTES)
        val excVal = getMuValueNotNull(excFak).asInstanceOf[MuRefValue]

        TrapHandlerResult.Rebind(newStack, HowToResume.ThrowExc(excVal))
      }
    }

    unexposeMuCtx(ctxFak) // Mu will close the context, but not un-expose it.

    scalaResult
  }
}

/**
 * This object routes calls from native clients to the MicroVM object.
 */
object NativeMuVM {
  import NativeMuHelpers._

  def new_context(microVM: MicroVM): MuCtxFak = {
    val ctx = microVM.newContext()
    val ptr = exposeMuCtx(ctx)
    ptr
  }
  def id_of(microVM: MicroVM, name: MuName): MuID = microVM.idOf(name)
  def name_of(microVM: MicroVM, id: MuID): MuName = microVM.nameOf(id)
  def set_trap_handler(microVM: MicroVM, trap_handler: MuTrapHandlerFP, userdata: MuCPtr): Unit = {
    microVM.setTrapHandler(new NativeTrapHandler(trap_handler, userdata))
  }
  def execute(microVM: MicroVM): Unit = microVM.execute()
  def get_mu_error_ptr(microVM: MicroVM): MuCPtr = ClientAccessibleClassExposer.muErrorPtr.address()
}

/**
 * This object routes calls from native clients to a MuCtx object.
 * <p>
 * Parameters are usually auto-converted from low-level types (pointers to function tables or C MuValue pointer), but
 * return values are usually explicitly converted to pointers by the "expose*" methods, such as exposeMuValue.
 */
object NativeMuCtx {
  import NativeMuHelpers._

  def id_of(ctx: MuCtx, name: MuName): MuID = ctx.idOf(name)
  def name_of(ctx: MuCtx, id: MuID): MuName = ctx.nameOf(id)

  /**
   * NOTE: Should have taken MuCtx as the first parameter, but we need to de-allocate
   * the function table, too.
   */
  def close_context(funcTableAddr: FuncTablePtr): Unit = {
    val ctx = getObjFromFuncTableAddrNotNull(funcTableAddr, NativeClientSupport.muCtxs)
    unexposeMuCtx(funcTableAddr)
    ctx.closeContext()
  }

  def load_bundle(ctx: MuCtx, buf: CharArrayPtr, sz: Int): Unit = {
    val str = theMemory.getString(buf, sz, StandardCharsets.US_ASCII)
    logger.trace("Loading bundle:\n%s".format(str))
    ctx.loadBundle(str)
  }

  def load_hail(ctx: MuCtx, buf: CharArrayPtr, sz: Int): Unit = {
    val str = theMemory.getString(buf, sz, StandardCharsets.US_ASCII)
    logger.trace("Loading hail:\n%s".format(str))
    ctx.loadHail(str)
  }

  def handle_from_sint8(ctx: MuCtx, num: Byte, len: Int): MuValueFak = handleFromInt(ctx, num, len)
  def handle_from_uint8(ctx: MuCtx, num: Byte, len: Int): MuValueFak = handleFromInt(ctx, trunc(num, 8), len)
  def handle_from_sint16(ctx: MuCtx, num: Short, len: Int): MuValueFak = handleFromInt(ctx, num, len)
  def handle_from_uint16(ctx: MuCtx, num: Short, len: Int): MuValueFak = handleFromInt(ctx, trunc(num, 16), len)
  def handle_from_sint32(ctx: MuCtx, num: Int, len: Int): MuValueFak = handleFromInt(ctx, num, len)
  def handle_from_uint32(ctx: MuCtx, num: Int, len: Int): MuValueFak = handleFromInt(ctx, trunc(num, 32), len)
  def handle_from_sint64(ctx: MuCtx, num: Long, len: Int): MuValueFak = handleFromInt(ctx, num, len)
  def handle_from_uint64(ctx: MuCtx, num: Long, len: Int): MuValueFak = handleFromInt(ctx, trunc(num, 64), len)
  def handle_from_float(ctx: MuCtx, num: Float): MuValueFak = exposeMuValue(ctx, ctx.handleFromFloat(num))
  def handle_from_double(ctx: MuCtx, num: Double): MuValueFak = exposeMuValue(ctx, ctx.handleFromDouble(num))
  def handle_from_ptr(ctx: MuCtx, mu_type: MuID, ptr: MuCPtr): MuValueFak = exposeMuValue(ctx, ctx.handleFromPtr(mu_type, ptr))
  def handle_from_fp(ctx: MuCtx, mu_type: MuID, fp: MuCFP): MuValueFak = exposeMuValue(ctx, ctx.handleFromFP(mu_type, fp))

  def handle_to_sint8(ctx: MuCtx, opnd: MuIntValue): Byte = ctx.handleToSInt(opnd).toByte
  def handle_to_uint8(ctx: MuCtx, opnd: MuIntValue): Byte = ctx.handleToUInt(opnd).toByte
  def handle_to_sint16(ctx: MuCtx, opnd: MuIntValue): Short = ctx.handleToSInt(opnd).toShort
  def handle_to_uint16(ctx: MuCtx, opnd: MuIntValue): Short = ctx.handleToUInt(opnd).toShort
  def handle_to_sint32(ctx: MuCtx, opnd: MuIntValue): Int = ctx.handleToSInt(opnd).toInt
  def handle_to_uint32(ctx: MuCtx, opnd: MuIntValue): Int = ctx.handleToUInt(opnd).toInt
  def handle_to_sint64(ctx: MuCtx, opnd: MuIntValue): Long = ctx.handleToSInt(opnd).toLong
  def handle_to_uint64(ctx: MuCtx, opnd: MuIntValue): Long = ctx.handleToUInt(opnd).toLong
  def handle_to_float(ctx: MuCtx, opnd: MuFloatValue): Float = ctx.handleToFloat(opnd)
  def handle_to_double(ctx: MuCtx, opnd: MuDoubleValue): Double = ctx.handleToDouble(opnd)
  def handle_to_ptr(ctx: MuCtx, opnd: MuUPtrValue): MuCPtr = ctx.handleToPtr(opnd)
  def handle_to_fp(ctx: MuCtx, opnd: MuUFPValue): MuCFP = ctx.handleToFP(opnd)

  def handle_from_const(ctx: MuCtx, id: MuID): MuValueFak = exposeMuValue(ctx, ctx.handleFromConst(id))
  def handle_from_global(ctx: MuCtx, id: MuID): MuValueFak = exposeMuValue(ctx, ctx.handleFromGlobal(id))
  def handle_from_func(ctx: MuCtx, id: MuID): MuValueFak = exposeMuValue(ctx, ctx.handleFromFunc(id))
  def handle_from_expose(ctx: MuCtx, id: MuID): MuValueFak = exposeMuValue(ctx, ctx.handleFromExpose(id))

  /** Need to dispose the value manually. */
  def delete_value(ctx: MuCtx, opnd: MuValueFak): Unit = {
    logger.debug("Deleting %d 0x%x".format(opnd, opnd))
    val muVal = getMuValueNotNull(opnd)
    unexposeMuValue(ctx, opnd)
    ctx.deleteValue(muVal)
  }

  def ref_eq(ctx: MuCtx, lhs: MuGenRefValue, rhs: MuGenRefValue): Int = if (ctx.refEq(lhs, rhs)) 1 else 0
  def ref_ult(ctx: MuCtx, lhs: MuIRefValue, rhs: MuIRefValue): Int = if (ctx.refUlt(lhs, rhs)) 1 else 0

  def extract_value(ctx: MuCtx, str: MuStructValue, index: Int): MuValueFak = exposeMuValue(ctx, ctx.extractValue(str, index))
  def insert_value(ctx: MuCtx, str: MuStructValue, index: Int, newval: MuValue): MuValueFak = exposeMuValue(ctx, ctx.insertValue(str, index, newval))

  def extract_element(ctx: MuCtx, str: MuSeqValue, index: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.extractElement(str, index))
  def insert_element(ctx: MuCtx, str: MuSeqValue, index: MuIntValue, newval: MuValue): MuValueFak = exposeMuValue(ctx, ctx.insertElement(str, index, newval))

  def new_fixed(ctx: MuCtx, mu_type: MuID): MuValueFak = exposeMuValue(ctx, ctx.newFixed(mu_type))
  def new_hybrid(ctx: MuCtx, mu_type: MuID, length: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.newHybrid(mu_type, length))

  def refcast(ctx: MuCtx, opnd: MuGenRefValue, new_type: MuID): MuValueFak = exposeMuValue(ctx, ctx.refcast(opnd, new_type))

  def get_iref(ctx: MuCtx, opnd: MuRefValue): MuValueFak = exposeMuValue(ctx, ctx.getIRef(opnd))
  def get_field_iref(ctx: MuCtx, opnd: MuIRefValue, field: Int): MuValueFak = exposeMuValue(ctx, ctx.getFieldIRef(opnd, field))
  def get_elem_iref(ctx: MuCtx, opnd: MuIRefValue, index: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.getElemIRef(opnd, index))
  def shift_iref(ctx: MuCtx, opnd: MuIRefValue, offset: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.shiftIRef(opnd, offset))
  def get_var_part_iref(ctx: MuCtx, opnd: MuIRefValue): MuValueFak = exposeMuValue(ctx, ctx.getVarPartIRef(opnd))

  def load(ctx: MuCtx, ord: MuMemOrd, loc: MuIRefValue): MuValueFak = exposeMuValue(ctx, ctx.load(ord, loc))
  def store(ctx: MuCtx, ord: MuMemOrd, loc: MuIRefValue, newval: MuValue): Unit = ctx.store(ord, loc, newval)
  def cmpxchg(ctx: MuCtx, ord_succ: MuMemOrd, ord_fail: MuMemOrd, weak: Int, loc: MuIRefValue, expected: MuValue, desired: MuValue, is_succ: IntPtr): MuValueFak = {
    val (rv, succ) = ctx.cmpXchg(ord_succ, ord_fail, weak != 0, loc, expected, desired)
    theMemory.putInt(is_succ, if (succ) 1 else 0)
    exposeMuValue(ctx, rv)
  }
  def atomicrmw(ctx: MuCtx, ord: MuMemOrd, op: MuAtomicRMWOptr, loc: MuIRefValue, opnd: MuValue): MuValueFak = {
    val rv = ctx.atomicRMW(ord, op, loc, opnd)
    exposeMuValue(ctx, rv)
  }
  def fence(ctx: MuCtx, ord: MuMemOrd): Unit = ctx.fence(ord)

  def new_stack(ctx: MuCtx, func: MuFuncRefValue): MuValueFak = exposeMuValue(ctx, ctx.newStack(func))

  // NOTE: parameter exc must not be a MuValue because this parameter is ignored when htr is MU_REBIND_PASS_VALUE.
  // Setting the type to MuValue (or MuRefValue) will force the exposer to eagerly resolve the underlying actual MuValue.
  def new_thread(ctx: MuCtx, stack: MuStackRefValue, threadLocal: Option[MuRefValue], htr: MuHowToResume, vals: MuValueFakArrayPtr, nvals: Int, exc: MuValueFak): MuValueFak = {
    val scalaHtr = htr match {
      case MU_REBIND_PASS_VALUES => {
        val values = readFromValueFakArray(vals, nvals)
        HowToResume.PassValues(values)
      }
      case MU_REBIND_THROW_EXC => {
        val excVal = getMuValueNotNull(exc).asInstanceOf[MuRefValue]
        HowToResume.ThrowExc(excVal)
      }
    }
    val rv = ctx.newThread(stack, threadLocal, scalaHtr)
    exposeMuValue(ctx, rv)
  }
  def kill_stack(ctx: MuCtx, stack: MuStackRefValue): Unit = ctx.killStack(stack)
  def set_threadlocal(ctx: MuCtx, thread: MuThreadRefValue, threadLocal: MuRefValue): Unit = {
    ctx.setThreadlocal(thread, threadLocal)
  }
  def get_threadlocal(ctx: MuCtx, thread: MuThreadRefValue): MuValueFak = {
    val rv = ctx.getThreadlocal(thread)
    exposeMuValue(ctx, rv)
  }

  // Frame cursor operations
  def new_cursor(ctx: MuCtx, stack: MuStackRefValue): MuValueFak = exposeMuValue(ctx, ctx.newCursor(stack))
  def next_frame(ctx: MuCtx, cursor: MuFCRefValue): Unit = ctx.nextFrame(cursor)
  def copy_cursor(ctx: MuCtx, cursor: MuFCRefValue): MuValueFak = exposeMuValue(ctx, ctx.copyCursor(cursor))
  def close_cursor(ctx: MuCtx, cursor: MuFCRefValue): Unit = ctx.closeCursor(cursor)

  // Stack introspection
  def cur_func(ctx: MuCtx, cursor: MuFCRefValue): MuID = ctx.curFunc(cursor)
  def cur_func_ver(ctx: MuCtx, cursor: MuFCRefValue): MuID = ctx.curFuncVer(cursor)
  def cur_inst(ctx: MuCtx, cursor: MuFCRefValue): MuID = ctx.curInst(cursor)
  def dump_keepalives(ctx: MuCtx, cursor: MuFCRefValue, results: MuValueFakArrayPtr): Unit = {
    val kas = ctx.dumpKeepalives(cursor)
    for ((ka, i) <- kas.zipWithIndex) {
      val fak = exposeMuValue(ctx, ka)
      val addr = results + i * WORD_SIZE_BYTES
      theMemory.putAddress(addr, fak)
    }
  }
  def pop_frames_to(ctx: MuCtx, cursor: MuFCRefValue): Unit = ctx.popFramesTo(cursor)
  def push_frame(ctx: MuCtx, stack: MuStackRefValue, func: MuFuncRefValue): Unit = ctx.pushFrame(stack, func)

  def tr64_is_fp(ctx: MuCtx, value: MuTagRef64Value): Int = if (ctx.tr64IsFP(value)) 1 else 0
  def tr64_is_int(ctx: MuCtx, value: MuTagRef64Value): Int = if (ctx.tr64IsInt(value)) 1 else 0
  def tr64_is_ref(ctx: MuCtx, value: MuTagRef64Value): Int = if (ctx.tr64IsRef(value)) 1 else 0
  def tr64_to_fp(ctx: MuCtx, value: MuTagRef64Value): MuValueFak = exposeMuValue(ctx, ctx.tr64ToFP(value))
  def tr64_to_int(ctx: MuCtx, value: MuTagRef64Value): MuValueFak = exposeMuValue(ctx, ctx.tr64ToInt(value))
  def tr64_to_ref(ctx: MuCtx, value: MuTagRef64Value): MuValueFak = exposeMuValue(ctx, ctx.tr64ToRef(value))
  def tr64_to_tag(ctx: MuCtx, value: MuTagRef64Value): MuValueFak = exposeMuValue(ctx, ctx.tr64ToTag(value))
  def tr64_from_fp(ctx: MuCtx, value: MuDoubleValue): MuValueFak = exposeMuValue(ctx, ctx.tr64FromFP(value))
  def tr64_from_int(ctx: MuCtx, value: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.tr64FromInt(value))
  def tr64_from_ref(ctx: MuCtx, ref: MuRefValue, tag: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.tr64FromRef(ref, tag))

  def enable_watchpoint(ctx: MuCtx, wpid: Int): Unit = ctx.enableWatchPoint(wpid)
  def disable_watchpoint(ctx: MuCtx, wpid: Int): Unit = ctx.disableWatchPoint(wpid)

  def pin(ctx: MuCtx, loc: MuValue): MuValueFak = exposeMuValue(ctx, ctx.pin(loc))
  def unpin(ctx: MuCtx, loc: MuValue): Unit = ctx.unpin(loc)

  def expose(ctx: MuCtx, func: MuFuncRefValue, call_conv: MuCallConv, cookie: MuIntValue): MuValueFak = exposeMuValue(ctx, ctx.expose(func, call_conv, cookie))
  def unexpose(ctx: MuCtx, call_conv: MuCallConv, value: MuUFPValue): Unit = ctx.unexpose(call_conv, value)

  // PASTE FROM HERE
  def new_bundle(ctx: MuCtx): MuValueFak = exposeMuValue(ctx, ctx.newBundle())
  def load_bundle_from_node(ctx: MuCtx, b: MuBundleNode): Unit = ctx.loadBundleFromNode(b)
  def abort_bundle_node(ctx: MuCtx, b: MuBundleNode): Unit = ctx.abortBundleNode(b)
  def get_node(ctx: MuCtx, b: MuBundleNode, id: MuID): MuValueFak = exposeMuValue(ctx, ctx.getNode(b, id))
  def get_id(ctx: MuCtx, b: MuBundleNode, node: MuChildNode): MuID = ctx.getID(b, node)
  def set_name(ctx: MuCtx, b: MuBundleNode, node: MuChildNode, name: MuName): Unit = ctx.setName(b, node, name)
  def new_type_int(ctx: MuCtx, b: MuBundleNode, len: Int): MuValueFak = exposeMuValue(ctx, ctx.newTypeInt(b, len))
  def new_type_float(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeFloat(b))
  def new_type_double(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeDouble(b))
  def new_type_uptr(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeUPtr(b))
  def set_type_uptr(ctx: MuCtx, uptr: MuTypeNode, ty: MuTypeNode): Unit = ctx.setTypeUPtr(uptr, ty)
  def new_type_ufuncptr(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeUFuncPtr(b))
  def set_type_ufuncptr(ctx: MuCtx, ufuncptr: MuTypeNode, sig: MuFuncSigNode): Unit = ctx.setTypeUFuncPtr(ufuncptr, sig)
  def new_type_struct(ctx: MuCtx, b: MuBundleNode, fieldtys: MuValueFakArrayPtr, nfieldtys: Int): MuValueFak = exposeMuValue(ctx, ctx.newTypeStruct(b, readFromValueFakArray(fieldtys, nfieldtys)))
  def new_type_hybrid(ctx: MuCtx, b: MuBundleNode, fixedtys: MuValueFakArrayPtr, nfixedtys: Int, varty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeHybrid(b, readFromValueFakArray(fixedtys, nfixedtys), varty))
  def new_type_array(ctx: MuCtx, b: MuBundleNode, elemty: MuTypeNode, len: Long): MuValueFak = exposeMuValue(ctx, ctx.newTypeArray(b, elemty, len))
  def new_type_vector(ctx: MuCtx, b: MuBundleNode, elemty: MuTypeNode, len: Long): MuValueFak = exposeMuValue(ctx, ctx.newTypeVector(b, elemty, len))
  def new_type_void(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeVoid(b))
  def new_type_ref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeRef(b))
  def set_type_ref(ctx: MuCtx, ref: MuTypeNode, ty: MuTypeNode): Unit = ctx.setTypeRef(ref, ty)
  def new_type_iref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeIRef(b))
  def set_type_iref(ctx: MuCtx, iref: MuTypeNode, ty: MuTypeNode): Unit = ctx.setTypeIRef(iref, ty)
  def new_type_weakref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeWeakRef(b))
  def set_type_weakref(ctx: MuCtx, weakref: MuTypeNode, ty: MuTypeNode): Unit = ctx.setTypeWeakRef(weakref, ty)
  def new_type_funcref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeFuncRef(b))
  def set_type_funcref(ctx: MuCtx, funcref: MuTypeNode, sig: MuFuncSigNode): Unit = ctx.setTypeFuncRef(funcref, sig)
  def new_type_tagref64(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeTagRef64(b))
  def new_type_threadref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeThreadRef(b))
  def new_type_stackref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeStackRef(b))
  def new_type_framecursorref(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeFrameCursorRef(b))
  def new_type_irnoderef(ctx: MuCtx, b: MuBundleNode): MuValueFak = exposeMuValue(ctx, ctx.newTypeIRNodeRef(b))
  def new_funcsig(ctx: MuCtx, b: MuBundleNode, paramtys: MuValueFakArrayPtr, nparamtys: Int, rettys: MuValueFakArrayPtr, nrettys: Int): MuValueFak = exposeMuValue(ctx, ctx.newFuncSig(b, readFromValueFakArray(paramtys, nparamtys), readFromValueFakArray(rettys, nrettys)))
  def new_const_int(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode, value: Long): MuValueFak = exposeMuValue(ctx, ctx.newConstInt(b, ty, value))
  def new_const_int_ex(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode, values: LongPtr, nvalues: Int): MuValueFak = exposeMuValue(ctx, ctx.newConstIntEx(b, ty, values, nvalues))
  def new_const_float(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode, value: Float): MuValueFak = exposeMuValue(ctx, ctx.newConstFloat(b, ty, value))
  def new_const_double(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode, value: Double): MuValueFak = exposeMuValue(ctx, ctx.newConstDouble(b, ty, value))
  def new_const_null(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newConstNull(b, ty))
  def new_const_seq(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode, elems: MuValueFakArrayPtr, nelems: Int): MuValueFak = exposeMuValue(ctx, ctx.newConstSeq(b, ty, readFromValueFakArray(elems, nelems)))
  def new_global_cell(ctx: MuCtx, b: MuBundleNode, ty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newGlobalCell(b, ty))
  def new_func(ctx: MuCtx, b: MuBundleNode, sig: MuFuncSigNode): MuValueFak = exposeMuValue(ctx, ctx.newFunc(b, sig))
  def new_func_ver(ctx: MuCtx, b: MuBundleNode, func: MuFuncNode): MuValueFak = exposeMuValue(ctx, ctx.newFuncVer(b, func))
  def new_exp_func(ctx: MuCtx, b: MuBundleNode, func: MuFuncNode, callconv: MuCallConv, cookie: MuConstNode): MuValueFak = exposeMuValue(ctx, ctx.newExpFunc(b, func, callconv, cookie))
  def new_bb(ctx: MuCtx, fv: MuFuncVerNode): MuValueFak = exposeMuValue(ctx, ctx.newBB(fv))
  def new_nor_param(ctx: MuCtx, bb: MuBBNode, ty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newNorParam(bb, ty))
  def new_exc_param(ctx: MuCtx, bb: MuBBNode): MuValueFak = exposeMuValue(ctx, ctx.newExcParam(bb))
  def new_inst_res(ctx: MuCtx, inst: MuInstNode): MuValueFak = exposeMuValue(ctx, ctx.newInstRes(inst))
  def add_dest(ctx: MuCtx, inst: MuInstNode, kind: MuDestKind, dest: MuBBNode, vars: MuValueFakArrayPtr, nvars: Int): Unit = ctx.addDest(inst, kind, dest, readFromValueFakArray(vars, nvars))
  def add_keepalives(ctx: MuCtx, inst: MuInstNode, vars: MuValueFakArrayPtr, nvars: Int): Unit = ctx.addKeepalives(inst, readFromValueFakArray(vars, nvars))
  def new_binop(ctx: MuCtx, bb: MuBBNode, optr: MuBinOptr, ty: MuTypeNode, opnd1: MuVarNode, opnd2: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newBinOp(bb, optr, ty, opnd1, opnd2))
  def new_cmp(ctx: MuCtx, bb: MuBBNode, optr: MuCmpOptr, ty: MuTypeNode, opnd1: MuVarNode, opnd2: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newCmp(bb, optr, ty, opnd1, opnd2))
  def new_conv(ctx: MuCtx, bb: MuBBNode, optr: MuConvOptr, from_ty: MuTypeNode, to_ty: MuTypeNode, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newConv(bb, optr, from_ty, to_ty, opnd))
  def new_select(ctx: MuCtx, bb: MuBBNode, cond_ty: MuTypeNode, opnd_ty: MuTypeNode, cond: MuVarNode, if_true: MuVarNode, if_false: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newSelect(bb, cond_ty, opnd_ty, cond, if_true, if_false))
  def new_branch(ctx: MuCtx, bb: MuBBNode): MuValueFak = exposeMuValue(ctx, ctx.newBranch(bb))
  def new_branch2(ctx: MuCtx, bb: MuBBNode, cond: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newBranch2(bb, cond))
  def new_switch(ctx: MuCtx, bb: MuBBNode, opnd_ty: MuTypeNode, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newSwitch(bb, opnd_ty, opnd))
  def add_switch_dest(ctx: MuCtx, sw: MuInstNode, key: MuConstNode, dest: MuBBNode, vars: MuValueFakArrayPtr, nvars: Int): Unit = ctx.addSwitchDest(sw, key, dest, readFromValueFakArray(vars, nvars))
  def new_call(ctx: MuCtx, bb: MuBBNode, sig: MuFuncSigNode, callee: MuVarNode, args: MuValueFakArrayPtr, nargs: Int): MuValueFak = exposeMuValue(ctx, ctx.newCall(bb, sig, callee, readFromValueFakArray(args, nargs)))
  def new_tailcall(ctx: MuCtx, bb: MuBBNode, sig: MuFuncSigNode, callee: MuVarNode, args: MuValueFakArrayPtr, nargs: Int): MuValueFak = exposeMuValue(ctx, ctx.newTailCall(bb, sig, callee, readFromValueFakArray(args, nargs)))
  def new_ret(ctx: MuCtx, bb: MuBBNode, rvs: MuValueFakArrayPtr, nrvs: Int): MuValueFak = exposeMuValue(ctx, ctx.newRet(bb, readFromValueFakArray(rvs, nrvs)))
  def new_throw(ctx: MuCtx, bb: MuBBNode, exc: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newThrow(bb, exc))
  def new_extractvalue(ctx: MuCtx, bb: MuBBNode, strty: MuTypeNode, index: Int, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newExtractValue(bb, strty, index, opnd))
  def new_insertvalue(ctx: MuCtx, bb: MuBBNode, strty: MuTypeNode, index: Int, opnd: MuVarNode, newval: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newInsertValue(bb, strty, index, opnd, newval))
  def new_extractelement(ctx: MuCtx, bb: MuBBNode, seqty: MuTypeNode, indty: MuTypeNode, opnd: MuVarNode, index: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newExtractElement(bb, seqty, indty, opnd, index))
  def new_insertelement(ctx: MuCtx, bb: MuBBNode, seqty: MuTypeNode, indty: MuTypeNode, opnd: MuVarNode, index: MuVarNode, newval: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newInsertElement(bb, seqty, indty, opnd, index, newval))
  def new_shufflevector(ctx: MuCtx, bb: MuBBNode, vecty: MuTypeNode, maskty: MuTypeNode, vec1: MuVarNode, vec2: MuVarNode, mask: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newShuffleVector(bb, vecty, maskty, vec1, vec2, mask))
  def new_new(ctx: MuCtx, bb: MuBBNode, allocty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newNew(bb, allocty))
  def new_newhybrid(ctx: MuCtx, bb: MuBBNode, allocty: MuTypeNode, lenty: MuTypeNode, length: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newNewHybrid(bb, allocty, lenty, length))
  def new_alloca(ctx: MuCtx, bb: MuBBNode, allocty: MuTypeNode): MuValueFak = exposeMuValue(ctx, ctx.newAlloca(bb, allocty))
  def new_allocahybrid(ctx: MuCtx, bb: MuBBNode, allocty: MuTypeNode, lenty: MuTypeNode, length: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newAllocaHybrid(bb, allocty, lenty, length))
  def new_getiref(ctx: MuCtx, bb: MuBBNode, refty: MuTypeNode, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newGetIRef(bb, refty, opnd))
  def new_getfieldiref(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, refty: MuTypeNode, index: Int, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newGetFieldIRef(bb, is_ptr != 0, refty, index, opnd))
  def new_getelemiref(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, refty: MuTypeNode, indty: MuTypeNode, opnd: MuVarNode, index: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newGetElemIRef(bb, is_ptr != 0, refty, indty, opnd, index))
  def new_shiftiref(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, refty: MuTypeNode, offty: MuTypeNode, opnd: MuVarNode, offset: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newShiftIRef(bb, is_ptr != 0, refty, offty, opnd, offset))
  def new_getvarpartiref(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, refty: MuTypeNode, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newGetVarPartIRef(bb, is_ptr != 0, refty, opnd))
  def new_load(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, ord: MuMemOrd, refty: MuTypeNode, loc: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newLoad(bb, is_ptr != 0, ord, refty, loc))
  def new_store(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, ord: MuMemOrd, refty: MuTypeNode, loc: MuVarNode, newval: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newStore(bb, is_ptr != 0, ord, refty, loc, newval))
  def new_cmpxchg(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, is_weak: Int, ord_succ: MuMemOrd, ord_fail: MuMemOrd, refty: MuTypeNode, loc: MuVarNode, expected: MuVarNode, desired: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newCmpXchg(bb, is_ptr != 0, is_weak != 0, ord_succ, ord_fail, refty, loc, expected, desired))
  def new_atomicrmw(ctx: MuCtx, bb: MuBBNode, is_ptr: Int, ord: MuMemOrd, optr: MuAtomicRMWOptr, refTy: MuTypeNode, loc: MuVarNode, opnd: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newAtomicRMW(bb, is_ptr != 0, ord, optr, refTy, loc, opnd))
  def new_fence(ctx: MuCtx, bb: MuBBNode, ord: MuMemOrd): MuValueFak = exposeMuValue(ctx, ctx.newFence(bb, ord))
  def new_trap(ctx: MuCtx, bb: MuBBNode, rettys: MuValueFakArrayPtr, nrettys: Int): MuValueFak = exposeMuValue(ctx, ctx.newTrap(bb, readFromValueFakArray(rettys, nrettys)))
  def new_watchpoint(ctx: MuCtx, bb: MuBBNode, wpid: MuWPID, rettys: MuValueFakArrayPtr, nrettys: Int): MuValueFak = exposeMuValue(ctx, ctx.newWatchPoint(bb, wpid, readFromValueFakArray(rettys, nrettys)))
  def new_wpbranch(ctx: MuCtx, bb: MuBBNode, wpid: MuWPID): MuValueFak = exposeMuValue(ctx, ctx.newWPBranch(bb, wpid))
  def new_ccall(ctx: MuCtx, bb: MuBBNode, callconv: MuCallConv, callee_ty: MuTypeNode, sig: MuFuncSigNode, callee: MuVarNode, args: MuValueFakArrayPtr, nargs: Int): MuValueFak = exposeMuValue(ctx, ctx.newCCall(bb, callconv, callee_ty, sig, callee, readFromValueFakArray(args, nargs)))
  def new_newthread(ctx: MuCtx, bb: MuBBNode, stack: MuVarNode, threadlocal: Option[MuVarNode]): MuValueFak = exposeMuValue(ctx, ctx.newNewThread(bb, stack, threadlocal))
  def new_swapstack_ret(ctx: MuCtx, bb: MuBBNode, swappee: MuVarNode, ret_tys: MuValueFakArrayPtr, nret_tys: Int): MuValueFak = exposeMuValue(ctx, ctx.newSwapStackRet(bb, swappee, readFromValueFakArray(ret_tys, nret_tys)))
  def new_swapstack_kill(ctx: MuCtx, bb: MuBBNode, swappee: MuVarNode): MuValueFak = exposeMuValue(ctx, ctx.newSwapStackKill(bb, swappee))
  def set_newstack_pass_values(ctx: MuCtx, inst: MuInstNode, tys: MuValueFakArrayPtr, vars: MuValueFakArrayPtr, nvars: Int): Unit = ctx.setNewStackPassValues(inst, readFromValueFakArray(tys, nvars), readFromValueFakArray(vars, nvars))
  def set_newstack_throw_exc(ctx: MuCtx, inst: MuInstNode, exc: MuVarNode): Unit = ctx.setNewStackThrowExc(inst, exc)
  def new_comminst(ctx: MuCtx, bb: MuBBNode, opcode: MuCommInst, flags: MuFlagArrayPtr, nflags: Int, tys: MuValueFakArrayPtr, ntys: Int, sigs: MuValueFakArrayPtr, nsigs: Int, args: MuValueFakArrayPtr, nargs: Int): MuValueFak = exposeMuValue(ctx, ctx.newCommInst(bb, opcode, readFromFlagArray(flags, nflags), readFromValueFakArray(tys, ntys), readFromValueFakArray(sigs, nsigs), readFromValueFakArray(args, nargs)))

  // PASTE TO HERE

}

/**
 * These functions are not exposed.
 */
object NativeMuHelpers {

  private val ONE = BigInt(1)

  /**
   * Convert unsigned integer to BigInt.
   * <p>
   * NOTE: Scala implicitly converts Byte, Short, Int or Long to BigInt using sign-extension.
   * But when zero-extension is desired (unsigned native arguments), we must truncate the BigInt.
   *
   * @param len The number of bits in the INPUT number (not the output Mu int).
   */
  def trunc(num: BigInt, len: Int): BigInt = num & ((ONE << len) - ONE)

  def handleFromInt(ctx: MuCtx, num: BigInt, len: Int): MuValueFak = {
    exposeMuValue(ctx, ctx.handleFromInt(num, len))
  }

  val MU_THREAD_EXIT = 0x00
  val MU_REBIND_PASS_VALUES = 0x01
  val MU_REBIND_THROW_EXC = 0x02

  val MU_NOT_ATOMIC = 0x00
  val MU_RELAXED = 0x01
  val MU_CONSUME = 0x02
  val MU_ACQUIRE = 0x03
  val MU_RELEASE = 0x04
  val MU_ACQ_REL = 0x05
  val MU_SEQ_CST = 0x06

  val MU_XCHG = 0x00
  val MU_ADD = 0x01
  val MU_SUB = 0x02
  val MU_AND = 0x03
  val MU_NAND = 0x04
  val MU_OR = 0x05
  val MU_XOR = 0x06
  val MU_MAX = 0x07
  val MU_MIN = 0x08
  val MU_UMAX = 0x09
  val MU_UMIN = 0x0A

  val MU_DEFAULT = 0x00

  implicit def intToMemOrd(ord: MuMemOrd): MemoryOrder.MemoryOrder = ord match {
    case MU_NOT_ATOMIC => MemoryOrder.NOT_ATOMIC
    case MU_RELAXED    => MemoryOrder.RELAXED
    case MU_CONSUME    => MemoryOrder.CONSUME
    case MU_ACQUIRE    => MemoryOrder.ACQUIRE
    case MU_RELEASE    => MemoryOrder.RELEASE
    case MU_ACQ_REL    => MemoryOrder.ACQ_REL
    case MU_SEQ_CST    => MemoryOrder.SEQ_CST
  }

  implicit def intToMemAtomicRMWOp(op: MuAtomicRMWOptr): AtomicRMWOptr.AtomicRMWOptr = op match {
    case MU_XCHG => AtomicRMWOptr.XCHG
    case MU_ADD  => AtomicRMWOptr.ADD
    case MU_SUB  => AtomicRMWOptr.SUB
    case MU_AND  => AtomicRMWOptr.AND
    case MU_NAND => AtomicRMWOptr.NAND
    case MU_OR   => AtomicRMWOptr.OR
    case MU_XOR  => AtomicRMWOptr.XOR
    case MU_MAX  => AtomicRMWOptr.MAX
    case MU_MIN  => AtomicRMWOptr.MIN
    case MU_UMAX => AtomicRMWOptr.UMAX
    case MU_UMIN => AtomicRMWOptr.UMIN
  }

  implicit def intToFlag(cc: MuFlag): Flag = cc match {
    case MU_DEFAULT => Flag("#DEFAULT")
    case _          => throw new UvmRuntimeException("Unknown flag %d 0x%x".format(cc, cc))
  }

  def readFromValueFakArray[T <: MuValue](faks: MuValueFakArrayPtr, len: Long): Seq[T] = {
    if (faks == 0L) {
      Seq()
    } else {
      for (i <- 0L until len) yield {
        val addr = faks + i * WORD_SIZE_BYTES
        val valFak = theMemory.getAddress(addr)
        val v = getMuValueNotNull(valFak)
        v.asInstanceOf[T]
      }
    }
  }

  def readFromFlagArray(flags: MuValueFakArrayPtr, len: Long): Seq[Flag] = {
    if (flags == 0L) {
      Seq()
    } else {
      val flagVals = for (i <- 0L until len) yield {
        val addr = flags + i * 32
        val flagVal = theMemory.getInt(addr)
        flagVal
      }
      flagVals.map(intToFlag)
    }
  }

  implicit class MuCtxExtras(val ctx: MuCtx) extends AnyVal {
    def newConstIntEx(b: MuBundleNode, ty: MuTypeNode, values: LongPtr, nvalues: Int): MuConstNode = {
      val longs = for (i <- 0L until nvalues.toLong) yield {
        val addr = values + i * WORD_SIZE_BYTES
        val v = theMemory.getAddress(addr)
        v
      }

      var bi = BigInt(0L)
      var power = 0

      for (l <- longs) {
        bi |= ((BigInt(l) & 0xffffffffffffffffl) << power)
        power += WORD_SIZE_BITS.toInt
      }

      ctx.newConstInt(b, ty, bi)
    }
  }

  val MU_DEST_NORMAL = 0x01L
  val MU_DEST_EXCEPT = 0x02L
  val MU_DEST_TRUE = 0x03L
  val MU_DEST_FALSE = 0x04L
  val MU_DEST_DEFAULT = 0x05L
  val MU_DEST_DISABLED = 0x06L
  val MU_DEST_ENABLED = 0x07L

  implicit def intToDestKind(dk: MuDestKind): DestKind.Value = dk match {
    case MU_DEST_NORMAL   => DestKind.NORMAL
    case MU_DEST_EXCEPT   => DestKind.EXCEPT
    case MU_DEST_TRUE     => DestKind.TRUE
    case MU_DEST_FALSE    => DestKind.FALSE
    case MU_DEST_DEFAULT  => DestKind.DEFAULT
    case MU_DEST_DISABLED => DestKind.DISABLED
    case MU_DEST_ENABLED  => DestKind.ENABLED
  }

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

  implicit def intToBinOptr(binop: MuBinOptr): BinOptr.Value = binop match {
    case MU_BINOP_ADD  => BinOptr.ADD
    case MU_BINOP_SUB  => BinOptr.SUB
    case MU_BINOP_MUL  => BinOptr.MUL
    case MU_BINOP_SDIV => BinOptr.SDIV
    case MU_BINOP_SREM => BinOptr.SREM
    case MU_BINOP_UDIV => BinOptr.UDIV
    case MU_BINOP_UREM => BinOptr.UREM
    case MU_BINOP_SHL  => BinOptr.SHL
    case MU_BINOP_LSHR => BinOptr.LSHR
    case MU_BINOP_ASHR => BinOptr.ASHR
    case MU_BINOP_AND  => BinOptr.AND
    case MU_BINOP_OR   => BinOptr.OR
    case MU_BINOP_XOR  => BinOptr.XOR
    case MU_BINOP_FADD => BinOptr.FADD
    case MU_BINOP_FSUB => BinOptr.FSUB
    case MU_BINOP_FMUL => BinOptr.FMUL
    case MU_BINOP_FDIV => BinOptr.FDIV
    case MU_BINOP_FREM => BinOptr.FREM
  }

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

  implicit def intToCmpOptr(cmpop: MuCmpOptr): CmpOptr.Value = cmpop match {
    case MU_CMP_EQ     => CmpOptr.EQ
    case MU_CMP_NE     => CmpOptr.NE
    case MU_CMP_SGE    => CmpOptr.SGE
    case MU_CMP_SGT    => CmpOptr.SGT
    case MU_CMP_SLE    => CmpOptr.SLE
    case MU_CMP_SLT    => CmpOptr.SLT
    case MU_CMP_UGE    => CmpOptr.UGE
    case MU_CMP_UGT    => CmpOptr.UGT
    case MU_CMP_ULE    => CmpOptr.ULE
    case MU_CMP_ULT    => CmpOptr.ULT
    case MU_CMP_FFALSE => CmpOptr.FFALSE
    case MU_CMP_FTRUE  => CmpOptr.FTRUE
    case MU_CMP_FUNO   => CmpOptr.FUNO
    case MU_CMP_FUEQ   => CmpOptr.FUEQ
    case MU_CMP_FUNE   => CmpOptr.FUNE
    case MU_CMP_FUGT   => CmpOptr.FUGT
    case MU_CMP_FUGE   => CmpOptr.FUGE
    case MU_CMP_FULT   => CmpOptr.FULT
    case MU_CMP_FULE   => CmpOptr.FULE
    case MU_CMP_FORD   => CmpOptr.FORD
    case MU_CMP_FOEQ   => CmpOptr.FOEQ
    case MU_CMP_FONE   => CmpOptr.FONE
    case MU_CMP_FOGT   => CmpOptr.FOGT
    case MU_CMP_FOGE   => CmpOptr.FOGE
    case MU_CMP_FOLT   => CmpOptr.FOLT
    case MU_CMP_FOLE   => CmpOptr.FOLE

  }

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

  implicit def intToConvOptr(convop: MuConvOptr): ConvOptr.Value = convop match {
    case MU_CONV_TRUNC   => ConvOptr.TRUNC
    case MU_CONV_ZEXT    => ConvOptr.ZEXT
    case MU_CONV_SEXT    => ConvOptr.SEXT
    case MU_CONV_FPTRUNC => ConvOptr.FPTRUNC
    case MU_CONV_FPEXT   => ConvOptr.FPEXT
    case MU_CONV_FPTOUI  => ConvOptr.FPTOUI
    case MU_CONV_FPTOSI  => ConvOptr.FPTOSI
    case MU_CONV_UITOFP  => ConvOptr.UITOFP
    case MU_CONV_SITOFP  => ConvOptr.SITOFP
    case MU_CONV_BITCAST => ConvOptr.BITCAST
    case MU_CONV_REFCAST => ConvOptr.REFCAST
    case MU_CONV_PTRCAST => ConvOptr.PTRCAST
  }

}

object ClientAccessibleClassExposer {
  import NativeClientSupport._

  logger.debug("Creating mirror...")

  // Reflection utils.
  val mirror = ru.runtimeMirror(getClass.getClassLoader)

  val MAX_NAME_SIZE = 65536
  
  logger.debug("Types for common types...")

  // Type objects for common types.
  val TUnit = ru.typeTag[Unit].tpe
  val TByte = ru.typeTag[Byte].tpe
  val TShort = ru.typeTag[Short].tpe
  val TInt = ru.typeTag[Int].tpe
  val TLong = ru.typeTag[Long].tpe
  val TFloat = ru.typeTag[Float].tpe
  val TDouble = ru.typeTag[Double].tpe
  val TString = ru.typeTag[String].tpe
  val TMicroVM = ru.typeTag[MicroVM].tpe
  val TMuCtx = ru.typeTag[MuCtx].tpe
  val TMuValue = ru.typeTag[MuValue].tpe
  val TOptMuValue = ru.typeTag[Option[MuValue]].tpe

  // com.kenai.jffi.Closure.Buffer param getters and return value setters.
  // These are partially-applied functions, and will be called in closures (callback from C).
  def paramByte(index: Int)(buffer: Buffer): Any = buffer.getByte(index)
  def paramShort(index: Int)(buffer: Buffer): Any = buffer.getShort(index)
  def paramInt(index: Int)(buffer: Buffer): Any = buffer.getInt(index)
  def paramLong(index: Int)(buffer: Buffer): Any = buffer.getLong(index)
  def paramFloat(index: Int)(buffer: Buffer): Any = buffer.getFloat(index)
  def paramDouble(index: Int)(buffer: Buffer): Any = buffer.getDouble(index)
  def paramString(index: Int)(buffer: Buffer): Any = getStr(buffer, index)
  def paramMicroVM(index: Int)(buffer: Buffer): Any = getObjFromFuncTableAddr(buffer, index, NativeClientSupport.microVMs)
  def paramMuCtx(index: Int)(buffer: Buffer): Any = getObjFromFuncTableAddr(buffer, index, NativeClientSupport.muCtxs)
  def paramMuValue(index: Int, funcName: String, tpe: ru.Type)(buffer: Buffer): Any = {
    val muValue = getMuValue(buffer, index)
    val t = mirror.classSymbol(muValue.getClass).toType

    require(t <:< tpe, "Argument %d of %s expect %s, found %s".format(index, funcName, tpe, t))

    muValue
  }
  def paramOptMuValue(index: Int, funcName: String, tpe: ru.Type)(buffer: Buffer): Any = {
    val muOptValue = getOptMuValue(buffer, index)

    muOptValue foreach { muValue => // only check if it is Some(value)
      val t = mirror.classSymbol(muValue.getClass).toType
      val tpeArg = tpe.typeArgs(0)

      require(t <:< tpeArg, "Argument %d of %s expect %s, found %s".format(index, funcName, tpe, t))
    }

    muOptValue
  }

  def retVoid(buffer: Buffer, v: Any): Unit = {}
  def retByte(buffer: Buffer, v: Any): Unit = buffer.setByteReturn(v.asInstanceOf[Byte])
  def retShort(buffer: Buffer, v: Any): Unit = buffer.setShortReturn(v.asInstanceOf[Short])
  def retInt(buffer: Buffer, v: Any): Unit = buffer.setIntReturn(v.asInstanceOf[Int])
  def retLong(buffer: Buffer, v: Any): Unit = buffer.setLongReturn(v.asInstanceOf[Long])
  def retFloat(buffer: Buffer, v: Any): Unit = buffer.setFloatReturn(v.asInstanceOf[Float])
  def retDouble(buffer: Buffer, v: Any): Unit = buffer.setDoubleReturn(v.asInstanceOf[Double])
  def retString(buffer: Buffer, v: Any): Unit = buffer.setLongReturn(exposeStr(v.asInstanceOf[String]))

  private def getStr(buffer: Buffer, index: Int): String = {
    val addr = buffer.getAddress(index)
    val str = theMemory.getString(addr, MAX_NAME_SIZE, StandardCharsets.US_ASCII)
    str
  }

  private def getMuValue(buffer: Buffer, index: Int): MuValue = {
    val addr = buffer.getAddress(index)
    NativeClientSupport.getMuValueNotNull(addr)
  }

  private def getOptMuValue(buffer: Buffer, index: Int): Option[MuValue] = {
    val addr = buffer.getAddress(index)
    if (addr == 0L) {
      None
    } else {
      Some(NativeClientSupport.getMuValueNotNull(addr))
    }
  }

  private def exposeStr(str: String): Word = {
    val ptr = NativeClientSupport.stringPool.getOrElseUpdate(str, {
      val bytes = str.getBytes(StandardCharsets.US_ASCII)
      val newPtr = jnrMemoryManager.allocateDirect(bytes.size + 1)
      newPtr.put(0L, bytes, 0, bytes.length)
      newPtr.putByte(bytes.length, 0)
      newPtr
    })
    ptr.address()
  }

  private def getObjFromFuncTableAddr[T](buffer: Buffer, index: Int, orm: ObjectReferenceManager[T]): T = {
    val funcTableAddr = buffer.getLong(index)
    NativeClientSupport.getObjFromFuncTableAddrNotNull(funcTableAddr, orm)
  }

  // Convert to JFFI native types.
  def toJFFIType(t: ru.Type): JType = t match {
    case t if t =:= TUnit       => JType.VOID
    case t if t =:= TByte       => JType.SINT8
    case t if t =:= TShort      => JType.SINT16
    case t if t =:= TInt        => JType.SINT32
    case t if t =:= TLong       => JType.SINT64
    case t if t =:= TFloat      => JType.FLOAT
    case t if t =:= TDouble     => JType.DOUBLE
    case t if t =:= TString     => JType.POINTER
    case t if t =:= TMicroVM    => JType.POINTER
    case t if t =:= TMuCtx      => JType.POINTER
    case t if t <:< TMuValue    => JType.POINTER
    case t if t <:< TOptMuValue => JType.POINTER
  }
  
  logger.debug("Allocating Mu error number memory...")

  val MU_NATIVE_ERRNO = 6481626 // muErrno is set to this number if an exception is thrown
  val muErrorPtr = jnrMemoryManager.allocateDirect(16)

  def getMuError(): Int = muErrorPtr.getInt(0)
  def setMuError(errno: Int): Unit = muErrorPtr.putInt(0, errno)

  /**
   * Let a native program call a reflectively-exposed method.
   */
  class ExposedMethodClosure(method: ru.MethodMirror, paramGetters: Seq[Buffer => Any], returnSetter: (Buffer, Any) => Unit) extends Closure {
    def invoke(buffer: Buffer): Unit = {
      try {
        val params = paramGetters.map(_(buffer))
        val rv = method.apply(params: _*)
        returnSetter(buffer, rv)
      } catch {
        case e: Throwable => {
          logger.error("Exception thrown before returning to native. This is fatal", e)
          System.out.flush()
          System.err.flush()
          setMuError(MU_NATIVE_ERRNO)
          throw e
        }
      }
    }
  }
}

/**
 * Expose a class (NativeMuVM and NativeMuCtx) as a function table.
 */
class ClientAccessibleClassExposer[T: ru.TypeTag: ClassTag](obj: T) {
  import ClientAccessibleClassExposer._
  
  logger.debug("Creating closure handles...")

  /**
   * A list of JFFI closure handles, one for each declared method in obj, in the declared order.
   */
  val closureHandles: IndexedSeq[Closure.Handle] = {
    val tt = ru.typeTag[T]
    val iter = for (m <- tt.tpe.decls if m.isMethod && !m.isConstructor) yield {
      val meth = m.asMethod
      logger.debug("Exposing method: %s".format(meth))

      val paramTypes = meth.paramLists(0).map(_.asTerm.typeSignature)
      val returnType = meth.returnType

      val paramGetters = for ((ty, i) <- paramTypes.zipWithIndex) yield ty match {
        case t if t =:= TByte       => paramByte(i) _
        case t if t =:= TShort      => paramShort(i) _
        case t if t =:= TInt        => paramInt(i) _
        case t if t =:= TLong       => paramLong(i) _
        case t if t =:= TFloat      => paramFloat(i) _
        case t if t =:= TDouble     => paramDouble(i) _
        case t if t =:= TString     => paramString(i) _
        case t if t =:= TMicroVM    => paramMicroVM(i) _
        case t if t =:= TMuCtx      => paramMuCtx(i) _
        case t if t <:< TMuValue    => paramMuValue(i, meth.name.toString, t) _
        case t if t <:< TOptMuValue => paramOptMuValue(i, meth.name.toString, t) _
      }

      val returnSetter = returnType match {
        case t if t =:= TUnit   => retVoid _
        case t if t =:= TByte   => retByte _
        case t if t =:= TShort  => retShort _
        case t if t =:= TInt    => retInt _
        case t if t =:= TLong   => retLong _
        case t if t =:= TFloat  => retFloat _
        case t if t =:= TDouble => retDouble _
        case t if t =:= TString => retString _
      }

      val paramNativeTypes = for (ty <- paramTypes) yield toJFFIType(ty)
      val returnNativeType = toJFFIType(returnType)

      val objMirror = mirror.reflect(obj)
      val methMirror = objMirror.reflectMethod(meth)

      val closure = new ExposedMethodClosure(methMirror, paramGetters, returnSetter)

      val closureHandle = jffiClosureManager.newClosure(closure, returnNativeType, paramNativeTypes.toArray, CallingConvention.DEFAULT)

      closureHandle
    }

    iter.toIndexedSeq
  }

  /**
   * Allocate and populate a native function table,
   * i.e. the in-memory structure of struct MuVM and struct MuCtx in the client interface.
   *
   * @param objAddr The "address" of the object, created by the ObjectReferenceManager.
   * This will be the first field of the table.
   *
   * @return The address of the function table.
   */
  def makeFuncTable(objAddr: Word): FuncTablePtr = {
    val nMethods = closureHandles.size
    val structSizeWords = nMethods + 1
    val structSizeBytes = structSizeWords * WORD_SIZE_BYTES.toInt
    val s = jnrMemoryManager.allocateDirect(structSizeBytes)
    s.putLong(0L, objAddr)
    for ((handle, i) <- closureHandles.zipWithIndex) {
      val offset = (i + 1) * WORD_SIZE_BYTES
      val funcAddr = handle.getAddress()
      s.putLong(offset, funcAddr)
    }
    val funcTableAddr = s.address()
    NativeClientSupport.funcTableToPointer(funcTableAddr) = s
    funcTableAddr
  }
}

/**
 * Support for native clients (such as C programs).
 * <p>
 * MicroVM, MuCtx and MuValue instances can be exposed to native programs.
 * <p>
 * An exposed MicroVM has a function table (the MuVM struct in C) and a fake pointer (managed by the
 * ObjectReferenceManager) referring to the Scala MicroVM instance. The former is kept in the funcTableToPtr map,
 * and the latter is kept in the microVMs ORM. The fake pointer is stored in the header field of the MuVM struct.
 * <p>
 * An exposed MuCtx has a function table, a fake pointer, and a hash set containing all exposed MuValues derived from
 * that MuCtx, held in muCtxToMuValue(theCtx). Unexposing a MuCtx automatically unexposes all of its MuValues. The fake
 * pointer is the header of the MuCtx struct.
 * <p>
 * An exposed MuValue only has a fake pointer, held in muValues. The pointer is also stored in muCtxToMuValues(ctx) for
 * its context ctx.
 */
object NativeClientSupport {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  // Syntax sugar for C-level pointer types.
  // "Fak" means "fake pointer", i.e. ObjectReferenceManager-generated "pointer".
  type FuncTablePtr = Word
  type MuVMFak = FuncTablePtr
  type MuCtxFak = FuncTablePtr
  type MuValueFak = Word
  type MuTrapHandlerFP = Word
  type IntPtr = Word
  type LongPtr = Word
  type CharArrayPtr = Word
  type MuValueFakPtr = Word
  type MuValueFakArrayPtr = Word
  type MuValuesFreerFP = Word
  type MuFlag = Int
  type MuFlagPtr = Word
  type MuFlagArrayPtr = Word

  // Mu API C-level types.
  type MuID = Int
  type MuName = String
  type MuCPtr = Word
  type MuCFP = Word
  type MuMemOrd = Int
  type MuAtomicRMWOptr = Int
  type MuHowToResume = Int
  type MuHowToResumePtr = IntPtr
  type MuCallConv = Int
  type MuBinOptr = Int
  type MuCmpOptr = Int
  type MuConvOptr = Int
  type MuDestKind = Int
  type MuWPID = Int
  type MuCommInst = Int

  // Give exposed objects a random "memory address" so native programs can pass them back to Mu as parameters.
  val microVMs = jnrRuntime.newObjectReferenceManager[MicroVM]()
  val muCtxs = jnrRuntime.newObjectReferenceManager[MuCtx]()
  val muValues = jnrRuntime.newObjectReferenceManager[MuValue]()

  /** Map each MuCtx to all of its current MuValues. This is needed when closing a MuCtx. */
  val muCtxToMuValueFaks = HashMap[MuCtx, HashSet[MuValueFak]]()
  
  def getMicroVM(funcTableAddr: FuncTablePtr): MicroVM = getObjFromFuncTableAddrNotNull(funcTableAddr, microVMs)
  def getMuCtx(funcTableAddr: FuncTablePtr): MuCtx = getObjFromFuncTableAddrNotNull(funcTableAddr, muCtxs)

  /** Given a function table pointer, get an object from a ObjectReferenceManager. Assert it is not null. */
  def getObjFromFuncTableAddrNotNull[T](funcTableAddr: FuncTablePtr, orm: ObjectReferenceManager[T]): T = {
    val objFak = theMemory.getAddress(funcTableAddr)
    val obj = orm.get(jnrMemoryManager.newPointer(objFak))
    if (obj == null) {
      throw new UvmRefImplException("Exposed object not found. Fake address: %d 0x%x".format(objFak, objFak))
    }
    obj
  }

  /** Get the MuValue instance form a C MuValue (a pointer). */
  def getMuValueNotNull[T <: MuValue](fak: MuValueFak): T = {
    val muVal = muValues.get(jnrMemoryManager.newPointer(fak))
    if (muVal == null) {
      throw new UvmRefImplException("Exposed MuValue not found. Fake address: %d 0x%x".format(fak, fak))
    }
    muVal.asInstanceOf[T]
  }

  /** Get the MuValue instance form a C MuValue (a pointer). */
  def getMuValueNullable[T <: MuValue](fak: MuValueFak): Option[T] = {
    val muVal = muValues.get(jnrMemoryManager.newPointer(fak))
    if (muVal == null) {
      None
    } else {
      Some(muVal.asInstanceOf[T])
    }
  }

  /**
   * Map function table addresses to Pointer so they can be closed. JFFI uses the TrantientNativeMemory (wrapped
   * in the Pointer object) to keep the allocated native memory alive.
   */
  val funcTableToPointer = HashMap[FuncTablePtr, Pointer]()

  /**
   * Map Java strings (currently only names of Identified objects in MU) to Pointers to keep the allocated strings
   * alive so that the native program can access them.
   */
  val stringPool = HashMap[String, Pointer]()

  // These "exposer" can repeatedly generate function tables.
  logger.debug("Creating Mu VM Exposer...")
  val muVMExposer = new ClientAccessibleClassExposer(NativeMuVM)
  logger.debug("Creating Mu Ctx Exposer...")
  val muCtxExposer = new ClientAccessibleClassExposer(NativeMuCtx)

  // Expose and unexpose objects

  /** Expose a MicroVM. Return a pointer to the C MuVM structure. */
  def exposeMicroVM(microVM: MicroVM): FuncTablePtr = {
    val objFak = microVMs.add(microVM).address()
    val funcTableAddr = muVMExposer.makeFuncTable(objFak)
    funcTableAddr
  }

  /** Expose a MuCtx. Return a pointer to the C MuCtx structure. */
  def exposeMuCtx(muCtx: MuCtx): FuncTablePtr = {
    require(!muCtxToMuValueFaks.contains(muCtx), "Context %s is already exposed.".format(muCtx))
    muCtxToMuValueFaks(muCtx) = HashSet()
    val objFak = muCtxs.add(muCtx).address()
    val funcTableAddr = muCtxExposer.makeFuncTable(objFak)
    funcTableAddr
  }

  /** Expose a MuValue. Return an "object pointer", i.e. faked by ObjectReferenceManager. */
  def exposeMuValue(muCtx: MuCtx, muValue: MuValue): MuValueFak = {
    val fak = muValues.add(muValue).address()
    muCtxToMuValueFaks(muCtx).add(fak)
    fak
  }

  /** Unexpose a MicroVM. Remove the faked pointer and deallocate the function table (by removing the Pointer). */
  def unexposeMicroVM(funcTableAddr: FuncTablePtr): Unit = {
    require(funcTableToPointer.contains(funcTableAddr),
      "MuVM struct pointer %d 0x%x does not exist".format(funcTableAddr, funcTableAddr))
    val objPtr = theMemory.getPointer(funcTableAddr)
    microVMs.remove(objPtr).ensuring(_ == true)
    funcTableToPointer.remove(funcTableAddr)
  }

  /**
   * Unexpose a MuCtx. Remove the faked pointer and deallocate the function table (by removing the Pointer).
   *  Also unexpose all MuValues held by the MuCtx.
   */
  def unexposeMuCtx(funcTableAddr: FuncTablePtr): Unit = {
    require(funcTableToPointer.contains(funcTableAddr),
      "MuCtx struct pointer %d 0x%x does not exist".format(funcTableAddr, funcTableAddr))
    val objPtr = theMemory.getPointer(funcTableAddr)
    val muCtx = muCtxs.get(objPtr).ensuring { _ != null }
    for (muValueAddr <- muCtxToMuValueFaks.remove(muCtx).ensuring(_.isDefined).get) {
      // Don't remove it from muCtxToMuValues(muCtx) because the whole HashSet has just been detached.  
      muValues.remove(jnrMemoryManager.newPointer(muValueAddr)).ensuring(_ == true)
    }
    muCtxs.remove(objPtr)
    funcTableToPointer.remove(funcTableAddr)
  }

  /** Unexpose a MuValue by removing it from the ORM. */
  def unexposeMuValue(muCtx: MuCtx, addr: MuValueFak): Unit = {
    muCtxToMuValueFaks(muCtx).remove(addr)
    muValues.remove(jnrMemoryManager.newPointer(addr)).ensuring(_ == true)
  }
}