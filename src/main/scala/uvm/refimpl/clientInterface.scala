package uvm.refimpl

import uvm.types._
import uvm.refimpl.itpr._
import java.io.Reader
import scala.collection.mutable.HashSet
import uvm.refimpl.mem.TypeSizes._
import uvm.ssavariables.MemoryOrder._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.refimpl.mem._
import uvm.ssavariables.HasKeepAliveClause
import scala.collection.mutable.ArrayBuffer
import uvm.ssavariables.Flag

object MuValue {
  def apply(ty: Type, vb: ValueBox): MuValue = (ty, vb) match {
    case (t: TypeInt, v: BoxInt)           => MuIntValue(t, v)
    case (t: TypeFloat, v: BoxFloat)       => MuFloatValue(t, v)
    case (t: TypeDouble, v: BoxDouble)     => MuDoubleValue(t, v)
    case (t: TypeRef, v: BoxRef)           => MuRefValue(t, v)
    case (t: TypeIRef, v: BoxIRef)         => MuIRefValue(t, v)
    case (t: TypeStruct, v: BoxSeq)        => MuStructValue(t, v)
    case (t: TypeArray, v: BoxSeq)         => MuArrayValue(t, v)
    case (t: TypeFuncRef, v: BoxFunc)      => MuFuncRefValue(t, v)
    case (t: TypeThreadRef, v: BoxThread)  => MuThreadRefValue(t, v)
    case (t: TypeStackRef, v: BoxStack)    => MuStackRefValue(t, v)
    case (t: TypeTagRef64, v: BoxTagRef64) => MuTagRef64Value(t, v)
    case (t: TypeUPtr, v: BoxPointer)      => MuUPtrValue(t, v)
    case (t: TypeUFuncPtr, v: BoxPointer)  => MuUFPValue(t, v)
    case (t, v) => {
      throw new IllegalArgumentException("Improper type-box pair: %s,%s".format(t.getClass.getSimpleName, v.getClass.getSimpleName))
    }
  }
}

/**
 * A handle to a Mu value, held by a MuCtx. In the Scala API, Handles are immutable and cannot be copied.
 * Only use the handle in the MuCtx it is defined.
 */
abstract class MuValue {
  def ty: Type
  def vb: ValueBox
}

case class MuIntValue(ty: TypeInt, vb: BoxInt) extends MuValue
case class MuFloatValue(ty: TypeFloat, vb: BoxFloat) extends MuValue
case class MuDoubleValue(ty: TypeDouble, vb: BoxDouble) extends MuValue
case class MuRefValue(ty: TypeRef, vb: BoxRef) extends MuValue
case class MuIRefValue(ty: TypeIRef, vb: BoxIRef) extends MuValue
case class MuStructValue(ty: TypeStruct, vb: BoxSeq) extends MuValue
case class MuArrayValue(ty: TypeArray, vb: BoxSeq) extends MuValue
case class MuFuncRefValue(ty: TypeFuncRef, vb: BoxFunc) extends MuValue
case class MuThreadRefValue(ty: TypeThreadRef, vb: BoxThread) extends MuValue
case class MuStackRefValue(ty: TypeStackRef, vb: BoxStack) extends MuValue
case class MuTagRef64Value(ty: TypeTagRef64, vb: BoxTagRef64) extends MuValue
case class MuUPtrValue(ty: TypeUPtr, vb: BoxPointer) extends MuValue
case class MuUFPValue(ty: TypeUFuncPtr, vb: BoxPointer) extends MuValue

abstract class TrapHandlerResult
case class ThreadExit() extends TrapHandlerResult

abstract class HowToResume extends TrapHandlerResult
case class RebindPassValues(newStack: MuStackRefValue, values: Seq[MuValue]) extends HowToResume
case class RebindThrowExc(newStack: MuStackRefValue, exc: MuRefValue) extends HowToResume

trait TrapHandler {
  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult
}

trait UndefinedFunctionHandler {
  def handleUndefinedFunction(functionID: Int): Unit
}

class MuCtx(mutator: Mutator)(
    implicit microVM: MicroVM, memorySupport: MemorySupport) extends ObjectPinner {
  val handles = new HashSet[MuValue]()

  val pinSet = new ArrayBuffer[Word]

  /** Given a name, get the ID of an identified entity. */
  def idOf(name: String): Int = microVM.idOf(name)

  /** Given an ID, get the name of an identified entity. */
  def nameOf(id: Int): String = microVM.nameOf(id)

  /** Close the context. */
  def closeContext(): Unit = {
    handles.clear()
    mutator.close()
    microVM.clientAgents.remove(this)
  }

  /** Load a Mu IR bundle */
  def loadBundle(r: Reader): Unit = {
    val bundle = microVM.irReader.read(r, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  /** Load a Mu IR bundle */
  def loadBundle(s: String): Unit = {
    val bundle = microVM.irReader.read(s, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  /** Load a HAIL script */
  def loadHail(r: Reader): Unit = ???

  /** Load a HAIL script */
  def loadHail(s: String): Unit = ???

  private def addHandle[T <: MuValue](h: T): T = {
    handles.add(h)
    h
  }

  /** Convert any int (BigInt) to a handle. */
  def handleFromInt(num: BigInt, len: Int): MuIntValue = {
    val t = InternalTypePool.intOf(len)
    val v = OpHelper.unprepare(num, len)
    addHandle(MuIntValue(t, BoxInt(v)))
  }

  /** Convert float to a handle. */
  def handleFromFloat(num: Float): MuFloatValue = {
    val t = InternalTypes.FLOAT
    addHandle(MuFloatValue(t, BoxFloat(num)))
  }

  /** Convert double to a handle. */
  def handleFromDouble(num: Double): MuDoubleValue = {
    val t = InternalTypes.DOUBLE
    addHandle(MuDoubleValue(t, BoxDouble(num)))
  }

  /** Convert pointer (Long) to a handle. */
  def handleFromPtr(muType: Int, v: Word): MuUPtrValue = {
    val t = microVM.globalBundle.typeNs(muType).asInstanceOf[TypeUPtr]
    addHandle(MuUPtrValue(t, BoxPointer(v)))
  }

  /** Convert function pointer (Long) to a handle. */
  def handleFromFP(muType: Int, v: Word): MuUFPValue = {
    val t = microVM.globalBundle.typeNs(muType).asInstanceOf[TypeUFuncPtr]
    addHandle(MuUFPValue(t, BoxPointer(v)))
  }

  /** Convert handle to an integer (BigInt). */
  def handleToInt(opnd: MuIntValue, signExt: Boolean): BigInt = {
    val t = opnd.ty
    val ib = opnd.vb
    if (signExt) OpHelper.prepareSigned(ib.value, t.length) else OpHelper.prepareUnsigned(ib.value, t.length)
  }

  /** Convert handle to integer (BigInt), assume signed. */
  def handleToSInt(opnd: MuIntValue) = handleToInt(opnd, true)

  /** Convert handle to integer (BigInt), assume unsigned. */
  def handleToUInt(opnd: MuIntValue) = handleToInt(opnd, false)

  /** Convert handle to float. */
  def handleToFloat(opnd: MuFloatValue): Float = {
    opnd.vb.value
  }

  /** Convert handle to double. */
  def handleToDouble(opnd: MuDoubleValue): Double = {
    opnd.vb.value
  }

  /** Convert handle to pointer (Long). */
  def handleToPtr(opnd: MuUPtrValue): Word = {
    opnd.vb.addr
  }

  /** Convert handle to function pointer (Long). */
  def handleToFP(opnd: MuUFPValue): Word = {
    opnd.vb.addr
  }

  /** Make a handle for a constant. */
  def handleFromConst(id: Int): MuValue = {
    val c = microVM.globalBundle.constantNs(id)
    val t = c.constTy
    val box = microVM.constantPool.getGlobalVarBox(c)
    addHandle(MuValue(t, box))
  }

  /** Make a handle for a global cell (its iref). */
  def handleFromGlobal(id: Int): MuIRefValue = {
    val g = microVM.globalBundle.globalCellNs(id)
    val t = InternalTypePool.irefOf(g.cellTy)
    val a = microVM.memoryManager.globalMemory.addrForGlobalCell(g)
    val box = BoxIRef(0L, a)
    addHandle(MuIRefValue(t, box))
  }

  /** Make a handle for a function (funcref). */
  def handleFromFunc(id: Int): MuFuncRefValue = {
    val f = microVM.globalBundle.funcNs(id)
    val t = InternalTypePool.funcOf(f.sig)
    val box = BoxFunc(Some(f))
    addHandle(MuFuncRefValue(t, box))
  }

  /**
   * Make a handle for an exposed function. In this implementation, the type is ufuncptr, but other implementations
   * may be different.
   */
  def handleFromExpose(id: Int): MuUFPValue = {
    val ef = microVM.globalBundle.expFuncNs(id)
    val t = InternalTypePool.funcPtrOf(ef.func.sig)
    val box = BoxPointer(microVM.nativeCallHelper.getStaticExpFuncAddr(ef))
    addHandle(MuUFPValue(t, box))
  }

  /** Delete a handle. */
  def deleteValue(opnd: MuValue): Unit = {
    handles.remove(opnd)
  }

  /** Compare general reference types for equality. */
  def refEq(lhs: MuValue, rhs: MuValue): Boolean = (lhs, rhs) match {
    case (l: MuRefValue, r: MuRefValue)             => l.vb.objRef == r.vb.objRef
    case (l: MuIRefValue, r: MuIRefValue)           => l.vb.oo == r.vb.oo
    case (l: MuFuncRefValue, r: MuFuncRefValue)     => l.vb.func == r.vb.func
    case (l: MuThreadRefValue, r: MuThreadRefValue) => l.vb.thread == r.vb.thread
    case (l: MuStackRefValue, r: MuStackRefValue)   => l.vb.stack == r.vb.stack
    case (l, r) => {
      throw new IllegalArgumentException("Bad types for refEq: %s and %s".format(
        l.getClass.getSimpleName, r.getClass.getSimpleName))
    }
  }

  /** Compare general reference types for less-than. */
  def refUlt(lhs: MuIRefValue, rhs: MuIRefValue): Boolean = {
    if (lhs.vb.objRef != rhs.vb.objRef) {
      val (lb, lo) = lhs.vb.oo
      val (rb, ro) = rhs.vb.oo
      throw new UvmRefImplException(("Two operands refer to different objects. This is an undefined behaviour. " +
        "lhs: %d+%d (0x%x+0x%x); rhs: %d+%d (0x%x+0x%x)".format(lb, lo, lb, lo, rb, ro, rb, ro)))
    }
    lhs.vb.offset < rhs.vb.offset
  }

  def extractValue(str: Handle, index: Int): Handle = {
    val st = str.ty.asInstanceOf[TypeStruct]
    val sb = str.vb.asInstanceOf[BoxStruct]
    val et = st.fieldTys(index)
    val eb = sb.values(index)
    newHandle(et, eb)
  }

  def insertValue(str: Handle, index: Int, newVal: Handle): Handle = {
    val st = str.ty.asInstanceOf[TypeStruct]
    val sb = str.vb.asInstanceOf[BoxStruct]
    val nsb = BoxStruct(for ((b, i) <- sb.values.zipWithIndex) yield if (i == index) newVal.vb else b)
    newHandle(st, nsb)
  }

  def newFixed(tid: Int): Handle = {
    val t = microVM.globalBundle.typeNs(tid)
    val objRef = mutator.newScalar(t)
    val b = BoxRef(objRef)
    val rt = InternalTypePool.refOf(t)
    newHandle(rt, b)
  }

  def newHybrid(tid: Int, length: Handle): Handle = {
    val t = microVM.globalBundle.typeNs(tid).asInstanceOf[TypeHybrid]
    val len = toInt(length).longValue
    val objRef = mutator.newHybrid(t, len)
    val b = BoxRef(objRef)
    val rt = InternalTypePool.refOf(t)
    newHandle(rt, b)
  }

  def refCast(handle: Handle, newType: Int): Handle = {
    val t = microVM.globalBundle.typeNs(newType)
    newHandle(t, handle.vb)
  }

  def getIRef(handle: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeRef]
    val nt = InternalTypePool.irefOf(t.ty)
    val ob = handle.vb.asInstanceOf[BoxRef]
    val nb = BoxIRef(ob.objRef, 0L)
    newHandle(nt, nb)
  }

  def getFieldIRef(handle: Handle, index: Int): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val st = t.ty.asInstanceOf[TypeStruct]
    val ft = st.fieldTys(index)
    val nt = InternalTypePool.irefOf(ft)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.fieldOffsetOf(st, index))
    newHandle(nt, nb)
  }

  def getElemIRef(handle: Handle, index: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val st = t.ty.asInstanceOf[AbstractSeqType]
    val et = st.elemTy
    val nt = InternalTypePool.irefOf(et)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val i = toInt(index, signExt = true).longValue
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.elemOffsetOf(st, i))
    newHandle(nt, nb)
  }

  def shiftIRef(handle: Handle, index: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val rt = t.ty
    val nt = InternalTypePool.irefOf(rt)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val i = toInt(index, signExt = true).longValue
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.shiftOffsetOf(rt, i))
    newHandle(nt, nb)
  }

  def getVarPartIRef(handle: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val ht = t.ty.asInstanceOf[TypeHybrid]
    val vt = ht.varTy
    val nt = InternalTypePool.irefOf(vt)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.varPartOffsetOf(ht))
    newHandle(nt, nb)
  }

  def load(ord: MemoryOrder, loc: Handle): Handle = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypeUPtr(t) => (true, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val nb = ValueBox.makeBoxForType(uty)

    MemoryOperations.load(ptr, uty, addr, nb)

    newHandle(uty, nb)
  }

  def store(ord: MemoryOrder, loc: Handle, newVal: Handle): Unit = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypeUPtr(t) => (true, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val nvb = newVal.vb

    MemoryOperations.store(ptr, uty, addr, nvb)
  }

  def cmpXchg(ordSucc: MemoryOrder, ordFail: MemoryOrder, weak: Boolean, loc: Handle, expected: Handle, desired: Handle): (Boolean, Handle) = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypeUPtr(t) => (true, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val eb = expected.vb
    val db = desired.vb
    val br = ValueBox.makeBoxForType(uty)
    val succ = MemoryOperations.cmpXchg(ptr, uty, addr, eb, db, br)
    (succ, newHandle(uty, br))
  }

  def atomicRMW(ord: MemoryOrder, op: AtomicRMWOptr, loc: Handle, opnd: Handle): Handle = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypeUPtr(t) => (true, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val ob = opnd.vb
    val br = ValueBox.makeBoxForType(uty)
    MemoryOperations.atomicRMW(ptr, uty, op, addr, ob, br)
    newHandle(uty, br)
  }

  def fence(ord: MemoryOrder): Unit = {
  }

  def newStack(func: Handle): Handle = {
    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    val sta = microVM.threadStackManager.newStack(funcVal, mutator)

    val nb = BoxStack(Some(sta))
    newHandle(InternalTypes.STACK, nb)
  }

  private def getStackNotNull(stack: Handle): InterpreterStack = {
    stack.vb.asInstanceOf[BoxStack].stack match {
      case None    => throw new UvmRuntimeException("Stack argument cannot be a NULL micro VM stack value.")
      case Some(v) => v
    }
  }

  def newThread(stack: Handle): Handle = {
    val sv = getStackNotNull(stack)
    val thr = microVM.threadStackManager.newThread(sv)

    val nb = BoxThread(Some(thr))
    newHandle(InternalTypes.THREAD, nb)
  }

  def killStack(stack: Handle): Unit = {
    val sv = getStackNotNull(stack)

    sv.kill()
  }

  private def nthFrame(stack: InterpreterStack, n: Int): InterpreterFrame = {
    val it = stack.frames
    for (i <- (0 until n)) {
      if (it.hasNext) {
        it.next()
      } else {
        throw new UvmRuntimeException("The stack only has %d frames, but the %d-th frame is requested.".format(i, n))
      }
    }
    if (it.hasNext) {
      it.next()
    } else {
      throw new UvmRuntimeException("The stack only has %d frames, but the %d-th frame is requested.".format(n, n))
    }

  }

  def currentFuncVer(stack: Handle, frame: Int): Int = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr match {
      case f: NativeFrame      => 0
      case f: UndefinedMuFrame => 0
      case f: DefinedMuFrame   => f.funcVer.id
    }
  }

  def currentInstruction(stack: Handle, frame: Int): Int = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr match {
      case f: NativeFrame      => 0
      case f: UndefinedMuFrame => 0
      case f: DefinedMuFrame   => f.curInst.id
    }
  }

  def dumpKeepalives(stack: Handle, frame: Int): Seq[Handle] = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr match {
      case f: NativeFrame => {
        throw new UvmRefImplException("Attempt to dump keepalive variables for a native frame for native funciton 0x%x".format(f.func))
      }
      case f: UndefinedMuFrame => {
        for ((ty, box) <- f.func.sig.paramTys zip f.boxes) yield {
          newHandle(ty, box)
        }
      }
      case f: DefinedMuFrame => {
        val i = f.curInst
        i match {
          case hkac: HasKeepAliveClause => {
            val kas = hkac.keepAlives
            for (ka <- kas) yield {
              val box = f.boxes(ka)
              val ty = TypeInferer.inferType(ka)
              newHandle(ty, box)
            }
          }
          case _ => {
            throw new UvmRuntimeException("The current instruction %s does not have keep-alive clause.".format(i.repr))
          }
        }
      }
    }
  }

  def popFrame(stack: Handle): Unit = {
    val st = getStackNotNull(stack)
    val top = st.top
    top match {
      case f: NativeFrame => throw new UvmRuntimeException("Attempting to pop a native frame. It has implementation-defined behaviour and this refimpl does not allow it.")
      case f: MuFrame => f.prev match {
        case None       => throw new UvmRuntimeException("Attempting to pop the last frame of a stack.")
        case Some(prev) => st.popFrame()
      }
    }
  }

  def pushFrame(stack: Handle, func: Handle): Unit = {
    val sta = stack.vb.asInstanceOf[BoxStack].stack.getOrElse {
      throw new UvmRuntimeException("Stack must not be NULL")
    }

    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    sta.pushFrame(funcVal)
  }

  def tr64IsFp(handle: Handle): Boolean = {
    OpHelper.tr64IsFp(handle.vb.asInstanceOf[BoxTagRef64].raw)
  }

  def tr64IsInt(handle: Handle): Boolean = {
    OpHelper.tr64IsInt(handle.vb.asInstanceOf[BoxTagRef64].raw)
  }

  def tr64IsRef(handle: Handle): Boolean = {
    OpHelper.tr64IsRef(handle.vb.asInstanceOf[BoxTagRef64].raw)
  }

  def tr64ToFp(handle: Handle): Handle = {
    val raw = handle.vb.asInstanceOf[BoxTagRef64].raw
    val box = new BoxDouble(OpHelper.tr64ToFp(raw))
    newHandle(InternalTypes.DOUBLE, box)
  }

  def tr64ToInt(handle: Handle): Handle = {
    val raw = handle.vb.asInstanceOf[BoxTagRef64].raw
    val box = new BoxInt(OpHelper.tr64ToInt(raw))
    newHandle(InternalTypes.I52, box)
  }

  def tr64ToRef(handle: Handle): Handle = {
    val raw = handle.vb.asInstanceOf[BoxTagRef64].raw
    val box = new BoxRef(OpHelper.tr64ToRef(raw))
    newHandle(InternalTypes.REF_VOID, box)
  }

  def tr64ToTag(handle: Handle): Handle = {
    val raw = handle.vb.asInstanceOf[BoxTagRef64].raw
    val box = new BoxInt(OpHelper.tr64ToTag(raw))
    newHandle(InternalTypes.I6, box)
  }

  def tr64FromFp(handle: Handle): Handle = {
    val fp = handle.vb.asInstanceOf[BoxDouble].value
    val box = new BoxTagRef64(OpHelper.fpToTr64(fp))
    newHandle(InternalTypes.TAGREF64, box)
  }

  def tr64FromInt(handle: Handle): Handle = {
    val i = handle.vb.asInstanceOf[BoxInt].value
    val box = new BoxTagRef64(OpHelper.intToTr64(i.longValue))
    newHandle(InternalTypes.TAGREF64, box)
  }

  def tr64FromRef(ref: Handle, tag: Handle): Handle = {
    val refv = ref.vb.asInstanceOf[BoxRef].objRef
    val tagv = tag.vb.asInstanceOf[BoxInt].value
    val box = new BoxTagRef64(OpHelper.refToTr64(refv, tagv.longValue))
    newHandle(InternalTypes.TAGREF64, box)
  }

  def enableWatchPoint(wpID: Int): Unit = {
    microVM.trapManager.enableWatchPoint(wpID)
  }

  def disableWatchPoint(wpID: Int): Unit = {
    microVM.trapManager.disableWatchPoint(wpID)
  }

  def ptrcast(handle: Handle, newType: Type): Handle = {
    require(handle.ty.isInstanceOf[AbstractPointerType] || handle.ty.isInstanceOf[TypeInt], "handle must have type int, ptr or funcptr. %s found".format(handle.ty.repr))
    require(newType.isInstanceOf[AbstractPointerType] || newType.isInstanceOf[TypeInt], "can only convert to int, ptr or funcptr. %s found".format(newType.repr))

    val addr = handle.ty match {
      case TypeInt(n)             => OpHelper.trunc(handle.vb.asInstanceOf[BoxInt].value, 64).toLong
      case _: AbstractPointerType => handle.vb.asInstanceOf[BoxPointer].addr
    }

    val box = newType match {
      case TypeInt(n)             => new BoxInt(OpHelper.trunc(BigInt(addr), Math.min(n, 64)))
      case _: AbstractPointerType => new BoxPointer(addr)
    }

    newHandle(newType, box)
  }

  def pin(handle: Handle): Handle = {
    val (objTy, (objRef, offset)) = handle.ty match {
      case TypeRef(t)  => (t, (handle.vb.asInstanceOf[BoxRef].objRef, 0L))
      case TypeIRef(t) => (t, handle.vb.asInstanceOf[BoxIRef].oo)
    }
    pin(objRef)
    val ptrTy = InternalTypePool.ptrOf(objTy)
    val box = new BoxPointer(objRef + offset)
    newHandle(ptrTy, box)
  }

  def unpin(handle: Handle): Unit = {
    val (objTy, objRef) = handle.ty match {
      case TypeRef(t)  => (t, handle.vb.asInstanceOf[BoxRef].objRef)
      case TypeIRef(t) => (t, handle.vb.asInstanceOf[BoxIRef].objRef)
    }
    unpin(objRef)
  }

  def expose(func: Handle, callConv: Flag, cookie: Handle): Handle = {
    val TypeFuncRef(sig) = func.ty
    val f = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Attempt to expose NULL Mu function")
    }

    val c = cookie.vb.asInstanceOf[BoxInt].value.toLong

    val addr = microVM.nativeCallHelper.exposeFuncDynamic(f, c)
    newHandle(InternalTypePool.funcPtrOf(sig), BoxPointer(addr))
  }

  def unexpose(callConv: Flag, addr: Handle): Unit = {
    val a = addr.vb.asInstanceOf[BoxPointer].addr
    microVM.nativeCallHelper.unexposeFunc(a)
  }

  // Internal methods for the micro VM

  def putThread(thr: Option[InterpreterThread]): Handle = {
    val t = InternalTypes.THREAD
    val box = BoxThread(thr)
    newHandle(t, box)
  }

  def putStack(sta: Option[InterpreterStack]): Handle = {
    val t = InternalTypes.STACK
    val box = BoxStack(sta)
    newHandle(t, box)
  }

}