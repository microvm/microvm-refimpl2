package uvm.refimpl

import java.io.Reader
import java.nio.charset.Charset

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

import org.slf4j.LoggerFactory

import com.typesafe.scalalogging.Logger

import uvm._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes._
import uvm.ssavariables._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.HasKeepaliveClause
import uvm.ssavariables.MemoryOrder._
import uvm.types._

object MuCtx {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val US_ASCII = Charset.forName("US-ASCII")
}

/**
 * A client context. The main part of the API. It keeps thread-local states, including a set of handles. It provides
 * operations on the Mu VM.
 */
class MuCtx(val ctxID: Int, _mutator: Mutator)(
    implicit protected val microVM: MicroVM, memorySupport: MemorySupport) extends ObjectPinner with MuCtxIRBuilderPart {
  import MuCtx._

  implicit def mutator = _mutator

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
    microVM.contexts.remove(this)
  }

  /** Load a Mu IR bundle */
  def loadBundle(r: Reader): Unit = {
    MetaOperations.loadBundle(r)
  }

  /** Load a Mu IR bundle */
  def loadBundle(s: String): Unit = {
    MetaOperations.loadBundle(s)
  }

  /** Load a HAIL script */
  def loadHail(r: Reader): Unit = {
    MetaOperations.loadHail(r)
  }

  /** Load a HAIL script */
  def loadHail(s: String): Unit = {
    MetaOperations.loadHail(s)
  }

  protected def addHandle[T <: MuValue](h: T): T = {
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
  def refEq(lhs: MuGenRefValue, rhs: MuGenRefValue): Boolean = (lhs, rhs) match {
    case (l: MuRefValue, r: MuRefValue)             => l.vb.objRef == r.vb.objRef
    case (l: MuIRefValue, r: MuIRefValue)           => l.vb.oo == r.vb.oo
    case (l: MuFuncRefValue, r: MuFuncRefValue)     => l.vb.func == r.vb.func
    case (l: MuThreadRefValue, r: MuThreadRefValue) => l.vb.thread == r.vb.thread
    case (l: MuStackRefValue, r: MuStackRefValue)   => l.vb.stack == r.vb.stack
    case (l: MuFCRefValue, r: MuFCRefValue)         => l.vb.cursor == r.vb.cursor
    case (l, r) => {
      throw new IllegalArgumentException("Bad types for refEq: %s and %s".format(
        l.showTy, r.showTy))
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

  /** Get the value of a field of a struct value. */
  def extractValue(str: MuStructValue, index: Int): MuValue = {
    val st = str.ty
    val sb = str.vb
    val ft = st.fieldTys(index)
    val fb = sb.values(index)
    addHandle(MuValue(ft, fb))
  }

  /** Copy a struct value, replacing one of its fields. */
  def insertValue(str: MuStructValue, index: Int, newval: MuValue): MuStructValue = {
    val st = str.ty
    val sb = str.vb
    val nsb = BoxSeq(for ((b, i) <- sb.values.zipWithIndex) yield if (i == index) newval.vb else b)
    addHandle(MuStructValue(st, nsb))
  }

  /** Get the value of an element of an array or value. */
  def extractElement[T <: MuSeqValue](seq: T, index: MuIntValue): MuValue = {
    val indexVal = handleToUInt(index).toInt

    val et = seq.ty.elemTy
    val eb = seq.vb.values(indexVal)
    addHandle(MuValue(et, eb))
  }

  /** Copy an array or vector value, replacing one of its elements. */
  def insertElement[T <: MuSeqValue](seq: T, index: MuIntValue, newval: MuValue): T = {
    val indexVal = handleToUInt(index).toInt

    val st = seq.ty
    val sb = seq.vb
    val nsb = BoxSeq(for ((b, i) <- sb.values.zipWithIndex) yield if (i == indexVal) newval.vb else b)
    addHandle(MuValue(st, nsb).asInstanceOf[T])
  }

  /** Allocate a fixed object in the heap. */
  def newFixed(muType: Int): MuRefValue = {
    val t = microVM.globalBundle.typeNs(muType)
    val objRef = mutator.newScalar(t)
    val b = BoxRef(objRef)
    val rt = InternalTypePool.refOf(t)
    addHandle(MuRefValue(rt, b))
  }

  /** Allocate a hybrid object in the heap. */
  def newHybrid(tid: Int, length: MuIntValue): MuRefValue = {
    val t = microVM.globalBundle.typeNs(tid).asInstanceOf[TypeHybrid]
    val len = handleToUInt(length).longValue
    val objRef = mutator.newHybrid(t, len)
    val b = BoxRef(objRef)
    val rt = InternalTypePool.refOf(t)
    addHandle(MuRefValue(rt, b))
  }

  /** Cast between two refs, two irefs or two funcrefs */
  def refcast[T <: MuGenRefValue](opnd: T, newType: Int): T = {
    val nt = microVM.globalBundle.typeNs(newType)

    val nh = (opnd, nt) match {
      case (MuRefValue(ty, vb), ty2 @ TypeRef(_))         => MuRefValue(ty2, vb)
      case (MuIRefValue(ty, vb), ty2 @ TypeIRef(_))       => MuIRefValue(ty2, vb)
      case (MuFuncRefValue(ty, vb), ty2 @ TypeFuncRef(_)) => MuFuncRefValue(ty2, vb)
      case _ => {
        throw new IllegalArgumentException("Bad types for refcast: opnd:%s, newType:%s".format(
          opnd.showTy, nt.repr))
      }
    }

    addHandle(nh.asInstanceOf[T])
  }

  /** Convert ref to iref */
  def getIRef(opnd: MuRefValue): MuIRefValue = {
    val nt = InternalTypePool.irefOf(opnd.ty.ty)
    val nb = BoxIRef(opnd.vb.objRef, 0L)
    addHandle(MuIRefValue(nt, nb))
  }

  /** Get the iref to a field of a struct or hybrid location */
  def getFieldIRef(opnd: MuIRefValue, field: Int): MuIRefValue = {
    val st = try {
      opnd.ty.ty.asInstanceOf[AbstractStructType]
    } catch {
      case e: ClassCastException => throw new IllegalArgumentException(
        "opnd must refer to a struct or hybrid location. Actual referenct type: %s".format(opnd.ty.ty.repr), e)
    }

    val ft = st.fieldTys(field)
    val nt = InternalTypePool.irefOf(ft)
    val ob = opnd.vb
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.fieldOffsetOf(st, field))
    addHandle(MuIRefValue(nt, nb))
  }

  /** Get the iref to an element of an array or vector location. */
  def getElemIRef(opnd: MuIRefValue, index: MuIntValue): MuIRefValue = {
    val st = try {
      opnd.ty.ty.asInstanceOf[AbstractSeqType]
    } catch {
      case e: ClassCastException => throw new IllegalArgumentException(
        "opnd must refer to an array or vector location. Actual referenct type: %s".format(opnd.ty.ty.repr), e)
    }
    val i = handleToUInt(index).longValue
    val et = st.elemTy
    val nt = InternalTypePool.irefOf(et)
    val ob = opnd.vb
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.elemOffsetOf(st, i))
    addHandle(MuIRefValue(nt, nb))
  }

  /** Shift an iref to an array element by offset elements. */
  def shiftIRef(opnd: MuIRefValue, offset: MuIntValue): MuIRefValue = {
    val ot = opnd.ty
    val ob = opnd.vb
    val rt = ot.ty
    val nt = ot
    val off = handleToSInt(offset).longValue
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.shiftOffsetOf(rt, off))
    addHandle(MuIRefValue(nt, nb))
  }

  /** Get the iref to the 0-th element of the variable part of a hybrid location. */
  def getVarPartIRef(opnd: MuIRefValue): MuIRefValue = {
    val ht = try {
      opnd.ty.ty.asInstanceOf[TypeHybrid]
    } catch {
      case e: ClassCastException => throw new IllegalArgumentException(
        "opnd must refer to a hybrid location. Actual referenct type: %s".format(opnd.ty.ty.repr), e)
    }
    val vt = ht.varTy
    val nt = InternalTypePool.irefOf(vt)
    val ob = opnd.vb
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.varPartOffsetOf(ht))
    addHandle(MuIRefValue(nt, nb))
  }

  /** Load from a location. */
  def load(ord: MemoryOrder, loc: MuIRefValue): MuValue = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
    }

    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val nb = ValueBox.makeBoxForType(uty)

    MemoryOperations.load(ptr, uty, addr, nb)

    addHandle(MuValue(uty, nb))
  }

  /** Store to a location. */
  def store(ord: MemoryOrder, loc: MuIRefValue, newVal: MuValue): Unit = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val nvb = newVal.vb

    MemoryOperations.store(ptr, uty, addr, nvb)
  }

  /** Perform compare exchange on a location. */
  def cmpXchg(ordSucc: MemoryOrder, ordFail: MemoryOrder, weak: Boolean,
    loc: MuIRefValue, expected: MuValue, desired: MuValue): (MuValue, Boolean) = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val eb = expected.vb
    val db = desired.vb
    val br = ValueBox.makeBoxForType(uty)
    val succ = MemoryOperations.cmpXchg(ptr, uty, addr, eb, db, br)
    (addHandle(MuValue(uty, br)), succ)
  }

  /** Perform an atomic read-modify-write operation on a location. */
  def atomicRMW(ord: MemoryOrder, op: AtomicRMWOptr, loc: MuIRefValue, opnd: MuValue): MuValue = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val ob = opnd.vb
    val br = ValueBox.makeBoxForType(uty)
    MemoryOperations.atomicRMW(ptr, uty, op, addr, ob, br)
    addHandle(MuValue(uty, br))
  }

  /** Memory fence */
  def fence(ord: MemoryOrder): Unit = {
    // No-op
  }

  /** Create a Mu stack with fa stack-bottom function */
  def newStack(func: MuFuncRefValue): MuStackRefValue = {
    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    val sta = microVM.threadStackManager.newStack(funcVal, mutator)

    val nb = BoxStack(Some(sta))
    addHandle(MuStackRefValue(InternalTypes.STACKREF, nb))
  }

  private def getStackNotNull(stack: MuStackRefValue): InterpreterStack = {
    stack.vb.stack.getOrElse {
      throw new UvmRuntimeException("Stack argument cannot be a NULL stackref value.")
    }
  }

  private def getThreadNotNull(thread: MuThreadRefValue): InterpreterThread = {
    thread.vb.thread.getOrElse {
      throw new UvmRuntimeException("Thread argument cannot be a NULL threadref value.")
    }
  }

  /** Create a Mu thread and bind it to a Mu stack. */
  def newThread(stack: MuStackRefValue, threadLocal: Option[MuRefValue], htr: HowToResume): MuThreadRefValue = {
    val sv = getStackNotNull(stack)
    val itprHtr = htr match {
      case HowToResume.PassValues(values) => itpr.HowToResume.PassValues(values.map(_.vb))
      case HowToResume.ThrowExc(exc)      => itpr.HowToResume.ThrowExc(exc.vb.objRef)
    }

    val threadLocalAddr = threadLocal.map(tl => tl.vb.objRef).getOrElse(0L)
    val thr = microVM.threadStackManager.newThread(sv, threadLocalAddr, itprHtr)

    val nb = BoxThread(Some(thr))
    addHandle(MuThreadRefValue(InternalTypes.THREADREF, nb))
  }

  /** Kill a Mu stack. */
  def killStack(stack: MuStackRefValue): Unit = {
    val sv = getStackNotNull(stack)

    sv.kill()
  }

  /** Set the thread-local object reference */
  def setThreadlocal(thread: MuThreadRefValue, threadLocal: MuRefValue): Unit = {
    val th = getThreadNotNull(thread)
    val threadLocalAddr = threadLocal.vb.objRef

    th.threadLocal.objRef = threadLocalAddr
  }

  /** Get the thread-local object reference */
  def getThreadlocal(thread: MuThreadRefValue): MuRefValue = {
    val th = getThreadNotNull(thread)
    val threadLocalAddr = th.threadLocal.objRef

    val nb = BoxRef(threadLocalAddr)
    addHandle(MuRefValue(InternalTypes.REF_VOID, nb))
  }

  private def getCursorNotNull(cursor: MuFCRefValue): FrameCursor = {
    cursor.vb.cursor.getOrElse {
      throw new UvmRuntimeException("Frame cursor argument cannot be a NULL framecursorref value.")
    }
  }

  /** Create a frame cursor. */
  def newCursor(stack: MuStackRefValue): MuFCRefValue = {
    val s = getStackNotNull(stack)
    val c = microVM.threadStackManager.newFrameCursor(s)
    val nb = BoxFrameCursor(Some(c))
    addHandle(MuFCRefValue(InternalTypes.FRAMECURSORREF, nb))
  }

  /** Move cursor to the frame below the current frame. */
  def nextFrame(cursor: MuFCRefValue): Unit = {
    getCursorNotNull(cursor).nextFrame()
  }

  /** Copy a frame cursor. */
  def copyCursor(cursor: MuFCRefValue): MuFCRefValue = {
    val c = getCursorNotNull(cursor)
    val nc = microVM.threadStackManager.copyCursor(c)
    val nb = BoxFrameCursor(Some(nc))
    addHandle(MuFCRefValue(InternalTypes.FRAMECURSORREF, nb))
  }

  /** Close a frame cursor. */
  def closeCursor(cursor: MuFCRefValue): Unit = {
    val c = getCursorNotNull(cursor)
    microVM.threadStackManager.closeCursor(c)
  }

  /** Get the ID of the current function of a frame. Return 0 for native frames. */
  def curFunc(cursor: MuFCRefValue): Int = {
    val c = getCursorNotNull(cursor)
    c.frame.curFuncID
  }

  /**
   * Get the ID of the current function version of a frame. Return 0 for native frames
   *  or Mu frames of undefined functions
   */
  def curFuncVer(cursor: MuFCRefValue): Int = {
    val c = getCursorNotNull(cursor)
    c.frame.curFuncVerID
  }

  /**
   * Get the ID of the current instruction of a frame. Return 0 for native frames, Mu frames for undefined
   *  functions, or if the frame is just created by newStack or pushFrame.
   */
  def curInst(cursor: MuFCRefValue): Int = {
    val c = getCursorNotNull(cursor)
    c.frame.curInstID
  }

  /** Dump keep-alive variables of the current instruction. */
  def dumpKeepalives(cursor: MuFCRefValue): Seq[MuValue] = {
    val c = getCursorNotNull(cursor)
    val fr = c.frame
    fr match {
      case f: NativeFrame => {
        throw new UvmRefImplException("Attempt to dump keepalives from a native frame. Funciton 0x%x".format(f.func))
      }
      case f: UndefinedMuFrame => {
        for ((ty, box) <- f.func.sig.paramTys zip f.boxes) yield {
          addHandle(MuValue(ty, box))
        }
      }
      case f: DefinedMuFrame => {
        val i = f.curInst
        i match {
          case hkac: HasKeepaliveClause => {
            val kas = hkac.keepalives
            for (ka <- kas) yield {
              val box = f.boxes(ka)
              val ty = TypeInferer.inferType(ka)
              addHandle(MuValue(ty, box))
            }
          }
          case _ => {
            throw new UvmRuntimeException("The current instruction %s does not have keep-alive clause.".format(i.repr))
          }
        }
      }
    }
  }

  /** Pop the top frame of a Mu stack. */
  def popFramesTo(cursor: MuFCRefValue): Unit = {
    val c = getCursorNotNull(cursor)

    c.stack.popFramesTo(c)
  }

  /** Create a new frame for a Mu function and push it to the top of a stack. */
  def pushFrame(stack: MuStackRefValue, func: MuFuncRefValue): Unit = {
    val st = getStackNotNull(stack)

    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    st.pushFrame(funcVal)
  }

  /** Test if a tagref64 holds a double. */
  def tr64IsFP(value: MuTagRef64Value): Boolean = {
    OpHelper.tr64IsFP(value.vb.raw)
  }

  /** Test if a tagref64 holds an int. */
  def tr64IsInt(value: MuTagRef64Value): Boolean = {
    OpHelper.tr64IsInt(value.vb.raw)
  }

  /** Test if a tagref64 holds a ref. */
  def tr64IsRef(value: MuTagRef64Value): Boolean = {
    OpHelper.tr64IsRef(value.vb.raw)
  }

  /** Extract the double held by a tagref64. */
  def tr64ToFP(value: MuTagRef64Value): MuDoubleValue = {
    val raw = value.vb.raw
    val box = new BoxDouble(OpHelper.tr64ToFP(raw))
    addHandle(MuDoubleValue(InternalTypes.DOUBLE, box))
  }

  /** Extract the int held by a tagref64. */
  def tr64ToInt(value: MuTagRef64Value): MuIntValue = {
    val raw = value.vb.raw
    val box = new BoxInt(OpHelper.tr64ToInt(raw))
    addHandle(MuIntValue(InternalTypes.I52, box))
  }

  /** Extract the object ref held by a tagref64. */
  def tr64ToRef(value: MuTagRef64Value): MuRefValue = {
    val raw = value.vb.raw
    val box = new BoxRef(OpHelper.tr64ToRef(raw))
    addHandle(MuRefValue(InternalTypes.REF_VOID, box))
  }

  /** Extract the tag held by a tagref64. */
  def tr64ToTag(value: MuTagRef64Value): MuIntValue = {
    val raw = value.vb.raw
    val box = new BoxInt(OpHelper.tr64ToTag(raw))
    addHandle(MuIntValue(InternalTypes.I6, box))
  }

  /** Convert a double to a tagref64. */
  def tr64FromFP(value: MuDoubleValue): MuTagRef64Value = {
    val fp = value.vb.value
    val box = new BoxTagRef64(OpHelper.fpToTr64(fp))
    addHandle(MuTagRef64Value(InternalTypes.TAGREF64, box))
  }

  /** Convert an int to a tagref64. */
  def tr64FromInt(value: MuIntValue): MuTagRef64Value = {
    if (value.ty.length != 52) throw new IllegalArgumentException("Expect int<52>, found %s".format(value.ty.repr))
    val i = value.vb.value
    val box = new BoxTagRef64(OpHelper.intToTr64(i.longValue))
    addHandle(MuTagRef64Value(InternalTypes.TAGREF64, box))
  }

  /** Convert an object ref and a tag to a tagref64. */
  def tr64FromRef(ref: MuRefValue, tag: MuIntValue): MuTagRef64Value = {
    if (tag.ty.length != 6) throw new IllegalArgumentException("Expect int<6> tag, found %s".format(tag.ty.repr))
    val refv = ref.vb.objRef
    val tagv = tag.vb.value
    val box = new BoxTagRef64(OpHelper.refToTr64(refv, tagv.longValue))
    addHandle(MuTagRef64Value(InternalTypes.TAGREF64, box))
  }

  def enableWatchPoint(wpID: Int): Unit = {
    microVM.trapManager.enableWatchPoint(wpID)
  }

  def disableWatchPoint(wpID: Int): Unit = {
    microVM.trapManager.disableWatchPoint(wpID)
  }

  def pin(loc: MuValue): MuUPtrValue = {
    val (objTy, (objRef, offset)) = loc match {
      case MuRefValue(t, vb)  => (t, (vb.objRef, 0L))
      case MuIRefValue(t, vb) => (t, vb.oo)
      case _ => {
        throw new IllegalArgumentException("loc must be ref or iref. Found %s".format(loc.ty))
      }
    }
    pin(objRef)
    val ptrTy = InternalTypePool.ptrOf(objTy)
    val box = new BoxPointer(objRef + offset)
    addHandle(MuUPtrValue(ptrTy, box))
  }

  def unpin(loc: MuValue): Unit = {
    val (objTy, objRef) = loc match {
      case MuRefValue(t, vb)  => (t, vb.objRef)
      case MuIRefValue(t, vb) => (t, vb.objRef)
      case _ => {
        throw new IllegalArgumentException("loc must be ref or iref. Found %s".format(loc.ty))
      }
    }
    unpin(objRef)
  }

  def expose(func: MuFuncRefValue, callConv: Flag, cookie: MuIntValue): MuUFPValue = {
    val TypeFuncRef(sig) = func.ty
    val f = func.vb.func.getOrElse {
      throw new UvmRuntimeException("Attempt to expose NULL Mu function")
    }

    val c = cookie.vb.value.toLong

    val addr = microVM.nativeCallHelper.exposeFuncDynamic(f, c)
    addHandle(MuUFPValue(InternalTypePool.funcPtrOf(sig), BoxPointer(addr)))
  }

  def unexpose(callConv: Flag, addr: MuUFPValue): Unit = {
    val a = addr.vb.addr
    microVM.nativeCallHelper.unexposeFunc(a)
  }

  // Internal methods for the micro VM

  def handleFromInterpreterThread(thr: Option[InterpreterThread]): MuThreadRefValue = {
    val t = InternalTypes.THREADREF
    val box = BoxThread(thr)
    addHandle(MuThreadRefValue(t, box))
  }

  def handleFromInterpreterStack(sta: Option[InterpreterStack]): MuStackRefValue = {
    val s = InternalTypes.STACKREF
    val box = BoxStack(sta)
    addHandle(MuStackRefValue(s, box))
  }

  def handleFromAddr(addr: Long): MuRefValue = {
    val t = InternalTypes.REF_VOID
    val box = BoxRef(addr)
    addHandle(MuRefValue(t, box))
  }
}

