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
import uvm.refimpl.itpr.{ HowToResume => ItprHowToResume }
import scala.collection.mutable.Buffer
import java.nio.charset.Charset
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

object MuValue {
  def apply(ty: Type, vb: ValueBox): MuValue = (ty, vb) match {
    case (t: TypeInt, v: BoxInt)                    => MuIntValue(t, v)
    case (t: TypeFloat, v: BoxFloat)                => MuFloatValue(t, v)
    case (t: TypeDouble, v: BoxDouble)              => MuDoubleValue(t, v)
    case (t: TypeRef, v: BoxRef)                    => MuRefValue(t, v)
    case (t: TypeIRef, v: BoxIRef)                  => MuIRefValue(t, v)
    case (t: TypeStruct, v: BoxSeq)                 => MuStructValue(t, v)
    case (t: TypeArray, v: BoxSeq)                  => MuArrayValue(t, v)
    case (t: TypeVector, v: BoxSeq)                 => MuVectorValue(t, v)
    case (t: TypeFuncRef, v: BoxFunc)               => MuFuncRefValue(t, v)
    case (t: TypeThreadRef, v: BoxThread)           => MuThreadRefValue(t, v)
    case (t: TypeStackRef, v: BoxStack)             => MuStackRefValue(t, v)
    case (t: TypeTagRef64, v: BoxTagRef64)          => MuTagRef64Value(t, v)
    case (t: TypeUPtr, v: BoxPointer)               => MuUPtrValue(t, v)
    case (t: TypeUFuncPtr, v: BoxPointer)           => MuUFPValue(t, v)
    case (t: TypeFrameCursorRef, v: BoxFrameCursor) => MuFCRefValue(t, v)
    case (t, v) => {
      throw new IllegalArgumentException("Improper type-box pair: %s,%s".format(t.toString, vb.getClass.getSimpleName))
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

  def showTy: String = "%s: %s".format(this.getClass.getSimpleName, ty)
}

abstract class MuSeqValue extends MuValue {
  def ty: AbstractSeqType
  def vb: BoxSeq
}

abstract class MuGenRefValue extends MuValue {
  def ty: AbstractGenRefType
}

case class MuIntValue(ty: TypeInt, vb: BoxInt) extends MuValue
case class MuFloatValue(ty: TypeFloat, vb: BoxFloat) extends MuValue
case class MuDoubleValue(ty: TypeDouble, vb: BoxDouble) extends MuValue
case class MuRefValue(ty: TypeRef, vb: BoxRef) extends MuGenRefValue
case class MuIRefValue(ty: TypeIRef, vb: BoxIRef) extends MuGenRefValue
case class MuStructValue(ty: TypeStruct, vb: BoxSeq) extends MuValue
case class MuArrayValue(ty: TypeArray, vb: BoxSeq) extends MuSeqValue
case class MuVectorValue(ty: TypeVector, vb: BoxSeq) extends MuSeqValue
case class MuFuncRefValue(ty: TypeFuncRef, vb: BoxFunc) extends MuGenRefValue
case class MuThreadRefValue(ty: TypeThreadRef, vb: BoxThread) extends MuGenRefValue
case class MuStackRefValue(ty: TypeStackRef, vb: BoxStack) extends MuGenRefValue
case class MuTagRef64Value(ty: TypeTagRef64, vb: BoxTagRef64) extends MuValue
case class MuUPtrValue(ty: TypeUPtr, vb: BoxPointer) extends MuValue
case class MuUFPValue(ty: TypeUFuncPtr, vb: BoxPointer) extends MuValue
case class MuFCRefValue(ty: TypeFrameCursorRef, vb: BoxFrameCursor) extends MuGenRefValue

abstract class TrapHandlerResult
object TrapHandlerResult {
  case class ThreadExit() extends TrapHandlerResult
  case class Rebind(newStack: MuStackRefValue, htr: HowToResume) extends TrapHandlerResult
}

abstract class HowToResume
object HowToResume {
  case class PassValues(values: Seq[MuValue]) extends HowToResume
  case class ThrowExc(exc: MuRefValue) extends HowToResume
}

trait TrapHandler {
  def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult
}

trait UndefinedFunctionHandler {
  def handleUndefinedFunction(functionID: Int): Unit
}

object MuCtx {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
  
  val US_ASCII = Charset.forName("US-ASCII")
}

/**
 * A client context. The main part of the API. It keeps thread-local states, including a set of handles. It provides
 * operations on the Mu VM.
 */
class MuCtx(_mutator: Mutator)(
    implicit microVM: MicroVM, memorySupport: MemorySupport) extends ObjectPinner {
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

  /** Create a Mu thread and bind it to a Mu stack. */
  def newThread(stack: MuStackRefValue, htr: HowToResume): MuThreadRefValue = {
    val sv = getStackNotNull(stack)
    val itprHtr = htr match {
      case HowToResume.PassValues(values) => ItprHowToResume.PassValues(values.map(_.vb))
      case HowToResume.ThrowExc(exc)      => ItprHowToResume.ThrowExc(exc.vb.objRef)
    }
    val thr = microVM.threadStackManager.newThread(sv, 0L, itprHtr) // TODO: FIXME!!

    val nb = BoxThread(Some(thr))
    addHandle(MuThreadRefValue(InternalTypes.THREADREF, nb))
  }

  /** Kill a Mu stack. */
  def killStack(stack: MuStackRefValue): Unit = {
    val sv = getStackNotNull(stack)

    sv.kill()
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
          case hkac: HasKeepAliveClause => {
            val kas = hkac.keepAlives
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
  def tr64IsFp(value: MuTagRef64Value): Boolean = {
    OpHelper.tr64IsFp(value.vb.raw)
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
  def tr64ToFp(value: MuTagRef64Value): MuDoubleValue = {
    val raw = value.vb.raw
    val box = new BoxDouble(OpHelper.tr64ToFp(raw))
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
  def tr64FromFp(value: MuDoubleValue): MuTagRef64Value = {
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

  def enableWatchpoint(wpID: Int): Unit = {
    microVM.trapManager.enableWatchPoint(wpID)
  }

  def disableWatchpoint(wpID: Int): Unit = {
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

object RichMuCtx {
  /**
   * Allow you to use the `val h = x << ctx.makeSomeHandle()` or `val h = x(ctx.makeSomeHandle())`
   *  syntax to dispose it later
   */
  class DelayedDisposer(garbageList: Buffer[MuValue]) {
    def apply[T <: MuValue](v: T): T = {
      garbageList += v
      v
    }
    def <<[T <: MuValue](v: T): T = {
      garbageList += v
      v
    }
  }

  /** Extensions to the MuCtx interface. Not officially part of the client API. */
  implicit class RichMuCtx(val ctx: MuCtx) extends AnyVal {
    def handleFromBoolean(b: Boolean) = ctx.handleFromInt(if (b) 1 else 0, 1)
    def handleFromInt1(num: BigInt) = ctx.handleFromInt(num, 1)
    def handleFromInt6(num: BigInt) = ctx.handleFromInt(num, 6)
    def handleFromInt8(num: BigInt) = ctx.handleFromInt(num, 8)
    def handleFromInt16(num: BigInt) = ctx.handleFromInt(num, 16)
    def handleFromInt32(num: BigInt) = ctx.handleFromInt(num, 32)
    def handleFromInt52(num: BigInt) = ctx.handleFromInt(num, 52)
    def handleFromInt64(num: BigInt) = ctx.handleFromInt(num, 64)
    def deleteValue(vs: MuValue*) = vs.foreach(ctx.deleteValue)

    def autoDispose[T](f: DelayedDisposer => T) = {
      val garbages = ArrayBuffer[MuValue]()
      val dd = new DelayedDisposer(garbages)
      val rv = f(dd)
      garbages.foreach(ctx.deleteValue)
      rv
    }

    def loadInt(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuIntValue]
    def loadFloat(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuFloatValue]
    def loadDouble(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuDoubleValue]
    def loadRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuRefValue]
    def loadIRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuIRefValue]
    def loadFuncRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuFuncRefValue]
    def loadThreadRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuThreadRefValue]
    def loadStackRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuStackRefValue]
    def loadTagRef64(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuTagRef64Value]
    def loadVector(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuVectorValue]

    def storeInt(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuIntValue]
    def storeFloat(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuFloatValue]
    def storeDouble(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuDoubleValue]
    def storeRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuRefValue]
    def storeIRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuIRefValue]
    def storeFuncRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuFuncRefValue]
    def storeThreadRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuThreadRefValue]
    def storeStackRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuStackRefValue]
    def storeTagRef64(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuTagRef64Value]
    def storeVector(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuVectorValue]

    def bytesToStr(bytes: MuRefValue): String = ctx.autoDispose { x =>
      val hIR = x << ctx.getIRef(bytes)
      val hFix = x << ctx.getFieldIRef(hIR, 0)
      val hLen = x << ctx.loadInt(NOT_ATOMIC, hFix)
      val len = ctx.handleToSInt(hLen).toLong
      val hVar = x << ctx.getVarPartIRef(hIR)

      val byteValues = for (i <- 0L until len) yield ctx.autoDispose { x =>
        val hI = x << ctx.handleFromInt64(i)
        val hVarI = x << ctx.shiftIRef(hVar, hI)
        val hByte = x << ctx.loadInt(NOT_ATOMIC, hVarI)
        val byte = ctx.handleToSInt(hByte).toByte
        byte
      }
      val bytesArray = byteValues.toArray
      val str = new String(bytesArray, MuCtx.US_ASCII)
      str
    }

    // legacy support

    /** Get the ID of the current function of a frame. Return 0 for native frames. */
    def curFunc(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curFunc(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /**
     * Get the ID of the current function version of a frame. Return 0 for native frames
     *  or Mu frames of undefined functions
     */
    def curFuncVer(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curFuncVer(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /**
     * Get the ID of the current instruction of a frame. Return 0 for native frames, Mu frames for undefined
     *  functions, or if the frame is just created by newStack or pushFrame.
     */
    def curInst(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curInst(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /** Dump keep-alive variables of the current instruction. */
    def dumpKeepalives(stack: MuStackRefValue, frame: Int): Seq[MuValue] = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val kas = ctx.dumpKeepalives(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      kas
    }
  }
}
