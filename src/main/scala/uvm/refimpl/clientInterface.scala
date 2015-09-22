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

case class Handle(ty: Type, vb: ValueBox)

abstract class TrapHandlerResult
case class TrapExit() extends TrapHandlerResult
case class TrapRebindPassValue(newStack: Handle, value: Handle) extends TrapHandlerResult
case class TrapRebindPassVoid(newStack: Handle) extends TrapHandlerResult
case class TrapRebindThrowExc(newStack: Handle, exc: Handle) extends TrapHandlerResult

trait TrapHandler {
  def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult
}

trait UndefinedFunctionHandler {
  def handleUndefinedFunction(functionID: Int): Unit
}

class ClientAgent(mutator: Mutator)(
    implicit microVM: MicroVM, memorySupport: MemorySupport) extends ObjectPinner {
  val handles = new HashSet[Handle]()

  val pinSet = new ArrayBuffer[Word]

  /**
   * Given a name, get the ID of an identified entity.
   */
  def idOf(name: String): Int = microVM.idOf(name)

  /**
   * Given an ID, get the name of an identified entity.
   */
  def nameOf(id: Int): String = microVM.nameOf(id)

  def close(): Unit = {
    handles.clear()
    mutator.close()
    microVM.clientAgents.remove(this)
  }

  def loadBundle(r: Reader): Unit = {
    val bundle = microVM.irReader.read(r, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  def loadBundle(s: String): Unit = {
    val bundle = microVM.irReader.read(s, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  private def newHandle(t: Type, vb: ValueBox): Handle = {
    val handle = Handle(t, vb)
    handles.add(handle)
    return handle
  }

  def putInt(typeID: Int, v: BigInt): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeInt]
    val preparedV = OpHelper.unprepare(v, t.length)
    newHandle(t, BoxInt(preparedV))
  }

  def putFloat(typeID: Int, v: Float): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeFloat]
    newHandle(t, BoxFloat(v))
  }

  def putDouble(typeID: Int, v: Double): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeDouble]
    newHandle(t, BoxDouble(v))
  }

  def putIntVec(typeID: Int, vs: Seq[BigInt]): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeVector]
    val et = t.elemTy.asInstanceOf[TypeInt]
    val preparedVs = for (v <- vs) yield OpHelper.trunc(v, et.length)
    newHandle(t, BoxVector(preparedVs.map(BoxInt)))
  }

  def putFloatVec(typeID: Int, vs: Seq[Float]): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeVector]
    val et = t.elemTy.asInstanceOf[TypeFloat]
    newHandle(t, BoxVector(vs.map(BoxFloat)))
  }

  def putDoubleVec(typeID: Int, vs: Seq[Double]): Handle = {
    val t = microVM.globalBundle.typeNs(typeID).asInstanceOf[TypeVector]
    val et = t.elemTy.asInstanceOf[TypeDouble]
    newHandle(t, BoxVector(vs.map(BoxDouble)))
  }

  def putPointer(typeID: Int, v: Word): Handle = {
    val t = microVM.globalBundle.typeNs(typeID)
    newHandle(t, BoxPointer(v))
  }

  def putConstant(id: Int): Handle = {
    val c = microVM.globalBundle.constantNs(id)
    val t = c.constTy
    val box = microVM.constantPool.getGlobalVarBox(c)
    newHandle(t, box)
  }

  def putGlobal(id: Int): Handle = {
    val g = microVM.globalBundle.globalCellNs(id)
    val t = InternalTypePool.irefOf(g.cellTy)
    val a = microVM.memoryManager.globalMemory.addrForGlobalCell(g)
    val box = BoxIRef(0L, a)
    newHandle(t, box)
  }

  def putFunction(id: Int): Handle = {
    val f = microVM.globalBundle.funcNs(id)
    val t = InternalTypePool.funcOf(f.sig)
    val box = BoxFunc(Some(f))
    newHandle(t, box)
  }

  def putExpFunc(id: Int): Handle = {
    val ef = microVM.globalBundle.expFuncNs(id)
    val t = InternalTypePool.funcPtrOf(ef.func.sig)
    val box = BoxPointer(ef.addr)
    newHandle(t, box)
  }

  def deleteHandle(h: Handle): Unit = {
    handles.remove(h)
  }

  def toInt(h: Handle, signExt: Boolean = false): BigInt = {
    val t = h.ty.asInstanceOf[TypeInt]
    val ib = h.vb.asInstanceOf[BoxInt]
    if (signExt) OpHelper.prepareSigned(ib.value, t.length) else OpHelper.prepareUnsigned(ib.value, t.length)
  }

  def toFloat(h: Handle): Float = {
    h.vb.asInstanceOf[BoxFloat].value
  }

  def toDouble(h: Handle): Double = {
    h.vb.asInstanceOf[BoxDouble].value
  }

  def toIntVec(h: Handle, signExt: Boolean = false): Seq[BigInt] = {
    val t = h.ty.asInstanceOf[TypeVector]
    val et = t.elemTy.asInstanceOf[TypeInt]
    val bv = h.vb.asInstanceOf[BoxVector]
    for (b <- bv.values) yield {
      val ib = b.asInstanceOf[BoxInt]
      if (signExt) OpHelper.prepareSigned(ib.value, et.length) else OpHelper.prepareUnsigned(ib.value, et.length)
    }
  }

  def toFloatVec(h: Handle): Seq[Float] = {
    h.vb.asInstanceOf[BoxVector].values.map(b => b.asInstanceOf[BoxFloat].value)
  }

  def toDoubleVec(h: Handle): Seq[Double] = {
    h.vb.asInstanceOf[BoxVector].values.map(b => b.asInstanceOf[BoxDouble].value)
  }

  def toPointer(h: Handle): Word = {
    h.vb.asInstanceOf[BoxPointer].addr
  }

  def extractValue(str: Handle, index: Int): Handle = {
    val st = str.ty.asInstanceOf[TypeStruct]
    val sb = str.vb.asInstanceOf[BoxStruct]
    val et = st.fieldTy(index)
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
    val ft = st.fieldTy(index)
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

  def getFixedPartIRef(handle: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val ht = t.ty.asInstanceOf[TypeHybrid]
    val ft = ht.fixedTy
    val nt = InternalTypePool.irefOf(ft)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val nb = ob
    newHandle(nt, nb)
  }

  def getVarPartIRef(handle: Handle): Handle = {
    val t = handle.ty.asInstanceOf[TypeIRef]
    val ht = t.ty.asInstanceOf[TypeHybrid]
    val ft = ht.fixedTy
    val vt = ht.varTy
    val nt = InternalTypePool.irefOf(vt)
    val ob = handle.vb.asInstanceOf[BoxIRef]
    val nb = BoxIRef(ob.objRef, ob.offset + TypeSizes.varPartOffsetOf(ht))
    newHandle(nt, nb)
  }

  def load(ord: MemoryOrder, loc: Handle): Handle = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypePtr(t)  => (true, t)
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
      case TypePtr(t)  => (true, t)
    }
    val uty = InternalTypePool.unmarkedOf(ty)
    val addr = MemoryOperations.addressOf(ptr, loc.vb)
    val nvb = newVal.vb
    val nb = ValueBox.makeBoxForType(uty)

    MemoryOperations.store(ptr, uty, addr, nvb, nb)
  }

  def cmpXchg(ordSucc: MemoryOrder, ordFail: MemoryOrder, weak: Boolean, loc: Handle, expected: Handle, desired: Handle): (Boolean, Handle) = {
    val (ptr, ty) = loc.ty match {
      case TypeIRef(t) => (false, t)
      case TypePtr(t)  => (true, t)
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
      case TypePtr(t)  => (true, t)
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

  def newStack(func: Handle, args: Seq[Handle]): Handle = {
    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    val funcVer = funcVal.versions.headOption.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function %s is not defined.".format(funcVal.repr))
    }

    val argBoxes = args.map(_.vb)

    val sta = microVM.threadStackManager.newStack(funcVer, argBoxes, mutator)

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
      case f: NativeFrame => 0
      case f: MuFrame     => f.funcVer.id
    }
  }

  def currentInstruction(stack: Handle, frame: Int): Int = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr match {
      case f: NativeFrame => 0
      case f: MuFrame     => f.curInst.id
    }
  }

  def dumpKeepalives(stack: Handle, frame: Int): Seq[Handle] = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr match {
      case f: NativeFrame => {
        throw new UvmRefImplException("Attempt to dump keepalive variables for a native frame for native funciton 0x%x".format(f.func))
      }
      case f: MuFrame => {
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

  def pushFrame(stack: Handle, func: Handle, argList: Seq[Handle]): Unit = {
    val sta = stack.vb.asInstanceOf[BoxStack].stack.getOrElse {
      throw new UvmRuntimeException("Stack must not be NULL")
    }

    val funcVal = func.vb.asInstanceOf[BoxFunc].func.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function must not be NULL")
    }

    val funcVer = funcVal.versions.headOption.getOrElse {
      throw new UvmRuntimeException("Stack-bottom function %s is not defined.".format(funcVal.repr))
    }

    val argBoxes = argList.map(_.vb)

    sta.pushMuFrame(funcVer, argBoxes)
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