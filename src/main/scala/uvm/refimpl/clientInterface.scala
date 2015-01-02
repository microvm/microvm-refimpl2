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

class ClientAgent(microVM: MicroVM) {
  val handles = new HashSet[Handle]()

  microVM.clientAgents.add(this)

  val mutator = microVM.memoryManager.heap.makeMutator()

  def close(): Unit = {
    handles.clear()
    mutator.close()
    microVM.clientAgents.remove(this)
  }

  def loadBundle(r: Reader): Unit = {
    val bundle = microVM.irReader.read(r, microVM.globalBundle)
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
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val b = loc.vb.asInstanceOf[BoxIRef]
    val iRef = b.objRef + b.offset
    val nb = ValueBox.makeBoxForType(uty) 
    
    MemoryOperations.load(uty, iRef, nb, microVM)

    newHandle(uty, nb)
  }

  def store(ord: MemoryOrder, loc: Handle, newVal: Handle): Unit = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val nvb = newVal.vb    
    val nb = ValueBox.makeBoxForType(uty)

    MemoryOperations.store(uty, iRef, nvb, nb, microVM)
  }

  def cmpXchg(ordSucc: MemoryOrder, ordFail: MemoryOrder, weak: Boolean, loc: Handle, expected: Handle, desired: Handle): (Boolean, Handle) = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val eb = expected.vb
    val db = desired.vb
    val br = ValueBox.makeBoxForType(uty)
    val succ = MemoryOperations.cmpXchg(uty, iRef, eb, db, br, microVM)
    (succ, newHandle(uty, br))
  }

  def atomicRMW(ord: MemoryOrder, op: AtomicRMWOptr, loc: Handle, opnd: Handle): Handle = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val ob = opnd.vb
    val br = ValueBox.makeBoxForType(uty)
    MemoryOperations.atomicRMW(uty, op, iRef, ob, br, microVM)
    newHandle(uty, br)
  }

  def fence(ord: MemoryOrder): Unit = {
  }

  def newStack(func: Handle, args: Seq[Handle]): Handle = {
    val fv = func.vb.asInstanceOf[BoxFunc].func match {
      case None    => throw new UvmRuntimeException("Stack-bottom function must not be NULL")
      case Some(v) => v
    }

    val argBoxes = args.map(_.vb)

    val sta = microVM.threadStackManager.newStack(fv, argBoxes, mutator)

    val nb = BoxStack(Some(sta))
    newHandle(InternalTypes.STACK, nb)
  }

  private def getStackNotNull(stack: Handle): InterpreterStack = {
    stack.vb.asInstanceOf[BoxStack].stack match {
      case None    => throw new UvmRuntimeException("Stack argument cannot be a NULL MicroVM stack value.")
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

    sv.state = StackState.Dead
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
    fr.funcVer.id
  }

  def currentInstruction(stack: Handle, frame: Int): Int = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    fr.curInst.id
  }

  def dumpKeepalives(stack: Handle, frame: Int): Seq[Handle] = {
    val sv = getStackNotNull(stack)
    val fr = nthFrame(sv, frame)
    val i = fr.curInst
    i match {
      case hkac: HasKeepAliveClause => {
        val kas = hkac.keepAlives
        for (ka <- kas) yield {
          val box = fr.boxes(ka)
          val ty = TypeInferer.inferType(ka)
          newHandle(ty, box)
        }
      }
      case _ => {
        throw new UvmRuntimeException("The current instruction %s does not have keep-alive clause.".format(i.repr))
      }
    }
  }

  def popFrame(stack: Handle): Unit = {
    val st = getStackNotNull(stack)
    val top = st.top
    top.prev match {
      case None => throw new UvmRuntimeException("Attempting to pop the last frame of a stack.")
      case Some(prev) => st.top = prev
    }
  }

  def pushFrame = throw new UvmRefImplException("Not defined in spec")

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

  // Internal methods for µVM

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