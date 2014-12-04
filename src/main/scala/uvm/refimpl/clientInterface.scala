package uvm.refimpl

import uvm.types._
import uvm.refimpl.itpr._
import java.io.Reader
import scala.collection.mutable.HashSet
import uvm.refimpl.mem.TypeSizes._
import uvm.ssavariables.MemoryOrder._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.refimpl.mem._

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
    val nb = uty match {
      case TypeInt(l) =>
        val bi: BigInt = l match {
          case 8 => MemorySupport.loadByte(iRef)
          case 16 => MemorySupport.loadShort(iRef)
          case 32 => MemorySupport.loadInt(iRef)
          case 64 => MemorySupport.loadLong(iRef)
          case _ => throw new UnimplementedOprationException("Loading int of length %d is not supported".format(l))
        }
        BoxInt(OpHelper.unprepare(bi, l))
      case _: TypeFloat =>
        val fv = MemorySupport.loadFloat(iRef)
        BoxFloat(fv)
      case _: TypeDouble =>
        val dv = MemorySupport.loadDouble(iRef)
        BoxDouble(dv)
      case _: TypeRef =>
        val addr = MemorySupport.loadLong(iRef)
        BoxRef(addr)
      case _: TypeIRef =>
        val base = MemorySupport.loadLong(iRef)
        val offset = MemorySupport.loadLong(iRef + WORD_SIZE_BYTES)
        BoxIRef(base, offset)
      case _: TypeFunc =>
        val fid = MemorySupport.loadLong(iRef).toInt
        val func = microVM.globalBundle.funcNs.get(fid)
        BoxFunc(func)
      case _: TypeThread =>
        val tid = MemorySupport.loadLong(iRef).toInt
        val thr = microVM.threadStackManager.getThreadByID(tid)
        BoxThread(thr)
      case _: TypeStack =>
        val sid = MemorySupport.loadLong(iRef).toInt
        val sta = microVM.threadStackManager.getStackByID(sid)
        BoxStack(sta)
      case _: TypeTagRef64 =>
        val raw = MemorySupport.loadLong(iRef)
        BoxTagRef64(raw)
      case TypeVector(ety, len) =>
        ety match {
          case TypeInt(32) =>
            val vs = for (i <- (0L until len)) yield MemorySupport.loadInt(iRef + i * 4L)
            BoxVector(vs.map(v => BoxInt(OpHelper.unprepare(v, 32))))
          case _: TypeFloat =>
            val vs = for (i <- (0L until len)) yield MemorySupport.loadFloat(iRef + i * 4L)
            BoxVector(vs.map(v => BoxFloat(v)))
          case _: TypeDouble =>
            val vs = for (i <- (0L until len)) yield MemorySupport.loadDouble(iRef + i * 8L)
            BoxVector(vs.map(v => BoxDouble(v)))
          case _ => throw new UnimplementedOprationException("Loading of vector type with element type %s is not supporing".format(ety.getClass.getName))
        }
      case _ => throw new UnimplementedOprationException("Loading of type %s is not supporing".format(uty.getClass.getName))
    }
    newHandle(uty, nb)
  }

  def store(ord: MemoryOrder, loc: Handle, newVal: Handle): Unit = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val nvb = newVal.vb
    uty match {
      case TypeInt(l) =>
        val bi = nvb.asInstanceOf[BoxInt].value
        l match {
          case 8 => MemorySupport.storeByte(iRef, bi.byteValue)
          case 16 => MemorySupport.storeShort(iRef, bi.shortValue)
          case 32 => MemorySupport.storeInt(iRef, bi.intValue)
          case 64 => MemorySupport.storeLong(iRef, bi.longValue)
          case _ => throw new UnimplementedOprationException("Storing int of length %d is not supported".format(l))
        }
        BoxInt(OpHelper.unprepare(bi, l))
      case _: TypeFloat =>
        val fv = nvb.asInstanceOf[BoxFloat].value
        MemorySupport.storeFloat(iRef, fv)
      case _: TypeDouble =>
        val dv = nvb.asInstanceOf[BoxDouble].value
        MemorySupport.storeDouble(iRef, dv)
      case _: TypeRef =>
        val addr = nvb.asInstanceOf[BoxRef].objRef
        MemorySupport.storeLong(iRef, addr)
      case _: TypeIRef =>
        val BoxIRef(base, offset) = nvb.asInstanceOf[BoxIRef]
        MemorySupport.storeLong(iRef, base)
        MemorySupport.storeLong(iRef + WORD_SIZE_BYTES, offset)
      case _: TypeFunc =>
        val fid = nvb.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0)
        MemorySupport.storeLong(iRef, fid.toLong & 0xFFFFFFFFL)
      case _: TypeThread =>
        val tid = nvb.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0)
        MemorySupport.storeLong(iRef, tid.toLong & 0xFFFFFFFFL)
      case _: TypeStack =>
        val sid = nvb.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0)
        MemorySupport.storeLong(iRef, sid.toLong & 0xFFFFFFFFL)
      case _: TypeTagRef64 =>
        val raw = nvb.asInstanceOf[BoxTagRef64].raw
        MemorySupport.storeLong(iRef, raw)
      case TypeVector(ety, len) =>
        val vbs = nvb.asInstanceOf[BoxVector].values
        ety match {
          case TypeInt(32) =>
            for (i <- (0L until len)) MemorySupport.storeInt(iRef + i * 4L, vbs(i.toInt).asInstanceOf[BoxInt].value.intValue)
          case _: TypeFloat =>
            for (i <- (0L until len)) MemorySupport.storeFloat(iRef + i * 4L, vbs(i.toInt).asInstanceOf[BoxFloat].value)
          case _: TypeDouble =>
            for (i <- (0L until len)) MemorySupport.storeDouble(iRef + i * 8L, vbs(i.toInt).asInstanceOf[BoxDouble].value)
          case _ => throw new UnimplementedOprationException("Storing of vector type with element type %s is not supporing".format(ety.getClass.getName))
        }
      case _ => throw new UnimplementedOprationException("Storing of type %s is not supporing".format(uty.getClass.getName))
    }
  }

  def cmpXchg(ordSucc: MemoryOrder, ordFail: MemoryOrder, weak: Boolean, loc: Handle, expected: Handle, desired: Handle): (Boolean, Handle) = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val eb = expected.vb
    val db = desired.vb
    uty match {
      case TypeInt(l) =>
        val ebi = eb.asInstanceOf[BoxInt].value
        val dbi = db.asInstanceOf[BoxInt].value
        val (succ, rbi) = l match {
          case 32 => {
            val (succ2, rv) = MemorySupport.cmpXchgInt(iRef, ebi.intValue, dbi.intValue)
            (succ2, BigInt(rv))
          }
          case 64 => {
            val (succ2, rv) = MemorySupport.cmpXchgLong(iRef, ebi.longValue, dbi.longValue)
            (succ2, BigInt(rv))
          }
          case _ => throw new UnimplementedOprationException("CmpXchg on int of length %d is not supported".format(l))
        }
        val rb = BoxInt(OpHelper.unprepare(rbi, l))
        val rh = newHandle(uty, rb)
        (succ, rh)
      case _: TypeRef =>
        val el = eb.asInstanceOf[BoxRef].objRef
        val dl = db.asInstanceOf[BoxRef].objRef
        val (succ, rl) = MemorySupport.cmpXchgLong(iRef, el, dl)
        val rh = newHandle(uty, BoxRef(rl))
        (succ, rh)
      case _: TypeIRef =>
        val BoxIRef(el, eh) = eb.asInstanceOf[BoxIRef]
        val BoxIRef(dl, dh) = db.asInstanceOf[BoxIRef]
        val (succ, (rl, rh)) = MemorySupport.cmpXchgI128(iRef, (el, eh), (dl, dh))
        val rhdl = newHandle(uty, BoxIRef(rl, rh))
        (succ, rhdl)
      case _: TypeFunc =>
        val el = eb.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(iRef, el, dl)
        val rf = microVM.globalBundle.funcNs.get(rl.toInt)
        val rh = newHandle(uty, BoxFunc(rf))
        (succ, rh)
      case _: TypeThread =>
        val el = eb.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(iRef, el, dl)
        val rt = microVM.threadStackManager.getThreadByID(rl.toInt)
        val rh = newHandle(uty, BoxThread(rt))
        (succ, rh)
      case _: TypeStack =>
        val el = eb.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
        val dl = db.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
        val (succ, rl) = MemorySupport.cmpXchgLong(iRef, el, dl)
        val rs = microVM.threadStackManager.getStackByID(rl.toInt)
        val rh = newHandle(uty, BoxStack(rs))
        (succ, rh)
      case _ => throw new UnimplementedOprationException("CmpXchg of type %s is not supporing".format(uty.getClass.getName))
    }
  }

  def atomicRMW(ord: MemoryOrder, op: AtomicRMWOptr, loc: Handle, opnd: Handle): Handle = {
    val ty = loc.ty.asInstanceOf[TypeIRef].ty
    val uty = InternalTypePool.unmarkedOf(ty)
    val lb = loc.vb.asInstanceOf[BoxIRef]
    val iRef = lb.objRef + lb.offset
    val ob = opnd.vb
    uty match {
      case TypeInt(l) =>
        val obi = ob.asInstanceOf[BoxInt].value
        val rbi: BigInt = l match {
          case 32 => {
            MemorySupport.atomicRMWInt(op, iRef, obi.intValue)
          }
          case 64 => {
            MemorySupport.atomicRMWLong(op, iRef, obi.longValue)
          }
          case _ => throw new UnimplementedOprationException("AtomicRMW on int of length %d is not supported".format(l))
        }
        val rb = BoxInt(OpHelper.unprepare(rbi, l))
        newHandle(uty, rb)
      case _ =>
        if (op != XCHG) {
          throw new UnimplementedOprationException("AtomicRMW operation other than XCHG only supports int. %s found.".format(uty.getClass.getName))
        } else {
          uty match {
            case _: TypeRef =>
              val ol = ob.asInstanceOf[BoxRef].objRef
              val rl = MemorySupport.atomicRMWLong(op, iRef, ol)
              newHandle(uty, BoxRef(rl))
            case _: TypeIRef =>
              val BoxIRef(ol, oh) = ob.asInstanceOf[BoxIRef]
              val (rl, rh) = MemorySupport.xchgI128(iRef, (ol, oh))
              newHandle(uty, BoxIRef(rl, rh))
            case _: TypeFunc =>
              val ol = ob.asInstanceOf[BoxFunc].func.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, iRef, ol)
              val rf = microVM.globalBundle.funcNs.get(rl.toInt)
              newHandle(uty, BoxFunc(rf))
            case _: TypeThread =>
              val ol = ob.asInstanceOf[BoxThread].thread.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, iRef, ol)
              val rt = microVM.threadStackManager.getThreadByID(rl.toInt)
              newHandle(uty, BoxThread(rt))
            case _: TypeStack =>
              val ol = ob.asInstanceOf[BoxStack].stack.map(_.id).getOrElse(0).toLong
              val rl = MemorySupport.atomicRMWLong(op, iRef, ol)
              val rs = microVM.threadStackManager.getStackByID(rl.toInt)
              newHandle(uty, BoxStack(rs))
            case _: TypeTagRef64 =>
              val ol = ob.asInstanceOf[BoxTagRef64].raw
              val rl = MemorySupport.atomicRMWLong(op, iRef, ol)
              newHandle(uty, BoxTagRef64(rl))
            case _ =>
              throw new UnimplementedOprationException("AtomicRMW XCHG of type %s is not supporing".format(uty.getClass.getName))
          }
        }
    }
  }

  def fence(ord: MemoryOrder): Unit = {
  }

  def newStack(func: Handle, args: Seq[Handle]): Handle = {
    throw new UvmRefImplException("Not Implemented")
  }

  def newThread(stack: Handle): Handle = {
    throw new UvmRefImplException("Not Implemented")
  }

  def killStack(stack: Handle): Unit = {
    throw new UvmRefImplException("Not Implemented")
  }

  def currentFuncVer(stack: Handle, frame: Int): Int = {
    throw new UvmRefImplException("Not Implemented")
  }

  def currentInstruction(stack: Handle, frame: Int): Int = {
    throw new UvmRefImplException("Not Implemented")
  }

  def dumpKeepalives(stack: Handle, frame: Int): Seq[Handle] = {
    throw new UvmRefImplException("Not Implemented")
  }

  def popFrame(stack: Handle): Unit = {
    throw new UvmRefImplException("Not Implemented")
  }

  def pushFrame = throw new UvmRefImplException("Not Implemented")

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

}