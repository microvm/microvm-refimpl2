package uvm.refimpl

import uvm.types._
import uvm.refimpl.itpr._
import java.io.Reader
import scala.collection.mutable.HashSet

case class Handle(ty: Type, vb: ValueBox)

class ClientAgent(microVM: MicroVM) {
  val handles = new HashSet[Handle]()

  microVM.addClientAgent(this)

  /** Help the Client look up the ID of a name */
  def idOf(name: String): Int = {
    microVM.globalBundle.allNs(name).id
  }

  /** Help the Client look up the optional name of an ID */
  def nameOf(id: Int): Option[String] = {
    microVM.globalBundle.allNs(id).name
  }

  def close(): Unit = {
    handles.clear()
    microVM.removeClientAgent(this)
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
    val preparedV = OpHelper.trunc(v, t.length)
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
}
