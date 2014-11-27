package uvm.refimpl.mem.scanning

import uvm.refimpl.itpr.HasObjRef
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.MemorySupport
import uvm.refimpl.itpr.OpHelper

trait RefFieldHandler {
  /** Scan a box. Return true if the GC should follow this reference. */
  def fromBox(box: HasObjRef): Boolean
  /** Scan a memory location. Return true if the GC should follow this reference. */
  def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Boolean
}

object RefFieldUpdater {
  def updateBox(box: HasObjRef, newObjRef: Word): Unit = box.setObjRef(newObjRef)
  def updateMemory(iRef: Word, isTR64: Boolean, newObjRef: Word): Unit = {
    if (isTR64) {
      val oldRaw = MemorySupport.loadLong(iRef)
      val oldTag = OpHelper.tr64ToTag(oldRaw)
      val newRaw = OpHelper.refToTr64(newObjRef, oldTag)
      MemorySupport.storeLong(iRef, newRaw)
    } else {
      MemorySupport.storeLong(iRef, newObjRef)
    }
  }
}