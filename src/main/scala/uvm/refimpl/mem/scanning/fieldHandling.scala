package uvm.refimpl.mem.scanning

import uvm.refimpl.itpr.HasObjRef
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.MemorySupport
import uvm.refimpl.itpr.OpHelper

/**
 * Handle reference fields or references in boxes.
 * <p>
 * Both fromBox and fromMem method return true if and only if the scanner should follow the reference.
 * <p>
 * The caller invokes the methods on all boxes/locations it finds. The Callee checks if the box/location
 * actually contain references or non-null references.
 */
trait RefFieldHandler {
  /** Scan a box. */
  def fromBox(box: HasObjRef): Boolean
  /** Scan a memory location. */
  def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Boolean
  /**
   * A reference from somewhere internal to the µVM.
   * For example, from the StackMemory to the memory byte array;
   * from a finaliser table to a finalisable object (to be added).
   */
  def fromInternal(toObj: Word): Boolean
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