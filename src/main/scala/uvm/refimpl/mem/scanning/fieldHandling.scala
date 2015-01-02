package uvm.refimpl.mem.scanning

import uvm.refimpl.itpr.HasObjRef
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.MemorySupport
import uvm.refimpl.itpr.OpHelper

/**
 * Handle reference fields or references in boxes.
 * <p>
 * Both fromBox and fromMem method return Some(addr) if addr is to be enqueued by the scanner, or None otherwise. If an
 * object is moved when scanning an object, the returned addr must be the new address.
 * <p>
 * The caller invokes the methods on all boxes/locations it finds. The Callee checks if the box/location
 * actually contain references or non-null references.
 */
trait RefFieldHandler {
  /** Scan a box. */
  def fromBox(box: HasObjRef): Option[Word]
  /** Scan a memory location. */
  def fromMem(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word]
  /**
   * A reference from somewhere internal to the µVM.
   * For example, from the StackMemory to the memory byte array;
   * from a finaliser table to a finalisable object (to be added).
   */
  def fromInternal(toObj: Word): Option[Word]
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