package uvm.refimpl.mem.scanning

import uvm.refimpl.itpr.HasObjRef
import uvm.refimpl.itpr.BoxStack
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.MemorySupport
import uvm.refimpl.itpr.OpHelper
import uvm.refimpl.itpr.InterpreterThread
import uvm.refimpl.itpr.InterpreterStack
import uvm.refimpl.UvmRefImplException

/**
 * Handle references in the memory, value boxes or other Micro VM structures such as threads and stacks.
 * <ul>
 * <li>The caller invokes the methods on all boxes/locations it finds, no matter whether it is NULL or not.</li>
 * <li>The callee checks if the box/location actually contain non-null references.</li>
 * </ul>
 * <p>
 * The return value of the methods will be queued for recursive traversing. They should be the old value,
 * the updated value by the copying GC, or None if the reference should not be followed.
 */
trait RefFieldHandler {
  /** A stack value box referring to a heap object. */
  def boxToHeap(box: HasObjRef): Option[Word]
  /** A stack value box referring to a "stack"-typed value. */
  def boxToStack(box: BoxStack): Option[InterpreterStack]
  /** A memory location referring to a heap object. */
  def memToHeap(objRef: Word, iRef: Word, toObj: Word, isWeak: Boolean, isTR64: Boolean): Option[Word]
  /** A memory location referring to a heap object.  Return the new stack ID or 0. */
  def memToStack(objRef: Word, iRef: Word, toStack: Option[InterpreterStack]): Option[InterpreterStack]
  /** An InterpreterStack object referring to its stackMemory field. Stack memory cannot move. */
  def stackToStackMem(stack: InterpreterStack, toObj: Word): Option[Word]
  /** An InterpreterThread referring to its stack. GC cannot rebind stacks. */
  def threadToStack(thread: InterpreterThread, toStack: Option[InterpreterStack]): Option[InterpreterStack]
}

object RefFieldUpdater {
  def updateBoxToHeap(box: HasObjRef, newObjRef: Word): Unit = box.setObjRef(newObjRef)
  def updateBoxToStack(box: BoxStack, newStack: Option[InterpreterStack]) = box.stack = newStack
  def updateMemToHeap(iRef: Word, isTR64: Boolean, newObjRef: Word): Unit = {
    if (isTR64) {
      val oldRaw = MemorySupport.loadLong(iRef)
      val oldTag = OpHelper.tr64ToTag(oldRaw)
      val newRaw = OpHelper.refToTr64(newObjRef, oldTag)
      MemorySupport.storeLong(iRef, newRaw)
    } else {
      MemorySupport.storeLong(iRef, newObjRef)
    }
  }
  def updateMemToStack(iRef: Word, newStack: Option[InterpreterStack]) = {
    MemorySupport.storeLong(iRef, newStack.map(_.id).getOrElse(0).toLong)
  }
}