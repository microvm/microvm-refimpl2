package uvm.refimpl.itpr

import scala.collection.mutable.ArrayBuffer
import uvm.refimpl.mem.Mutator
import uvm.refimpl.mem.TypeSizes.Word
import uvm.refimpl.UvmRuntimeException

/**
 * Trait for entities (threads and client agents) that can pin objects. GC can also scan such entities.
 */
trait ObjectPinner {
  /** Multi-set of pinned object references. May contain NULL. */
  def pinSet: ArrayBuffer[Word]
  
  def pin(addr: Word) {
    pinSet += addr
  }
  
  def unpin(addr: Word) {
    val index = pinSet.lastIndexOf(addr)
    if (index == -1) {
      throw new UvmRuntimeException("Attempt to unpin object at %d (0x%x), which is not pinned.".format(addr, addr))
    }
    pinSet.remove(index)
  }
}