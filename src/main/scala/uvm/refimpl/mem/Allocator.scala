package uvm.refimpl.mem

import uvm.refimpl.mem.TypeSizes.Word
import uvm.refimpl.UvmOutOfMemoryException

trait Allocator {
  /**
   * Allocate a contiguous range of memory of the given size, alignment and header size.
   * <p>
   * Return the address of the allocated memory range, or throw UvmOutOfMemoryException if out of memory.
   */
  @throws(classOf[UvmOutOfMemoryException])
  def alloc(size: Word, align: Word, headerSize: Word): Word
}
