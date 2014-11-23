package uvm.refimpl.mem

import uvm.refimpl.mem.TypeSizes.Word

trait Allocator {
  def alloc(size: Word, align: Word, headerSize: Word): Word
}
