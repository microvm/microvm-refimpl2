package uvm.refimpl.mem

import uvm.types._
import uvm.ssavariables.GlobalCell
import uvm.refimpl._
import uvm.refimpl.mem.TypeSizes._
import uvm.refimpl.mem.bumppointer.RewindableBumpPointerAllocator
import java.util.HashMap

class GlobalMemory(begin: Word, size: Word, microVM: MicroVM) extends Space("GlobalSpace", begin, size) {

  val allocator = new RewindableBumpPointerAllocator(begin, size, microVM)

  private val locationMap = new HashMap[GlobalCell, Word]()

  def addGlobalCell(gc: GlobalCell) {
    val ty = gc.cellTy
    if (ty.isInstanceOf[TypeHybrid]) {
      throw new UvmRefImplException("It does not make sense to make global hybrid (use array and any fixed types). global data: " + gc.repr)
    }
    val addr = allocateGlobalCellMemory(ty)
    locationMap.put(gc, addr)
  }

  private def allocateGlobalCellMemory(ty: Type): Word = {
    val tag = ty.id
    val size = sizeOf(ty)
    val align = alignOf(ty)
    val objAddr = allocator.alloc(size, align, TypeSizes.GC_HEADER_SIZE_SCALAR)
    HeaderUtils.postAllocScalar(objAddr, tag)
    objAddr
  }

  def addrForGlobalCell(gc: GlobalCell): Word = locationMap.get(gc)
}
