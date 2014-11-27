package uvm.refimpl.mem

import uvm.refimpl.MicroVM
import uvm.refimpl.mem.bumppointer.RewindableBumpPointerAllocator
import TypeSizes.Word

class StackMemory(val stackObjRef: Word, val extend: Word, microVM: MicroVM)
    extends RewindableBumpPointerAllocator(stackObjRef, extend, microVM)
