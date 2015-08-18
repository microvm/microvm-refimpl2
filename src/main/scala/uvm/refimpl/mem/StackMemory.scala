package uvm.refimpl.mem

import uvm.refimpl.MicroVM
import uvm.refimpl.mem.bumppointer.RewindableBumpPointerAllocator
import TypeSizes.Word

/**
 * Stack memory.
 * <p>
 * If the stack is Dead, the stackObjRef will be a dangling pointer.
 */
class StackMemory(val stackObjRef: Word, extend: Word, microVM: MicroVM)
    extends RewindableBumpPointerAllocator(stackObjRef, extend, microVM)
