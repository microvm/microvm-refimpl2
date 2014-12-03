package uvm.refimpl.mem.simpleimmix

import uvm.refimpl.UvmOutOfMemoryException

class NoMoreDefragBlockException(message: String = null, cause: Throwable = null) extends UvmOutOfMemoryException(message, cause)
