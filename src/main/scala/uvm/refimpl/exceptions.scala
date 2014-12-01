package uvm.refimpl

import uvm.UvmException

class UvmRefImplException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

class UvmOutOfMemoryException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

