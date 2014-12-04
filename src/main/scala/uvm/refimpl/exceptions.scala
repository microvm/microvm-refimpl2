package uvm.refimpl

import uvm.UvmException

/** Parent of all exceptions in the implementation part. This does not include the data structure and parser outside uvm.refimpl. */
class UvmRefImplException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

/** Thrown when the µVM cannot allocate memory. */
class UvmOutOfMemoryException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/** Thrown when an action not required by the specification and not implemented by this refimpl is performed. */
class UnimplementedOprationException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)
