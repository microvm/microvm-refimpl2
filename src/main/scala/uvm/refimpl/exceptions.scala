package uvm.refimpl

import uvm.UvmException

/** Parent of all exceptions in the implementation part. This does not include the data structure and parser outside uvm.refimpl. */
class UvmRefImplException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

/** Thrown when the micro VM cannot allocate memory. */
class UvmOutOfMemoryException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/** Thrown when an action not required by the specification and not implemented by this refimpl is performed. */
class UnimplementedOprationException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/**
 * Thrown when a dynamic error (errors that cannot be found at compile time) happens. This refimpl may sometimes throw
 * exceptions on static errors rather than checking before running because the micro VM has undefined behaviour on static
 * errors. (It has undefined behaviour on dynamic errors, too.)
 */
class UvmRuntimeException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/** Thrown when a division by zero is executed and the exception claues is not present. */
class UvmDivisionByZeroException(message: String = null, cause: Throwable = null) extends UvmRuntimeException(message, cause)