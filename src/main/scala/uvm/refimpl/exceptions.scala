package uvm.refimpl

import uvm.UvmException

/** Parent of all exceptions in the implementation part. This does not include the data structure and parser outside uvm.refimpl. */
class UvmRefImplException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

/** Thrown if the internal state of the micro VM is inconsistent. This usually mean there is a bug in the micro VM. */
class UvmInternalException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/** Thrown when an action not required by the specification and not implemented by this refimpl is performed. */
class UvmUnimplementedOperationException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/**
 * Thrown when a dynamic error (errors that cannot be found at compile time) happens. This refimpl may sometimes throw
 * exceptions on static errors rather than checking before running because the micro VM has undefined behaviour on static
 * errors. (It has undefined behaviour on dynamic errors, too.)
 */
class UvmRuntimeException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)

/** Thrown when the micro VM cannot allocate memory. */
class UvmOutOfMemoryException(message: String = null, cause: Throwable = null) extends UvmRuntimeException(message, cause)

/** Thrown when an operation has undefined behaviour according to the specification. */
class UvmUndefinedBehaviorException(message: String = null, cause: Throwable = null) extends UvmRuntimeException(message, cause)

/** Thrown when a division by zero is executed and the exception clause is not present. */
class UvmDivisionByZeroException(message: String = null, cause: Throwable = null) extends UvmUndefinedBehaviorException(message, cause)

/** Thrown when a general reference value is null when it must not be. */
class UvmNullGenRefException(message: String = null, cause: Throwable = null) extends UvmUndefinedBehaviorException(message, cause)

/** Thrown when accessing Mu memory but the address is outside the allocated region. */
class UvmIllegalMemoryAccessException(message: String = null, cause: Throwable = null) extends UvmRuntimeException(message, cause)

/** Thrown on syntax errors in HAIL scripts. */
class UvmHailParsingException(message: String = null, cause: Throwable = null) extends UvmRefImplException(message, cause)
