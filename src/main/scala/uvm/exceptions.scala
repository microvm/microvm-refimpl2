package uvm

class UvmException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

class TypeResolutionException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

class NameConflictException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)