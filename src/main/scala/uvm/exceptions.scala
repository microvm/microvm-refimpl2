package uvm

class UvmException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

class BundleLoadingException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

class TypeResolutionException(message: String = null, cause: Throwable = null) extends BundleLoadingException(message, cause)

class IllegalRedefinitionException(message: String = null, cause: Throwable = null) extends BundleLoadingException(message, cause)

class NameConflictException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)
