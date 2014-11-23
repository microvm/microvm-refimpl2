package uvm.refimpl

import uvm.UvmException

case class UvmRefImplException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

