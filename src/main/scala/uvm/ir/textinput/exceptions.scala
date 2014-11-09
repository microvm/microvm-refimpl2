package uvm.ir.textinput

import uvm.UvmException

class TextIRParsingException(message: String = null, cause: Throwable = null) extends UvmException(message, cause)

class UnexpectedTypeException(message: String = null, cause: Throwable = null) extends TextIRParsingException(message, cause)