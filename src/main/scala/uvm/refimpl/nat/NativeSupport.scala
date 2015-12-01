package uvm.refimpl.nat

import jnr.ffi.{ Runtime, Memory, Pointer }
import com.kenai.jffi.ClosureManager
import jnr.ffi.provider.jffi.NativeRuntime

/**
 * Holder of JNR-specific resources.
 */
object NativeSupport {
  // Force using NativeRuntime (although that's default).
  val jnrRuntime = NativeRuntime.getInstance
  val jnrMemoryManager = jnrRuntime.getMemoryManager
  val theMemory = jnrMemoryManager.newPointer(0L)
  
  // This is from JFFI, not JNR-FFI.
  val jffiClosureManager = ClosureManager.getInstance()
}