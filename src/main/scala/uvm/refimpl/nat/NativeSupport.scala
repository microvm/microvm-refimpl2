package uvm.refimpl.nat

import jnr.ffi.{ Runtime, Memory, Pointer }
import com.kenai.jffi.ClosureManager

/**
 * Holder of JNR-specific resources.
 */
object NativeSupport {
  val jnrRuntime = Runtime.getSystemRuntime
  val theMemory = Pointer.wrap(jnrRuntime, 0L)
  val closureManager = ClosureManager.getInstance()
}