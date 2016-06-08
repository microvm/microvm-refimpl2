package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._

class ExposedMethod(jRetTy: JType, jParamTys: Array[JType], invokeFunc: Buffer => Unit) {
  val closure = new SimpleClosure(invokeFunc)
  val handle = jffiClosureManager.newClosure(closure, jRetTy, jParamTys, CallingConvention.DEFAULT)
  def address = handle.getAddress()
}

class SimpleClosure(f: Buffer => Unit) extends Closure {
  def invoke(buffer: Buffer): Unit = f(buffer)
}

object CDefs {
  def exposedMethod(jRetTy: JType, jParamTys: Array[JType])(invokeFunc: Buffer => Unit) = {
    new ExposedMethod(jRetTy, jParamTys, invokeFunc)
  }

  /// SCRIPT: GENERATED CODE BEGIN
// goodbye world
  /// SCRIPT: GENERATED CODE END
}