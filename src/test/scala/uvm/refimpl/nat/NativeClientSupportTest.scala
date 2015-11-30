package uvm.refimpl.nat

import scala.reflect.runtime.universe

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class NativeClientSupportTest extends FlatSpec with Matchers {
  "The ClientAccessibleClassExposer" should "enumerate declared methods" in {
    val cace = new ClientAccessibleClassExposer(NativeMuVM)
  }
}