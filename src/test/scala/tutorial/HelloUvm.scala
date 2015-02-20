package tutorial

import uvm.refimpl._

object HelloUvm extends App {
  val microVM = new MicroVM()  // Create a µVM.
  
  val ca = microVM.newClientAgent()  // Create a "client agent" to control the µVM.
  
  val reader1 = new java.io.FileReader("tests/uvm-refimpl-test/primitives.uir")
  ca.loadBundle(reader1)  // Load a bundle.
  reader1.close()
  
  val reader2 = new java.io.FileReader("tests/uvm-refimpl-test/simple-tests.uir")
  ca.loadBundle(reader2)  // Load another bundle.
  reader2.close()

  ca.close() // Remember to close the "client agent".
}