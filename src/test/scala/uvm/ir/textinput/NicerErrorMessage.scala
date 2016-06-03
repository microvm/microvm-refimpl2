package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import uvm.GlobalBundle
import uvm.TrantientBundle
import uvm.utils.IDFactory
import uvm.UvmTestBase

class NicerErrorMessage extends UvmTestBase with TestingBundlesValidators {

  def parseFile(fileName: String, globalBundle: GlobalBundle, fac: Option[IDFactory] = None): TrantientBundle = {
    val idf = fac.getOrElse(new IDFactory(uvm.refimpl.MicroVM.FIRST_CLIENT_USABLE_ID))
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.FileReader(fileName), globalBundle)
    ir
  }

  behavior of "UIRTextReader"

  it should "give nice error messages" in {
    try {
      val gb = new GlobalBundle()
      val b = parseFile("tests/uvm-parsing-test/bundle-with-error.uir", gb)
    } catch {
      case e: TextIRParsingException => // expected
        travisFriendlyExceptionPrint(e)
    }
  }

}