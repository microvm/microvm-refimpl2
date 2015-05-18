package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import uvm.Bundle

class NicerErrorMessage extends FlatSpec with Matchers
  with TestingBundlesValidators {

  def parseFile(fileName: String, globalBundle: Bundle, fac: Option[IDFactory] = None): Bundle = {
    val idf = fac.getOrElse(new IDFactory())
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.FileReader(fileName), globalBundle)
    ir
  }

  val EMPTY_BUNDLE = new Bundle()

  behavior of "UIRTextReader"

  it should "give nice error messages" in {
    try {
      val b = parseFile("tests/uvm-parsing-test/bundle-with-error.uir", EMPTY_BUNDLE)
    } catch {
      case e: TextIRParsingException => // expected
        e.printStackTrace()
    }
  }
}