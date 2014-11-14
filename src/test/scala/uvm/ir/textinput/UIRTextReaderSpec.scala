package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import uvm.Bundle

class UIRTextReaderSpec extends  FlatSpec with Matchers
  with TestingBundlesValidators  {

   def parseFile(fileName: String, globalBundle: Bundle, fac: Option[IDFactory] = None): Bundle = {
    val idf = fac.getOrElse(new IDFactory())
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.FileReader(fileName), globalBundle)
    ir
  }

  val EMPTY_BUNDLE = new Bundle()

  behavior of "UIRTextReader"

  it should "read simple type definitions" in {
    val b = parseFile("tests/uvm-parsing-test/types.uir", EMPTY_BUNDLE)
    validateTypes(b)
  }
  it should "read simple constant definitions" in {
    val b = parseFile("tests/uvm-parsing-test/constants.uir", EMPTY_BUNDLE)
    validateConstants(b)
  }
  it should "read simple function definitions" in {
    val b = parseFile("tests/uvm-parsing-test/functions.uir", EMPTY_BUNDLE)
    validateFunctions(b)
  }
  it should "read simple instruction definitions" in {
    val b = parseFile("tests/uvm-parsing-test/instructions.uir", EMPTY_BUNDLE)
    validateInstructions(b)
  }
  it should "handle loading of multiple bundles" in {
    val idf = new IDFactory()
    val gb = parseFile("tests/uvm-parsing-test/redef-base.uir", EMPTY_BUNDLE, Some(idf))
    val b = parseFile("tests/uvm-parsing-test/redef-overlay.uir", gb, Some(idf))
    validateRedef(gb, b)

    //gb.merge(b)
    //validateRedefAfterMerge(gb, b)
  }

}