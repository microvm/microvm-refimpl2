package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import uvm.GlobalBundle
import uvm.TrantientBundle

class UIRTextReaderSpec extends FlatSpec with Matchers
    with TestingBundlesValidators {

  def parseFile(fileName: String, globalBundle: GlobalBundle, fac: Option[IDFactory] = None): TrantientBundle = {
    val idf = fac.getOrElse(new IDFactory())
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.FileReader(fileName), globalBundle)
    ir
  }

  behavior of "UIRTextReader"

  def parseFresh(fileName: String): GlobalBundle = {
    val gb = new GlobalBundle()
    val tb = parseFile(fileName, gb)
    gb.merge(tb)
    gb
  }

  it should "read simple type definitions" in {
    val b = parseFresh("tests/uvm-parsing-test/types.uir")
    validateTypes(b)
  }
  it should "read simple constant definitions" in {
    val b = parseFresh("tests/uvm-parsing-test/constants.uir")
    validateConstants(b)
  }
  it should "read simple function definitions" in {
    val b = parseFresh("tests/uvm-parsing-test/functions.uir")
    validateFunctions(b)
  }
  it should "read simple instruction definitions" in {
    val b = parseFresh("tests/uvm-parsing-test/instructions.uir")
    validateInstructions(b)
  }
  /*
  it should "handle loading of multiple bundles" in {
    val idf = new IDFactory()
    val gb = new GlobalBundle()
    val b1 = parseFile("tests/uvm-parsing-test/redef-base.uir", gb, Some(idf))
    gb.merge(b1)
    val b2 = parseFile("tests/uvm-parsing-test/redef-overlay.uir", gb, Some(idf))
    validateRedef(gb, b1, b2)

    gb.merge(b2)
    validateRedefAfterMerge(gb, b2)
  }
  */
}