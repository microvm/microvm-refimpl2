package uvm.ir.textinput

import org.scalatest._
import uvm._

trait AbstractReaderSpec extends FlatSpec with Matchers
  with TestingBundlesValidators {
  
  val EMPTY_BUNDLE = new Bundle()
  
  def parseFile(fileName: String, globalBundle: Bundle): Bundle

  def theSubject: String

  behavior of theSubject

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
    val gb = parseFile("tests/uvm-parsing-test/redef-base.uir", EMPTY_BUNDLE)
    val b = parseFile("tests/uvm-parsing-test/redef-overlay.uir", gb)
    validateRedef(gb, b)
    
    gb.merge(b)
    validateRedefAfterMerge(gb, b)    
  }
}