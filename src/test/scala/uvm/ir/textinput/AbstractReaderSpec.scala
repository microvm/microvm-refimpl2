package uvm.ir.textinput

import org.scalatest._
import uvm._

trait AbstractReaderSpec extends FlatSpec with Matchers
  with TestingBundlesValidators {

  def parseFile(fileName: String): Bundle

  def theSubject: String

  behavior of theSubject

  it should "read simple type definitions" in {
    val b = parseFile("tests/uvm-parsing-test/types.uir")
    validateTypes(b)
  }
  it should "read simple constant definitions" in {
    val b = parseFile("tests/uvm-parsing-test/constants.uir")
    validateConstants(b)
  }
  it should "read simple function definitions" in {
    val b = parseFile("tests/uvm-parsing-test/functions.uir")
    validateFunctions(b)
  }
  it should "read simple instruction definitions" in {
    val b = parseFile("tests/uvm-parsing-test/instructions.uir")
    validateInstructions(b)
  }
}