package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import uvm.Bundle

class AntlrUvmIRReaderSpec extends FlatSpec with Matchers {

  def parseFile(fileName: String): Bundle = {
    AntlrUvmIRReader.read(new java.io.FileReader(fileName))
  }

  "The AntlrUvmIRParser" should "parse simple type definitions" in {
    parseFile("tests/uvm-parsing-test/types.uir")
  }
  it should "parse simple constant definitions" in {
    parseFile("tests/uvm-parsing-test/constants.uir")
  }
  it should "parse simple function definitions" in {
    parseFile("tests/uvm-parsing-test/functions.uir")
  }
  it should "parse simple instruction definitions" in {
    parseFile("tests/uvm-parsing-test/instructions.uir")
  }

}