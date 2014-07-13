package uvm.ir.textinput

import collection.mutable.Stack
import org.scalatest._

class UvmIRParserSpec extends FlatSpec with Matchers {

  def parseFile(fileName: String) {
    val ir = UvmIRParser(new java.io.FileReader(fileName))
    println(ir)
  }

  "The IRParser" should "parse simple type definitions" in {
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