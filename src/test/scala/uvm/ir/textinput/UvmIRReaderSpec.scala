package uvm.ir.textinput

import org.scalatest._

import uvm.Bundle

class UvmIRReaderSpec extends AbstractReaderSpec {
  
  override def theSubject = "UvmIRReader"

  override def parseFile(fileName: String, globalBundle: Bundle): Bundle = {
    UvmIRReader.read(new java.io.FileReader(fileName), globalBundle)
  }


}