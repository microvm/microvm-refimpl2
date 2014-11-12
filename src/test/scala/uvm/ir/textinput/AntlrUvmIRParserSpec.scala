package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import uvm.Bundle

class AntlrUvmIRReaderSpec extends AbstractReaderSpec {
  override def theSubject = "AntlrUvmIRReader"

  override def parseFile(fileName: String, globalBundle: Bundle): Bundle = {
    val idf = new IDFactory()
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.FileReader(fileName), globalBundle)
    ir
  }

}