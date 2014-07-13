package uvm.ir.textinput

import uvm._
import uvm.types._
import uvm.ssavalues._

object UvmIRReader {
  import UvmIRAST._
  
  def read(ir: CharSequence): Bundle = {
    val ast = UvmIRParser(ir)
    read(ast)
  }
  
  def read(ir: java.io.Reader): Bundle = {
    val ast = UvmIRParser(ir)
    read(ast)
  }
  
  def read(ast: IR): Bundle = {
    null
  }
}