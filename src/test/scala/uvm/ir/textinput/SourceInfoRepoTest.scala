package uvm.ir.textinput

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import uvm.GlobalBundle
import uvm.TrantientBundle
import uvm.utils.IDFactory
import org.scalatest.exceptions.TestFailedException

class SourceInfoRepoTest extends FlatSpec with Matchers
  with TestingBundlesValidators {

  def parseText(globalBundle: GlobalBundle, fac: Option[IDFactory]=None)(uir: String): TrantientBundle = {
    val idf = fac.getOrElse(new IDFactory(uvm.refimpl.MicroVM.FIRST_CLIENT_USABLE_ID))
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.StringReader(uir), globalBundle)
    ir
  }

  behavior of "SourceInfoRepo"
  
  def catchExceptionWhenParsing(text: String): Unit = {
    try {
      val gb = new GlobalBundle()
      val b = parseText(gb)(text)
      fail()
    } catch {
      case e: TestFailedException => throw e
      case e: Exception => // expected
        e.printStackTrace()
    }
    
  }

  it should "give nice error messages for undefined types" in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i32>
      """)
  }

  it should "give nice error messages for undefined signatures" in {
    catchExceptionWhenParsing("""
      .typedef @foo = funcref<@unknown_sig>
      """)
  }

  it should "give nice error messages for undefined constants" in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64>
      .const @foo <@s> = {@UNKNOWN_CONST}
      """)
  }

  it should "give nice error messages for undefined functions" in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .const @ZERO <@i64> = 0
      .expose @exposed = @unknown_func #DEFAULT @ZERO
      """)
  }

  it should "give nice error messages for undefined basic blocks" in {
    catchExceptionWhenParsing("""
      .funcsig @f.sig = () -> ()
      .funcdef @f VERSION %v1 <@f.sig> {
        %entry():
          BRANCH %unknown_basic_block()
      }
      """)
  }

  it should "give nice error messages for undefined variables" in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @f.sig = () -> (@i64)
      .funcdef @f VERSION %v1 <@f.sig> {
        %entry():
          RET %unknwon_var
      }
      """)
  }
  
  it should "produce error when types are re-defined." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .typedef @i64 = double
      """)
  }

  it should "produce error when function signatures are re-defined." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @foo = () -> ()
      .funcsig @foo = (@i64) -> ()
      """)
  }

  it should "produce error when constants are re-defined." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .const @foo <@i64> = 1
      .const @foo <@i64> = 2
      """)
  }

  it should "produce error when global cells are re-defined." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .global @foo <@i64>
      .global @foo <@i64>
      """)
  }

  it should "produce error when exposed functions are re-defined." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @foo = () -> ()
      .funcdecl @f <@foo>
      .const @ZERO <@i64> = 0
      .const @ONE <@i64> = 1
      .expose @fe = @f #DEFAULT @ONE
      .expose @fe = @f #DEFAULT @ZERO
      """)
  }

  it should "produce error when two functions have the same version name." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @foo = () -> ()
      .funcdef @f VERSION @v1 <@foo> {
        %entry():
          RET ()
      }
      .funcdef @g VERSION @v1 <@foo> {
        %entry():
          RET ()
      }
      """)
  }

  it should "produce error when two local variables have the same name." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @foo = (@i64 @i64) -> ()
      .funcdef @f VERSION %v1 <@foo> {
        %entry(<@i64> %a <@i64> %b):
          %c = ADD <@i64> %a %b
          %c = SUB <@i64> %a %b
          RET ()
      }
      """)
  }
  
  it should "produce error when two random things have the same name." in {
    catchExceptionWhenParsing("""
      .typedef @i64 = int<64>
      .funcsig @i64 = () -> ()
    """)
  }
}