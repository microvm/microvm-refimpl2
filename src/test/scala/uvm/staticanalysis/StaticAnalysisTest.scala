package uvm.staticanalysis

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.exceptions.TestFailedException

import uvm.GlobalBundle
import uvm.TrantientBundle
import uvm.ir.textinput.UIRTextReader
import uvm.utils.IDFactory

class StaticAnalysisTest extends FlatSpec with Matchers {

  def parseText(globalBundle: GlobalBundle, fac: Option[IDFactory]=None)(uir: String): TrantientBundle = {
    val idf = fac.getOrElse(new IDFactory(uvm.refimpl.MicroVM.FIRST_CLIENT_USABLE_ID))
    val r = new UIRTextReader(idf)
    val ir = r.read(new java.io.StringReader(uir), globalBundle)
    ir
  }

  behavior of "StaticAnalyzer"
  
  def shouldWorkFineIn(text: String): Unit = {
    val gb = new GlobalBundle()
    val b = parseText(gb)(text)
    new StaticAnalyzer().checkBundle(b, Some(gb))
  }
  
  def catchExceptionWhenAnalyzing(text: String): Unit = {
    val gb = new GlobalBundle()
    val b = parseText(gb)(text)
    try {
      new StaticAnalyzer().checkBundle(b, Some(gb))
      fail()
    } catch {
      case e: TestFailedException => throw e
      case e: Exception => // expected
        e.printStackTrace()
    }
    
  }

  it should "complain if a struct contains itself" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @s @i64>
      """)
  }

  it should "complain if a struct contains its parent" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @t @i64>
      .typedef @t = struct<@i64 @s @i64>
      """)
  }
  
  it should "not complain if the type is recursive on refs" in {
    shouldWorkFineIn("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @t @i64>
      .typedef @t = struct<@i64 @refs @i64>
      .typedef @refs = ref<@s>
      """)
  }

  it should "complain if an array type is recursive" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @a @i64>
      .typedef @a = array<@s 10>
      """)
  }

  it should "complain if a vector type is recursive" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @v = vector<@a 2>
      .typedef @a = array<@v 10>
      """)
  }

  it should "complain if a hybrid is contained in other composite types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @h @i64>
      .typedef @h = hybrid<@i64 @i64>
      """)
  }

  it should "not complain if a hybrid contains any composite types" in {
    shouldWorkFineIn("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @i64>
      .typedef @h = hybrid<@s @s>
      """)
  }

  it should "complain if a function returns void" in {
    catchExceptionWhenAnalyzing("""
      .typedef @void = void
      .funcsig @sig = () -> (@void)
      """)
  }
  
  it should "complain if a hybrid is used for value types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i8 = int<8>
      .typedef @hybrid = hybrid<@i8>
      .funcsig @sig = (@hybrid) -> ()
      """)
  }

  it should "complain if a weak reference is used for value types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i8 = int<8>
      .typedef @wr = weakref<@i8>
      .funcsig @sig = (@wr) -> ()
      """)
  }
  
  
  it should "complain if a int literal is used on non-integer or non-pointer types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @double = double
      .const @C <@double> = 100
      """)
  }

  it should "complain if a float literal is used on non-float types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @double = double
      .const @C <@double> = 3.14f
      """)
  }

  it should "complain if a double literal is used on non-double types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .const @C <@i64> = 3.14d
      """)
  }

  it should "complain if a NULL literal is used on non-reference types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @ptri64 = uptr<@i64>
      .const @C <@ptri64> = NULL
      """)
  }

  it should "complain if a sequence literal is used on non-composite types" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .const @C1 <@i64> = 10
      .const @C <@i64> = { @C1 @C1 @C1 }
      """)
  }

  it should "complain if a sequence literal has the wrong arity for structs" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @i64>
      .const @C1 <@i64> = 10
      .const @C <@s> = { @C1 @C1 @C1 @C1}
      """)
  }

  it should "complain if a sequence literal has the wrong arity for arrays" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @a = array<@i64 4>
      .const @C1 <@i64> = 10
      .const @C <@a> = { @C1 @C1 @C1}
      """)
  }

  it should "complain if a sequence literal has the wrong arity for vectors" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @v = vector<@i64 4>
      .const @C1 <@i64> = 10
      .const @C <@v> = { @C1 @C1 @C1}
      """)
  }

  it should "complain if a sequence literal is recursive" in {
    catchExceptionWhenAnalyzing("""
      .typedef @i64 = int<64>
      .typedef @s = struct<@i64 @i64 @i64>
      .const @C1 <@i64> = 10
      .const @C <@s> = { @C @C @C }
      """)
  }
}