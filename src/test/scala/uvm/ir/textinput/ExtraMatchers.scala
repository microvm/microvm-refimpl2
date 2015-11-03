package uvm.ir.textinput

import scala.reflect._
import org.scalatest._

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.comminsts._

trait ExtraMatchers extends Assertions with Matchers {
  import ExtraMatchers._
  implicit def anythingToAnythingExtraMatcher[U](thing: U) = new AnythingExtraMatchers(thing)
  
  val thatsIt = { f: Any => }
  case object nan
  case class ExactFloat(num: Float)
  case class ExactDouble(num: Double)
  def exactly(num: Float) = ExactFloat(num)
  def exactly(num: Double) = ExactDouble(num)
  
  def bitsf(num: Int) = java.lang.Float.intBitsToFloat(num)
  def bitsd(num: Long) = java.lang.Double.longBitsToDouble(num)
}

object ExtraMatchers extends ExtraMatchers {
  implicit class AnythingExtraMatchers[U](val thing: U) extends AnyVal {
    def shouldBeA[T: ClassTag](f: T => Unit): Unit = {
      val ct = classTag[T]
      if (!ct.runtimeClass.isAssignableFrom(thing.getClass)) {
        fail("%s is not an instance of %s".format(thing.getClass, ct.runtimeClass))
      }
      f(thing.asInstanceOf[T])
    }
    
    def shouldBeATypeIntOf(len: Int) {
      thing shouldBeA[TypeInt] {_.length shouldEqual len}
    }
    
    def shouldBeAConstIntOf(len: Int, num: BigInt) {
      thing shouldBeA[ConstInt] { its =>
        its.constTy shouldBeATypeIntOf(len)
        its.num shouldEqual num
      }
    }
        
    def shouldBeAConstFloatOf(something: Any) {
      thing shouldBeA[ConstFloat] { its =>
        its.constTy shouldBeA[TypeFloat] thatsIt
        something match {
          case `nan` => assert(its.num.isNaN)
          case ExactFloat(num) => its.num shouldEqual num
          case num: Float => its.num shouldEqual (num +- 0.001F)
          case _ => its.num shouldEqual something
        }
      }
    }
    
    def shouldBeAConstDoubleOf(something: Any) {
      thing shouldBeA[ConstDouble] { its =>
        its.constTy shouldBeA[TypeDouble] thatsIt
        something match {
          case `nan` => assert(its.num.isNaN)
          case ExactDouble(num) => its.num shouldEqual num
          case num: Double => its.num shouldEqual (num +- 0.001F)
          case _ => its.num shouldEqual something
        }
      }
    }
  }

}
