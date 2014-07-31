package uvm.ir.textinput

import scala.reflect._
import org.scalatest._

trait ExtraMatchers extends Assertions with Matchers {
  implicit class AnythingExtraMatchers(val thing: Any) {
    def shouldBeA[T: ClassTag](f: T => Unit): Unit = {
      val ct = classTag[T]
      if (!ct.runtimeClass.isAssignableFrom(thing.getClass)) {
        fail("%s is not an instance of %s".format(thing.getClass, ct.runtimeClass))
      }
      f(thing.asInstanceOf[T])
    }
  }

  val thatsIt = { f: Any => }
}

object ExtraMatchers extends ExtraMatchers {

}