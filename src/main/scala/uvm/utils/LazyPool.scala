package uvm.utils

import scala.collection.mutable.HashMap

class LazyPool[FromT, ToT](factory: FromT => ToT) {
  val pool = HashMap[FromT, ToT]()
  def apply(obj: FromT): ToT = pool.get(obj).getOrElse(factory(obj))
}
object LazyPool {
  def apply[FromT, ToT](factory: FromT => ToT): LazyPool[FromT, ToT] = new LazyPool[FromT, ToT](factory)
}
