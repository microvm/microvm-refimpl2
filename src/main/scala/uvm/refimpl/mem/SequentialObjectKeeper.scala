package uvm.refimpl.mem

import scala.collection.mutable.HashMap

/**
 * This class keeps a map between Long and arbitrary Scala objects. In this way,
 * references to arbitrary Scala objects can be stored in the Mu memory.
 * <p>
 * Similar to JFFI's object exposer.
 */
class SequentialObjectKeeper[T] {
  private val longToObj = new HashMap[Long, T]()
  private val objToLong = new HashMap[T, Long]()

  var nextLong = 1L

  def objGetLong(maybeObj: Option[T]): Long = {
    maybeObj match {
      case None => 0L
      case Some(obj) =>
        objToLong.getOrElseUpdate(obj, {
          val myLong = nextLong
          nextLong += 1
          longToObj(myLong) = obj
          myLong
        })
    }
  }

  def longToObj(l: Long): Option[T] = longToObj.get(l)

  def maybeRemoveObj(obj: T): Unit = {
    objToLong.remove(obj).foreach { l =>
      longToObj.remove(l)
    }
  }
}