package uvm.clientsupport.text

import java.util
import scala.collection.JavaConversions

class IList[T](members: TraversableOnce[T]) extends util.AbstractList[T] with Traversable[T] {
  def this(membersIterable: java.lang.Iterable[T]) =
    this(JavaConversions iterableAsScalaIterable membersIterable)

  private val vector = members.toVector
  override def get(index: Int): T = vector(index)
  override def size: Int = vector.length
  override def isEmpty: Boolean = vector.isEmpty
  override def toString = vector mkString " "
  override def foreach[U](f: (T) => U): Unit = vector foreach f
  def apply(idx: Int): T = vector(idx)
}

object IList {
  def apply[T](members: T*): IList[T] = new IList(members)
  def empty[T]: IList[T] = new IList[T](Nil)
}

