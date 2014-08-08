package uvm

abstract class Namespace[T <: Identified] {
  def apply(id: Int): T
  def apply(name: String): T

  def get(id: Int): Option[T]
  def get(name: String): Option[T]

  def add(obj: T): Unit
  
  def all: Iterable[T]
}

class SimpleNamespace[T <: Identified] extends Namespace[T] {
  private type MapType[K, V] = collection.mutable.HashMap[K, V]

  private val idMap = new MapType[Int, T]()
  private val nameMap = new MapType[String, T]()

  def apply(id: Int): T = idMap(id)
  def apply(name: String): T = nameMap(name)

  def get(id: Int): Option[T] = idMap.get(id)
  def get(name: String): Option[T] = nameMap.get(name)

  def add(obj: T): Unit = {
    for (obj2 <- get(obj.id)) {
      throw new NameConflictException(
        "Object %s ID-conflicts with %s".format(obj.repr, obj2.repr))
    }

    for (name <- obj.name; obj2 <- get(name)) {
      throw new NameConflictException(
        "Object %s name-conflicts with %s".format(obj.repr, obj2.repr))
    }

    idMap.put(obj.id, obj)
    obj.name match {
      case None =>
      case Some(name) => nameMap.put(name, obj)
    }
  }
  
  def all = idMap.values
}
