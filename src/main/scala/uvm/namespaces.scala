package uvm

abstract class Namespace[T <: Identified] {
  def apply(id: Int): T
  def apply(name: String): T

  def get(id: Int): Option[T]
  def get(name: String): Option[T]

  def add(obj: T): Unit

  def all: Iterable[T]
}

/**
 * A simple namespace implementation.
 * 
 * @param kind: a human-readable name for the thing this namespace is supposed to contain.
 */
class SimpleNamespace[T <: Identified](val kind: String = SimpleNamespace.DEFAULT_KIND) extends Namespace[T] {
  private type MapType[K, V] = collection.mutable.HashMap[K, V]

  private val idMap = new MapType[Int, T]()
  private val nameMap = new MapType[String, T]()

  @throws(classOf[NoSuchElementException])
  def apply(id: Int): T = try {
    idMap(id)
  } catch {
    case e: NoSuchElementException => throw new NoSuchElementException("No %s has ID %d".format(kind, id))
  }
  @throws(classOf[NoSuchElementException])
  def apply(name: String): T = try {
    nameMap(name)
  } catch {
    case e: NoSuchElementException => throw new NoSuchElementException("No %s has name '%s'".format(kind, name))
  }

  def get(id: Int): Option[T] = idMap.get(id)
  def get(name: String): Option[T] = nameMap.get(name)

  def add(obj: T) {
    for (obj2 <- get(obj.id)) {
      throw new NameConflictException(kind, "id", obj, obj2)
    }

    for (name <- obj.name; obj2 <- get(name)) {
      throw new NameConflictException(kind, "name", obj, obj2)
    }

    idMap.put(obj.id, obj)
    obj.name match {
      case None       =>
      case Some(name) => nameMap.put(name, obj)
    }
  }

  def all = idMap.values
}

object SimpleNamespace {
  val DEFAULT_KIND = "object"
}

class NestedNamespace[T <: Identified](var maybeParent: Option[NestedNamespace[_ >: T <: Identified]],
    kind: String = SimpleNamespace.DEFAULT_KIND) extends SimpleNamespace[T](kind) {
  override def add(obj: T): Unit = {
    super.add(obj)
    maybeParent.foreach(_.add(obj))
  }

  def makeSubSpace[U <: T](kind: String): NestedNamespace[U] = {
    new NestedNamespace[U](Some(this), kind)
  }

  def reparent[U >: T <: Identified](newParent: NestedNamespace[U]) = {
    maybeParent = Some(newParent)
    for (obj <- all) {
      newParent.add(obj)
    }
  }
}

