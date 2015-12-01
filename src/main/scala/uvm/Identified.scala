package uvm

trait Identified {
  def id: Int
  def name: Option[String]

  def repr: String = "[%d:%s]".format(id, name.getOrElse("_"))
  
  // Identified objects should use reference equality rather than structural equality. (case classes use the latter)
  override def hashCode(): Int = if (id != 0) id else System.identityHashCode(this)
  override def equals(that: Any): Boolean = that match {
    case v: AnyRef => this eq v
    case _         => false
  }
  override def toString = "%s%s".format(this.getClass.getSimpleName, this.repr)
}

trait IdentifiedSettable extends Identified {
  var id: Int = 0
  var name: Option[String] = None
}

object RichIdentifiedSettable {
  implicit class RichIdentifiedSettable[T <: IdentifiedSettable](val is: T) extends AnyVal {
    def :=(p: Int): T = {
      is.id = p
      is
    }
    def :=(p: (Int, String)): T = {
      is.id = p._1
      is.name = Some(p._2)
      is
    }
  }
}
