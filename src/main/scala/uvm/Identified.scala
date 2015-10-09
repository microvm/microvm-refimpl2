package uvm

trait Identified {
  def id: Int
  def name: Option[String]

  def repr: String = "[%d:%s]".format(id, name.getOrElse("_"))
}

trait IdentifiedSettable extends Identified {
  var id: Int = 0
  var name: Option[String] = None
}

object RichIdentifiedSettable {
  implicit class RichIdentifiedSettable[T <: IdentifiedSettable](val is: T) {
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
