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