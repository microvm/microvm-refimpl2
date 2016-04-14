package uvm

/**
 * Source information repository.
 * <p>
 * This class provide information for debugging when things go wrong. For each identified item, this repository records
 * source-level information about that item, such as file name, line, column.
 */
class SourceInfoRepo {
  private type MapType[K, V] = collection.mutable.HashMap[K, V]

  private val dict = new MapType[AnyRef, SourceInfo]()

  def apply(obj: AnyRef): SourceInfo = dict.getOrElse(obj, NoSourceInfo)

  def update(obj: AnyRef, info: SourceInfo) = dict.update(obj, info)
  
  def merge(that: SourceInfoRepo):Unit = {
    for ((k,v) <- dict) {
      dict(k) = v
    }
  }
}

abstract class SourceInfo {
  def toString(): String
  def prettyPrint(): String
}

object NoSourceInfo extends SourceInfo {
  override def toString(): String = "<Source info not available>"
  override def prettyPrint(): String = toString()
}

/**
 * Location of an item (usually a name, expression, ...) in text-based source code.
 * <p>
 * @param line the line number, starting from 0
 * @param columnStart the first column of the item, starting with 0
 * @param columnEnd the last column that contains the item, starting from 0
 * @param token the first token (defined by the lexical analyzer) of the item, which roughly identifies where the item
 * 					is even when the exact information is not available.
 * @param lineText the whole line of source code at the given line
 */
class TextSourceInfo(val line: Int, val columnStart: Int, val columnEnd: Int, val token: String, val lineText: String) extends SourceInfo {
  private def position: String = {
    "In %d:%d-%d near '%s'".format(line+1, columnStart+1, columnEnd+1, token)
  }
  override def toString(): String = position

  override def prettyPrint(): String = {
    val marker = " " * columnStart + "^" + "~" * (columnEnd - columnStart)
    "%s\n%s\n%s".format(position, lineText, marker)
  }
}