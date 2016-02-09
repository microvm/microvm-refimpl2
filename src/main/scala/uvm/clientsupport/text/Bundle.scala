package uvm.clientsupport.text

import java.util
import scala.collection.JavaConversions._

/**
 * A Mu IR bundle. This class is more representation-oriented rather than semantic oriented. For example, there are
 * "type definitions" rather than "types". Similarly "function definitions" and "function declarations" are separated
 * rather than contained in each other.
 */
class Bundle(
  cTypeDefs: TraversableOnce[(TypeName, TypeCtor)],
  cFuncSigDefs: TraversableOnce[(FuncSigName, FuncSig)],
  cConstDefs: TraversableOnce[(GlobalVarName, Const)],
  cGlobalCellDefs: TraversableOnce[(GlobalVarName, TypeName)],
  cFuncDecls: TraversableOnce[(GlobalVarName, FuncSigName)],
  cFuncVers: TraversableOnce[(FuncVerName, PostFuncVer)],
  cFuncExpDefs: TraversableOnce[(GlobalVarName, Expose)],
  cComments: TraversableOnce[(GlobalName, String)]
) {
  private def buildMap[K, V](seq: TraversableOnce[(K, V)]): util.Map[K, V] = {
    val map = new util.LinkedHashMap[K, V]
    seq foreach { case (k, v) => map.put(k, v) }
    map
  }
  lazy val typeDefs = buildMap(cTypeDefs)
  lazy val funcSigDefs = buildMap(cFuncSigDefs)
  lazy val constDefs = buildMap(cConstDefs)
  lazy val globalCellDefs = buildMap(cGlobalCellDefs)
  lazy val funcDecls = buildMap(cFuncDecls)
  lazy val funcVers = buildMap(cFuncVers)
  lazy val funcExpDefs = buildMap(cFuncExpDefs)
  lazy val comments = buildMap(cComments)

  override lazy val toString = {
    val sb = new StringBuilder
    def printBlock[N <: GlobalName, V](block: util.Map[N, V])(fn: (N, V) => Unit): Unit =
      if (!block.isEmpty) {
        sb append "\n"
        block foreach { case (name, value) =>
          Option(comments get name) foreach (_ split '\n' map ("// " + _ + "\n") foreach sb.append)
          fn(name, value)
        }
      }

    printBlock(typeDefs)((name, ctor) => sb append s".typedef $name = $ctor\n")
    printBlock(funcSigDefs)((name, sig) => sb append s".funcsig $name = $sig\n")
    printBlock(constDefs)((name, const) => sb append s".const $name <${const.ty}> = $const\n")
    printBlock(globalCellDefs)((name, ty) => sb append s".global $name <$ty>\n")
    printBlock(funcDecls)((name, sig) => sb append s".funcdecl $name <$sig>\n")
    printBlock(funcExpDefs)((name, exp) => sb append s".expose $name = $exp\n")
    printBlock(funcVers)((name, ver) =>
      sb append s".funcdef ${ver.func} VERSION $name <${ver.sig}> $ver\n\n")

    sb.toString
  }
}
