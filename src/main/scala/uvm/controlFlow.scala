package uvm

import uvm.types._
import uvm.ssavalues._

case class FuncSig(var retTy: Type, var paramTy: Seq[Type]) extends IdentifiedSettable

object FuncSig {
  def prettyPrint(sig: FuncSig): String =
    "%s (%s)".format(sig.retTy.repr, sig.paramTy.map(_.repr).mkString(" "))
}

class Function extends IdentifiedSettable {
  var sig: FuncSig = null
  var cfg: Option[CFG] = None
}

class CFG {
  var func: Function = null
  var bbs: Seq[BasicBlock] = null
  var entry: BasicBlock = null
  var params: Seq[Parameter] = null

  val bbNs: Namespace[BasicBlock] = new SimpleNamespace[BasicBlock] // Consider using one global bb ns
  val lvNs: Namespace[LocalValue] = new SimpleNamespace[LocalValue] // Consider using one global value ns
}

class BasicBlock extends IdentifiedSettable {
  var insts: Seq[Instruction] = null
}