package uvm

import uvm.types._
import uvm.ssavariables._

case class FuncSig(var retTy: Type, var paramTy: Seq[Type]) extends IdentifiedSettable

object FuncSig {
  def prettyPrint(sig: FuncSig): String =
    "%s (%s)".format(sig.retTy.repr, sig.paramTy.map(_.repr).mkString(" "))
}

class Function extends GlobalVariable {
  var sig: FuncSig = null
  var versions: Seq[FuncVersion] = Nil
}

/**
 * A version of a function. Also known as a "control flow graph".
 */
class FuncVersion extends IdentifiedSettable {
  var func: Function = null
  var bbs: Seq[BasicBlock] = null
  var entry: BasicBlock = null
  var params: Seq[Parameter] = null

  val bbNs: Namespace[BasicBlock] = new SimpleNamespace[BasicBlock]
}

class BasicBlock extends IdentifiedSettable {
  var insts: Seq[Instruction] = null
}