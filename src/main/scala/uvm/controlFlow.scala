package uvm

import uvm.types._
import uvm.ssavariables._

case class FuncSig(var retTy: Type, var paramTy: Seq[Type]) extends IdentifiedSettable {
  override final def toString: String = FuncSig.prettyPrint(this)
  override def hashCode(): Int = System.identityHashCode(this)
  override def equals(that: Any): Boolean = that match {
    case v: AnyRef => this eq v
    case _         => false
  }
}

object FuncSig {
  def prettyPrint(sig: FuncSig): String =
    "%s (%s)".format(sig.retTy.repr, sig.paramTy.map(_.repr).mkString(" "))
}

class Function extends GlobalVariable {
  var sig: FuncSig = null
  var versions: List[FuncVer] = Nil
}

/**
 * A version of a function. Also known as a "control flow graph".
 */
class FuncVer extends IdentifiedSettable {
  var func: Function = null
  var bbs: Seq[BasicBlock] = null
  var entry: BasicBlock = null

  def sig: FuncSig = func.sig

  val bbNs = new SimpleNamespace[BasicBlock]
}

class BasicBlock extends IdentifiedSettable {
  var norParams: Seq[NorParam] = null
  var excParam: Option[ExcParam] = null
  var insts: Seq[Instruction] = null

  val localVarNs = new SimpleNamespace[LocalVariable]
}
