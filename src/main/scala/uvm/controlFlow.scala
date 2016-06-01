package uvm

import scala.collection.mutable.ArrayBuffer

import uvm.types._
import uvm.ssavariables._

case class FuncSig(var paramTys: Seq[Type], var retTys: Seq[Type]) extends IdentifiedSettable {
  override final def toString: String = FuncSig.prettyPrint(this)
}

object FuncSig {
  def prettyPrint(sig: FuncSig): String = {
    def mkReprList(is: Seq[Identified]): String = is.map(_.repr).mkString(" ")
    "(%s) -> (%s)".format(mkReprList(sig.paramTys), mkReprList(sig.retTys))
  }
}

class Function extends GlobalVariable {
  var sig: FuncSig = null
}

/**
 * A version of a function. Also known as a "control flow graph".
 */
class FuncVer extends IdentifiedSettable {
  var func: Function = null
  var bbs: ArrayBuffer[BasicBlock] = null
  def entry: BasicBlock = bbs.head

  def sig: FuncSig = func.sig

  var bbNs: NestedNamespace[BasicBlock] = null  // sub-namespace of allNs
}

class BasicBlock extends IdentifiedSettable {
  var norParams: ArrayBuffer[NorParam] = null
  var excParam: Option[ExcParam] = null
  var insts: ArrayBuffer[Instruction] = null

  var localVarNs: NestedNamespace[LocalVariable] = null // sub-namespace of allNs
  var localInstNs: NestedNamespace[Instruction] = null // sub-namespace of allNs
}
