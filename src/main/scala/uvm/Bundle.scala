package uvm

import uvm.types._
import uvm.ssavariables._

class Bundle {
  /**
   * Namespace of all SSA variables, global or local.
   */
  val varNs = new SimpleNamespace[SSAVariable]()

  /**
   * All global SSA varaibles.
   */
  val globalVarNs = new SimpleNamespace[GlobalVariable]()

  val typeNs = new SimpleNamespace[Type]()
  val funcSigNs = new SimpleNamespace[FuncSig]()
  val constantNs = new SimpleNamespace[Constant]()
  val globalCellNs = new SimpleNamespace[GlobalCell]()
  val funcNs = new SimpleNamespace[Function]()

  val funcVerNs = new SimpleNamespace[FuncVer]()

  private def simpleMerge[T <: Identified](oldNs: Namespace[T], newNs: Namespace[T]) {
    for (cand <- newNs.all) {
      try {
        oldNs.add(cand)
      } catch {
        case e: NameConflictException =>
          throw new IllegalRedefinitionException(
            "Redefinition of type, function signature, constant or" +
              " global cell is not allowed", e);
      }
    }
  }
  
  private def mergeFunc(oldNs: Namespace[Function], newNs: Namespace[Function]) {
    for (cand <- newNs.all) {
      val id = cand.id
      oldNs.get(id) match {
        case None => oldNs.add(cand)
        case Some(oldObj) => oldObj.versions = cand.versions.head :: oldObj.versions
      }
    }
  }
  def merge(newBundle: Bundle) {
    simpleMerge(varNs, newBundle.varNs)
    simpleMerge(globalVarNs, newBundle.globalVarNs)
    simpleMerge(typeNs, newBundle.typeNs)
    simpleMerge(funcSigNs, newBundle.funcSigNs)
    simpleMerge(constantNs, newBundle.constantNs)
    simpleMerge(globalCellNs, newBundle.globalCellNs)
    simpleMerge(funcVerNs, newBundle.funcVerNs)
    mergeFunc(funcNs, newBundle.funcNs)
  }
}
