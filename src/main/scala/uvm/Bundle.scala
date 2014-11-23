package uvm

import uvm.types._
import uvm.ssavariables._

class Bundle {
  /*
   * There is a hierarchy of namespaces. A subnode is a subset of the parent.
   * 
   * + allNs                // All Identified entities
   *   + typeNs             // All types
   *   + funcSigNs          // All function signatures
   *   + funcVerNs          // All function versions
   *   + varNs              // All variables, global or local
   *     + globalVarNs      // Global variables
   *       + constantNs     // Constants
   *       + globalCellNs   // Global cells
   *       + funcNs         // Functions
   *     + localVarNs       // Local variables (per function version)
   *   + bbNs               // Basic blocks (per function version)
   * 
   * TODO: Should there be a global "basic block ns for all function versions"?
   */
  
  val allNs = new SimpleNamespace[Identified]()
  
  val typeNs = new SimpleNamespace[Type]()
  val funcSigNs = new SimpleNamespace[FuncSig]()
  val funcVerNs = new SimpleNamespace[FuncVer]()

  val varNs = new SimpleNamespace[SSAVariable]()
  val globalVarNs = new SimpleNamespace[GlobalVariable]()
  val constantNs = new SimpleNamespace[Constant]()
  val globalCellNs = new SimpleNamespace[GlobalCell]()
  val funcNs = new SimpleNamespace[Function]()

  private def simpleMerge[T <: Identified](oldNs: Namespace[T], newNs: Namespace[T]) {
    for (cand <- newNs.all) {
      if (!cand.isInstanceOf[Function] || oldNs.get(cand.id) == None) {
        // Function merging happens separately. Only add a function if it does not redefine an old one.
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
    simpleMerge(allNs, newBundle.allNs)
    simpleMerge(typeNs, newBundle.typeNs)
    simpleMerge(funcSigNs, newBundle.funcSigNs)
    simpleMerge(funcVerNs, newBundle.funcVerNs)
    simpleMerge(varNs, newBundle.varNs)
    simpleMerge(globalVarNs, newBundle.globalVarNs)
    simpleMerge(constantNs, newBundle.constantNs)
    simpleMerge(globalCellNs, newBundle.globalCellNs)
    mergeFunc(funcNs, newBundle.funcNs)
  }
}
