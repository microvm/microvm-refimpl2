package uvm

import uvm.types._
import uvm.ssavariables._

class Bundle {
  /*
   * There is a hierarchy of namespaces. A subnode is a subset of the parent.
   * 
   * + allNs                // All globally Identified entities
   *   + typeNs             // All types
   *   + funcSigNs          // All function signatures
   *   + funcVerNs          // All function versions
   *   + varNs              // All variables, global or local
   *     + globalVarNs      // Global variables
   *       + constantNs     // Constants
   *       + globalCellNs   // Global cells
   *       + funcNs         // Functions
   *       + expFuncNs      // Exposed functions
   * 
   * These namespaces are local, i.e. they cannot be directly looked up from a bundle:
   * + bbNs                 // Basic blocks (per function version)
   * + localVarNs           // Local variables (per basic block)
   */

  val allNs = new NestedNamespace[Identified](None)

  val typeNs = allNs.makeSubSpace[Type]()
  val funcSigNs = allNs.makeSubSpace[FuncSig]()
  val funcVerNs = allNs.makeSubSpace[FuncVer]()
  val varNs = allNs.makeSubSpace[SSAVariable]()

  val globalVarNs = varNs.makeSubSpace[GlobalVariable]()
  val constantNs = globalVarNs.makeSubSpace[Constant]()
  val globalCellNs = globalVarNs.makeSubSpace[GlobalCell]()
  val funcNs = globalVarNs.makeSubSpace[Function]()
  val expFuncNs = globalVarNs.makeSubSpace[ExposedFunc]()

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

  def merge(newBundle: Bundle) {
    // Only merge leaves
    simpleMerge(typeNs, newBundle.typeNs)
    simpleMerge(funcSigNs, newBundle.funcSigNs)
    simpleMerge(funcVerNs, newBundle.funcVerNs)
    simpleMerge(constantNs, newBundle.constantNs)
    simpleMerge(globalCellNs, newBundle.globalCellNs)
    simpleMerge(funcNs, newBundle.funcNs)
    simpleMerge(expFuncNs, newBundle.expFuncNs)
  }
}
