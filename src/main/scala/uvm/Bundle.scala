package uvm

import uvm.types._
import uvm.ssavariables._
import scala.collection.mutable.HashMap

abstract class Bundle {
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
}

/**
 * This kind of bundle is generated when parsing a .UIR file.
 * <p>
 * In this kind of bundle, a Function does not have a FuncVer as its version.
 * The funcNs only contains new functions declared in this bundle, not existing
 * functions declared previously. When this bundle is merged with the global bundle,
 * both funcNs and funcVerNs are simply merged, and new FuncVer objects become the
 * newest version of the Function, whether the Function is newly declared or not.
 */
class TrantientBundle extends Bundle {
  /**
   * All functions (declared here or previously) that are defined in this bundle.
   * <p>
   * Mainly for debugging purpose.
   */
  //val defFuncNs = new SimpleNamespace[Function]
}

/**
 * This kind of bundle holds the global state. Functions and versions are fully merged.
 */
class GlobalBundle extends Bundle {
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

  private def redefineFunctions(newNs: Namespace[FuncVer]) {
    for (fv <- newNs.all) {
      fv.func.versions = fv :: fv.func.versions
    }
  }

  def merge(newBundle: TrantientBundle) {
    // Only merge leaves
    simpleMerge(typeNs, newBundle.typeNs)
    simpleMerge(funcSigNs, newBundle.funcSigNs)
    simpleMerge(funcVerNs, newBundle.funcVerNs)
    simpleMerge(constantNs, newBundle.constantNs)
    simpleMerge(globalCellNs, newBundle.globalCellNs)
    simpleMerge(funcNs, newBundle.funcNs)
    simpleMerge(expFuncNs, newBundle.expFuncNs)

    redefineFunctions(newBundle.funcVerNs)
  }

}