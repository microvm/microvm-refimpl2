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
 	 *   + globalVarNs      // Global variables
 	 *     + constantNs     // Constants
 	 *     + globalCellNs   // Global cells
 	 *     + funcNs         // Functions
 	 *     + expFuncNs      // Exposed functions
   *   + bbNs               // Basic blocks (per function version)
   *   + instNs							// All instructions
   *   	 + localInstNs  		// Instructions in a basic block (per basic block)
   * 
   * bbNs and localVarNs are part of particular FuncVers and BBs.
   */

  val allNs = new NestedNamespace[Identified](None, "Mu entity")

  val typeNs = allNs.makeSubSpace[Type]("type")
  val funcSigNs = allNs.makeSubSpace[FuncSig]("function signature")
  val funcVerNs = allNs.makeSubSpace[FuncVer]("function version")

  val globalVarNs = allNs.makeSubSpace[GlobalVariable]("global variable")
  val constantNs = globalVarNs.makeSubSpace[Constant]("constant")
  val globalCellNs = globalVarNs.makeSubSpace[GlobalCell]("global cell")
  val funcNs = globalVarNs.makeSubSpace[Function]("function")
  val expFuncNs = globalVarNs.makeSubSpace[ExposedFunc]("exposed function")
  
  val instNs = allNs.makeSubSpace[Instruction]("instruction")
  
  val sourceInfoRepo = new SourceInfoRepo()
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
  private def simpleMerge[T <: Identified](oldNs: Namespace[T], newNs: Namespace[T], newSourceInfoRepo: SourceInfoRepo) {
    for (cand <- newNs.all) {
      try {
        oldNs.add(cand)
//        def assertPresent[T <: Identified](ns: NestedNamespace[T], obj: T): Unit = {
//          assert(ns.get(obj.id) == Some(obj))
//          if (obj.id == 65731) {
//            printf("Obj[65731] found in ns " + ns)
//          }
//          ns.maybeParent match {
//            case None =>
//            case Some(ns2) =>
//              assertPresent(ns2, obj)
//          }
//        }
//        assertPresent(oldNs.asInstanceOf[NestedNamespace[T]], cand)
      } catch {
        case e: NameConflictException => throw e.toIllegalRedefinitionException(newSourceInfoRepo, sourceInfoRepo)
      }
    }
  }

  private def redefineFunctions(newNs: Namespace[FuncVer]) {
    for (fv <- newNs.all) {
      fv.func.versions = fv :: fv.func.versions
    }
  }
  
  private def mergeLocalNamespaces(newBundle: TrantientBundle) {
    try {
    for (fv <- newBundle.funcVerNs.all) {
      fv.bbNs.reparent(allNs)
      for (bb <- fv.bbs) {
        bb.localVarNs.reparent(allNs)
        bb.localInstNs.reparent(allNs)
      }
    }
    } catch {
      case e: NameConflictException => throw e.toIllegalRedefinitionException(newBundle.sourceInfoRepo, sourceInfoRepo)
    }
  }

  def merge(newBundle: TrantientBundle) {
    // Only merge leaves
    simpleMerge(typeNs, newBundle.typeNs, newBundle.sourceInfoRepo)
    simpleMerge(funcSigNs, newBundle.funcSigNs, newBundle.sourceInfoRepo)
    simpleMerge(funcVerNs, newBundle.funcVerNs, newBundle.sourceInfoRepo)
    simpleMerge(constantNs, newBundle.constantNs, newBundle.sourceInfoRepo)
    simpleMerge(globalCellNs, newBundle.globalCellNs, newBundle.sourceInfoRepo)
    simpleMerge(funcNs, newBundle.funcNs, newBundle.sourceInfoRepo)
    simpleMerge(expFuncNs, newBundle.expFuncNs, newBundle.sourceInfoRepo)
    simpleMerge(instNs, newBundle.instNs, newBundle.sourceInfoRepo)

    redefineFunctions(newBundle.funcVerNs)
    
    mergeLocalNamespaces(newBundle)
    
    sourceInfoRepo.merge(newBundle.sourceInfoRepo)
  }

}