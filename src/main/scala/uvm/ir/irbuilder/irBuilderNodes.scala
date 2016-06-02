package uvm.ir.irbuilder

import uvm._
import uvm.types._
import uvm.ssavariables._
import scala.collection.mutable.ArrayBuffer

/**
 * Abstract class for all nodes.
 * @param obj The contained object.
 */
abstract class IRNode

/**
 * Node for the newly-built bundle.
 * @param bundle The bundle
 */
class BundleNode(val bundle: TrantientBundle) extends IRNode {
  /**
   * Nodes not in this bundle, but is referred to, such as types defined in previously loaded bundles.
   * In a non-garbage-collected implementation, these nodes need to be explicitly freed when the bundle node is loaded
   * or aborted.
   */
  val externalNodes = new ArrayBuffer[IRNode]()
}

/**
 * Any children of BundleNode.
 * @param obj the underlying object
 */
class ChildNode[+T <: Identified](val obj: T) extends IRNode
