package uvm.refimpl

import uvm._
import uvm.refimpl.itpr._

/**
 * Mix-in to MuCtx to support the IR building API.
 */
trait MuCtxIRBuilderPart {
  protected def microVM: MicroVM
  
  protected def addHandle[T <: MuValue](h: T): T

  @inline
  def IRNODEREF = InternalTypes.IRNODEREF
  
  def newBundle(): MuBundleNode = {
    val b = new TrantientBundle()
    val node = new BundleNode(b)
    addHandle(MuBundleNode(IRNODEREF, BoxIRNode(Some(node))))
  }

  def loadBundleFromNode(b: MuBundleNode): Unit = {
    require(!b.isNull, "bundle must not be NULL")
    val bundle = b.bundle
    microVM.addBundle(bundle)
  }
  
  def abortBundleNode(b: MuBundleNode): Unit = {
    // no op
  }
  
  def getNode(b: MuBundleNode, id: Int): MuIRNode = {
    ???
  }
  
  def getID(node: MuChildNode): Int = {
    require(!node.isNull)
    node.obj.id
  }
}