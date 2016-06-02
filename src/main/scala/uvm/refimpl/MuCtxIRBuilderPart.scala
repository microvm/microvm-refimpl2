package uvm.refimpl

import uvm._
import uvm.refimpl.itpr._
import uvm.ir.irbuilder._

/**
 * Mix-in to MuCtx to support the IR building API.
 */
trait MuCtxIRBuilderPart {
  protected def microVM: MicroVM
  
  protected def addHandle[T <: MuValue](h: T): T

  @inline
  private def IRNODEREF = InternalTypes.IRNODEREF
  
  private def irBuilder = microVM.irBuilder
  
  private def makeConcreteMuIRNode(node: IRNode): MuIRNode = {
    ???
  }
  
  def newBundle(): MuBundleNode = {
    val node = irBuilder.newBundle()
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
    require(!b.isNull, "bundle must not be NULL")
    val node = irBuilder.getNode(b.vb.node.get.asInstanceOf[BundleNode], id)
    addHandle(makeConcreteMuIRNode(node))
  }
  
  def getID(node: MuChildNode): Int = {
    require(!node.isNull)
    node.obj.id
  }
}