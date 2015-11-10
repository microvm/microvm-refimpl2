package uvm.refimpl

import java.io.Reader

import uvm.refimpl.mem.Mutator

/** Common implementations of operations on the MicroVM. Used by both the MuCtx and many comminsts. */
object MetaOperations {
  /** Load a Mu IR bundle */
  def loadBundle(r: Reader)(implicit microVM: MicroVM): Unit = {
    val bundle = microVM.irReader.read(r, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  /** Load a Mu IR bundle */
  def loadBundle(s: String)(implicit microVM: MicroVM): Unit = {
    val bundle = microVM.irReader.read(s, microVM.globalBundle)
    microVM.addBundle(bundle)
  }

  /** Load a HAIL script */
  def loadHail(r: Reader)(implicit microVM: MicroVM, mutator: Mutator): Unit = {
    microVM.hailScriptLoader.loadHail(r)
  }

  /** Load a HAIL script */
  def loadHail(s: String)(implicit microVM: MicroVM, mutator: Mutator): Unit = {
    microVM.hailScriptLoader.loadHail(s)
  }
}