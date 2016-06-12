package uvm.refimpl

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl.itpr._
import uvm.ir.irbuilder._

object MuCtxIRBuilderPart {
  implicit def bundleNodeToHandle(bundleNode: BundleNode): MuBundleNode = {
    MuBundleNode(InternalTypes.IRNODEREF, BoxIRNode(Some(bundleNode)))
  }

  implicit def childNodeToHandle[T <: Identified, U <: MuIRNode](childNode: ChildNode[T]): U = childNode.obj match {
    case n: Type        => MuTypeNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: FuncSig     => MuFuncSigNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: Constant    => MuConstNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: GlobalCell  => MuGlobalNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: Function    => MuFuncNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: ExposedFunc => MuExpFuncNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: FuncVer     => MuFuncVerNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: BasicBlock  => MuBBNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: NorParam    => MuNorParamNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: ExcParam    => MuExcParamNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: InstResult  => MuInstResNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n: Instruction => MuInstNode(InternalTypes.IRNODEREF, BoxIRNode(Some(childNode))).asInstanceOf[U]
    case n => {
      throw new UvmRuntimeException("Oops. It looks like a ChildNode is holding an unexpected object. type: %s"
        .format(n.getClass.getName))
    }
  }
  
  implicit def bundleNodeHandleToBundleNode(handle: MuBundleNode): BundleNode = {
    handle.vb.node.get.asInstanceOf[BundleNode]
  }
  
  implicit def childNodeHandleToChildNode[T <: Identified](handle: MuChildNode): ChildNode[T] = {
    handle.vb.node.get.asInstanceOf[ChildNode[T]]
  }
  
  implicit def childNodeHandleToChildNodeMany[T <: Identified](handles: Seq[MuChildNode]): Seq[ChildNode[T]] = {
    handles.map(childNodeHandleToChildNode)
  }

  implicit def childNodeHandleToChildNodeOption[T <: Identified](handles: Option[MuChildNode]): Option[ChildNode[T]] = {
    handles.map(childNodeHandleToChildNode)
  }
}

/**
 * Mix-in to MuCtx to support the IR building API.
 */
trait MuCtxIRBuilderPart {
  import MuCtxIRBuilderPart._

  protected def microVM: MicroVM

  protected def addHandle[T <: MuValue](h: T): T

  @inline
  private def IRNODEREF = InternalTypes.IRNODEREF

  private def irBuilder = microVM.irBuilder

  def loadBundleFromNode(b: MuBundleNode): Unit = {
    require(!b.isNull, "bundle must not be NULL")
    val bundle = b.bundle
    microVM.addBundle(bundle)
  }

  def abortBundleNode(b: MuBundleNode): Unit = {
    // no op
  }
  
    def newBundle(): MuBundleNode = {
    addHandle(irBuilder.newBundle())
  }

  def getNode(b: MuBundleNode, id: Int): MuChildNode = {
    require(!b.isNull, "b must not be NULL")
    irBuilder.getNode(b, id)
  }

  def getID(b: MuBundleNode, node: MuChildNode): Int = {
    require(!b.isNull, "b must not be NULL")
    require(!node.isNull, "node must not be NULL")
    irBuilder.getID(b, node)
  }

  def setName(b: MuBundleNode, node: MuChildNode, name: String): Unit = {
    require(!b.isNull, "b must not be NULL")
    require(!node.isNull, "node must not be NULL")
    irBuilder.setName(b, node, name)
  }

  def newTypeInt           (b: MuBundleNode, len: Int): MuTypeNode            = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeInt(b, len))
  }

  def newTypeFloat         (b: MuBundleNode):           MuTypeNode          = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeFloat(b))
  }

  def newTypeDouble        (b: MuBundleNode):           MuTypeNode         = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeDouble(b))
  }

  def newTypeUPtr          (b: MuBundleNode):           MuTypeNode           = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeUPtr(b))
  }

  def newTypeUFuncPtr      (b: MuBundleNode):           MuTypeNode       = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeUFuncPtr(b))
  }

  def newTypeVoid          (b: MuBundleNode):           MuTypeNode           = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeVoid(b))
  }

  def newTypeRef           (b: MuBundleNode):           MuTypeNode            = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeRef(b))
  }

  def newTypeIRef          (b: MuBundleNode):           MuTypeNode           = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeIRef(b))
  }

  def newTypeWeakRef       (b: MuBundleNode):           MuTypeNode        = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeWeakRef(b))
  }

  def newTypeFuncRef       (b: MuBundleNode):           MuTypeNode        = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeFuncRef(b))
  }

  def newTypeTagRef64      (b: MuBundleNode):           MuTypeNode       = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeTagRef64(b))
  }

  def newTypeThreadRef     (b: MuBundleNode):           MuTypeNode      = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeThreadRef(b))
  }

  def newTypeStackRef      (b: MuBundleNode):           MuTypeNode       = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeStackRef(b))
  }

  def newTypeFrameCursorRef(b: MuBundleNode):           MuTypeNode = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeFrameCursorRef(b))
  }

  def newTypeIRNodeRef     (b: MuBundleNode):           MuTypeNode      = {
    require(!b.isNull, "b must not be NULL")
    addHandle(irBuilder.newTypeIRNodeRef(b))
  }

  def newTypeStruct(b: MuBundleNode, fieldTys: Seq[MuTypeNode]):                  MuTypeNode = {
    require(!b.isNull, "b must not be NULL")
    for((n,i) <- fieldTys.zipWithIndex) require(!n.isNull, "fieldTys[%d] must not be NULL".format(i))
    addHandle(irBuilder.newTypeStruct(b, fieldTys))
  }

  def newTypeHybrid(b: MuBundleNode, fixedTys: Seq[MuTypeNode], varTy: MuTypeNode): MuTypeNode = {
    require(!b.isNull, "b must not be NULL")
    for((n,i) <- fixedTys.zipWithIndex) require(!n.isNull, "fixedTys[%d] must not be NULL".format(i))
    require(!varTy.isNull, "varTy must not be NULL")
    addHandle(irBuilder.newTypeHybrid(b, fixedTys, varTy))
  }

  def newTypeArray (b: MuBundleNode, elemTy: MuTypeNode, len: Long):              MuTypeNode  = {
    require(!b.isNull, "b must not be NULL")
    require(!elemTy.isNull, "elemTy must not be NULL")
    addHandle(irBuilder.newTypeArray(b, elemTy, len))
  }

  def newTypeVector(b: MuBundleNode, elemTy: MuTypeNode, len: Long):              MuTypeNode = {
    require(!b.isNull, "b must not be NULL")
    require(!elemTy.isNull, "elemTy must not be NULL")
    addHandle(irBuilder.newTypeVector(b, elemTy, len))
  }

  def setTypeUPtr    (uptr:     MuTypeNode,     ty: MuTypeNode):            Unit = {
    require(!uptr.isNull, "uptr must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    irBuilder.setTypeUPtr(uptr, ty)
  }

  def setTypeRef     (ref:      MuTypeNode,      ty: MuTypeNode):     Unit = {
    require(!ref.isNull, "ref must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    irBuilder.setTypeRef(ref, ty)
  }

  def setTypeIRef    (iref:     MuTypeNode,     ty: MuTypeNode):     Unit = {
    require(!iref.isNull, "iref must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    irBuilder.setTypeIRef(iref, ty)
  }

  def setTypeWeakRef (weakref:  MuTypeNode,  ty: MuTypeNode):     Unit = {
    require(!weakref.isNull, "weakref must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    irBuilder.setTypeWeakRef(weakref, ty)
  }

  def setTypeUFuncPtr(ufuncptr: MuTypeNode, sig: MuFuncSigNode): Unit = {
    require(!ufuncptr.isNull, "ufuncptr must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    irBuilder.setTypeUFuncPtr(ufuncptr, sig)
  }

  def setTypeFuncRef (funcref:  MuTypeNode,  sig: MuFuncSigNode): Unit = {
    require(!funcref.isNull, "funcref must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    irBuilder.setTypeFuncRef(funcref, sig)
  }

  def newFuncSig(b: MuBundleNode, paramTys: Seq[MuTypeNode], retTys: Seq[MuTypeNode]): MuFuncSigNode = {
    require(!b.isNull, "b must not be NULL")
    for((n,i) <- paramTys.zipWithIndex) require(!n.isNull, "paramTys[%d] must not be NULL".format(i))
    for((n,i) <- retTys.zipWithIndex) require(!n.isNull, "retTys[%d] must not be NULL".format(i))
    addHandle(irBuilder.newFuncSig(b, paramTys, retTys))
  }

  def newConstInt   (b: MuBundleNode, ty: MuTypeNode, value: BigInt): MuConstNode    = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newConstInt(b, ty, value))
  }

  def newConstFloat (b: MuBundleNode, ty: MuTypeNode, value: Float):  MuConstNode  = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newConstFloat(b, ty, value))
  }

  def newConstDouble(b: MuBundleNode, ty: MuTypeNode, value: Double): MuConstNode = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newConstDouble(b, ty, value))
  }

  def newConstNull  (b: MuBundleNode, ty: MuTypeNode):                MuConstNode   = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newConstNull(b, ty))
  }

  def newConstSeq   (b: MuBundleNode, ty: MuTypeNode, elems: Seq[MuConstNode]): MuConstNode = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    for((n,i) <- elems.zipWithIndex) require(!n.isNull, "elems[%d] must not be NULL".format(i))
    addHandle(irBuilder.newConstSeq(b, ty, elems))
  }

  def newGlobalCell(b: MuBundleNode, ty: MuTypeNode): MuGlobalNode = {
    require(!b.isNull, "b must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newGlobalCell(b, ty))
  }

  def newFunc(b: MuBundleNode, sig: MuFuncSigNode): MuFuncNode = {
    require(!b.isNull, "b must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    addHandle(irBuilder.newFunc(b, sig))
  }

  def newFuncVer(b: MuBundleNode, func: MuFuncNode): MuFuncVerNode = {
    require(!b.isNull, "b must not be NULL")
    require(!func.isNull, "func must not be NULL")
    addHandle(irBuilder.newFuncVer(b, func))
  }

  def newExpFunc(b: MuBundleNode, func: MuFuncNode, callconv: Flag, cookie: MuConstNode): MuExpFuncNode = {
    require(!b.isNull, "b must not be NULL")
    require(!func.isNull, "func must not be NULL")
    require(!cookie.isNull, "cookie must not be NULL")
    addHandle(irBuilder.newExpFunc(b, func, callconv, cookie))
  }

  def newBB(fv: MuFuncVerNode): MuBBNode = {
    require(!fv.isNull, "fv must not be NULL")
    addHandle(irBuilder.newBB(fv))
  }

  def newNorParam(bb: MuBBNode, ty: MuTypeNode): MuNorParamNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    addHandle(irBuilder.newNorParam(bb, ty))
  }

  def newExcParam(bb: MuBBNode): MuExcParamNode = {
    require(!bb.isNull, "bb must not be NULL")
    addHandle(irBuilder.newExcParam(bb))
  }

  def newInstRes(inst: MuInstNode): MuInstResNode = {
    require(!inst.isNull, "inst must not be NULL")
    addHandle(irBuilder.newInstRes(inst))
  }

  def addDest(inst: MuInstNode, kind: DestKind.Value, bb: MuBBNode, vars: Seq[MuVarNode]): Unit = {
    require(!inst.isNull, "inst must not be NULL")
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- vars.zipWithIndex) require(!n.isNull, "vars[%d] must not be NULL".format(i))
    irBuilder.addDest(inst, kind, bb, vars)
  }

  def addKeepalives(inst: MuInstNode, lvs: Seq[MuLocalVarNode]): Unit = {
    require(!inst.isNull, "inst must not be NULL")
    for((n,i) <- lvs.zipWithIndex) require(!n.isNull, "lvs[%d] must not be NULL".format(i))
    irBuilder.addKeepalives(inst, lvs)
  }

  def newBinOp(bb: MuBBNode, optr: BinOptr.Value, ty: MuTypeNode, opnd1: MuVarNode, opnd2: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    require(!opnd1.isNull, "opnd1 must not be NULL")
    require(!opnd2.isNull, "opnd2 must not be NULL")
    addHandle(irBuilder.newBinOp(bb, optr, ty, opnd1, opnd2))
  }

  def newCmp(bb: MuBBNode, optr: CmpOptr.Value, ty: MuTypeNode, opnd1: MuVarNode, opnd2: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!ty.isNull, "ty must not be NULL")
    require(!opnd1.isNull, "opnd1 must not be NULL")
    require(!opnd2.isNull, "opnd2 must not be NULL")
    addHandle(irBuilder.newCmp(bb, optr, ty, opnd1, opnd2))
  }

  def newConv(bb: MuBBNode, optr: ConvOptr.Value, fromTy: MuTypeNode, toTy: MuTypeNode, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!fromTy.isNull, "fromTy must not be NULL")
    require(!toTy.isNull, "toTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newConv(bb, optr, fromTy, toTy, opnd))
  }

  def newSelect(bb: MuBBNode, condTy: MuTypeNode, opndTy: MuTypeNode, cond: MuVarNode, ifTrue: MuVarNode, ifFalse: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!condTy.isNull, "condTy must not be NULL")
    require(!opndTy.isNull, "opndTy must not be NULL")
    require(!cond.isNull, "cond must not be NULL")
    require(!ifTrue.isNull, "ifTrue must not be NULL")
    require(!ifFalse.isNull, "ifFalse must not be NULL")
    addHandle(irBuilder.newSelect(bb, condTy, opndTy, cond, ifTrue, ifFalse))
  }

  def newBranch(bb: MuBBNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    addHandle(irBuilder.newBranch(bb))
  }

  def newBranch2(bb: MuBBNode, cond: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!cond.isNull, "cond must not be NULL")
    addHandle(irBuilder.newBranch2(bb, cond))
  }

  def newSwitch(bb: MuBBNode, opndTy: MuTypeNode, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!opndTy.isNull, "opndTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newSwitch(bb, opndTy, opnd))
  }

  def addSwitchDest(inst: MuInstNode, key: MuVarNode, bb: MuBBNode, vars: Seq[MuVarNode]): Unit = {
    require(!inst.isNull, "inst must not be NULL")
    require(!key.isNull, "key must not be NULL")
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- vars.zipWithIndex) require(!n.isNull, "vars[%d] must not be NULL".format(i))
    irBuilder.addSwitchDest(inst, key, bb, vars)
  }

  def newCall(bb: MuBBNode, sig: MuFuncSigNode, callee: MuVarNode, args: Seq[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    require(!callee.isNull, "callee must not be NULL")
    for((n,i) <- args.zipWithIndex) require(!n.isNull, "args[%d] must not be NULL".format(i))
    addHandle(irBuilder.newCall(bb, sig, callee, args))
  }

  def newTailCall(bb: MuBBNode, sig: MuFuncSigNode, callee: MuVarNode, args: Seq[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    require(!callee.isNull, "callee must not be NULL")
    for((n,i) <- args.zipWithIndex) require(!n.isNull, "args[%d] must not be NULL".format(i))
    addHandle(irBuilder.newTailCall(bb, sig, callee, args))
  }

  def newRet(bb: MuBBNode, rvs: Seq[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- rvs.zipWithIndex) require(!n.isNull, "rvs[%d] must not be NULL".format(i))
    addHandle(irBuilder.newRet(bb, rvs))
  }

  def newThrow(bb: MuBBNode, exc: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!exc.isNull, "exc must not be NULL")
    addHandle(irBuilder.newThrow(bb, exc))
  }

  def newExtractValue(bb: MuBBNode, strTy: MuTypeNode, index: Int, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!strTy.isNull, "strTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newExtractValue(bb, strTy, index, opnd))
  }

  def newInsertValue(bb: MuBBNode, strTy: MuTypeNode, index: Int, opnd: MuVarNode, newVal: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!strTy.isNull, "strTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    require(!newVal.isNull, "newVal must not be NULL")
    addHandle(irBuilder.newInsertValue(bb, strTy, index, opnd, newVal))
  }

  def newExtractElement(bb: MuBBNode, seqTy: MuTypeNode, indTy: MuTypeNode, opnd: MuVarNode, index: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!seqTy.isNull, "seqTy must not be NULL")
    require(!indTy.isNull, "indTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    require(!index.isNull, "index must not be NULL")
    addHandle(irBuilder.newExtractElement(bb, seqTy, indTy, opnd, index))
  }

  def newInsertElement(bb: MuBBNode, seqTy: MuTypeNode, indTy: MuTypeNode, opnd: MuVarNode, index: MuVarNode, newVal: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!seqTy.isNull, "seqTy must not be NULL")
    require(!indTy.isNull, "indTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    require(!index.isNull, "index must not be NULL")
    require(!newVal.isNull, "newVal must not be NULL")
    addHandle(irBuilder.newInsertElement(bb, seqTy, indTy, opnd, index, newVal))
  }

  def newShuffleVector(bb: MuBBNode, vecTy: MuTypeNode, maskTy: MuTypeNode, vec1: MuVarNode, vec2: MuVarNode, mask: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!vecTy.isNull, "vecTy must not be NULL")
    require(!maskTy.isNull, "maskTy must not be NULL")
    require(!vec1.isNull, "vec1 must not be NULL")
    require(!vec2.isNull, "vec2 must not be NULL")
    require(!mask.isNull, "mask must not be NULL")
    addHandle(irBuilder.newShuffleVector(bb, vecTy, maskTy, vec1, vec2, mask))
  }

  def newNew(bb: MuBBNode, allocTy: MuTypeNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!allocTy.isNull, "allocTy must not be NULL")
    addHandle(irBuilder.newNew(bb, allocTy))
  }

  def newNewHybrid(bb: MuBBNode, allocTy: MuTypeNode, lenTy: MuTypeNode, len: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!allocTy.isNull, "allocTy must not be NULL")
    require(!lenTy.isNull, "lenTy must not be NULL")
    require(!len.isNull, "len must not be NULL")
    addHandle(irBuilder.newNewHybrid(bb, allocTy, lenTy, len))
  }

  def newAlloca(bb: MuBBNode, allocTy: MuTypeNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!allocTy.isNull, "allocTy must not be NULL")
    addHandle(irBuilder.newAlloca(bb, allocTy))
  }

  def newAllocaHybrid(bb: MuBBNode, allocTy: MuTypeNode, lenTy: MuTypeNode, len: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!allocTy.isNull, "allocTy must not be NULL")
    require(!lenTy.isNull, "lenTy must not be NULL")
    require(!len.isNull, "len must not be NULL")
    addHandle(irBuilder.newAllocaHybrid(bb, allocTy, lenTy, len))
  }

  def newGetIRef(bb: MuBBNode, refTy: MuTypeNode, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newGetIRef(bb, refTy, opnd))
  }

  def newGetFieldIRef(bb: MuBBNode, isPtr: Boolean, refTy: MuTypeNode, index: Int, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newGetFieldIRef(bb, isPtr, refTy, index, opnd))
  }

  def newGetElemIRef(bb: MuBBNode, isPtr: Boolean, refTy: MuTypeNode, indTy: MuTypeNode, opnd: MuVarNode, index: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!indTy.isNull, "indTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    require(!index.isNull, "index must not be NULL")
    addHandle(irBuilder.newGetElemIRef(bb, isPtr, refTy, indTy, opnd, index))
  }

  def newShiftIRef(bb: MuBBNode, isPtr: Boolean, refTy: MuTypeNode, offTy: MuTypeNode, opnd: MuVarNode, offset: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!offTy.isNull, "offTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    require(!offset.isNull, "offset must not be NULL")
    addHandle(irBuilder.newShiftIRef(bb, isPtr, refTy, offTy, opnd, offset))
  }

  def newGetVarPartIRef(bb: MuBBNode, isPtr: Boolean, refTy: MuTypeNode, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newGetVarPartIRef(bb, isPtr, refTy, opnd))
  }

  def newLoad(bb: MuBBNode, isPtr: Boolean, ord: MemoryOrder.Value, refTy: MuTypeNode, loc: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!loc.isNull, "loc must not be NULL")
    addHandle(irBuilder.newLoad(bb, isPtr, ord, refTy, loc))
  }

  def newStore(bb: MuBBNode, isPtr: Boolean, ord: MemoryOrder.Value, refTy: MuTypeNode, loc: MuVarNode, newVal: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!loc.isNull, "loc must not be NULL")
    require(!newVal.isNull, "newVal must not be NULL")
    addHandle(irBuilder.newStore(bb, isPtr, ord, refTy, loc, newVal))
  }

  def newCmpXchg(bb: MuBBNode, isPtr: Boolean, isWeak: Boolean, ordSucc: MemoryOrder.Value, ordFail: MemoryOrder.Value,
    refTy: MuTypeNode, loc: MuVarNode, expected: MuVarNode, desired: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!loc.isNull, "loc must not be NULL")
    require(!expected.isNull, "expected must not be NULL")
    require(!desired.isNull, "desired must not be NULL")
    addHandle(irBuilder.newCmpXchg(bb, isPtr, isWeak, ordSucc, ordFail, refTy, loc, expected, desired))
  }

  def newAtomicRMW(bb: MuBBNode, isPtr: Boolean, ord: MemoryOrder.Value, optr: AtomicRMWOptr.Value, refTy: MuTypeNode, loc: MuVarNode, opnd: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!refTy.isNull, "refTy must not be NULL")
    require(!loc.isNull, "loc must not be NULL")
    require(!opnd.isNull, "opnd must not be NULL")
    addHandle(irBuilder.newAtomicRMW(bb, isPtr, ord, optr, refTy, loc, opnd))
  }

  def newFence(bb: MuBBNode, ord: MemoryOrder.Value): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    addHandle(irBuilder.newFence(bb, ord))
  }

  def newTrap(bb: MuBBNode, retTys: Seq[MuTypeNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- retTys.zipWithIndex) require(!n.isNull, "retTys[%d] must not be NULL".format(i))
    addHandle(irBuilder.newTrap(bb, retTys))
  }

  def newWatchPoint(bb: MuBBNode, wpid: Int, retTys: Seq[MuTypeNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- retTys.zipWithIndex) require(!n.isNull, "retTys[%d] must not be NULL".format(i))
    addHandle(irBuilder.newWatchPoint(bb, wpid, retTys))
  }

  def newWPBranch(bb: MuBBNode, wpid: Int): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    addHandle(irBuilder.newWPBranch(bb, wpid))
  }

  def newCCall(bb: MuBBNode, callConv: Flag, calleeTy: MuTypeNode, sig: MuFuncSigNode, callee: MuVarNode, args: Seq[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!calleeTy.isNull, "calleeTy must not be NULL")
    require(!sig.isNull, "sig must not be NULL")
    require(!callee.isNull, "callee must not be NULL")
    for((n,i) <- args.zipWithIndex) require(!n.isNull, "args[%d] must not be NULL".format(i))
    addHandle(irBuilder.newCCall(bb, callConv, calleeTy, sig, callee, args))
  }

  def newNewThread(bb: MuBBNode, stack: MuVarNode, threadLocal: Option[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!stack.isNull, "stack must not be NULL")
    addHandle(irBuilder.newNewThread(bb, stack, threadLocal))
  }

  def newSwapStackRet(bb: MuBBNode, swappee: MuVarNode, retTys: Seq[MuTypeNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!swappee.isNull, "swappee must not be NULL")
    for((n,i) <- retTys.zipWithIndex) require(!n.isNull, "retTys[%d] must not be NULL".format(i))
    addHandle(irBuilder.newSwapStackRet(bb, swappee, retTys))
  }

  def newSwapStackKill(bb: MuBBNode, swappee: MuVarNode): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    require(!swappee.isNull, "swappee must not be NULL")
    addHandle(irBuilder.newSwapStackKill(bb, swappee))
  }

  def setNewStackPassValues(inst: MuInstNode, tys: Seq[MuTypeNode], vars: Seq[MuVarNode]): Unit = {
    require(!inst.isNull, "inst must not be NULL")
    for((n,i) <- tys.zipWithIndex) require(!n.isNull, "tys[%d] must not be NULL".format(i))
    for((n,i) <- vars.zipWithIndex) require(!n.isNull, "vars[%d] must not be NULL".format(i))
    irBuilder.setNewStackPassValues(inst, tys, vars)
  }

  def setNewStackThrowExc(inst: MuInstNode, exc: MuVarNode): Unit = {
    require(!inst.isNull, "inst must not be NULL")
    require(!exc.isNull, "exc must not be NULL")
    irBuilder.setNewStackThrowExc(inst, exc)
  }

  def newCommInst(bb: MuBBNode, opcode: Int, flags: Seq[Flag], tys: Seq[MuTypeNode], sigs: Seq[MuFuncSigNode], args: Seq[MuVarNode]): MuInstNode = {
    require(!bb.isNull, "bb must not be NULL")
    for((n,i) <- tys.zipWithIndex) require(!n.isNull, "tys[%d] must not be NULL".format(i))
    for((n,i) <- sigs.zipWithIndex) require(!n.isNull, "sigs[%d] must not be NULL".format(i))
    for((n,i) <- args.zipWithIndex) require(!n.isNull, "args[%d] must not be NULL".format(i))
    addHandle(irBuilder.newCommInst(bb, opcode, flags, tys, sigs, args))
  }

// BEGIN: auto-generated code
  // END: auto-generated code
}