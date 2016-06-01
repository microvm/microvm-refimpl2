package uvm.refimpl

import scala.collection.mutable.ArrayBuffer

import uvm.utils.IDFactory
import uvm._
import uvm.types._
import uvm.ssavariables._

object IRBuilder {
  type BN = BundleNode
  type CN[+T <: Identified] = ChildNode[T]

  implicit def BundleNodeAutoUnbox(node: BN): TrantientBundle = {
    node.bundle
  }

  implicit def childNodeAutoUnbox[T <: Identified](node: CN[T]): T = {
    node.obj
  }

  implicit def childNodeAutoUnboxMany[T <: Identified](nodes: Seq[CN[T]]): Seq[T] = {
    nodes.map(_.obj)
  }
}

class IRBuilder(idFactory: IDFactory)(implicit microVM: MicroVM) {
  import IRBuilder._
  import uvm.RichIdentifiedSettable._

  private def globalBundle = microVM.globalBundle
  private def nextID(): Int = idFactory.getID()

  private def newObj[T <: IdentifiedSettable, U >: T <: Identified](ns: Namespace[U], obj: T): CN[T] = {
    obj := nextID()
    ns.add(obj)
    new CN(obj)
  }

  def newBundle(): BN = {
    val b = new TrantientBundle()
    val node = new BN(b)
    node
  }

  def loadBundleFromNode(b: BN): Unit = {
    val bundle = b.bundle
    microVM.addBundle(bundle)
  }

  def abortBundleNode(b: BN): Unit = {
    // no-op for this implementation
  }

  def getNode(b: BN, id: Int): CN[_] = {
    val ent = globalBundle.allNs.get(id).getOrElse {
      throw new UvmRuntimeException("No entity has id %d".format(id))
    }

    new ChildNode(ent)
  }

  def getID(b: BN, node: CN[Identified]): Int = {
    node.id
  }

  def setName(b: BN, node: CN[IdentifiedSettable], name: String): Unit = {
    node.name = Some(name)
  }

  // format: OFF
  def newTypeInt           (b: BN, len: Int): CN[TypeInt]            = newObj(b.typeNs, TypeInt(len))
  def newTypeFloat         (b: BN):           CN[TypeFloat]          = newObj(b.typeNs, TypeFloat())
  def newTypeDouble        (b: BN):           CN[TypeDouble]         = newObj(b.typeNs, TypeDouble())
  def newTypeUPtr          (b: BN):           CN[TypeUPtr]           = newObj(b.typeNs, TypeUPtr(null))
  def newTypeUFuncPtr      (b: BN):           CN[TypeUFuncPtr]       = newObj(b.typeNs, TypeUFuncPtr(null))
  def newTypeVoid          (b: BN):           CN[TypeVoid]           = newObj(b.typeNs, TypeVoid())
  def newTypeRef           (b: BN):           CN[TypeRef]            = newObj(b.typeNs, TypeRef(null))
  def newTypeIRef          (b: BN):           CN[TypeIRef]           = newObj(b.typeNs, TypeIRef(null))
  def newTypeWeakRef       (b: BN):           CN[TypeWeakRef]        = newObj(b.typeNs, TypeWeakRef(null))
  def newTypeFuncRef       (b: BN):           CN[TypeFuncRef]        = newObj(b.typeNs, TypeFuncRef(null))
  def newTypeTagRef64      (b: BN):           CN[TypeTagRef64]       = newObj(b.typeNs, TypeTagRef64())
  def newTypeThreadRef     (b: BN):           CN[TypeThreadRef]      = newObj(b.typeNs, TypeThreadRef())
  def newTypeStackRef      (b: BN):           CN[TypeStackRef]       = newObj(b.typeNs, TypeStackRef())
  def newTypeFrameCursorRef(b: BN):           CN[TypeFrameCursorRef] = newObj(b.typeNs, TypeFrameCursorRef())
  def newTypeIRNodeRef     (b: BN):           CN[TypeIRNodeRef]      = newObj(b.typeNs, TypeIRNodeRef())

  def newTypeStruct(b: BN, fieldTys: Seq[CN[Type]]):                  CN[TypeStruct] = newObj(b.typeNs, TypeStruct(fieldTys))
  def newTypeHybrid(b: BN, fixedTys: Seq[CN[Type]], varTy: CN[Type]): CN[TypeHybrid] = newObj(b.typeNs, TypeHybrid(fixedTys, varTy))
  def newTypeArray (b: BN, elemTy: CN[Type], len: Long):              CN[TypeArray]  = newObj(b.typeNs, TypeArray(elemTy, len))
  def newTypeVector(b: BN, elemTy: CN[Type], len: Long):              CN[TypeVector] = newObj(b.typeNs, TypeVector(elemTy, len))

  def setTypeUPtr    (uptr:     CN[TypeUPtr],     ty: CN[Type]):            Unit = uptr.ty = ty
  def setTypeRef     (ref:      CN[TypeRef],      ty: ChildNode[Type]):     Unit = ref.ty = ty
  def setTypeIRef    (iref:     CN[TypeIRef],     ty: ChildNode[Type]):     Unit = iref.ty = ty
  def setTypeWeakRef (weakref:  CN[TypeWeakRef],  ty: ChildNode[Type]):     Unit = weakref.ty = ty
  def setTypeUFuncPtr(ufuncptr: CN[TypeUFuncPtr], sig: ChildNode[FuncSig]): Unit = ufuncptr.sig = sig
  def setTypeFuncRef (funcref:  CN[TypeFuncRef],  sig: ChildNode[FuncSig]): Unit = funcref.sig = sig
  // format: ON

  def newFuncSig(b: BN, paramTys: Seq[CN[Type]], retTys: Seq[CN[Type]]): CN[FuncSig] = {
    newObj(b.funcSigNs, new FuncSig(paramTys, retTys))
  }
  
  // format: OFF
  def newConstInt   (b: BN, ty: CN[Type], value: BigInt): CN[ConstInt]    = newObj(b.constantNs, ConstInt(ty, value))
  def newConstFloat (b: BN, ty: CN[Type], value: Float):  CN[ConstFloat]  = newObj(b.constantNs, ConstFloat(ty, value))
  def newConstDouble(b: BN, ty: CN[Type], value: Double): CN[ConstDouble] = newObj(b.constantNs, ConstDouble(ty, value))
  def newConstNull  (b: BN, ty: CN[Type]):                CN[ConstNull]   = newObj(b.constantNs, ConstNull(ty))
  def newConstSeq   (b: BN, ty: CN[Type], elems: Seq[CN[Constant]]): CN[ConstSeq] = newObj(b.constantNs, ConstSeq(ty, elems))
  // format: ON

  def newGlobalCell(b: BN, ty: CN[Type]): CN[GlobalCell] = newObj(b.globalCellNs, new GlobalCell(ty))

  def newFunc(b: BN, sig: CN[FuncSig]): CN[Function] = {
    val func = new Function()
    func.sig = sig
    newObj(b.funcNs, func)
  }
  
  def newFuncVer(b: BN, func: CN[Function]): CN[FuncVer] = {
    val funcVer = new FuncVer()
    funcVer.func = func
    funcVer.bbs = new ArrayBuffer()
    funcVer.bbNs = b.allNs.makeSubSpace[BasicBlock]("basic block")
    newObj(b.funcVerNs, funcVer)
  }
  
  def newExpFunc(b: BN, func: CN[Function], callconv: Flag, cookie: CN[ConstInt]): CN[ExposedFunc] = {
    val expFunc = new ExposedFunc(func, callconv, cookie)
    newObj(b.expFuncNs, expFunc)
  }
  
  def newBB(fv: CN[FuncVer]): CN[BasicBlock] = {
    val bb = new BasicBlock()
    
    bb.norParams = new ArrayBuffer()
    bb.excParam = None
    bb.insts = new ArrayBuffer()
    
    val allNs = fv.bbNs.maybeParent.get.asInstanceOf[NestedNamespace[Identified]]

    bb.localVarNs = allNs.makeSubSpace[LocalVariable]("local variable")
    bb.localInstNs = allNs.makeSubSpace[Instruction]("instruction")
    
    newObj(fv.bbNs, bb)
  }
  
  def newNorParam(bb: CN[BasicBlock], ty: CN[Type]): CN[NorParam] = {
    val np = NorParam(ty)
    bb.norParams += np
    newObj(bb.localVarNs, np)
  }
  
  def newExcParam(bb: CN[BasicBlock]): CN[ExcParam] = {
    val ep = ExcParam()
    bb.excParam = Some(ep)
    newObj(bb.localVarNs, ep)
  }
  
  def newInstRes(inst: CN[Instruction]): CN[InstResult] = {
    val nResults = inst.results.size
    val ir = InstResult(inst, nResults)
    inst.results += ir
    newObj(inst.bb.localVarNs, ir)
  }
  
  private def newInst[T <: Instruction](bb: BasicBlock, inst: T): CN[T] = {
    inst.bb = bb
    inst.results = new ArrayBuffer(2)
    newObj(bb.localInstNs, inst)
  }
}