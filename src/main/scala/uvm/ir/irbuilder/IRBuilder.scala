package uvm.ir.irbuilder

import scala.collection.mutable.ArrayBuffer
import uvm.utils.IDFactory
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.comminsts.CommInsts

object DestKind extends Enumeration {
  val NORMAL, EXCEPT, TRUE, FALSE, DEFAULT, DISABLED, ENABLED = Value
}

private[irbuilder] object IRBuilder {
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

class IRBuilder(globalBundle: GlobalBundle, idFactory: IDFactory) {
  import IRBuilder._
  import uvm.RichIdentifiedSettable._

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

  def getNode(b: BN, id: Int): CN[_ <: Identified] = {
    val ent = globalBundle.allNs.get(id).getOrElse {
      throw new IllegalArgumentException("No entity has id %d".format(id))
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
    val funcVer = new FuncVer(func)
    funcVer.bbs = new ArrayBuffer()
    funcVer.bbNs = b.allNs.makeSubSpace[BasicBlock]("basic block")
    newObj(b.funcVerNs, funcVer)
  }

  def newExpFunc(b: BN, func: CN[Function], callconv: Flag, cookie: CN[ConstInt]): CN[ExposedFunc] = {
    val expFunc = new ExposedFunc(func, callconv, cookie)
    newObj(b.expFuncNs, expFunc)
  }

  def newBB(fv: CN[FuncVer]): CN[BasicBlock] = {
    val bb = new BasicBlock(fv)

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

  private def getOrCreateExcClause(inst: HasExcClause): ExcClause = {
    inst.excClause.getOrElse {
      val newExcClause = ExcClause(null, null)
      inst.excClause = Some(newExcClause)
      newExcClause
    }
  }

  def addDest(inst: CN[Instruction], kind: DestKind.Value, bb: CN[BasicBlock], vars: Seq[CN[SSAVariable]]): Unit = {
    val destClause = DestClause(bb, vars)
    kind match {
      case DestKind.NORMAL => inst.obj match {
        case i: HasExcClause => getOrCreateExcClause(i).nor = destClause
        case i => {
          throw new IllegalArgumentException(
            "NORMAL requires instruction with exception clause. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.EXCEPT => inst.obj match {
        case i: HasExcClause => getOrCreateExcClause(i).exc = destClause
        case i: InstWatchPoint => {
          i.exc = Some(destClause)
        }
        case i => {
          throw new IllegalArgumentException(
            "EXCEPT requires WATCHPOINT or instruction with exception clause. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.TRUE => inst.obj match {
        case i: InstBranch2 => i.ifTrue = destClause
        case i => {
          throw new IllegalArgumentException(
            "TRUE requires BRANCH2. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.FALSE => inst.obj match {
        case i: InstBranch2 => i.ifFalse = destClause
        case i => {
          throw new IllegalArgumentException(
            "FALSE requires BRANCH2. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.DEFAULT => inst.obj match {
        case i: InstSwitch => i.defDest = destClause
        case i => {
          throw new IllegalArgumentException(
            "DEFAULT requires SWITCH. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.DISABLED => inst.obj match {
        case i: InstWatchPoint => i.dis = destClause
        case i: InstWPBranch   => i.dis = destClause
        case i => {
          throw new IllegalArgumentException(
            "DISABLED requires WATCHPOINT OR WPBRANCH. Found: %s".format(i.getClass.getName))
        }
      }
      case DestKind.ENABLED => inst.obj match {
        case i: InstWatchPoint => i.ena = destClause
        case i: InstWPBranch   => i.ena = destClause
        case i => {
          throw new IllegalArgumentException(
            "ENABLED requires WATCHPOINT OR WPBRANCH. Found: %s".format(i.getClass.getName))
        }
      }
    }
  }

  def addKeepAlives(inst: CN[HasKeepAliveClause], lvs: Seq[CN[LocalVariable]]): Unit = {
    inst.keepAlives = lvs
  }

  private def newInst[T <: Instruction](bb: BasicBlock, inst: T): CN[T] = {
    inst.bb = bb
    inst.results = new ArrayBuffer(2)
    newObj(bb.localInstNs, inst)
  }

  // For relieving my eye fatigue :)
  private type BB = BasicBlock
  private type Var = SSAVariable

  def newBinOp(bb: CN[BB], optr: BinOptr.Value, ty: CN[Type], opnd1: CN[Var], opnd2: CN[Var]): CN[InstBinOp] = {
    val inst = InstBinOp(optr, ty, opnd1, opnd2, None)
    newInst(bb, inst)
  }

  def newCmp(bb: CN[BB], optr: CmpOptr.Value, ty: CN[Type], opnd1: CN[Var], opnd2: CN[Var]): CN[InstCmp] = {
    val inst = InstCmp(optr, ty, opnd1, opnd2)
    newInst(bb, inst)
  }

  def newConv(bb: CN[BB], optr: ConvOptr.Value, fromTy: CN[Type], toTy: CN[Type], opnd: CN[Var]): CN[InstConv] = {
    val inst = InstConv(optr, fromTy, toTy, opnd)
    newInst(bb, inst)
  }

  def newSelect(bb: CN[BB], condTy: CN[Type], opndTy: CN[Type], cond: CN[Var], ifTrue: CN[Var], ifFalse: CN[Var]): CN[InstSelect] = {
    val inst = InstSelect(condTy, opndTy, cond, ifTrue, ifFalse)
    newInst(bb, inst)
  }

  def newBranch(bb: CN[BB]): CN[InstBranch] = {
    val inst = InstBranch(null)
    newInst(bb, inst)
  }

  def newBranch2(bb: CN[BB], cond: CN[Var]): CN[InstBranch2] = {
    val inst = InstBranch2(cond, null, null)
    newInst(bb, inst)
  }

  def newSwitch(bb: CN[BB], opndTy: CN[Type], opnd: CN[Var]): CN[InstSwitch] = {
    val inst = InstSwitch(opndTy, opnd, null, new ArrayBuffer())
    newInst(bb, inst)
  }

  def addSwitchDest(inst: CN[InstSwitch], key: CN[Var], bb: CN[BB], vars: Seq[CN[Var]]): Unit = {
    val destClause = DestClause(bb, vars)
    inst.cases += (key.obj -> destClause)
  }

  def newCall(bb: CN[BB], sig: CN[FuncSig], callee: CN[Var], args: Seq[CN[Var]]): CN[InstCall] = {
    val inst = InstCall(sig, callee, args, None, Seq())
    newInst(bb, inst)
  }

  def newTailCall(bb: CN[BB], sig: CN[FuncSig], callee: CN[Var], args: Seq[CN[Var]]): CN[InstTailCall] = {
    val inst = InstTailCall(sig, callee, args)
    newInst(bb, inst)
  }

  def newRet(bb: CN[BB], rvs: Seq[CN[Var]]): CN[InstRet] = {
    val inst = InstRet(bb.funcVer, rvs)
    newInst(bb, inst)
  }

  def newThrow(bb: CN[BB], exc: CN[Var]): CN[InstThrow] = {
    val inst = InstThrow(exc)
    newInst(bb, inst)
  }

  def newExtractValue(bb: CN[BB], strTy: CN[TypeStruct], index: Int, opnd: CN[Var]): CN[InstExtractValue] = {
    val inst = InstExtractValue(strTy, index, opnd)
    newInst(bb, inst)
  }

  def newInsertValue(bb: CN[BB], strTy: CN[TypeStruct], index: Int, opnd: CN[Var], newVal: CN[Var]): CN[InstInsertValue] = {
    val inst = InstInsertValue(strTy, index, opnd, newVal)
    newInst(bb, inst)
  }

  def newExtractElement(bb: CN[BB], seqTy: CN[AbstractSeqType], indTy: CN[TypeInt], opnd: CN[Var], index: CN[Var]): CN[InstExtractElement] = {
    val inst = InstExtractElement(seqTy, indTy, opnd, index)
    newInst(bb, inst)
  }

  def newInsertElement(bb: CN[BB], seqTy: CN[AbstractSeqType], indTy: CN[TypeInt], opnd: CN[Var], index: CN[Var], newVal: CN[Var]): CN[InstInsertElement] = {
    val inst = InstInsertElement(seqTy, indTy, opnd, index, newVal)
    newInst(bb, inst)
  }

  def newShuffleVector(bb: CN[BB], vecTy: CN[TypeVector], maskTy: CN[TypeVector], vec1: CN[Var], vec2: CN[Var], mask: CN[Var]): CN[InstShuffleVector] = {
    val inst = InstShuffleVector(vecTy, maskTy, vec1, vec2, mask)
    newInst(bb, inst)
  }

  def newNew(bb: CN[BB], allocTy: CN[Type]): CN[InstNew] = {
    val inst = InstNew(allocTy, None)
    newInst(bb, inst)
  }

  def newNewHybrid(bb: CN[BB], allocTy: CN[TypeHybrid], lenTy: CN[TypeInt], len: CN[Var]): CN[InstNewHybrid] = {
    val inst = InstNewHybrid(allocTy, lenTy, len, None)
    newInst(bb, inst)
  }

  def newAlloca(bb: CN[BB], allocTy: CN[Type]): CN[InstAlloca] = {
    val inst = InstAlloca(allocTy, None)
    newInst(bb, inst)
  }

  def newAllocaHybrid(bb: CN[BB], allocTy: CN[TypeHybrid], lenTy: CN[TypeInt], len: CN[Var]): CN[InstAllocaHybrid] = {
    val inst = InstAllocaHybrid(allocTy, lenTy, len, None)
    newInst(bb, inst)
  }

  def newGetIRef(bb: CN[BB], refTy: CN[Type], opnd: CN[Var]): CN[InstGetIRef] = {
    val inst = InstGetIRef(refTy, opnd)
    newInst(bb, inst)
  }

  def newGetFieldIRef(bb: CN[BB], isPtr: Boolean, refTy: CN[TypeStruct], index: Int, opnd: CN[Var]): CN[InstGetFieldIRef] = {
    val inst = InstGetFieldIRef(isPtr, refTy, index, opnd)
    newInst(bb, inst)
  }

  def newGetElemIRef(bb: CN[BB], isPtr: Boolean, refTy: CN[AbstractSeqType], indTy: CN[TypeInt], opnd: CN[Var], index: CN[Var]): CN[InstGetElemIRef] = {
    val inst = InstGetElemIRef(isPtr, refTy, indTy, opnd, index)
    newInst(bb, inst)
  }

  def newShiftIRef(bb: CN[BB], isPtr: Boolean, refTy: CN[Type], offTy: CN[TypeInt], opnd: CN[Var], offset: CN[Var]): CN[InstShiftIRef] = {
    val inst = InstShiftIRef(isPtr, refTy, offTy, opnd, offset)
    newInst(bb, inst)
  }

  def newGetVarPartIRef(bb: CN[BB], isPtr: Boolean, refTy: CN[TypeHybrid], opnd: CN[Var]): CN[InstGetVarPartIRef] = {
    val inst = InstGetVarPartIRef(isPtr, refTy, opnd)
    newInst(bb, inst)
  }

  def newLoad(bb: CN[BB], isPtr: Boolean, ord: MemoryOrder.Value, refTy: CN[Type], loc: CN[Var]): CN[InstLoad] = {
    val inst = InstLoad(isPtr, ord, refTy, loc, None)
    newInst(bb, inst)
  }

  def newStore(bb: CN[BB], isPtr: Boolean, ord: MemoryOrder.Value, refTy: CN[Type], loc: CN[Var], newVal: CN[Var]): CN[InstStore] = {
    val inst = InstStore(isPtr, ord, refTy, loc, newVal, None)
    newInst(bb, inst)
  }

  def newCmpXchg(bb: CN[BB], isPtr: Boolean, isWeak: Boolean, ordSucc: MemoryOrder.Value, ordFail: MemoryOrder.Value,
    refTy: CN[Type], loc: CN[Var], expected: CN[Var], desired: CN[Var]): CN[InstCmpXchg] = {
    val inst = InstCmpXchg(isPtr, isWeak, ordSucc, ordFail, refTy, loc, expected, desired, None)
    newInst(bb, inst)
  }

  def newAtomicRMW(bb: CN[BB], isPtr: Boolean, ord: MemoryOrder.Value, optr: AtomicRMWOptr.Value, refTy: CN[Type], loc: CN[Var], opnd: CN[Var]): CN[InstAtomicRMW] = {
    val inst = InstAtomicRMW(isPtr, ord, optr, refTy, loc, opnd, None)
    newInst(bb, inst)
  }

  def newFence(bb: CN[BB], ord: MemoryOrder.Value): CN[InstFence] = {
    val inst = InstFence(ord)
    newInst(bb, inst)
  }

  def newTrap(bb: CN[BB], retTys: Seq[CN[Type]]): CN[InstTrap] = {
    val inst = InstTrap(retTys, None, Seq())
    newInst(bb, inst)
  }

  def newWatchPoint(bb: CN[BB], wpid: Int, retTys: Seq[CN[Type]]): CN[InstWatchPoint] = {
    val inst = InstWatchPoint(wpid, retTys, null, null, None, Seq())
    newInst(bb, inst)
  }

  def newCCall(bb: CN[BB], callConv: Flag, calleeTy: CN[Type], sig: CN[FuncSig], callee: CN[Var], args: Seq[CN[Var]]): CN[InstCCall] = {
    val inst = InstCCall(callConv, calleeTy, sig, callee, args, None, Seq())
    newInst(bb, inst)
  }

  def newNewThread(bb: CN[BB], stack: CN[Var], threadLocal: Option[CN[Var]]): CN[InstNewThread] = {
    val inst = InstNewThread(stack, threadLocal.map(_.obj), null, None)
    newInst(bb, inst)
  }

  def newSwapStackRet(bb: CN[BB], swappee: CN[Var], retTys: Seq[CN[Type]]): CN[InstSwapStack] = {
    val curStackClause = RetWith(retTys)
    val inst = InstSwapStack(swappee, curStackClause, null, None, Seq())
    newInst(bb, inst)
  }

  def newSwapStackKill(bb: CN[BB], swappee: CN[Var]): CN[InstSwapStack] = {
    val curStackClause = KillOld()
    val inst = InstSwapStack(swappee, curStackClause, null, None, Seq())
    newInst(bb, inst)
  }

  def setNewStackPassValues(inst: CN[Instruction], tys: Seq[CN[Type]], vars: Seq[CN[Var]]): Unit = {
    val newStackClause = PassValues(tys, vars)
    inst.obj match {
      case i: InstNewThread => i.newStackAction = newStackClause
      case i: InstSwapStack => i.newStackAction = newStackClause
      case i => {
        throw new IllegalArgumentException("Expected NEWTHREAD or SWAPSTACK, found %s".format(i.getClass.getName))
      }
    }
  }
  
  def newCommInst(bb: CN[BB], opcode: Int, flags: Seq[Flag], tys: Seq[CN[Type]], sigs: Seq[CN[FuncSig]], args: Seq[CN[Var]]): CN[InstCommInst] = {
    val commInst = CommInsts.get(opcode).getOrElse {
      throw new IllegalArgumentException("No such common instruction. opcode: %d 0x%x".format(opcode, opcode))
    }
    val inst = InstCommInst(commInst, flags, tys, sigs, args, None, Seq())
    newInst(bb, inst)
  }

}