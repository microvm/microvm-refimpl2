package uvm.refimpl

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.utils.LazyPool
import uvm.ir.textinput.IDFactory
import scala.collection.mutable.HashMap
import uvm.FuncSig
import uvm.IdentifiedSettable

object InternalIDFactory extends IDFactory(32768) // IDs from 32768-65535 are for implementation internal use.

object InternalTypes {
  private implicit class IdentifiedSettableAssignable[T <: IdentifiedSettable](i: T) {
    def :=(name: String): T = {
      i.id = InternalIDFactory.getID()
      i.name = Some(name)
      i
    }
  }

  def internal(name: String) = "@uvm.internal.types." + name

  val I1 = TypeInt(1) := internal("i1")
  val I6 = TypeInt(6) := internal("i6")
  val I32 = TypeInt(32) := internal("i32")
  val I52 = TypeInt(52) := internal("i52")
  val I64 = TypeInt(52) := internal("i64")
  val DOUBLE = TypeDouble() := internal("double")
  val VOID = TypeVoid() := internal("void")

  val BYTE = TypeInt(8) := internal("byte")
  val BYTE_ARRAY = TypeHybrid(VOID, BYTE) := internal("byte_array")

  val REF_VOID = TypeRef(VOID) := internal("ref_void")

  val STACK = TypeStackRef() := internal("stack")
  val THREAD = TypeThreadRef() := internal("thread")
  val TAGREF64 = TypeTagRef64() := internal("tagref64")
}

object InternalTypePool {
  val refOf = LazyPool(TypeRef)
  val irefOf = LazyPool(TypeIRef)
  val ptrOf = LazyPool(TypeUPtr)
  val funcOf = LazyPool(TypeFuncRef)
  val funcPtrOf = LazyPool(TypeUFuncPtr)
  val vecOf = LazyPool[(Type, Long), TypeVector] { case (t, l) => TypeVector(t, l) }
  def unmarkedOf(t: Type): Type = t match {
    case TypeWeakRef(r) => refOf(r)
    case _ => t
  }
}

object TypeInferer {
  import InternalTypes._
  import InternalTypePool._
  
  def ptrOrIRefOf(ptr: Boolean, ty: Type): Type = {
    if (ptr) ptrOf(ty) else irefOf(ty)
  }
  
  def inferType(v: SSAVariable): Type = v match {
    case c: Constant => c.constTy
    case g: GlobalCell => irefOf(g.cellTy)
    case f: Function => funcOf(f.sig)
    case p: NorParam => p.ty
    case p: ExcParam => REF_VOID
    case i: InstBinOp => i.opndTy
    case i: InstCmp => i.opndTy match {
      case TypeVector(_, l) => vecOf(I1, l)
      case _ => I1
    }
    case i: InstConv => i.toTy
    case i: InstSelect => i.opndTy
    case i: InstBranch => VOID
    case i: InstBranch2 => VOID
    case i: InstSwitch => VOID
    case i: InstCall => i.sig.retTy
    case i: InstTailCall => VOID
    case i: InstRet => VOID
    case i: InstThrow => VOID
    case i: InstExtractValue => i.strTy.fieldTys(i.index)
    case i: InstInsertValue => i.strTy
    case i: InstExtractElement => i.seqTy.elemTy
    case i: InstInsertElement => i.seqTy
    case i: InstShuffleVector => vecOf((i.vecTy.elemTy, i.maskTy.len))
    case i: InstNew => refOf(i.allocTy)
    case i: InstNewHybrid => refOf(i.allocTy)
    case i: InstAlloca => irefOf(i.allocTy)
    case i: InstAllocaHybrid => irefOf(i.allocTy)
    case i: InstGetIRef => irefOf(i.referentTy)
    case i: InstGetFieldIRef => ptrOrIRefOf(i.ptr, i.referentTy.fieldTys(i.index))
    case i: InstGetElemIRef => ptrOrIRefOf(i.ptr, i.referentTy.elemTy)
    case i: InstShiftIRef => ptrOrIRefOf(i.ptr, i.referentTy)
    case i: InstGetFixedPartIRef => ptrOrIRefOf(i.ptr, i.referentTy.fixedTy)
    case i: InstGetVarPartIRef => ptrOrIRefOf(i.ptr, i.referentTy.varTy)
    case i: InstLoad => unmarkedOf(i.referentTy)
    case i: InstStore => VOID
    case i: InstCmpXchg => unmarkedOf(i.referentTy)
    case i: InstAtomicRMW => unmarkedOf(i.referentTy)
    case i: InstFence => VOID
    case i: InstTrap => i.retTy
    case i: InstWatchPoint => i.retTy
    case i: InstCCall => i.sig.retTy
    case i: InstNewStack => STACK
    case i: InstSwapStack => i.curStackAction match {
      case RetWith(t) => t
      case _: KillOld => VOID
    }
    case i: InstCommInst => i.inst.name.get match {
      case "@uvm.new_thread" => THREAD
      case "@uvm.kill_stack" => VOID
      case "@uvm.thread_exit" => VOID
      case "@uvm.current_stack" => STACK
      case "@uvm.tr64.is_fp" => I1
      case "@uvm.tr64.is_int" => I1
      case "@uvm.tr64.is_ref" => I1
      case "@uvm.tr64.from_fp" => TAGREF64
      case "@uvm.tr64.from_int" => TAGREF64
      case "@uvm.tr64.from_ref" => TAGREF64
      case "@uvm.tr64.to_fp" => DOUBLE
      case "@uvm.tr64.to_int" => I52
      case "@uvm.tr64.to_ref" => REF_VOID
      case "@uvm.tr64.to_tag" => I6
      case "@uvm.futex.wait" => I32
      case "@uvm.futex.wait_timeout" => I32
      case "@uvm.futex.wake" => I32
      case "@uvm.futex.cmp_requeue" => I32
      case "@uvm.kill_dependency" => i.typeList(0)
      case "@uvm.native.pin" => i.typeList(0) match{
        case TypeRef(t) => ptrOf(t)
        case TypeIRef(t) => ptrOf(t)
      }
      case "@uvm.native.unpin" => VOID
      case "@uvm.native.expose" => funcPtrOf(i.funcSigList(0))
      case "@uvm.native.unexpose" => VOID
      case "@uvm.native.get_cookie" => I64
    }
  }
}