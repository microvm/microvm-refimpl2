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
  val FLOAT = TypeFloat() := internal("float")
  val DOUBLE = TypeDouble() := internal("double")
  val VOID = TypeVoid() := internal("void")

  val BYTE = TypeInt(8) := internal("byte")
  val BYTE_ARRAY = TypeHybrid(Seq(), BYTE) := internal("byte_array")

  val REF_VOID = TypeRef(VOID) := internal("ref_void")

  val STACK = TypeStackRef() := internal("stack")
  val THREAD = TypeThreadRef() := internal("thread")
  val TAGREF64 = TypeTagRef64() := internal("tagref64")
}

object InternalTypePool {
  val intOf = LazyPool(TypeInt)
  val refOf = LazyPool(TypeRef)
  val irefOf = LazyPool(TypeIRef)
  val ptrOf = LazyPool(TypeUPtr)
  val funcOf = LazyPool(TypeFuncRef)
  val funcPtrOf = LazyPool(TypeUFuncPtr)
  val vecOf = LazyPool[(Type, Long), TypeVector] { case (t, l) => TypeVector(t, l) }
  def unmarkedOf(t: Type): Type = t match {
    case TypeWeakRef(r) => refOf(r)
    case _              => t
  }
}

object TypeInferer {
  import InternalTypes._
  import InternalTypePool._

  def ptrOrIRefOf(ptr: Boolean, ty: Type): Type = {
    if (ptr) ptrOf(ty) else irefOf(ty)
  }

  def inferType(v: SSAVariable): Type = v match {
    case c: Constant   => c.constTy
    case g: GlobalCell => irefOf(g.cellTy)
    case f: Function   => funcOf(f.sig)
    case p: NorParam   => p.ty
    case p: ExcParam   => REF_VOID
    case r: InstResult => {
      val resTys = inferInstResultTypes(r.inst)
      try {
        resTys(r.index)
      } catch {
        case e: IndexOutOfBoundsException => throw new UvmRefImplException(
          s"Instruction ${r.inst} produces only ${resTys.size} results, but result index ${r.index} is requested")
      }
    }
  }

  def inferInstResultTypes(inst: Instruction): Seq[Type] = inst match {
    case i: InstBinOp => Seq(i.opndTy)
    case i: InstCmp => i.opndTy match {
      case TypeVector(_, l) => Seq(vecOf(I1, l))
      case _                => Seq(I1)
    }
    case i: InstConv           => Seq(i.toTy)
    case i: InstSelect         => Seq(i.opndTy)
    case i: InstBranch         => Seq()
    case i: InstBranch2        => Seq()
    case i: InstSwitch         => Seq()
    case i: InstCall           => i.sig.retTys
    case i: InstTailCall       => Seq()
    case i: InstRet            => Seq()
    case i: InstThrow          => Seq()
    case i: InstExtractValue   => Seq(i.strTy.fieldTys(i.index))
    case i: InstInsertValue    => Seq(i.strTy)
    case i: InstExtractElement => Seq(i.seqTy.elemTy)
    case i: InstInsertElement  => Seq(i.seqTy)
    case i: InstShuffleVector  => Seq(vecOf((i.vecTy.elemTy, i.maskTy.len)))
    case i: InstNew            => Seq(refOf(i.allocTy))
    case i: InstNewHybrid      => Seq(refOf(i.allocTy))
    case i: InstAlloca         => Seq(irefOf(i.allocTy))
    case i: InstAllocaHybrid   => Seq(irefOf(i.allocTy))
    case i: InstGetIRef        => Seq(irefOf(i.referentTy))
    case i: InstGetFieldIRef   => Seq(ptrOrIRefOf(i.ptr, i.referentTy.fieldTys(i.index)))
    case i: InstGetElemIRef    => Seq(ptrOrIRefOf(i.ptr, i.referentTy.elemTy))
    case i: InstShiftIRef      => Seq(ptrOrIRefOf(i.ptr, i.referentTy))
    case i: InstGetVarPartIRef => Seq(ptrOrIRefOf(i.ptr, i.referentTy.varTy))
    case i: InstLoad           => Seq(unmarkedOf(i.referentTy))
    case i: InstStore          => Seq()
    case i: InstCmpXchg        => Seq(unmarkedOf(i.referentTy), I1)
    case i: InstAtomicRMW      => Seq(unmarkedOf(i.referentTy))
    case i: InstFence          => Seq()
    case i: InstTrap           => i.retTys
    case i: InstWatchPoint     => i.retTys
    case i: InstCCall          => i.sig.retTys
    case i: InstNewThread      => Seq(THREAD)
    case i: InstSwapStack => i.curStackAction match {
      case RetWith(t) => t
      case _: KillOld => Seq()
    }
    case i: InstCommInst => i.inst.name.get match {
      case "@uvm.new_stack"          => Seq(STACK)
      case "@uvm.kill_stack"         => Seq()
      case "@uvm.thread_exit"        => Seq()
      case "@uvm.current_stack"      => Seq(STACK)
      case "@uvm.tr64.is_fp"         => Seq(I1)
      case "@uvm.tr64.is_int"        => Seq(I1)
      case "@uvm.tr64.is_ref"        => Seq(I1)
      case "@uvm.tr64.from_fp"       => Seq(TAGREF64)
      case "@uvm.tr64.from_int"      => Seq(TAGREF64)
      case "@uvm.tr64.from_ref"      => Seq(TAGREF64)
      case "@uvm.tr64.to_fp"         => Seq(DOUBLE)
      case "@uvm.tr64.to_int"        => Seq(I52)
      case "@uvm.tr64.to_ref"        => Seq(REF_VOID)
      case "@uvm.tr64.to_tag"        => Seq(I6)
      case "@uvm.futex.wait"         => Seq(I32)
      case "@uvm.futex.wait_timeout" => Seq(I32)
      case "@uvm.futex.wake"         => Seq(I32)
      case "@uvm.futex.cmp_requeue"  => Seq(I32)
      case "@uvm.kill_dependency"    => Seq(i.typeList(0))
      case "@uvm.native.pin" => i.typeList(0) match {
        case TypeRef(t)  => Seq(ptrOf(t))
        case TypeIRef(t) => Seq(ptrOf(t))
      }
      case "@uvm.native.unpin"      => Seq()
      case "@uvm.native.expose"     => Seq(funcPtrOf(i.funcSigList(0)))
      case "@uvm.native.unexpose"   => Seq()
      case "@uvm.native.get_cookie" => Seq(I64)
    }
  }
}