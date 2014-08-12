package uvm.ssavalues

import uvm._
import uvm.types._
import uvm.types.CommonTypes._
import uvm.ifuncs._

abstract class Value extends Identified {
  def ty: Type

  def resolve() {}
}

// Constants

abstract class GlobalValue extends Value

abstract class DeclaredConstant extends GlobalValue with IdentifiedSettable {
  var constTy: Type
  def ty = constTy
}

case class ConstInt(var constTy: Type, var num: BigInt) extends DeclaredConstant
case class ConstFloat(var constTy: Type, var num: Float) extends DeclaredConstant
case class ConstDouble(var constTy: Type, var num: Double) extends DeclaredConstant
case class ConstStruct(var constTy: Type, var fields: Seq[GlobalValue]) extends DeclaredConstant
case class ConstNull(var constTy: Type) extends DeclaredConstant

case class ConstFunc(var func: Function) extends GlobalValue {
  def ty = func.sig.retTy
  override def id: Int = func.id
  override def name: Option[String] = func.name
}

case class ConstGlobalData(var gd: GlobalData) extends GlobalValue {
  def ty = gd.ty
  override def id: Int = gd.id
  override def name: Option[String] = gd.name
}

// Local values: Parameter and Instructions

abstract class LocalValue extends Value

case class Parameter(var sig: FuncSig, var index: Int) extends LocalValue with IdentifiedSettable {
  def ty = sig.retTy
}

// Instructions

abstract class Instruction extends LocalValue with IdentifiedSettable

/// enumerations

object BinOptr extends Enumeration {
  type BinOptr = Value
  val ADD, SUB, MUL, UDIV, SDIV, UREM, SREM, SHL, LSHR, ASHR, AND, OR, XOR = Value
  val FADD, FSUB, FMUL, FDIV, FREM = Value
}

import BinOptr.BinOptr

object CmpOptr extends Enumeration {
  type CmpOptr = Value
  val EQ, NE, ULT, ULE, UGT, UGE, SLT, SLE, SGT, SGE = Value
  val FTRUE, FFALSE, FORD, FOEQ, FONE, FOLT, FOLE, FOGT, FOGE = Value
  val FUNO, FUEQ, FUNE, FULT, FULE, FUGT, FUGE = Value
}

import CmpOptr.CmpOptr

object ConvOptr extends Enumeration {
  type ConvOptr = Value
  val TRUNC, ZEXT, SEXT, FPTRUNC, FPEXT, FPTOUI, FPTOSI, UITOFP, SITOFP, BITCAST = Value
  val REFCAST, IREFCAST, FUNCCAST = Value
}

import ConvOptr.ConvOptr

object MemoryOrdering extends Enumeration {
  type MemoryOrdering = Value
  val NOT_ATOMIC, UNORDERED, MONOTONIC, ACQUIRE, RELEASE, ACQ_REL, SEQ_CST = Value
}

import MemoryOrdering.MemoryOrdering

object AtomicRMWOptr extends Enumeration {
  type AtomicRMWOptr = Value
  val XCHG, ADD, SUB, AND, NAND, OR, XOR, MIN, MAX, UMIN, UMAX = Value
}

import AtomicRMWOptr.AtomicRMWOptr

object CallConv extends Enumeration {
  type CallConv = Value
  val DEFAULT = Value
}

import CallConv.CallConv

/// Abstract instructions and traits

trait HasArgs extends Instruction {
  var args: Seq[Value]
}

trait CallLike extends HasArgs {
  var sig: FuncSig
  var callee: Value
}

trait HasExceptionalDest extends Instruction {
  var nor: BasicBlock
  var exc: BasicBlock
}

trait HasKeepAlives extends Instruction {
  var keepAlives: Seq[Value]
}

abstract class AbstractCall extends HasArgs with CallLike

abstract class NonTailCall extends AbstractCall with HasKeepAlives {
  def ty = sig.retTy
}

abstract class AbstractRet extends Instruction {
  def ty = VOID
}

abstract class AbstractAlloc extends Instruction {
  def allocTy: Type
  var ty: Type = null
}

trait ScalarAlloc extends AbstractAlloc {
  def allocTy: Type
}

trait HybridAlloc extends AbstractAlloc {
  def allocTy: TypeHybrid
  def length: Value
}

abstract class HeapAlloc extends AbstractAlloc {
  override def resolve() { ty = TypeRef(allocTy) }
}

abstract class StackAlloc extends AbstractAlloc {
  override def resolve() { ty = TypeIRef(allocTy) }
}

abstract class WeakRefLoader extends Instruction {
  def referentTy: Type
  var ty: Type = null
  override def resolve() {
    referentTy match {
      case TypeWeakRef(t) => { ty = TypeRef(t) }
      case t => { ty = t }
    }
  }
}

abstract class AbstractTrap extends HasExceptionalDest with HasKeepAlives {
  var retTy: Type
  def ty = retTy
}

abstract class AbstractICall extends HasArgs with HasKeepAlives {
  var iFunc: IFunc
  def ty = iFunc.ty
}

/// Concrete instructions
case class InstBinOp(var op: BinOptr, var opndTy: Type, var op1: Value, var op2: Value) extends Instruction {
  def ty = opndTy
}

case class InstCmp(var op: CmpOptr, var opndTy: Type, var op1: Value, var op2: Value) extends Instruction {
  def ty = I1
}

case class InstConv(var op: ConvOptr, var fromTy: Type, var toTy: Type, var opnd: Value) extends Instruction {
  def ty = toTy
}

case class InstSelect(var opndTy: Type, var cond: Value, var ifTrue: Value, var ifFalse: Value) extends Instruction {
  def ty = opndTy
}

case class InstBranch(var dest: BasicBlock) extends Instruction {
  def ty = VOID
}

case class InstBranch2(var cond: Value, var ifTrue: BasicBlock, var ifFalse: BasicBlock) extends Instruction {
  def ty = VOID
}

case class InstSwitch(var opndTy: Type, var opnd: Value, var defDest: BasicBlock,
  var cases: Seq[(Value, BasicBlock)]) extends Instruction {
  def ty = VOID
}

case class InstPhi(var opndTy: Type, var cases: Seq[(BasicBlock, Value)]) extends Instruction {
  def ty = opndTy
}

case class InstCall(var sig: FuncSig, var callee: Value, var args: Seq[Value],
  var keepAlives: Seq[Value]) extends NonTailCall

case class InstInvoke(var sig: FuncSig, var callee: Value, var args: Seq[Value],
  var nor: BasicBlock, var exc: BasicBlock, var keepAlives: Seq[Value]) extends NonTailCall with HasExceptionalDest

case class InstTailCall(var sig: FuncSig, var callee: Value, var args: Seq[Value]) extends AbstractCall {
  def ty = VOID
}

case class InstRet(var retTy: Type, var retVal: Value) extends AbstractRet

case class InstRetVoid() extends AbstractRet

case class InstThrow(var excVal: Value) extends Instruction {
  def ty = VOID
}

case class InstLandingpad() extends Instruction {
  def ty = REFVOID
}

case class InstExtractValue(var strTy: TypeStruct, var index: Int, var opnd: Value) extends Instruction {
  def ty = strTy.fieldTy(index)
}

case class InstInsertValue(var strTy: TypeStruct, var index: Int, var opnd: Value, var newVal: Value) extends Instruction {
  def ty = strTy
}

case class InstNew(var allocTy: Type) extends HeapAlloc with ScalarAlloc
case class InstNewHybrid(var allocTy: TypeHybrid, var length: Value) extends HeapAlloc with HybridAlloc

case class InstAlloca(var allocTy: Type) extends StackAlloc with ScalarAlloc
case class InstAllocaHybrid(var allocTy: TypeHybrid, var length: Value) extends StackAlloc with HybridAlloc

case class InstGetIRef(var referentTy: Type, var opnd: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy) }
}

case class InstGetFieldIRef(var referentTy: TypeStruct, var index: Int, var opnd: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy.fieldTy(index)) }
}

case class InstGetElemIRef(var referentTy: TypeArray, var opnd: Value, var index: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy.elemTy) }
}

case class InstShiftIRef(var referentTy: Type, var opnd: Value, var offset: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy) }
}

case class InstGetFixedPartIRef(var referentTy: TypeHybrid, var opnd: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy.fixedPart) }
}

case class InstGetVarPartIRef(var referentTy: TypeHybrid, var opnd: Value) extends Instruction {
  var ty: Type = null
  override def resolve() { ty = TypeIRef(referentTy.varPart) }
}

case class InstLoad(var ord: MemoryOrdering, var referentTy: Type, var loc: Value)
  extends WeakRefLoader

case class InstStore(var ord: MemoryOrdering, var referentTy: Type, var loc: Value, var newVal: Value) extends Instruction {
  def ty = VOID
}

case class InstCmpXchg(var ordSucc: MemoryOrdering, var ordFail: MemoryOrdering,
  var referentTy: Type, var loc: Value, var expected: Value, var desired: Value)
  extends WeakRefLoader

case class InstFence(var ord: MemoryOrdering) extends Instruction {
  def ty = VOID
}

case class InstAtomicRMW(var ord: MemoryOrdering, var op: AtomicRMWOptr,
  var referentTy: Type, var loc: Value, var opnd: Value) extends Instruction {
  def ty = referentTy
}

case class InstTrap(var retTy: Type, var nor: BasicBlock, var exc: BasicBlock,
  var keepAlives: Seq[Value]) extends AbstractTrap

case class InstWatchpoint(var wpID: Int, var retTy: Type, var dis: BasicBlock,
  var nor: BasicBlock, var exc: BasicBlock, var keepAlives: Seq[Value]) extends AbstractTrap

case class InstCCall(var callConv: CallConv, var sig: FuncSig, var callee: Value, var args: Seq[Value])
  extends CallLike {
  def ty = sig.retTy
}

case class InstNewStack(var sig: FuncSig, var callee: Value, var args: Seq[Value]) extends CallLike {
  def ty = STACK
}

case class InstICall(var iFunc: IFunc, var args: Seq[Value], var keepAlives: Seq[Value])
  extends AbstractICall

case class InstIInvoke(var iFunc: IFunc, var args: Seq[Value],
  var nor: BasicBlock, var exc: BasicBlock, var keepAlives: Seq[Value])
  extends AbstractICall with HasExceptionalDest
