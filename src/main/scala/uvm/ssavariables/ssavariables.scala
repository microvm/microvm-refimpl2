package uvm.ssavariables

import uvm._
import uvm.comminsts._
import uvm.types._

abstract class SSAVariable extends IdentifiedSettable

// Global variables: Constants, Global Cells and Functions (Function is defined in controlFlow.scala)

abstract class GlobalVariable extends SSAVariable

abstract class Constant extends GlobalVariable {
  var constTy: Type
}

case class ConstInt(var constTy: Type, var num: BigInt) extends Constant

case class ConstFloat(var constTy: Type, var num: Float) extends Constant

case class ConstDouble(var constTy: Type, var num: Double) extends Constant

case class ConstStruct(var constTy: Type, var fields: Seq[GlobalVariable]) extends Constant

case class ConstNull(var constTy: Type) extends Constant

case class ConstVector(var constTy: Type, var elems: Seq[Constant]) extends Constant

case class GlobalCell(var cellTy: Type) extends GlobalVariable

// Local variables: Parameters and Instructions

abstract class LocalVariable extends SSAVariable

case class Parameter(var funcVer: FuncVer, var index: Int) extends LocalVariable

// Instructions

abstract class Instruction extends LocalVariable

/// enumerations

object BinOptr extends Enumeration {
  type BinOptr = Value
  val ADD, SUB, MUL, UDIV, SDIV, UREM, SREM, SHL, LSHR, ASHR, AND, OR, XOR = Value
  val FADD, FSUB, FMUL, FDIV, FREM = Value
}

import uvm.ssavariables.BinOptr.BinOptr

object CmpOptr extends Enumeration {
  type CmpOptr = Value
  val EQ, NE, ULT, ULE, UGT, UGE, SLT, SLE, SGT, SGE = Value
  val FTRUE, FFALSE, FORD, FOEQ, FONE, FOLT, FOLE, FOGT, FOGE = Value
  val FUNO, FUEQ, FUNE, FULT, FULE, FUGT, FUGE = Value
}

import uvm.ssavariables.CmpOptr.CmpOptr

object ConvOptr extends Enumeration {
  type ConvOptr = Value
  val TRUNC, ZEXT, SEXT, FPTRUNC, FPEXT, FPTOUI, FPTOSI, UITOFP, SITOFP, BITCAST, REFCAST = Value
}

import uvm.ssavariables.ConvOptr.ConvOptr

object MemoryOrder extends Enumeration {
  type MemoryOrder = Value
  val NOT_ATOMIC, RELAXED, CONSUME, ACQUIRE, RELEASE, ACQ_REL, SEQ_CST = Value
}

import uvm.ssavariables.MemoryOrder.MemoryOrder

object AtomicRMWOptr extends Enumeration {
  type AtomicRMWOptr = Value
  val XCHG, ADD, SUB, AND, NAND, OR, XOR, MIN, MAX, UMIN, UMAX = Value
}

import uvm.ssavariables.AtomicRMWOptr.AtomicRMWOptr

object CallConv extends Enumeration {
  type CallConv = Value
  val DEFAULT = Value
}

import uvm.ssavariables.CallConv.CallConv

/// Abstract instructions and traits

trait HasTypeList extends Instruction {
  var typeList: Seq[Type]
}

trait HasArgList extends Instruction {
  var argList: Seq[SSAVariable]
}

trait CallLike extends HasArgList {
  var sig: FuncSig
  var callee: SSAVariable
}

case class ExcClause(val nor: BasicBlock, val exc: BasicBlock)

trait HasExcClause extends Instruction {
  var excClause: Option[ExcClause]
}

trait HasKeepAliveClause extends Instruction {
  var keepAlives: Seq[LocalVariable]
}

abstract class AbstractCall extends CallLike

abstract class AbstractRet extends Instruction

abstract class AbstractAlloc extends HasExcClause {
  def allocTy: Type
}

trait FixedAlloc extends AbstractAlloc

trait HybridAlloc extends AbstractAlloc {
  def lenTy: TypeInt
  def length: SSAVariable
}

abstract class HeapAlloc extends AbstractAlloc

abstract class StackAlloc extends AbstractAlloc

abstract class AbstractTrap extends HasKeepAliveClause {
  var retTy: Type
}

abstract class CurStackAction
case class RetWith(var retTy: Type) extends CurStackAction
case class KillOld() extends CurStackAction

abstract class NewStackAction
case class PassValue(var argTy: Type, var arg: SSAVariable) extends NewStackAction
case class PassVoid() extends NewStackAction
case class ThrowExc(var exc: SSAVariable) extends NewStackAction

/// Concrete instructions
case class InstBinOp(var op: BinOptr, var opndTy: Type, var op1: SSAVariable, var op2: SSAVariable,
                     var excClause: Option[ExcClause]) extends HasExcClause

case class InstCmp(var op: CmpOptr, var opndTy: Type, var op1: SSAVariable, var op2: SSAVariable) extends Instruction

case class InstConv(var op: ConvOptr, var fromTy: Type, var toTy: Type, var opnd: SSAVariable) extends Instruction

case class InstSelect(var opndTy: Type, var condTy: Type,
                      var cond: SSAVariable, var ifTrue: SSAVariable, var ifFalse: SSAVariable) extends Instruction

case class InstBranch(var dest: BasicBlock) extends Instruction

case class InstBranch2(var cond: SSAVariable, var ifTrue: BasicBlock, var ifFalse: BasicBlock) extends Instruction

case class InstSwitch(var opndTy: Type, var opnd: SSAVariable, var defDest: BasicBlock,
                      var cases: Seq[(SSAVariable, BasicBlock)]) extends Instruction

case class InstPhi(var opndTy: Type, var cases: Seq[(BasicBlock, SSAVariable)]) extends Instruction

case class InstCall(var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable],
                    var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable]
                    ) extends AbstractCall with HasExcClause with HasKeepAliveClause

case class InstTailCall(var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable]) extends AbstractCall

case class InstRet(var retTy: Type, var retVal: SSAVariable) extends AbstractRet

case class InstRetVoid() extends AbstractRet

case class InstThrow(var excVal: SSAVariable) extends Instruction

case class InstLandingPad() extends Instruction

case class InstExtractValue(var strTy: TypeStruct, var index: Int, var opnd: SSAVariable) extends Instruction


case class InstInsertValue(var strTy: TypeStruct, var index: Int, var opnd: SSAVariable, var newVal: SSAVariable) extends Instruction

case class InstExtractElement(var vecTy: TypeVector, var indTy: TypeInt,
                              var opnd: SSAVariable, var index: SSAVariable) extends Instruction

case class InstInsertElement(var vecTy: TypeVector, var indTy: TypeInt,
                             var opnd: SSAVariable, var index: SSAVariable, var newVal: SSAVariable) extends Instruction

case class InstShuffleVector(var vecTy: TypeVector, var maskTy: TypeVector,
                             var vec1: SSAVariable, var vec2: SSAVariable, var mask: SSAVariable) extends Instruction

case class InstNew(var allocTy: Type, var excClause: Option[ExcClause]) extends HeapAlloc with FixedAlloc

case class InstNewHybrid(var allocTy: TypeHybrid, var lenTy: TypeInt, var length: SSAVariable, var excClause: Option[ExcClause]) extends HeapAlloc with HybridAlloc

case class InstAlloca(var allocTy: Type, var excClause: Option[ExcClause]) extends StackAlloc with FixedAlloc

case class InstAllocaHybrid(var allocTy: TypeHybrid, var lenTy: TypeInt, var length: SSAVariable, var excClause: Option[ExcClause]) extends StackAlloc with HybridAlloc

case class InstGetIRef(var referentTy: Type, var opnd: SSAVariable) extends Instruction

case class InstGetFieldIRef(var referentTy: TypeStruct, var index: Int, var opnd: SSAVariable) extends Instruction

case class InstGetElemIRef(var referentTy: AbstractSeqType, var indTy: TypeInt,
                           var opnd: SSAVariable, var index: SSAVariable) extends Instruction

case class InstShiftIRef(var referentTy: Type, var offTy: TypeInt,
                         var opnd: SSAVariable, var offset: SSAVariable) extends Instruction

case class InstGetFixedPartIRef(var referentTy: TypeHybrid, var opnd: SSAVariable) extends Instruction

case class InstGetVarPartIRef(var referentTy: TypeHybrid, var opnd: SSAVariable) extends Instruction

case class InstLoad(var ord: MemoryOrder, var referentTy: Type, var loc: SSAVariable, var excClause: Option[ExcClause]) extends HasExcClause

case class InstStore(var ord: MemoryOrder, var referentTy: Type, var loc: SSAVariable, var newVal: SSAVariable, var excClause: Option[ExcClause]) extends HasExcClause

case class InstCmpXchg(var weak: Boolean, var ordSucc: MemoryOrder, var ordFail: MemoryOrder, var referentTy: Type,
                       var loc: SSAVariable, var expected: SSAVariable, var desired: SSAVariable, var excClause: Option[ExcClause])  extends HasExcClause

case class InstAtomicRMW(var ord: MemoryOrder, var op: AtomicRMWOptr,
                         var referentTy: Type, var loc: SSAVariable, var opnd: SSAVariable, var excClause: Option[ExcClause]) extends HasExcClause

case class InstFence(var ord: MemoryOrder) extends Instruction

case class InstTrap(var retTy: Type, var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable]) extends AbstractTrap

case class InstWatchPoint(var wpID: Int, var retTy: Type,
                          var dis: BasicBlock, var ena: BasicBlock, var exc: Option[BasicBlock],
                          var keepAlives: Seq[LocalVariable]) extends AbstractTrap

case class InstCCall(var callConv: CallConv, var funcTy: Type,
                     var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable]) extends CallLike

case class InstNewStack(var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable],
                        var excClause: Option[ExcClause]) extends CallLike with HasExcClause

case class InstSwapStack(var swappee: SSAVariable, var curStackAction: CurStackAction, var newStackAction: NewStackAction,
                         var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable]) extends HasExcClause with HasKeepAliveClause

case class InstCommInst(var inst: CommInst, var typeList: Seq[Type], var argList: Seq[SSAVariable],
                        var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable])
  extends HasTypeList with HasArgList with HasExcClause with HasKeepAliveClause
