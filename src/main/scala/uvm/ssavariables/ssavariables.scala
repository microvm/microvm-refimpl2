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

/** For both int<n> and pointers. Convert to Long when needed. */
case class ConstInt(var constTy: Type, var num: BigInt) extends Constant

case class ConstFloat(var constTy: Type, var num: Float) extends Constant

case class ConstDouble(var constTy: Type, var num: Double) extends Constant

/** For struct, array and vector. */
case class ConstSeq(var constTy: Type, var elems: Seq[GlobalVariable]) extends Constant

/** For all NULL-able values. Note: null pointers are ConstInt(???, 0) */
case class ConstNull(var constTy: Type) extends Constant

case class GlobalCell(var cellTy: Type) extends GlobalVariable

case class ExposedFunc(var func: Function, var callConv: Flag, var cookie: ConstInt) extends GlobalVariable

// Local variables: Parameters and Instructions

abstract class LocalVariable extends SSAVariable

abstract class Parameter extends LocalVariable
case class NorParam(ty: Type) extends Parameter
case class ExcParam() extends Parameter

case class InstResult(inst: Instruction, index: Int) extends LocalVariable

// Instructions

abstract class Instruction extends IdentifiedSettable {
  var results: Seq[InstResult] = Seq()
  
  override def toString = "[(%s) = %s %s]".format(this.results.map(_.repr).mkString(" "), this.repr, this.getClass.getSimpleName)
}

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
  val TRUNC, ZEXT, SEXT, FPTRUNC, FPEXT, FPTOUI, FPTOSI, UITOFP, SITOFP, BITCAST, REFCAST, PTRCAST = Value
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

/// Abstract instructions and traits

trait MaybeTerminator extends Instruction {
  def canTerminate: Boolean
}
trait Terminator extends MaybeTerminator {
  override def canTerminate: Boolean = true
}

trait OSRPoint extends Instruction

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

case class DestClause(val bb: BasicBlock, val args: Seq[SSAVariable])

case class ExcClause(val nor: DestClause, val exc: DestClause)

trait HasExcClause extends Instruction with MaybeTerminator {
  var excClause: Option[ExcClause]
  override def canTerminate: Boolean = excClause.isDefined
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

trait WorksWithPointer extends Instruction {
  var ptr: Boolean
}

abstract class AbstractTrap extends HasKeepAliveClause with OSRPoint {
  var retTys: Seq[Type]
}

abstract class CurStackAction
case class RetWith(var retTys: Seq[Type]) extends CurStackAction
case class KillOld() extends CurStackAction

abstract class NewStackAction
case class PassValues(var argTys: Seq[Type], var args: Seq[SSAVariable]) extends NewStackAction
case class ThrowExc(var exc: SSAVariable) extends NewStackAction

/**
 * Flags are used in common instructions.
 */
case class Flag(name: String)

/// Concrete instructions
case class InstBinOp(var op: BinOptr, var opndTy: Type, var op1: SSAVariable, var op2: SSAVariable,
                     var excClause: Option[ExcClause]) extends HasExcClause

case class InstCmp(var op: CmpOptr, var opndTy: Type, var op1: SSAVariable, var op2: SSAVariable) extends Instruction

case class InstConv(var op: ConvOptr, var fromTy: Type, var toTy: Type, var opnd: SSAVariable) extends Instruction

case class InstSelect(var condTy: Type, var opndTy: Type,
                      var cond: SSAVariable, var ifTrue: SSAVariable, var ifFalse: SSAVariable) extends Instruction

case class InstBranch(var dest: DestClause) extends Instruction with Terminator

case class InstBranch2(var cond: SSAVariable, var ifTrue: DestClause, var ifFalse: DestClause) extends Instruction with Terminator

case class InstSwitch(var opndTy: Type, var opnd: SSAVariable, var defDest: DestClause,
                      var cases: Seq[(SSAVariable, DestClause)]) extends Instruction with Terminator

case class InstCall(var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable],
                    var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable])
    extends AbstractCall with HasExcClause with HasKeepAliveClause with OSRPoint

case class InstTailCall(var sig: FuncSig, var callee: SSAVariable, var argList: Seq[SSAVariable]) extends AbstractCall with Terminator

case class InstRet(val funcVer: FuncVer, var retVals: Seq[SSAVariable]) extends AbstractRet with Terminator

case class InstThrow(var excVal: SSAVariable) extends Instruction with Terminator

case class InstExtractValue(var strTy: TypeStruct, var index: Int, var opnd: SSAVariable) extends Instruction

case class InstInsertValue(var strTy: TypeStruct, var index: Int, var opnd: SSAVariable, var newVal: SSAVariable) extends Instruction

case class InstExtractElement(var seqTy: AbstractSeqType, var indTy: TypeInt,
                              var opnd: SSAVariable, var index: SSAVariable) extends Instruction

case class InstInsertElement(var seqTy: AbstractSeqType, var indTy: TypeInt,
                             var opnd: SSAVariable, var index: SSAVariable, var newVal: SSAVariable) extends Instruction

case class InstShuffleVector(var vecTy: TypeVector, var maskTy: TypeVector,
                             var vec1: SSAVariable, var vec2: SSAVariable, var mask: SSAVariable) extends Instruction

case class InstNew(var allocTy: Type, var excClause: Option[ExcClause]) extends HeapAlloc with FixedAlloc

case class InstNewHybrid(var allocTy: TypeHybrid, var lenTy: TypeInt, var length: SSAVariable, var excClause: Option[ExcClause]) extends HeapAlloc with HybridAlloc

case class InstAlloca(var allocTy: Type, var excClause: Option[ExcClause]) extends StackAlloc with FixedAlloc

case class InstAllocaHybrid(var allocTy: TypeHybrid, var lenTy: TypeInt, var length: SSAVariable, var excClause: Option[ExcClause]) extends StackAlloc with HybridAlloc

case class InstGetIRef(var referentTy: Type, var opnd: SSAVariable) extends Instruction

case class InstGetFieldIRef(var ptr: Boolean, var referentTy: AbstractStructType, var index: Int, var opnd: SSAVariable) extends WorksWithPointer

case class InstGetElemIRef(var ptr: Boolean, var referentTy: AbstractSeqType, var indTy: TypeInt,
                           var opnd: SSAVariable, var index: SSAVariable) extends WorksWithPointer

case class InstShiftIRef(var ptr: Boolean, var referentTy: Type, var offTy: TypeInt,
                         var opnd: SSAVariable, var offset: SSAVariable) extends WorksWithPointer

case class InstGetVarPartIRef(var ptr: Boolean, var referentTy: TypeHybrid, var opnd: SSAVariable) extends WorksWithPointer

case class InstLoad(var ptr: Boolean, var ord: MemoryOrder, var referentTy: Type, var loc: SSAVariable, var excClause: Option[ExcClause]) extends WorksWithPointer with HasExcClause

case class InstStore(var ptr: Boolean, var ord: MemoryOrder, var referentTy: Type, var loc: SSAVariable, var newVal: SSAVariable, var excClause: Option[ExcClause]) extends WorksWithPointer with HasExcClause

case class InstCmpXchg(var ptr: Boolean, var weak: Boolean, var ordSucc: MemoryOrder, var ordFail: MemoryOrder, var referentTy: Type,
                       var loc: SSAVariable, var expected: SSAVariable, var desired: SSAVariable, var excClause: Option[ExcClause]) extends WorksWithPointer with HasExcClause

case class InstAtomicRMW(var ptr: Boolean, var ord: MemoryOrder, var op: AtomicRMWOptr,
                         var referentTy: Type, var loc: SSAVariable, var opnd: SSAVariable, var excClause: Option[ExcClause]) extends WorksWithPointer with HasExcClause

case class InstFence(var ord: MemoryOrder) extends Instruction

case class InstTrap(var retTys: Seq[Type], var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable])
  extends AbstractTrap with HasExcClause

case class InstWatchPoint(var wpID: Int, var retTys: Seq[Type],
                          var dis: DestClause, var ena: DestClause, var exc: Option[DestClause],
                          var keepAlives: Seq[LocalVariable]) extends AbstractTrap with Terminator

case class InstWPBranch(var wpID: Int, var dis: DestClause, var ena: DestClause) extends Instruction with Terminator

case class InstCCall(var callConv: Flag, var funcTy: Type, var sig: FuncSig, var callee: SSAVariable,
                     var argList: Seq[SSAVariable], var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable])
    extends CallLike with HasExcClause with HasKeepAliveClause with OSRPoint

case class InstNewThread(var stack: SSAVariable, var threadLocal: Option[SSAVariable],
                         var newStackAction: NewStackAction, var excClause: Option[ExcClause]) extends Instruction with HasExcClause

case class InstSwapStack(var swappee: SSAVariable, var curStackAction: CurStackAction, var newStackAction: NewStackAction,
                         var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable]) extends HasExcClause with HasKeepAliveClause with OSRPoint {
  override def canTerminate: Boolean = curStackAction == KillOld() || excClause.isDefined
}

case class InstCommInst(var inst: CommInst, var flagList: Seq[Flag], var typeList: Seq[Type], var funcSigList: Seq[FuncSig], var argList: Seq[SSAVariable],
                        var excClause: Option[ExcClause], var keepAlives: Seq[LocalVariable])
    extends HasTypeList with HasArgList with HasExcClause with HasKeepAliveClause {
  override def canTerminate: Boolean = excClause.isDefined || inst.isTerminator
}
