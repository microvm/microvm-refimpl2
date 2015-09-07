package uvm.clientsupport.text

import scala.beans._
import java.util._
import DefaultTypes._
import uvm.ir.textinput.gen.UIRParser.TopLevelDefContext

/**
 * This object contains common types and helper methods.
 */
object DefaultTypes {
  type MuName = String
  type TypeMuName = MuName
  type SigMuName = MuName
  type VarMuName = MuName
  type BBMuName = MuName
  type Flag = String
  type ListType[T] = List[T]
  type ListImpl[T] = ArrayList[T]

  def makeList[T](): ListType[T] = new ListImpl[T]()
}

/**
 * A Mu IR bundle. This class is more representation-oriented rather than semantic oriented. For example, there are
 * "type definitions" rather than "types". Similarly "function definitions" and "function declarations" are separated
 * rather than contained in each other.
 */
class Bundle() {
  @BeanProperty var typeDefs: ListType[TypeDef] = makeList()
  @BeanProperty var funcSigDefs: ListType[FuncSigDef] = makeList()
  @BeanProperty var constDefs: ListType[ConstDef] = makeList()
  @BeanProperty var globalCellDefs: ListType[GlobalCellDef] = makeList()
  @BeanProperty var funcDefs: ListType[FuncDef] = makeList()
  @BeanProperty var funcDecls: ListType[FuncDecl] = makeList()
  @BeanProperty var expFuncDefs: ListType[FuncExpDef] = makeList()
}

/**
 * Anything that has a name. The name may be global (@) or local (%). This includes all top-level definitions as well as
 * parameters, basic blocks and instructions.
 */
abstract class HasName {
  @BeanProperty var name: MuName = _
}

/**
 * The base of all top-level definitions.
 */
abstract class TopLevelDefinition extends HasName

/**
 * A type definition. ".typedef"
 */
class TypeDef extends TopLevelDefinition

/** The int type. */
class TypeInt() extends TypeDef {
  @BeanProperty var len: Int = _
}

/** The float type. */
class TypeFloat() extends TypeDef
/** The double type. */
class TypeDouble() extends TypeDef

abstract class AbstractRefType extends TypeDef {
  @BeanProperty var ty: MuName = _
}

/** The ref type. */
class TypeRef() extends AbstractRefType
/** The iref type. */
class TypeIRef() extends AbstractRefType
/** The weakref type. */
class TypeWeakRef() extends AbstractRefType

/** The struct type. */
class TypeStruct() extends TypeDef {
  @BeanProperty var fieldTy: List[MuName] = makeList()
}

abstract class AbstractSeqType extends TypeDef {
  @BeanProperty var elemTy: MuName = _
  @BeanProperty var len: Long = _
}

/** The array type. */
class TypeArray() extends AbstractSeqType
/** The vector type. */
class TypeVector() extends AbstractSeqType

/** The hybrid type. */
class TypeHybrid() extends TypeDef {
  @BeanProperty var fixedTy: MuName = _
  @BeanProperty var varTy: MuName = _
}

/** The void type. */
class TypeVoid() extends TypeDef

/** The func type. */
class TypeFunc() extends TypeDef {
  @BeanProperty var sig: MuName = _
}

/** The thread type. */
class TypeThread() extends TypeDef
/** The stack type. */
class TypeStack() extends TypeDef
/** The tagref64 type. */
class TypeTagRef64() extends TypeDef

abstract class AbstractPointerType extends TypeDef

/** The ptr type. */
class TypePtr() extends AbstractPointerType {
  @BeanProperty var ty: MuName = _
}

/** The funcptr type. */
class TypeFuncPtr() extends AbstractPointerType {
  @BeanProperty var sig: MuName = _
}

/** A function signature. ".funcsig" */
class FuncSigDef() extends TopLevelDefinition {
  @BeanProperty var retTy: MuName = _
  @BeanProperty var paramTy: List[MuName] = makeList()
}

/** A constant. ".const" */
abstract class ConstDef() extends TopLevelDefinition {
  @BeanProperty var ty: MuName = _
}

/** A global cell definition. ".global" */
class GlobalCellDef() extends TopLevelDefinition {
  @BeanProperty var ty: MuName = _
}

/** A function declaration. ".funcdecl" */
class FuncDecl() extends TopLevelDefinition {
  @BeanProperty var sig: MuName = _
}

/** A function definition. ".funcdef" */
class FuncDef() extends TopLevelDefinition {
  @BeanProperty var version: MuName = _
  @BeanProperty var sig: MuName = _
  @BeanProperty var params: List[MuName] = makeList()
  @BeanProperty var bbs: List[BasicBlock] = makeList()
}

/** A function exposing definition ".expose" */
class FuncExpDef() extends HasName {
  @BeanProperty var func: MuName = _
  @BeanProperty var callConv: Flag = _
  @BeanProperty var cookie: MuName = _
}

/** A basic block. "%blahblah:" */
class BasicBlock() extends HasName {
  @BeanProperty var insts: List[Instruction] = makeList()
}

/**
 * An instruction.
 *  <p>
 *  The name is optional in the text form (assign null to name), but it is highly recommended to name all instructions
 *  even if they return void (esp. function calls to functions that return void, in which case Mu can provide stack
 *  information).
 */
abstract class Instruction() extends HasName

/** An exception clause. */
case class ExcClause(
  @BeanProperty var nor: BBMuName,
  @BeanProperty var exc: BBMuName)

/**
 *  MixIn of all instructions that may have an exception clause.
 */
trait HasExcClause {
  /** The exception clause. May be null. */
  @BeanProperty var excClause: ExcClause = _
}

/**
 *  MixIn of all instructions that may have keep-alive variables.
 */
trait HasKeepAlives {
  /** A list of keep-alive variables. May be empty, but not null. */
  @BeanProperty var keepAlives: List[VarMuName] = makeList()
}

/**
 * A call-like instruction has a signature, a callee and an argument list.
 */
trait CallLike {
  /** The signature of the callee. */
  @BeanProperty var sig: SigMuName = _
  /** The callee. The type depends on the concrete instruction. For CALL, it is func&lt;sig&gt;. */
  @BeanProperty var callee: VarMuName = _
  /** The argument list. */
  @BeanProperty var argList: List[VarMuName] = makeList()
}
/** ADD, SUB, MUL, ... */
class InstBinOp() extends Instruction with HasExcClause {
  /** The operator (ADD, SUB, MUL, ...). */
  @BeanProperty var op: String = _ // binOp in spec
  @BeanProperty var opndTy: TypeMuName = _ // T in spec
  @BeanProperty var op1: VarMuName = _
  @BeanProperty var op2: VarMuName = _
}

/** EQ, NE, ... */
class InstCmp() extends Instruction {
  /** The operator (EQ, NE, SLT, SLE, ..., FUEQ, FSEQ, ...). */
  @BeanProperty var op: String = _ // compOp in spec
  @BeanProperty var opndTy: TypeMuName = _ // T in spec
  @BeanProperty var op1: VarMuName = _
  @BeanProperty var op2: VarMuName = _
}

/** TRUNC, ZEXT, ... */
class InstConv() extends Instruction {
  /** The operator (TRUNC, EXT, ...). */
  @BeanProperty var op: String = _ // convOp in spec
  @BeanProperty var fromTy: TypeMuName = _ // T1 in spec
  @BeanProperty var toTy: TypeMuName = _ // T2 in spec
  @BeanProperty var opnd: VarMuName = _
}

/** SELECT */
class InstSelect() extends Instruction {
  @BeanProperty var condTy: TypeMuName = _ // S in spec
  @BeanProperty var opndTy: TypeMuName = _ // T in spec
  @BeanProperty var cond: VarMuName = _
  @BeanProperty var ifTrue: VarMuName = _
  @BeanProperty var ifFalse: VarMuName = _
}

/** BRANCH */
class InstBranch() extends Instruction {
  @BeanProperty var dest: BBMuName = _
}

/** BRANCH2 */
class InstBranch2() extends Instruction {
  @BeanProperty var cond: VarMuName = _
  @BeanProperty var ifTrue: BBMuName = _
  @BeanProperty var ifFalse: BBMuName = _
}

/** A case in the SWITCH instruction. */
case class SwitchCase(
  @BeanProperty var value: VarMuName,
  @BeanProperty var dest: BBMuName)

/** SWITCH */
class InstSwitch() extends Instruction {
  @BeanProperty var opndTy: TypeMuName = _
  @BeanProperty var opnd: VarMuName = _
  @BeanProperty var defDest: BBMuName = _
  @BeanProperty var cases: List[SwitchCase] = makeList() // value:dest; pair in spec
}

/** A case in the PHI instruction. */
case class PhiCase(
  @BeanProperty var source: BBMuName,
  @BeanProperty var value: VarMuName)

/** PHI */
class InstPhi() extends Instruction {
  @BeanProperty var opndTy: TypeMuName = _
  @BeanProperty var cases: List[PhiCase] = makeList() // bb:value; pair in spec
}

/** CALL */
class InstCall() extends Instruction with CallLike with HasExcClause with HasKeepAlives

/** TAILCALL */
class InstTailCall() extends Instruction with CallLike

/** RET */
class InstRet() extends Instruction {
  @BeanProperty var retTy: TypeMuName = _ // T in spec
  @BeanProperty var retVal: VarMuName = _ // rv in spec
}

/** RETVOID */
class InstRetVoid() extends Instruction

/** THROW */
class InstThrow() extends Instruction {
  @BeanProperty var excVal: VarMuName = _ // exc in spec
}

/** LANDINGPAD */
class InstLandingPad() extends Instruction

/** EXTRACTVALUE */
class InstExtractValue() extends Instruction {
  @BeanProperty var strTy: TypeMuName = _ // T in spec
  @BeanProperty var index: Int = _
  @BeanProperty var opnd: VarMuName = _
}

/** INSERTVALUE */
class InstInsertValue() extends Instruction {
  @BeanProperty var strTy: TypeMuName = _ // T in spec
  @BeanProperty var index: Int = _
  @BeanProperty var opnd: VarMuName = _
  @BeanProperty var newVal: VarMuName = _
}

/** EXTRACTELEMENT */
class InstExtractElement() extends Instruction {
  @BeanProperty var vecTy: TypeMuName = _ // T1 in spec
  @BeanProperty var indTy: TypeMuName = _ // T2 in spec
  @BeanProperty var opnd: VarMuName = _
  @BeanProperty var index: VarMuName = _
}

/** INSERTELEMENT */
class InstInsertElement() extends Instruction {
  @BeanProperty var vecTy: TypeMuName = _ // T1 in spec
  @BeanProperty var indTy: TypeMuName = _ // T2 in spec
  @BeanProperty var opnd: VarMuName = _
  @BeanProperty var index: VarMuName = _
  @BeanProperty var newVal: VarMuName = _
}

/** SHUFFLEVECTOR */
class InstShuffleVector() extends Instruction {
  @BeanProperty var vecTy: TypeMuName = _ // T1 in spec
  @BeanProperty var maskTy: TypeMuName = _ // T2 in spec
  @BeanProperty var vec1: VarMuName = _
  @BeanProperty var vec2: VarMuName = _
  @BeanProperty var mask: VarMuName = _
}

/** MixIn for fixed-size allocation (NEW and ALLOCA) */
trait FixedAlloc {
  /** The type to allocate. */
  @BeanProperty var allocTy: TypeMuName = _ // T1 in spec
}

/** MixIn for hybrid allocation (NEWHYBRID and ALLOCAHYBRID) */
trait HybridAlloc {
  /** The type to allocate. */
  @BeanProperty var allocTy: TypeHybrid = _ // T1 in spec
  /** The type of the length argument. Must be int. */
  @BeanProperty var lenTy: TypeInt = _ // T2 in spec
  /** The length. */
  @BeanProperty var length: VarMuName = _
}

/** NEW */
class InstNew() extends Instruction with FixedAlloc with HasExcClause

/** NEWHYBRID */
class InstNewHybrid() extends Instruction with HybridAlloc with HasExcClause

/** ALLOCA */
class InstAlloca() extends Instruction with FixedAlloc with HasExcClause

/** ALLOCAHYBRID */
class InstAllocaHybrid() extends Instruction with HybridAlloc with HasExcClause

/** MixIn for memory addressing. All of them has a source type and an operand. */
trait MemoryAddressing {
  /** The referent type.  */
  @BeanProperty var referentTy: TypeMuName = _ // T in spec
  /** The source reference or pointer. */
  @BeanProperty var opnd: VarMuName = _
}

/** GETIREF */
class InstGetIRef() extends Instruction with MemoryAddressing

/** MixIn for instructions that can work with both irefs and ptrs. */
trait WorksWithPointer {
  @BeanProperty var ptr: Boolean = _
}

/** GETFIXEDPARTIREF */
class InstGetFieldIRef() extends Instruction with MemoryAddressing with WorksWithPointer {
  @BeanProperty var index: Int = _
}

/** GETELEMIREF */
class InstGetElemIRef() extends Instruction with MemoryAddressing with WorksWithPointer {
  @BeanProperty var indTy: TypeMuName = _ // T2 in spec
  @BeanProperty var index: VarMuName = _
}

/** SHIFTIREF */
class InstShiftIRefextends() extends Instruction with MemoryAddressing with WorksWithPointer {
  @BeanProperty var offTy: TypeMuName = _ // T2 in spec
  @BeanProperty var offset: VarMuName = _
}

/** GETFIXEDPARTIREF */
class InstGetFixedPartIRef() extends Instruction with MemoryAddressing with WorksWithPointer

/** GETVARPARTIREF */
class InstGetVarPartIRef() extends Instruction with MemoryAddressing with WorksWithPointer

/** MixIn for memory accessing. All of them has a source type and an operand. */

trait MemoryAccessing {
  /** The referent type.  */
  @BeanProperty var referentTy: TypeMuName = _ // T in spec
  /** The memory location/address to access. */
  @BeanProperty var loc: VarMuName = _
}

/** LOAD */
class InstLoad() extends Instruction with WorksWithPointer with MemoryAccessing with HasExcClause {
  /** The memory order. */
  @BeanProperty var ord: String = "NOT_ATOMIC"
}

/** STORE */
class InstStore() extends Instruction with WorksWithPointer with MemoryAccessing with HasExcClause {
  /** The memory order. */
  @BeanProperty var ord: String = "NOT_ATOMIC"
  @BeanProperty var newVal: VarMuName = _
}

/** CMPXCHG */
class InstCmpXchg() extends Instruction with WorksWithPointer with HasExcClause {
  @BeanProperty var weak: Boolean = false
  /** The memory order when successful. Must not be null. Cannot be NOT_ATOMIC. */
  @BeanProperty var ordSucc: String = _
  /** The memory order when failed. Must not be null. Cannot be NOT_ATOMIC. */
  @BeanProperty var ordFail: String = _
  @BeanProperty var expected: VarMuName = _
  @BeanProperty var desired: VarMuName = _
}

/** ATOMICRMW */
class InstAtomicRMW() extends Instruction with WorksWithPointer with HasExcClause {
  @BeanProperty var ord: String = _
  @BeanProperty var op: String = _
  @BeanProperty var opnd: VarMuName = _
}

class InstFence() extends Instruction {
  @BeanProperty var ord: String = _
}

/** TRAP */
class InstTrap() extends Instruction with HasExcClause with HasKeepAlives {
  @BeanProperty var retTy: TypeMuName = _ // T in spec
}

/** WATCHPOINT */
class InstWatchPoint() extends Instruction with HasKeepAlives {
  @BeanProperty var wpid: Int = _
  @BeanProperty var retTy: TypeMuName = _ // T in spec
  @BeanProperty var dis: BBMuName = _
  @BeanProperty var ena: BBMuName = _
  @BeanProperty var exc: BBMuName = _ // may be null
}

/** CCALL */
class InstCCall() extends Instruction with CallLike with HasKeepAlives {
  @BeanProperty var callConv: Flag = _
  @BeanProperty var funcTy: TypeMuName = _ // T in spec
}

/** NEWSTACK */
class InstNewStack() extends Instruction with CallLike with HasExcClause

abstract class CurStackClause
/** RET_WITH */
case class RetWith(
  @BeanProperty var retTy: TypeMuName /* T1 */) extends CurStackClause
/** KILL_OLD */
case class KillOld() extends CurStackClause

abstract class NewStackClause
/** PASS_VALUE */
case class PassValue(
  @BeanProperty var argTy: TypeMuName /* T2 */,
  @BeanProperty var arg: VarMuName) extends NewStackClause
/** PASS_VOID */
case class PassVoid() extends NewStackClause
/** THROW_EXC */
case class ThrowExc(
    @BeanProperty var exc: VarMuName) extends NewStackClause

/** SWAPSTACK */
class InstSwapStack() extends Instruction with HasExcClause with HasKeepAlives {
  @BeanProperty var swappee: VarMuName = _
  @BeanProperty var curStackClause: CurStackClause = _
  @BeanProperty var newStackClause: NewStackClause = _
}

/** COMMINST */
class InstCommInst() extends Instruction with HasExcClause with HasKeepAlives {
  @BeanProperty var inst: String = _
  @BeanProperty var flagList: List[Flag] = makeList()
  @BeanProperty var typeList: List[TypeMuName] = makeList()
  @BeanProperty var funcSigList: List[SigMuName] = makeList()
  @BeanProperty var argList: List[VarMuName] = makeList()
}
