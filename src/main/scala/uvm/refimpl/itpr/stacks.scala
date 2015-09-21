package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.HashMap
import scala.collection.AbstractIterator
import uvm.refimpl.nat.NativeStackKeeper
import uvm.refimpl.nat.NativeCallResult
import uvm.refimpl.nat.NativeCallHelper

abstract class StackState

object StackState {
  case class Ready(t: Type) extends uvm.refimpl.itpr.StackState
  case object Running extends StackState
  case object Dead extends StackState
}

class InterpreterStack(val id: Int, val stackMemory: StackMemory, stackBottomFunc: FuncVer, args: Seq[ValueBox])(
    implicit nativeCallHelper: NativeCallHelper) {
  var gcMark: Boolean = false // Mark for GC.

  private var _state: StackState = StackState.Ready(InternalTypes.VOID) // Initial state is READY<void>
  def state = _state
  private def state_=(s: StackState) = _state = s

  private var _top: InterpreterFrame = InterpreterFrame.forMuFunc(stackBottomFunc, args, None)
  def top = _top
  private def top_=(f: InterpreterFrame) = _top = f

  var maybeNSK: Option[NativeStackKeeper] = None

  private def ensureNSK(): Unit = {
    if (maybeNSK == None) {
      val nsk = new NativeStackKeeper()
      maybeNSK = Some(nsk)
    }
  }

  def frames: Iterator[InterpreterFrame] = new AbstractIterator[InterpreterFrame] {
    var curFrame: Option[InterpreterFrame] = Some(top)
    def hasNext = curFrame.isDefined
    def next = {
      val res = curFrame.get
      curFrame = res.prev
      res
    }
  }

  def muFrames: Iterator[MuFrame] = frames.filter(_.isInstanceOf[MuFrame]).map(_.asInstanceOf[MuFrame])

  def pushMuFrame(funcVer: FuncVer, args: Seq[ValueBox]): Unit = {
    val newFrame = InterpreterFrame.forMuFunc(funcVer, args, Some(top))
    top = newFrame
    top.savedStackPointer = stackMemory.top
  }

  def replaceTopMuFrame(funcVer: FuncVer, args: Seq[ValueBox]): Unit = {
    val newFrame = InterpreterFrame.forMuFunc(funcVer, args, top.prev)
    stackMemory.rewind(top.savedStackPointer)
    top = newFrame
    top.savedStackPointer = stackMemory.top
  }

  private def pushNativeFrame(func: Word): Unit = {
    val newFrame = InterpreterFrame.forNativeFunc(func, Some(top))
    top = newFrame
    top.savedStackPointer = stackMemory.top
  }

  def callNativeOnStack(sig: FuncSig, func: Word, args: Seq[ValueBox], retBox: ValueBox): NativeCallResult = {
    assert(top.isInstanceOf[MuFrame])
    ensureNSK()
    pushNativeFrame(func)
    val result = maybeNSK.get.callNative(sig, func, args, retBox)
    handleNativeCallResult(result)
    result
  }

  def returnToNativeOnStack(): NativeCallResult = {
    assert(top.isInstanceOf[NativeFrame])
    val result = maybeNSK.get.returnToCallBack()
    handleNativeCallResult(result)
    result
  }

  private def handleNativeCallResult(nsr: NativeCallResult): Unit = nsr match {
    case r: NativeCallResult.Return   => popNativeFrame()
    case r: NativeCallResult.CallBack => top.asInstanceOf[NativeFrame].maybeCallback = Some(r)
  }

  def popFrame(): Unit = {
    stackMemory.rewind(top.savedStackPointer)
    top = top.prev.getOrElse {
      throw new UvmRuntimeException("Attemting to pop the last frame of a stack. Stack ID: %d.".format(id))
    }
  }

  private def popNativeFrame(): Unit = {
    assert(top.isInstanceOf[NativeFrame])
    popFrame()
  }

  def unwindTo(f: InterpreterFrame): Unit = {
    top = f
    stackMemory.rewind(top.savedStackPointer)
  }

  def unbindFromThread(retTy: Type): Unit = {
    state = StackState.Ready(retTy)
  }

  def rebindToThread(): Unit = {
    state = StackState.Running
  }

  def kill(): Unit = {
    state = StackState.Dead
    maybeNSK.foreach { nsk =>
      nsk.close()
    }
  }
}

abstract class InterpreterFrame(val prev: Option[InterpreterFrame]) {
  /**
   * The stack pointer to restore to when unwinding THE CURRENT FRAME. In other word, this value is the stackMemory.top
   * of the stack when the current frame is pushed.
   * <p>
   * Native frames do not really use this. They use their own stacks.
   */
  var savedStackPointer: Word = 0
}

object InterpreterFrame {
  def forMuFunc(funcVer: FuncVer, args: Seq[ValueBox], prev: Option[InterpreterFrame]): MuFrame = {
    val frm = new MuFrame(funcVer, prev) // Bottom frame

    for ((p, a) <- (funcVer.params zip args)) {
      frm.boxes(p).copyFrom(a)
    }

    frm
  }

  def forNativeFunc(func: Word, prev: Option[InterpreterFrame]): NativeFrame = {
    val frm = new NativeFrame(func, prev)
    frm
  }
}

class MuFrame(val funcVer: FuncVer, prev: Option[InterpreterFrame]) extends InterpreterFrame(prev) {
  val boxes = new HashMap[LocalVariable, ValueBox]()

  /** Edge-assigned instructions take values determined at look backedges */
  val edgeAssignedBoxes = new HashMap[EdgeAssigned, ValueBox]()

  /** Current basic block */
  var curBB: BasicBlock = funcVer.entry

  /** Current instruction index within the current basic block */
  var curInstIndex: Int = 0

  /**
   * curInstHalfExecuted is true if the current instruction is partially executed and may continue when resumed.
   * <p>
   * Examples include:
   * <ul>
   * <li>CALL: When executing CALL, the interpretation continues with the new fame. At this time, the CALL is partially
   * executed. When returning, the CALL is exposed to the InterpreterThread again and the other half is executed --
   * assigning the return value to the ValueBox of the CALL instruction.
   * <li>SWAPSTACK: The first half is swapping away from the stack. The second half is when swapping back, the
   * interpreter will assign the return value and decide where to continue (normally or exceptionally).
   * <li>TRAP/WATCHPOINT: The current stack becomes unbound on entering the client. When returning from the client,
   * there is a stack re-binding. The same thing happens as SWAPSTACK.
   * </ul>
   * The CCALL instruction does not use this flag, because the control flow is deterministic. Unlike swapstack, CCALL
   * must return from the only source -- the native program.
   * </p>
   * This flag is set when executing CALL, SWAPSTACK, TRAP or WATCHPOINT. It is cleared when executing
   * InterpreterThread.finishHalfExecutedInst or InterpreterThread.catchException. Particularly, the RET, RETVOID,
   * THROW, TRAP, WATCHPOINT and SWAPSTACK instruction will call those two functions.
   */
  var curInstHalfExecuted: Boolean = false

  makeBoxes()

  private def makeBoxes() {
    for (param <- funcVer.params) {
      putBox(param)
    }
    for (bb <- funcVer.bbs; inst <- bb.insts) {
      putBox(inst)
    }
  }

  private def putBox(lv: LocalVariable) {
    val ty = TypeInferer.inferType(lv)
    boxes.put(lv, ValueBox.makeBoxForType(ty))
    if (lv.isInstanceOf[EdgeAssigned]) {
      edgeAssignedBoxes.put(lv.asInstanceOf[EdgeAssigned], ValueBox.makeBoxForType(ty))
    }
  }

  def curInst: Instruction = try {
    curBB.insts(curInstIndex)
  } catch {
    case e: IndexOutOfBoundsException =>
      if (curInstIndex == -1) {
        throw new UvmRefImplException(("The current stack has never been bound to a thread." +
          "FuncVer: %s, BasicBlock: %s").format(funcVer.repr, curBB.repr))
      } else {
        throw new UvmRefImplException(("Current instruction %d beyond the last instruction of a basic block. " +
          "FuncVer: %s, BasicBlock: %s").format(curInstIndex, funcVer.repr, curBB.repr))
      }
  }

  def incPC() {
    curInstIndex += 1
  }

  def jump(bb: BasicBlock, ix: Int) {
    curBB = bb
    curInstIndex = ix
  }

  def keepaliveBoxes(): Seq[ValueBox] = {
    curInst match {
      case hka: HasKeepAliveClause =>
        val kas = hka.keepAlives
        val kaBoxes = kas.map(boxes.apply)
        kaBoxes
      case i => throw new UvmRefImplException("Instruction does not have keepalives: " + i.repr)
    }
  }
}

class NativeFrame(val func: Word, prev: Option[InterpreterFrame]) extends InterpreterFrame(prev) {
  /**
   * When calling back from native, maybeCallback is set to the CallBack object.
   * Useful when returning value to the native.
   */
  var maybeCallback: Option[NativeCallResult.CallBack] = None
}
