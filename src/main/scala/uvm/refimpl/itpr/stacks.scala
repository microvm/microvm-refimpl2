package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import scala.collection.mutable.HashMap
import scala.collection.AbstractIterator

abstract class StackState

object StackState {
  case class Ready(t: Type) extends uvm.refimpl.itpr.StackState
  case object Running extends StackState
  case object Dead extends StackState
}

class InterpreterStack(val id: Int, val stackMemory: StackMemory, stackBottomFunc: FuncVer, args: Seq[ValueBox]) {
  var gcMark: Boolean = false  // Mark for GC.
  
  var state: StackState = StackState.Ready(InternalTypes.VOID) // Initial state is READY<void>

  var top: InterpreterFrame = InterpreterFrame.frameForCall(stackBottomFunc, args, None)

  def frames: Iterator[InterpreterFrame] = new AbstractIterator[InterpreterFrame] {
    var curFrame: Option[InterpreterFrame] = Some(top)
    def hasNext = curFrame.isDefined
    def next = {
      val res = curFrame.get
      curFrame = res.prev
      res
    }
  }

  def pushFrame(funcVer: FuncVer, args: Seq[ValueBox]): Unit = {
    val newFrame = InterpreterFrame.frameForCall(funcVer, args, Some(top))
    top = newFrame
    top.savedStackPointer = stackMemory.top
  }

  def replaceTop(funcVer: FuncVer, args: Seq[ValueBox]): Unit = {
    val newFrame = InterpreterFrame.frameForCall(funcVer, args, top.prev)
    stackMemory.rewind(top.savedStackPointer)
    top = newFrame
    top.savedStackPointer = stackMemory.top
  }

  def popFrame(): Unit = {
    stackMemory.rewind(top.savedStackPointer)
    top = top.prev.getOrElse {
      throw new UvmRuntimeException("Attemting to pop the last frame of a stack. Stack ID: %d.".format(id))
    }
  }
}

class InterpreterFrame(val funcVer: FuncVer, val prev: Option[InterpreterFrame]) {
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
   * This flag is set when executing CALL, SWAPSTACK, TRAP or WATCHPOINT. It is cleared when executing 
   * InterpreterThread.finishHalfExecutedInst or InterpreterThread.catchException. Particularly, the RET, RETVOID,
   * THROW, TRAP, WATCHPOINT and SWAPSTACK instruction will call those two functions.
   */
  var curInstHalfExecuted: Boolean = false

  /**
   * The stack pointer to restore to when unwinding THE CURRENT FRAME. In other word, this value is the stackMemory.top
   * of the stack when the current frame is pushed.
   */
  var savedStackPointer: Word = 0

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

object InterpreterFrame {
  def frameForCall(funcVer: FuncVer, args: Seq[ValueBox], prev: Option[InterpreterFrame]): InterpreterFrame = {
    val frm = new InterpreterFrame(funcVer, prev) // Bottom frame

    for ((p, a) <- (funcVer.params zip args)) {
      frm.boxes(p).copyFrom(a)
    }

    frm
  }
}