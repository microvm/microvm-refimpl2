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
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger

/** The state of a frame. */
abstract class FrameState

object FrameState {
  /** If values of ts are given, it is ready to resume. */
  case class Ready(ts: Seq[Type]) extends FrameState
  /** A thread is executing it. */
  case object Running extends FrameState
  /** Killed. */
  case object Dead extends FrameState
}

/**
 * Implements a Mu Stack. Contains both Mu frames and native frames.
 */
class InterpreterStack(val id: Int, val stackMemory: StackMemory, stackBottomFunc: Function)(
    implicit nativeCallHelper: NativeCallHelper) extends HasID {
  var gcMark: Boolean = false // Mark for GC.

  private var _top: InterpreterFrame = InterpreterFrame.forMuFunc(stackMemory.top, stackBottomFunc, 0L, None)
  /** The top frame */
  def top = _top
  private def top_=(f: InterpreterFrame) = _top = f

  /** The state of the stack (i.e. state of the top frame) */
  def state = top.state

  /** A lazily created native stack keeper. */
  var maybeNSK: Option[NativeStackKeeper] = None

  /** Lazily create the nsk. */
  private def ensureNSK(): Unit = {
    if (maybeNSK == None) {
      val nsk = new NativeStackKeeper()
      maybeNSK = Some(nsk)
    }
  }

  /** Iterate through all frames. */
  def frames: Iterator[InterpreterFrame] = new AbstractIterator[InterpreterFrame] {
    var curFrame: Option[InterpreterFrame] = Some(top)
    def hasNext = curFrame.isDefined
    def next = {
      val res = curFrame.get
      curFrame = res.prev
      res
    }
  }

  /** Iterate through Mu frames only. */
  def muFrames: Iterator[MuFrame] = frames.filter(_.isInstanceOf[MuFrame]).map(_.asInstanceOf[MuFrame])

  ///////// frame manipulations

  /** Create a new Mu frame, usually for Mu calling Mu, or OSR. */
  private def pushMuFrame(func: Function): Unit = {
    assert(top.isInstanceOf[MuFrame])
    val newFrame = InterpreterFrame.forMuFunc(stackMemory.top, func, 0L, Some(top))
    top = newFrame
  }

  /** Create a new Mu frame for calling from native. */
  private def pushMuFrameForCallBack(func: Function, cookie: Long): Unit = {
    assert(top.isInstanceOf[NativeFrame])
    val newFrame = InterpreterFrame.forMuFunc(stackMemory.top, func, cookie, Some(top))
    top = newFrame
  }

  /** Replace the top Mu frame with a frame of a different function. Usually used by tailcall. */
  private def replaceTopMuFrame(func: Function): Unit = {
    assert(top.isInstanceOf[MuFrame])
    if (top.isInstanceOf[DefinedMuFrame]) {
      stackMemory.rewind(top.asInstanceOf[DefinedMuFrame].savedStackPointer)
    }
    val newFrame = InterpreterFrame.forMuFunc(stackMemory.top, func, 0L, top.prev)
    top = newFrame
  }

  /** Push a (conceptual) native frame. The "real native frame" is held by the NSK slave, of course. */
  private def pushNativeFrame(sig: FuncSig, func: Word): Unit = {
    assert(top.isInstanceOf[MuFrame])
    val newFrame = InterpreterFrame.forNativeFunc(func, Some(top))
    top = newFrame
  }

  /** Pop a Mu frame or a native frame. Throw error when popping the bottom frame. */
  private def _popFrame(): Unit = {
    top = top.prev.getOrElse {
      throw new UvmRuntimeException("Attemting to pop the last frame of a stack. Stack ID: %d.".format(id))
    }
  }

  /** Pop a Mu frame */
  private def popMuFrame(): Unit = {
    assert(top.isInstanceOf[MuFrame])
    if (top.isInstanceOf[DefinedMuFrame]) {
      stackMemory.rewind(top.asInstanceOf[DefinedMuFrame].savedStackPointer)
    }
    _popFrame()
  }

  /** Pop a native frame */
  private def popNativeFrame(): Unit = {
    assert(top.isInstanceOf[NativeFrame])
    _popFrame()
  }

  /**
   * Return from a Mu frame to a native frame. This will eventually resume in Mu.
   * @return true if the interpreter needs to increment the PC.
   */
  private def returnToNative(maybeRvb: Option[ValueBox]): Boolean = {
    assert(top.isInstanceOf[NativeFrame])
    val result = maybeNSK.get.returnToNative(maybeRvb)
    handleNativeCallResult(result)
  }

  /**
   * Handle native call result. This will eventually resume in Mu.
   * @return true if the interpreter needs to increment the PC.
   */
  private def handleNativeCallResult(ncr: NativeCallResult): Boolean = {
    ncr match {
      case r @ NativeCallResult.ReturnToMu(maybeRvb) => {
        assert(top.isInstanceOf[NativeFrame])
        popNativeFrame()
        assert(top.isInstanceOf[MuFrame])
        top.asInstanceOf[MuFrame].resumeNormally(maybeRvb.toSeq)
      }
      case r @ NativeCallResult.CallMu(func, cookie, args) => {
        assert(top.isInstanceOf[NativeFrame])
        pushMuFrameForCallBack(func, cookie)
        top.asInstanceOf[MuFrame].resumeNormally(args.toSeq)
      }
    }
  }

  ///////// interpreter/client-visible stack state transitions

  /**
   * Mu calling a Mu function.
   */
  def callMu(func: Function, args: Seq[ValueBox]): Boolean = {
    assert(top.isInstanceOf[MuFrame])
    top.state = FrameState.Ready(func.sig.retTys)
    pushMuFrame(func)
    top.asInstanceOf[MuFrame].resumeNormally(args)
  }

  /** Mu tail-calling a Mu function. */
  def tailCallMu(func: Function, args: Seq[ValueBox]): Boolean = {
    assert(top.isInstanceOf[MuFrame])
    replaceTopMuFrame(func)
    top.asInstanceOf[MuFrame].resumeNormally(args)
  }

  /** Returning from a Mu frame. Will eventually resume in Mu.*/
  def retFromMu(rvs: Seq[ValueBox]): Boolean = {
    assert(top.isInstanceOf[MuFrame])
    popMuFrame()

    top match {
      case f: MuFrame => {
        f.resumeNormally(rvs)
      }
      case f: NativeFrame => {
        val maybeRvb = rvs match {
          case Seq()  => None
          case Seq(b) => Some(b)
          case bs => throw new UvmRefImplException(
            "Cannot return multiple values to native functions. Native func: 0x%x".format(f.func))
        }

        returnToNative(maybeRvb)
      }
    }
  }

  /** Mu calling a native function. */
  def callNative(sig: FuncSig, func: Word, args: Seq[ValueBox]): Boolean = {
    assert(top.isInstanceOf[MuFrame])
    ensureNSK()
    pushNativeFrame(sig, func)
    val result = maybeNSK.get.callNative(sig, func, args)
    handleNativeCallResult(result)
  }

  /** Unwind to a Mu frame. The interpreter will handle the resumption. */
  def unwindTo(f: InterpreterFrame): Unit = {
    top = f
    top match {
      case mf: DefinedMuFrame => {
        stackMemory.rewind(mf.savedStackPointer)
        top.state = FrameState.Running
      }
      case mf: UndefinedMuFrame => {
        throw new UvmRefImplException("Unwound to a frame for undefined Mu function %s. Stack ID: %d.".format(mf.func.repr, id))
      }
      case nf: NativeFrame => {
        throw new UnimplementedOprationException("Cannot unwind to native frames. Stack ID: %d.".format(id))
      }
    }
  }

  /** Unbind the stack from a thread, returning with values. */
  def unbindRetWith(retTys: Seq[Type]): Unit = {
    assert(top.isInstanceOf[MuFrame])
    top.state = FrameState.Ready(retTys)
  }

  /**
   * Rebind to this stack, passing values.
   * @return true if the interpreter should increment the PC by continueNormally().
   */
  def rebindPassValues(args: Seq[ValueBox]): Boolean = {
    if (!state.isInstanceOf[FrameState.Ready]) {
      throw new UvmRuntimeException("Attempt to bind to a stack not in the ready state. Actual state: %s".format(state))
    }
    top match {
      case mf: MuFrame => {
        mf.resumeNormally(args)
      }
      case nf: NativeFrame => {
        val maybeRvb = args match {
          case Seq()   => None
          case Seq(vb) => Some(vb)
          case bs => throw new UvmRuntimeException("Attempted to return multiple values to a native frame. " +
            "Stack ID: %d, Native function: 0x%x".format(id, nf.func))
        }
        returnToNative(maybeRvb)
      }
    }
  }

  /** Kill the stack */
  def kill(): Unit = {
    for (f <- frames) {
      f.state = FrameState.Dead
    }
    maybeNSK.foreach { nsk =>
      nsk.close()
    }
  }

  /**
   * Return the n-th frame of the current stack.
   */
  def nthFrame(n: Int): InterpreterFrame = {
    val dropped = frames.drop(n)
    if (dropped.hasNext) {
      dropped.next
    } else {
      throw new UvmRuntimeException("The stack %d only has %d frames, but the %d-th frame is requested.".format(id, n))
    }
  }

  /** Pop a frame. Part of the API. Also used by THROW */
  def popFrame(): Unit = {
    top match {
      case f: NativeFrame => throw new UnimplementedOprationException("Popping native frames is not supported.")
      case f: MuFrame => f.prev match {
        case None       => throw new UvmRuntimeException("Attempting to pop the last frame of a stack.")
        case Some(prev) => popMuFrame()
      }

    }
  }

  /** Push a Mu frame. Part of the API. */
  def pushFrame(func: Function) {
    pushMuFrame(func)
  }
}

abstract class InterpreterFrame(val prev: Option[InterpreterFrame]) {
  /**
   * The state of the frame. Must be set when creating the frame.
   */
  var state: FrameState = _

  /** ID of the current function. Return 0 for native frames. */
  def curFuncID: Int

  /** ID of the current function version Return 0 for native frames or undefined Mu frames. */
  def curFuncVerID: Int

  /** ID of the current instruction. Return 0 for native frames, undefined Mu frames, or a just-created frame. */
  def curInstID: Int

}

object InterpreterFrame {
  def forMuFunc(savedStackPointer: Word, func: Function, cookie: Long, prev: Option[InterpreterFrame]): MuFrame = {
    val frm = func.versions.headOption match {
      case None          => new UndefinedMuFrame(func, prev)
      case Some(funcVer) => new DefinedMuFrame(savedStackPointer, funcVer, cookie, prev)
    }

    frm.state = FrameState.Ready(func.sig.retTys)

    frm
  }

  def forNativeFunc(func: Word, prev: Option[InterpreterFrame]): NativeFrame = {
    val frm = new NativeFrame(func, prev)

    // A white lie. A native frame cannot be created until actualy calling a native function.
    frm.state = FrameState.Running

    frm
  }
}

class NativeFrame(val func: Word, prev: Option[InterpreterFrame]) extends InterpreterFrame(prev) {
  override def curFuncID: Int = 0
  override def curFuncVerID: Int = 0
  override def curInstID: Int = 0
  
  override def toString(): String = "NativeFrame(func=0x%x)".format(func)
}

abstract class MuFrame(val func: Function, prev: Option[InterpreterFrame]) extends InterpreterFrame(prev) {
  /**
   * Resume a Mu frame.
   * @return true if the interpreter should increment the PC
   */
  def resumeNormally(args: Seq[ValueBox]): Boolean

  /**
   * true if the frame is just created (push_frame or new_stack). Binding a thread to the stack or executing an
   *  instruction will make it false.
   */
  var justCreated: Boolean = true

  /**
   * Traverse over all local variable boxes. For GC.
   */
  def scannableBoxes: TraversableOnce[ValueBox]
}

object UndefinedMuFrame {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  val VIRT_INST_NOT_STARTED = 0
  val VIRT_INST_TRAP = 1
  val VIRT_INST_TAILCALL = 2
}

/**
 * A Mu frame for undefined Mu frames. Such frames can still be resumed, but will trap as soon as being executed.
 */
class UndefinedMuFrame(func: Function, prev: Option[InterpreterFrame]) extends MuFrame(func, prev) {
  import UndefinedMuFrame._

  val boxes = func.sig.paramTys.map { ty =>
    try {
      ValueBox.makeBoxForType(ty)
    } catch {
      case e: UvmRefImplException => {
        logger.error("Having problem creating box for parameter type: %s".format(ty))
        throw e
      }
    }
  }

  var virtInst = VIRT_INST_NOT_STARTED

  override def scannableBoxes = boxes
  
  override def curFuncID: Int = func.id
  override def curFuncVerID: Int = 0
  override def curInstID: Int = 0

  def resumeNormally(args: Seq[ValueBox]): Boolean = {
    if (justCreated) {
      if (boxes.length != args.length) {
        throw new UvmRefImplException("Function %s (not undefined yet) expects %d params, got %d args.".format(
          func.repr, boxes.length, args.length))
      }

      for ((dst, src) <- boxes zip args) {
        dst.copyFrom(src)
      }

      justCreated = false
      virtInst = VIRT_INST_TRAP
      state = FrameState.Running

      false
    } else {
      assert(virtInst != VIRT_INST_NOT_STARTED, "The previous if should handle justCreated UndefinedMuFrame")
      assert(virtInst != VIRT_INST_TAILCALL, "TAILCALL is not an OSR point")
      assert(virtInst == VIRT_INST_TRAP)

      if (args.length != 0) {
        throw new UvmRefImplException("Undefined function %s expects no param on its first virtual trap, got %d args.".format(
          func.repr, args.length))
      }

      virtInst = VIRT_INST_TAILCALL
      false
    }
  }

  override def toString(): String = "UndefinedMuFrame(func=%s)".format(func.repr)
}

object DefinedMuFrame {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}
/**
 * A Mu frame for defined Mu frames.
 *
 * @param savedStackPointer: The stack pointer to restore to when unwinding THE CURRENT FRAME. In other word, this value is the stackMemory.top
 * of the stack when the current frame is pushed.
 * @param cookie: The cookie in the native interface. When called by another Mu function, cookie can be any value.
 */
class DefinedMuFrame(val savedStackPointer: Word, val funcVer: FuncVer, val cookie: Long, prev: Option[InterpreterFrame])
    extends MuFrame(funcVer.func, prev) {
  import DefinedMuFrame._

  /** Boxes for all local variables. */
  val boxes = new HashMap[LocalVariable, ValueBox]()

  override def scannableBoxes = boxes.values

  /** Edge-assigned instructions take values determined at look backedges */
  val edgeAssignedBoxes = new HashMap[Parameter, ValueBox]()

  /** Current basic block */
  var curBB: BasicBlock = funcVer.entry

  /** Current instruction index within the current basic block */
  var curInstIndex: Int = 0

  makeBoxes()

  private def makeBoxes() {
    for (bb <- funcVer.bbs) {
      for (p <- bb.norParams) {
        putBox(p)
      }
      for (p <- bb.excParam) {
        putBox(p)
      }

      for (inst <- bb.insts; res <- inst.results) {
        putBox(res)
      }
    }
  }

  private def putBox(lv: LocalVariable) {
    val ty = TypeInferer.inferType(lv)
    try {
      boxes.put(lv, ValueBox.makeBoxForType(ty))
    } catch {
      case e: UvmRefImplException => {
        logger.error("Having problem creating box for lv: %s".format(lv))
        throw e
      }
    }
    if (lv.isInstanceOf[Parameter]) {
      edgeAssignedBoxes.put(lv.asInstanceOf[Parameter], ValueBox.makeBoxForType(ty))
    }
  }
  
  override def curFuncID: Int = func.id
  override def curFuncVerID: Int = funcVer.id
  override def curInstID: Int = if (justCreated) 0 else curInst.id

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

  /** Resume with values. May be used by any OSR point, including call, tailcall, ret, trap, wp, swapstack */
  def resumeNormally(args: Seq[ValueBox]): Boolean = {
    val destBoxes = if (justCreated) {
      // Just created. args go to the arguments of the entry block.
      // May be extended to resume from any basic block.
      val bb = curBB
      val norParams = bb.norParams

      if (norParams.length != args.length) {
        throw new UvmRefImplException("Basic block %s expects %d params, got %d args.".format(
          bb.repr, norParams.length, args.length))
      }

      norParams.map(np => boxes(np))

    } else {
      // Resuming to an instruction. args go to the results of the current instruction.
      val inst = curInst
      val results = inst.results

      if (results.length != args.length) {
        throw new UvmRefImplException("Instruction %s expects %d params, got %d args.".format(
          inst.repr, results.length, args.length))
      }

      results.map(r => boxes(r))
    }

    for ((dst, src) <- destBoxes zip args) {
      dst.copyFrom(src)
    }

    val wasJustCreated = justCreated

    justCreated = false
    state = FrameState.Running

    !wasJustCreated
  }

  override def toString(): String = "DefinedMuFrame(func=%s, funcVer=%s)".format(func.repr, funcVer.repr)
}

/**
 * A mutable cursor that iterates through stack frames.
 */
class FrameCursor(val id: Int, val stack: InterpreterStack, val frame: InterpreterFrame) extends HasID {
  /** The current frame it refers to. */
  var curFrame = frame
  
  def nextFrame(): Unit = {
    curFrame = curFrame.prev.getOrElse {
      throw new UvmRuntimeException("Attempt to go below the stack-bottom frame. Stack id: %d, Frame: %s".format(
          stack.id, curFrame.toString))
    }
  }
}
