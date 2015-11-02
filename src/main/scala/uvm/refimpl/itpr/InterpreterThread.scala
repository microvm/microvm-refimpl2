package uvm.refimpl.itpr

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm._
import uvm.comminsts._
import uvm.refimpl._
import uvm.refimpl.{ HowToResume => ClientHowToResume }
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables._
import uvm.types._
import uvm.refimpl.nat.NativeCallResult

object InterpreterThread {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

abstract class HowToResume
object HowToResume {
  case class PassValues(values: Seq[ValueBox]) extends HowToResume
  case class ThrowExc(exc: Word) extends HowToResume
}

/**
 * A thread that interprets Mu instruction.
 */
class InterpreterThread(val id: Int, initialStack: InterpreterStack, val mutator: Mutator, htr: HowToResume)(
    implicit protected val microVM: MicroVM) extends InstructionExecutor {
  import InterpreterThread._

  // Injectable resources (used by memory access instructions)
  implicit protected val memorySupport = microVM.memoryManager.memorySupport

  override def curThread = this

  // Initialisation

  htr match {
    case HowToResume.PassValues(values) => rebindPassValues(initialStack, values)
    case HowToResume.ThrowExc(exc)      => catchException(exc)
  }
}

/**
 * The states of an interpreter thread.
 */
trait InterpreterThreadState {
  /** Thread ID */
  def id: Int

  /** The Micro VM */
  implicit protected def microVM: MicroVM

  /** The underlying stack. */
  var stack: Option[InterpreterStack] = None

  /** True if the thread is running. False only if terminated. */
  var isRunning: Boolean = true

  /** True if the thread is waiting in a Futex waiting queue. */
  var isFutexWaiting: Boolean = false

  /** Object-pinnning multiset. */
  val pinSet = new ArrayBuffer[Word]

  protected def curStack = stack.get
  protected def top: InterpreterFrame = curStack.top
  protected def topMu: MuFrame = curStack.top match {
    case f: MuFrame => f
    case f: NativeFrame => throw new UvmRefImplException(("Stack %d: Expected Mu top frame, " +
      "found native frame for native function 0x%x.").format(curStack.id, f.func))
  }
  protected def topDefMu: DefinedMuFrame = topMu match {
    case f: DefinedMuFrame => f
    case f: UndefinedMuFrame => throw new UvmRefImplException(("Stack %d: Expected defined Mu top frame, " +
      "found undefined frame for Mu function %s.").format(curStack.id, f.func.repr))
  }
  protected def curBB = topDefMu.curBB
  protected def curInst = topDefMu.curInst
  protected def justCreated = topMu.justCreated
  protected def justCreated_=(v: Boolean) = topMu.justCreated_=(v)

  /** Make a string to identify the current function version, basic block and instruction for debugging. */
  protected def ctx = stack match {
    case None => "(Thred not bound to stack): "
    case Some(s) => s.top match {
      case f: NativeFrame      => "TID %d, Native frame for function 0x%x: ".format(id, f.func)
      case f: UndefinedMuFrame => "TID %d, Function %s (not defined): ".format(id, f.func.repr)
      case f: DefinedMuFrame => {
        val ix = f.curInstIndex
        if (ix >= curBB.insts.size) {
          "TID %d, FuncVer %s, BB %s, Inst(%d): index out of the basic block boundary (error): ".format(id, f.funcVer.repr, curBB.repr, ix)
        } else {
          "TID %d, FuncVer %s, BB %s, Inst(%d): %s (%s): ".format(id, f.funcVer.repr, curBB.repr, ix, curInst.repr, curInst match {
            case ci: InstCommInst => ci.inst.name.get
            case _                => curInst.getClass.getSimpleName()
          })
        }
      }
    }
  }

  /** Get the value box of an SSA variable in the current stack. */
  protected def boxOf(v: SSAVariable): ValueBox = v match {
    case g: GlobalVariable => microVM.constantPool.getGlobalVarBox(g)
    case l: LocalVariable  => topDefMu.boxes(l)
  }

  /** Get the edge-assigned value box of an edge-assigned instruction in the current stack. */
  protected def edgeAssignedBoxOf(p: Parameter): ValueBox = topDefMu.edgeAssignedBoxes(p)

  /** Get the result boxes of the current instruction. */
  protected def resultBoxes: Seq[ValueBox] = curInst.results.map(boxOf)

  /** Get one result box of the current instruction. */
  protected def resultBox(index: Int): ValueBox = try {
    boxOf(curInst.results(index))
  } catch {
    case e: IndexOutOfBoundsException => throw new UvmRefImplException(
        "Instruction %s is declared to have only %d results. Result %d is requested.".format(
            curInst, curInst.results.length, index), e)
  }
}

/**
 * Common interpreter actions. Depends on and manipulates the interpreter states.
 */
trait InterpreterActions extends InterpreterThreadState {
  import InterpreterThread.logger

  /**
   * Interpret the current instruction.
   * @see uvm.refimpl.itpr.InstructionExecutor
   */
  protected def interpretCurrentInstruction(): Unit

  /**
   * Specialised to interpet common instructions.
   */
  protected def interpretCurrentCommonInstruction(): Unit

  /**
   * Let InterpreterThread return self
   */
  protected def curThread: InterpreterThread

  /** Write the return value of futex. May be written from FutexManager */
  def futexReturn(rv: Int): Unit = {
    assert(curInst.isInstanceOf[InstCommInst])
    assert(Seq("@uvm.futex.wait", "@uvm.futex.wait_timeout", "@uvm.futex.wake", "@uvm.futex.cmp_requeue") contains
      curInst.asInstanceOf[InstCommInst].inst.name.get)

    logger.debug(ctx + "Setting futex return value")
    writeIntResult(32, rv, resultBoxes(0))
    continueNormally()
  }

  /** Execute one instruction. */
  def step(): Unit = {
    if (!isRunning) throw new UvmRefImplException(ctx + "Attempt to run thread after it has reached exit.")
    if (isFutexWaiting) throw new UvmRefImplException(ctx + "Attempt to run thread when it is waiting on a futex.")
    topMu.justCreated = false
    interpretCurrentInstruction()
  }

  /** Set PC to the next instruction of the same basic block. */
  protected def incPC(): Unit = topDefMu.incPC()
  /** Set PC to the beginning of another basic block. */
  protected def jump(bb: BasicBlock, ix: Int): Unit = topDefMu.jump(bb, ix)

  /** Branch to a basic block and assign parameters */
  protected def branchTo(destClause: DestClause, maybeExcAddr: Option[Word] = None): Unit = {
    val curBB = this.curBB
    val dest = destClause.bb
    val norArgs = destClause.args

    // Copy to edge-assigned boxes, first.
    if (norArgs.length != dest.norParams.length) {
      throw new UvmRefImplException(ctx + "Wrong number of arguments. Basic block: %s, expected: %d, actual: %d".format(
        dest.repr, dest.norParams.length, norArgs.length))
    }

    for ((arg, np) <- norArgs zip dest.norParams) {
      val argBox = boxOf(arg)
      val npEdgeBox = edgeAssignedBoxOf(np)
      npEdgeBox.copyFrom(argBox)
    }

    for (ep <- dest.excParam) {
      maybeExcAddr match {
        case None => throw new UvmRefImplException(ctx + "Branching normally to a basic block with ExcParam: %s".format(dest.repr))
        case Some(excAddr) => {
          val epEdgeBox = edgeAssignedBoxOf(ep).asInstanceOf[BoxRef]
          epEdgeBox.setObjRef(excAddr)
        }
      }
    }

    // Copy from edge-assigned boxes to their canonical boxes.
    for (np <- dest.norParams) {
      val npEdgeBox = edgeAssignedBoxOf(np)
      val npBox = boxOf(np)
      npBox.copyFrom(npEdgeBox)
    }

    for (ep <- dest.excParam) {
      val epEdgeBox = edgeAssignedBoxOf(ep)
      val epBox = boxOf(ep)
      epBox.copyFrom(epEdgeBox)
    }

    // Continue execution
    jump(dest, 0)
  }

  /** Continue normally. This will move to the next instruction. Work for all instructions. */
  protected def continueNormally(): Unit = {
    curInst match {
      case wp: InstWatchPoint => {
        branchTo(wp.ena)
        // NOTE: WatchPoint only "continue normally" when the current stack is rebound with value or void.
        // This includes executing a watch point. In any case, this watch point must have been enabled. If the watch
        // point is disabled during the course the stack is unbound, this watch point should still continue from the
        // destination determined WHEN THIS INSTRUCTION IS EXECUTED.
      }
      case h: HasExcClause => h.excClause match {
        case None => incPC()
        case Some(ec) => {
          branchTo(ec.nor)
        }
      }
      case _ => incPC()
    }
  }

  /**
   * Attempt to catch exception in the current frame. Will repeatedly unwind the stack until the exception can be
   * handled. Stack underflow is an undefined behaviour.
   */
  protected def catchException(exc: Word): Unit = {
    @tailrec
    def unwindUntilCatchable(frame: InterpreterFrame): (InterpreterFrame, DestClause) = frame match {
      case f: MuFrame => {
        val maybeDestClause = if (!f.justCreated && f.isInstanceOf[DefinedMuFrame]) {
          maybeFindExceptionHandler(f.asInstanceOf[DefinedMuFrame].curInst)
        } else {
          None
        }

        maybeDestClause match {
          case Some(dc) => (f, dc)
          case None => f.prev match {
            case None       => throw new UvmRuntimeException(ctx + "Exception is thrown out of the bottom frame.")
            case Some(prev) => unwindUntilCatchable(prev)
          }

        }
      }
      case f: NativeFrame => {
        throw new UvmRuntimeException(ctx + "Attempt to throw exception into a native frame. It has implementation-defined. " +
          "behaviour, and the refimpl does not allow it. Although not always forbidden elsewhere, it is almost always dangerous.")
      }
    }

    val s = curStack
    val f = s.top
    val (newFrame, dc) = unwindUntilCatchable(f)
    s.unwindTo(newFrame)

    branchTo(dc, Some(exc))
  }

  /**
   * Test if the current frame with i as the current instruction can catch an exception that unwinds the stack.
   *
   * @return Return Some(dc) if i can catch the exception and d is the destination clause for the exception. Return None if i
   * cannot catch exceptions.
   *
   * @throw Throw UvmRefimplException if a frame stops at an unexpected instruction. Normally the top frame can be
   * executing TRAP, WATCHPOINT, SWAPSTACK or CALL and all other frames must be executing CALL.
   */
  protected def maybeFindExceptionHandler(inst: Instruction): Option[DestClause] = {
    inst match {
      case i: InstCall       => i.excClause.map(_.exc)
      case i: InstTrap       => i.excClause.map(_.exc)
      case i: InstWatchPoint => i.exc
      case i: InstSwapStack  => i.excClause.map(_.exc)
      case _ => {
        throw new UvmRefImplException(ctx + "Non-OSR point instruction %s (%s) is in a stack frame when an exception is thrown.".format(inst.repr, inst.getClass.getName))
      }
    }
  }

  // Misc helper

  protected def writeBooleanResult(result: Boolean, box: ValueBox): Unit = {
    box.asInstanceOf[BoxInt].value = if (result) 1 else 0
  }

  protected def writeIntResult(len: Int, result: BigInt, box: ValueBox): Unit = {
    box.asInstanceOf[BoxInt].value = OpHelper.unprepare(result, len)
  }

  protected def incrementBoxIRefOrPointer(ptr: Boolean, src: SSAVariable, dst: SSAVariable, addrIncr: Word): Unit = {
    if (ptr) {
      val sb = boxOf(src).asInstanceOf[BoxPointer]
      val db = boxOf(dst).asInstanceOf[BoxPointer]
      db.addr = sb.addr + addrIncr
    } else {
      val sb = boxOf(src).asInstanceOf[BoxIRef]
      val db = boxOf(dst).asInstanceOf[BoxIRef]
      db.objRef = sb.objRef
      db.offset = sb.offset + addrIncr
    }
  }

  protected def addressOf(ptr: Boolean, v: SSAVariable): Word = {
    MemoryOperations.addressOf(ptr, boxOf(v))
  }

  // Thread termination

  /** Terminate the thread. Please only let the thread terminate itself. */
  protected def threadExit(): Unit = {
    curStack.kill()
    isRunning = false
  }

  // Thread/stack binding and unbinding

  /** Unbind the current thread from the stack. */
  protected def unbindRetWith(tys: Seq[Type]): Unit = {
    curStack.unbindRetWith(tys)
    stack = None
  }

  /** Unbind and kill the current stack. */
  protected def unbindAndKillStack(): Unit = {
    curStack.kill()
    stack = None
  }

  /**
   *  Rebind to a stack.
   */
  protected def rebind(newStack: InterpreterStack): Unit = {
    stack = Some(newStack)
  }

  /** Rebind to a stack and pass a value. */
  protected def rebindPassValues(newStack: InterpreterStack, values: Seq[ValueBox]): Unit = {
    rebind(newStack)

    val shouldIncrementPC = newStack.rebindPassValues(values)
    if (shouldIncrementPC) {
      continueNormally()
    }
  }

  /** Rebind to a stack and throw an exception on that stack. */
  protected def rebindThrowExc(newStack: InterpreterStack, exc: ValueBox): Unit = {
    rebind(newStack)

    val excObjRef = exc.asInstanceOf[BoxRef].objRef

    catchException(excObjRef)
  }

  // Trap and watchpoint handling

  /** Execute the trap handler in the Client. Work for both TRAP and WATCHPOINT. */
  protected def doTrap(retTys: Seq[Type], wpID: Int) = {
    val curCtx = ctx // save the context string for debugging

    val c = microVM.newContext()

    val hThread = c.handleFromInterpreterThread(Some(curThread))
    val hStack = c.handleFromInterpreterStack(Some(curStack))

    unbindRetWith(retTys)

    val res = microVM.trapManager.trapHandler.handleTrap(c, hThread, hStack, wpID)

    res match {
      case TrapHandlerResult.ThreadExit() => {
        isRunning = false
      }
      case TrapHandlerResult.Rebind(newStack, htr) => {
        val ns = newStack.vb.stack.getOrElse {
          throw new UvmRuntimeException(curCtx + "Attempt to rebind to NULL stack when returning from trap.")
        }

        htr match {
          case ClientHowToResume.PassValues(values) => {
            rebindPassValues(ns, values.map(_.vb))
          }
          case ClientHowToResume.ThrowExc(exc) => {
            rebindThrowExc(ns, exc.vb)
          }
        }
      }
    }

    c.closeContext()
  }

  // Internal control structures (syntax sugars)

  /**
   *  Branch to an exceptional destination. If there is no ExcClause, execute f. f usually throws an exception.
   *  @example {{{
   *  branchToExcDestOr(excClause) {
   *    throw new UvmRuntimeException("When abnormal thing happens, the absence of ExcClause has undefined behaviour.")
   *  }
   *  }}}
   */
  protected def branchToExcDestOr(excClause: Option[ExcClause])(f: => Unit): Unit = {
    excClause match {
      case None                      => f
      case Some(ExcClause(_, excBB)) => branchTo(excBB)
    }
  }

  /**
   * Execute f, but catch the UvmOutOfMemoryException thrown by most allocation methods in the allocator. Out-of-memory
   * errors in the micro VM usually branches to an exception destination, but has undefined behaviour when ExcClause is
   * absent.
   * @example {{{
   * handleOutOfMemory(excClause) {
   *   allocate
   *   allocate
   *   allocate
   *   ...
   * }
   * }}}
   */
  protected def handleOutOfMemory(excClause: Option[ExcClause])(f: => Unit): Unit = {
    try {
      f
    } catch {
      case e: UvmOutOfMemoryException => {
        branchToExcDestOr(excClause) {
          throw new UvmRuntimeException(ctx + "Out of memory and there is no handler.", e)
        }
      }
    }
  }

  /**
   * Raise NULL reference error. NULL reference errors in the micro VM usually branches to an exception destination, but has
   * undefined behaviour when ExcClause is absent.
   */
  protected def nullRefError(excClause: Option[ExcClause]): Unit = {
    branchToExcDestOr(excClause) {
      throw new UvmRuntimeException(ctx + "Accessing null reference.")
    }
  }
}

