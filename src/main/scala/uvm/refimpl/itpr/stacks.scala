package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.mem._
import scala.collection.mutable.HashMap
import scala.collection.AbstractIterator

abstract class StackState

object StackState {
  case class Ready(t: Type) extends uvm.refimpl.itpr.StackState
  case object Running extends StackState
  case object Dead extends StackState
}

class InterpreterStack(val id: Int, val stackMemory: StackMemory, stackBottomFunc: FuncVer, args: Seq[ValueBox]) {
  throw new UvmRefImplException("Not implemented")

  var state: StackState = StackState.Ready(InternalTypes.VOID) // Initial state is READY<void>

  var top: InterpreterFrame = new InterpreterFrame(stackBottomFunc, None) // Bottom frame

  def frames: Iterator[InterpreterFrame] = new AbstractIterator[InterpreterFrame] {
    var curFrame: Option[InterpreterFrame] = Some(top)
    def hasNext = curFrame.isDefined
    def next = {
      val res = curFrame.get
      curFrame = res.prev
      res
    }
  }
}

class InterpreterFrame(val funcVer: FuncVer, val prev: Option[InterpreterFrame]) {
  val boxes = new HashMap[LocalVariable, ValueBox]()

  var curBB: BasicBlock = funcVer.entry

  var curInstIndex: Int = 0

  var savedStackPointer: Long = 0

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
  }

  def curInst: Instruction = curBB.insts(curInstIndex)

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