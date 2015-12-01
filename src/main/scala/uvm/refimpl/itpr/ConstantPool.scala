package uvm.refimpl.itpr

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl.MicroVM
import scala.collection.mutable.HashMap

class ConstantPool(implicit microVM: MicroVM) {
  val globalVarBoxes = HashMap[GlobalVariable, ValueBox]()

  def addGlobalVar(g: GlobalVariable) {
    maybeMakeBox(g)
  }

  def maybeMakeBox(g: GlobalVariable): ValueBox = {
    val box = globalVarBoxes.get(g).getOrElse(makeBox(g))
    globalVarBoxes.put(g, box)
    box
  }

  def makeBox(g: GlobalVariable): ValueBox = g match {
    case ConstInt(ty, num) => ty match {
      case TypeInt(l)             => BoxInt(OpHelper.unprepare(num, l))
      case _: AbstractPointerType => BoxPointer(num.toLong)
    }
    case ConstFloat(ty, num)  => BoxFloat(num)
    case ConstDouble(ty, num) => BoxDouble(num)
    case ConstSeq(ty, elems)  => BoxSeq(elems.map(maybeMakeBox))
    case ConstNull(ty) => ty match {
      case _: TypeRef       => BoxRef(0L)
      case _: TypeIRef      => BoxIRef(0L, 0L)
      case _: TypeFuncRef   => BoxFunc(None)
      case _: TypeThreadRef => BoxThread(None)
      case _: TypeStackRef  => BoxStack(None)
    }
    case gc: GlobalCell  => BoxIRef(0L, microVM.memoryManager.globalMemory.addrForGlobalCell(gc))
    case f: Function     => BoxFunc(Some(f))
    case ef: ExposedFunc => BoxPointer(microVM.nativeCallHelper.getStaticExpFuncAddr(ef))
  }

  def getGlobalVarBox(g: GlobalVariable): ValueBox = globalVarBoxes(g)
}