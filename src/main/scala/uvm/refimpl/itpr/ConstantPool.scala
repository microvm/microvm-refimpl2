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
    case ConstInt(ty, num) => BoxInt(OpHelper.unprepare(num, ty.asInstanceOf[TypeInt].length))
    case ConstFloat(ty, num) => BoxFloat(num)
    case ConstDouble(ty, num) => BoxDouble(num)
    case ConstStruct(ty, flds) => BoxStruct(flds.map(maybeMakeBox))
    case ConstNull(ty) => ty match {
      case _:TypeRef => BoxRef(0L)
      case _:TypeIRef => BoxIRef(0L, 0L)
      case _:TypeFunc => BoxFunc(None)
      case _:TypeThread => BoxThread(None)
      case _:TypeStack => BoxStack(None)
    }
    case ConstVector(ty, elems) => BoxVector(elems.map(maybeMakeBox))
    case ConstPointer(ty, addr) => BoxPointer(addr)
    case gc:GlobalCell => BoxIRef(0L, microVM.memoryManager.globalMemory.addrForGlobalCell(gc))
    case f:Function => BoxFunc(Some(f))
  }
  
  def getGlobalVarBox(g: GlobalVariable): ValueBox = globalVarBoxes(g)
}