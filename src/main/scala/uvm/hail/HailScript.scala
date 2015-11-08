package uvm.hail

import scala.collection.mutable.ArrayBuffer
import uvm.IdentifiedSettable
import uvm.types.Type
import uvm.ssavariables.GlobalVariable
import uvm.types.TypeHybrid

trait Locatable {
  val line: Int
  val col: Int
  val span: Int
}

class HailScript {
  val defs = new ArrayBuffer[HailDef]()
}

abstract class HailDef

abstract class HailAlloc extends HailDef with IdentifiedSettable

case class NewFixed(ty: Type) extends HailAlloc

case class NewHybrid(ty: TypeHybrid, len: Long) extends HailAlloc

case class Init(lv: LValue, rv: RValue) extends HailDef

case class LValue(base: Base, indices: Seq[Long])

abstract class Base
case class BaseGlobal(gv: GlobalVariable)
case class BaseHail(ha: HailAlloc)

abstract class RValue
case class RVGlobal(gv: GlobalVariable) extends RValue
case class RVInt(num: Long) extends RValue
case class RVFloat(num: Float) extends RValue
case class RVDouble(num: Double) extends RValue
case class RVNull() extends RValue
case class RVHailRef(ha: HailAlloc) extends RValue
case class RVIRefOf(lv: LValue) extends RValue
case class RVList(rvs: Seq[RValue]) extends RValue

