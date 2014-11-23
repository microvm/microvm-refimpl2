package uvm.refimpl

import uvm.types._
import uvm.ir.textinput.IDFactory
import scala.collection.mutable.HashMap
import uvm.FuncSig

object InternalIDFactory extends IDFactory(32768) // IDs from 32768-65535 are for implementation internal use.

object InternalTypes {
  val VOID_TYPE = TypeVoid()
  VOID_TYPE.id = InternalIDFactory.getID()
  VOID_TYPE.name = Some("@uvm.internal.void_type")

  val BYTE_TYPE = TypeInt(8)
  BYTE_TYPE.id = InternalIDFactory.getID()
  BYTE_TYPE.name = Some("@uvm.internal.byte_type")

  val BYTE_ARRAY_TYPE = TypeHybrid(VOID_TYPE, BYTE_TYPE)
  BYTE_ARRAY_TYPE.id = InternalIDFactory.getID()
  BYTE_ARRAY_TYPE.name = Some("@uvm.internal.byte_array_type")
}

object InternalTypePool {
  val irefPool = HashMap[Type, TypeIRef]()
  def irefOf(t: Type): TypeIRef = irefPool.get(t).getOrElse(TypeIRef(t))
  val funcPool = HashMap[FuncSig, TypeFunc]()
  def funcOf(s: FuncSig): TypeFunc = funcPool.get(s).getOrElse(TypeFunc(s))
}