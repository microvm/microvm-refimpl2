package uvm.refimpl

import uvm.types._
import uvm.ir.textinput.IDFactory
import scala.collection.mutable.HashMap
import uvm.FuncSig
import uvm.IdentifiedSettable

object InternalIDFactory extends IDFactory(32768) // IDs from 32768-65535 are for implementation internal use.

object InternalTypes {
  private implicit class IdentifiedSettableAssignable[T <: IdentifiedSettable](i: T) {
    def :=(name: String): T = {
      i.id = InternalIDFactory.getID()
      i.name = Some(name)
      i
    }
  }

  def internal(name: String) = "@uvm.internal.types." + name

  val VOID = TypeVoid() := internal("void")
  val BYTE = TypeInt(8) := internal("byte")
  val BYTE_ARRAY = TypeHybrid(VOID, BYTE) := internal("byte_array")

  val DOUBLE = TypeDouble() := internal("double")
  val I52 = TypeInt(52) := internal("i52")
  val REF_VOID = TypeRef(VOID) := internal("ref_void")
  val I6 = TypeInt(6) := internal("i6")

  val TAGREF64 = TypeTagRef64() := internal("tagref64")
}

object InternalTypePool {
  class LazyPool[FromT, ToT](factory: FromT => ToT) {
    val pool = HashMap[FromT, ToT]()
    def apply(obj: FromT): ToT = pool.get(obj).getOrElse(factory(obj))
  }
  object LazyPool {
    def apply[FromT, ToT](factory: FromT => ToT): LazyPool[FromT, ToT] = new LazyPool[FromT, ToT](factory)
  }

  val irefOf = LazyPool(TypeIRef)
  val funcOf = LazyPool(TypeFunc)
}