package junks

import uvm._
import uvm.types._
import uvm.ssavariables._

object Main extends App {
  val i64 = TypeInt(64)
  println(i64.length)

  val ic = ConstInt(i64, 999)

  println(ic.ty)

  val iNew: Value = InstNew(i64)

  iNew.resolve

  println(i64)

  val ref = TypeRef(i64)

  val refNode: TypeRef = TypeRef(null)
  refNode.id = 2;
  refNode.name = Some("@refNode")

  val strNode = TypeStruct(Seq(i64, refNode))
  strNode.id = 3;
  strNode.name = Some("@strNode")
  
  refNode.ty = strNode

  println(refNode)
  println(strNode)
  
  val ns = new SimpleNamespace[Type]()
  ns.add(i64)
  ns.add(refNode)
  
  println(ns("@refNode"))
}