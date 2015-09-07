package uvm.clientsupport.text

import scala.collection.JavaConversions._

object TextIRWriter {
  def bundleToText(bundle: Bundle): String = {
    val sb = new StringBuilder()

    sb.toString()
  }

  def typeToText(typeDef: TypeDef): String = {
    val name = typeDef.name
    val ctor = typeDef match {
      case t: TypeInt     => "int<%d>".format(t.len)
      case t: TypeFloat   => "float"
      case t: TypeDouble  => "double"
      case t: TypeRef     => "ref<%s>".format(t.ty)
      case t: TypeIRef    => "iref<%s>".format(t.ty)
      case t: TypeWeakRef => "weakref<%s>".format(t.ty)
      case t: TypeStruct => {
        val sb = new StringBuilder("struct<")
        for (ty <- t.fieldTy) sb ++= " " ++= ty
        sb ++= " >"
        sb.toString
      }
      case t: TypeArray    => "array<%s %d>".format(t.elemTy, t.len)
      case t: TypeHybrid   => "hybrid<%s %s>".format(t.fixedTy, t.varTy)
      case t: TypeVoid     => "void"
      case t: TypeFunc     => "func<%s>".format(t.sig)
      case t: TypeThread   => "thread"
      case t: TypeStack    => "stack"
      case t: TypeTagRef64 => "tagref64"
      case t: TypePtr      => "ptr<%s>".format(t.ty)
      case t: TypeFuncPtr  => "funcptr<%s>".format(t.sig)
    }

    ".typedef %s = %s".format(name, ctor)
  }
  
  def funcSigToText(funcSigDef: FuncSigDef): String = {
    val paramTys = {
      val sb = new StringBuilder(" ")
      for (ty <- funcSigDef.paramTy) sb ++= ty ++= " "
      sb.toString
    }
    ".funcsig %s = %s (%s)".format(funcSigDef.name, funcSigDef.retTy, funcSigDef.paramTy)
  }
  
  def constToText(constDef: ConstDef): String = {
    val name = constDef.name
    val ty = constDef.ty
    val ctor: String = constDef match {
      case c: ConstInt => c.num.toString
      case c: ConstFloat => "bitsf(0x%x)".format(java.lang.Float.floatToRawIntBits(c.num))
      case c: ConstDouble => "bitsd(0x%x)".format(java.lang.Double.doubleToRawLongBits(c.num))
      case c: ConstStruct => {
        val sb = new StringBuilder("{ ")
        for (f <- c.fields) sb ++= f ++= " "
        sb ++= "}"
        sb.toString
      }
      case c: ConstNull => "NULL"
      case c: ConstPointer => c.addr.toString
    }
    ".const %s <%s> = %s".format(name, ty, ctor)
  }
  
  def globalToText(globalCellDef: GlobalCellDef): String = {
    ".global %s <%s>".format(globalCellDef.name, globalCellDef.ty)
  }
  
  def funcDeclToText(funcDecl: FuncDecl): String = {
    ".funcdecl %s <%s>".format(funcDecl.name, funcDecl.sig)
  }
  
  def funcExpToText(funcExp: FuncExpDef): String = {
    ".expose %s = %s %s %s".format(funcExp.name, funcExp.func, funcExp.callConv, funcExp.cookie)
  }
  
  def funcDefToText(funcDef: FuncDef): String = {
    ???
  }
}