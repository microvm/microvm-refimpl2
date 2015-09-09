package uvm.clientsupport.text

import scala.collection.JavaConversions._

object TextIRWriter {
  def bundleToText(bundle: Bundle): String = {
    val sb = new StringBuilder()

    for (d <- bundle.typeDefs) sb ++= typeToText(d) ++= "\n"
    for (d <- bundle.funcSigDefs) sb ++= funcSigToText(d) ++= "\n"
    for (d <- bundle.constDefs) sb ++= constToText(d) ++= "\n"
    for (d <- bundle.globalCellDefs) sb ++= globalToText(d) ++= "\n"
    for (d <- bundle.funcDecls) sb ++= funcDeclToText(d) ++= "\n"
    for (d <- bundle.funcDefs) sb ++= funcDefToText(d) ++= "\n"
    for (d <- bundle.funcExpDefs) sb ++= funcExpToText(d) ++= "\n"

    sb.toString()
  }

  def typeToText(typeDef: TypeDef): String = {
    val name = typeDef.name
    val ctor = typeDef match {
      case t: TypeInt      => "int<%d>".format(t.len)
      case t: TypeFloat    => "float"
      case t: TypeDouble   => "double"
      case t: TypeRef      => "ref<%s>".format(t.ty)
      case t: TypeIRef     => "iref<%s>".format(t.ty)
      case t: TypeWeakRef  => "weakref<%s>".format(t.ty)
      case t: TypeStruct   => "struct<%s>".format(t.fieldTy.mkString(" "))
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
    val paramTys = funcSigDef.paramTy.mkString(" ")
    ".funcsig %s = %s (%s)".format(funcSigDef.name, funcSigDef.retTy, paramTys)
  }

  def constToText(constDef: ConstDef): String = {
    val name = constDef.name
    val ty = constDef.ty
    val ctor: String = constDef match {
      case c: ConstInt     => c.num.toString
      case c: ConstFloat   => "bitsf(0x%x)".format(java.lang.Float.floatToRawIntBits(c.num))
      case c: ConstDouble  => "bitsd(0x%x)".format(java.lang.Double.doubleToRawLongBits(c.num))
      case c: ConstStruct  => "{%s}".format(c.fields.mkString(" "))
      case c: ConstVector  => "VEC{%s}".format(c.elems.mkString(" "))
      case c: ConstNull    => "NULL"
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
    val sb = new StringBuilder()
    val params = funcDef.params.mkString(" ")
    val header = ".funcdef %s VERSION %s <%s> (%s) {\n".format(funcDef.name, funcDef.version, funcDef.sig, params)
    sb ++= header

    for (bb <- funcDef.bbs) {
      sb ++= "  %s:\n".format(bb.name)
      for (inst <- bb.insts) {
        sb ++= "    %s\n".format(instToText(inst))
      }
    }

    sb ++= "}\n"
    sb.toString
  }

  def instToText(inst: Instruction): String = {
    val asgn = if (inst.name == null) "" else "%s = ".format(inst.name)
    val body = inst match {
      case i: InstBinOp   => "%s <%s> %s %s %s".format(i.op, i.opndTy, i.op1, i.op2, maybeExcClause(i))
      case i: InstCmp     => "%s <%s> %s %s".format(i.op, i.opndTy, i.op1, i.op2)
      case i: InstConv    => "%s <%s %s> %s".format(i.op, i.fromTy, i.toTy, i.opnd)
      case i: InstSelect  => "SELECT <%s %s> %s %s %s".format(i.condTy, i.opndTy, i.cond, i.ifTrue, i.ifFalse)
      case i: InstBranch  => "BRANCH %s".format(i.dest)
      case i: InstBranch2 => "BRANCH2 %s %s %s".format(i.cond, i.ifTrue, i.ifFalse)
      case i: InstSwitch => {
        val sHead = "SWITCH <%s> %s %s {\n".format(i.opndTy, i.opnd, i.defDest)
        val sBody = new StringBuilder()
        for (SwitchCase(v, d) <- i.cases) {
          sBody ++= "      %s: %s;\n".format(v, d)
        }
        sHead + sBody.toString + "      }"
      }
      case i: InstPhi => {
        val pHead = "PHI <%s> {\n".format(i.opndTy)
        val pBody = new StringBuilder()
        for (PhiCase(s, v) <- i.cases) {
          pBody ++= "      %s: %s;\n".format(s, v)
        }
        pHead + pBody.toString + "      }"
      }
      case i: InstCall => {
        val argList = i.argList.mkString(" ")
        val exc = maybeExcClause(i)
        val ka = maybeKeepAlives(i)
        "CALL <%s> %s (%s) %s %s".format(i.sig, i.callee, argList, exc, ka)
      }
      case i: InstTailCall => {
        val argList = i.argList.mkString(" ")
        "TAILCALL <%s> %s (%s)".format(i.sig, i.callee, argList)
      }
      case i: InstRet              => "RET <%s> %s".format(i.retTy, i.retVal)
      case i: InstRetVoid          => "RETVOID"
      case i: InstThrow            => "THROW %s".format(i.excVal)
      case i: InstLandingPad       => "LANDINGPAD"
      case i: InstExtractValue     => "EXTRACTVALUE <%s %d> %s".format(i.strTy, i.index, i.opnd)
      case i: InstInsertValue      => "INSERTVALUE <%s %d> %s %s".format(i.strTy, i.index, i.opnd, i.newVal)
      case i: InstExtractElement   => "EXTRACTELEMENT <%s %s> %s %s".format(i.vecTy, i.indTy, i.opnd, i.index)
      case i: InstInsertElement    => "INSERTELEMENT <%s %s> %s %s %s".format(i.vecTy, i.indTy, i.opnd, i.index, i.newVal)
      case i: InstShuffleVector    => "SHUFFLEVECTOR <%s %s> %s %s %s".format(i.vecTy, i.maskTy, i.vec1, i.vec2, i.mask)
      case i: InstNew              => "NEW <%s> %s".format(i.allocTy, maybeExcClause(i))
      case i: InstNewHybrid        => "NEWHYBRID <%s %s> %s %s".format(i.allocTy, i.lenTy, i.length, maybeExcClause(i))
      case i: InstAlloca           => "ALLOCA <%s> %s".format(i.allocTy, maybeExcClause(i))
      case i: InstAllocaHybrid     => "ALLOCAHYBRID <%s %s> %s %s".format(i.allocTy, i.lenTy, i.length, maybeExcClause(i))
      case i: InstGetIRef          => "GETIREF <%s> %s".format(i.referentTy, i.opnd)
      case i: InstGetFieldIRef     => "GETFIELDIREF %s <%s %d> %s".format(maybePtr(i), i.referentTy, i.index, i.opnd)
      case i: InstGetElemIRef      => "GETELEMIREF %s <%s %s> %s %s".format(maybePtr(i), i.referentTy, i.indTy, i.opnd, i.index)
      case i: InstShiftIRef        => "SHIFTIREF %s <%s %s> %s %s".format(maybePtr(i), i.referentTy, i.offTy, i.opnd, i.offset)
      case i: InstGetFixedPartIRef => "GETFIXEDPARTIREF %s <%s> %s".format(maybePtr(i), i.referentTy, i.opnd)
      case i: InstGetVarPartIRef   => "GETVARPARTIREF %s <%s> %s".format(maybePtr(i), i.referentTy, i.opnd)
      case i: InstLoad             => "LOAD %s %s <%s> %s %s".format(maybePtr(i), i.ord, i.referentTy, i.loc, maybeExcClause(i))
      case i: InstStore            => "STORE %s %s <%s> %s %s %s".format(maybePtr(i), i.ord, i.referentTy, i.loc, i.newVal, maybeExcClause(i))
      case i: InstCmpXchg => "CMPXCHG %s %s %s %s <%s> %s %s %s %s".format(maybePtr(i), maybeWeak(i), i.ordSucc, i.ordFail,
        i.referentTy, i.loc, i.expected, i.desired, maybeExcClause(i))
      case i: InstAtomicRMW => "ATOMICRMW %s %s %s <%s> %s %s %s".format(maybePtr(i), i.ord, i.op, i.referentTy, i.loc,
        i.opnd, maybeExcClause(i))
      case i: InstFence      => "FENCE %s".format(i.ord)
      case i: InstTrap       => "TRAP <%s> %s %s".format(i.retTy, maybeExcClause(i), maybeKeepAlives(i))
      case i: InstWatchPoint => "WATCHPOINT %d <%s> %s %s %s %s".format(i.wpid, i.retTy, i.dis, i.ena, maybeWPExc(i), maybeKeepAlives(i))
      case i: InstCCall => {
        val argList = i.argList.mkString(" ")
        val ka = maybeKeepAlives(i)
        "CCALL %s <%s %s> %s (%s) %s".format(i.callConv, i.calleeTy, i.sig, i.callee, argList, ka)
      }
      case i: InstNewStack => {
        val argList = i.argList.mkString(" ")
        val exc = maybeExcClause(i)
        "NEWSTACK <%s> %s (%s) %s".format(i.sig, i.callee, argList, exc)
      }
      case i: InstSwapStack => {
        val curStackClause = i.curStackClause match {
          case RetWith(t) => "RET_WITH (%s)".format(t)
          case KillOld()  => "KILL_OLD"
        }
        val newStackClause = i.newStackClause match {
          case PassValue(t, v) => "PASS_VALUE <%s> %s".format(t, v)
          case PassVoid()      => "PASS_VOID"
          case ThrowExc(e)     => "THROW_EXC(%s)".format(e)
        }
        "SWAPSTACK %s %s %s %s %s".format(i.swappee, curStackClause, newStackClause, maybeExcClause(i), maybeKeepAlives(i))
      }
      case i: InstCommInst => {
        val flagList = if (i.flagList.isEmpty()) "" else "[%s]".format(i.flagList.mkString(" "))
        val typeList = if (i.typeList.isEmpty()) "" else "<%s>".format(i.typeList.mkString(" "))
        val funcSigList = if (i.funcSigList.isEmpty()) "" else "<[%s]>".format(i.funcSigList.mkString(" "))
        val argList = if (i.argList.isEmpty()) "" else "(%s)".format(i.argList.mkString(" "))
        val exc = maybeExcClause(i)
        val ka = maybeKeepAlives(i)
        "COMMINST %s %s %s %s %s %s %s".format(i.inst, flagList, typeList, funcSigList, argList, exc, ka)
      }
    }
    asgn + body
  }

  def maybeExcClause(inst: HasExcClause): String = Option(inst.excClause) match {
    case Some(ExcClause(nor, exc)) => "EXC(%s %s)".format(nor, exc)
    case None                      => ""
  }

  def maybeKeepAlives(inst: HasKeepAlives): String = {
    if (inst.keepAlives.length == 0) ""
    else "KEEPALIVE(%s)".format(inst.keepAlives.mkString(" "))
  }

  def maybePtr(inst: WorksWithPointer): String = if (inst.ptr) "PTR" else ""
  def maybeWeak(inst: InstCmpXchg): String = if (inst.weak) "WEAK" else ""
  def maybeWPExc(inst: InstWatchPoint): String = if (inst.exc == null) "" else "WPEXC(%s)".format(inst.exc)
}