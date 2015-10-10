package uvm.clientsupport.text

import scala.collection.JavaConversions._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.matchers.{ Matcher, MatchResult }
import TextIRWriter._
import uvm.ir.textinput.UIRTextReader
import uvm.ir.textinput.IDFactory

class TextIRWriterTest extends FlatSpec with Matchers {

  behavior of "TextIRWriter"

  it should "correctly write types" in {

    typeToText {
      val t = new TypeInt()
      t.name = "@a"
      t.len = 32
      t
    } shouldEqual ".typedef @a = int<32>"

    typeToText {
      val t = new TypeFloat()
      t.name = "@a"
      t
    } shouldEqual ".typedef @a = float"

    typeToText {
      val t = new TypeDouble()
      t.name = "@a"
      t
    } shouldEqual ".typedef @a = double"

    typeToText {
      val t = new TypeRef()
      t.name = "@a"
      t.ty = "@b"
      t
    } shouldEqual ".typedef @a = ref<@b>"

    typeToText {
      val t = new TypeIRef()
      t.name = "@a"
      t.ty = "@b"
      t
    } shouldEqual ".typedef @a = iref<@b>"

    typeToText {
      val t = new TypeWeakRef()
      t.name = "@a"
      t.ty = "@b"
      t
    } shouldEqual ".typedef @a = weakref<@b>"

    typeToText {
      val t = new TypeStruct()
      t.name = "@a"
      t.fieldTy ++= Seq("@x", "@y", "@z")
      t
    } shouldEqual ".typedef @a = struct<@x @y @z>"

    typeToText {
      val t = new TypeArray()
      t.name = "@a"
      t.elemTy = "@b"
      t.len = 123456789012345L
      t
    } shouldEqual ".typedef @a = array<@b 123456789012345>"

    typeToText {
      val t = new TypeHybrid()
      t.name = "@a"
      t.fixedTy = "@b"
      t.varTy = "@c"
      t
    } shouldEqual ".typedef @a = hybrid<@b @c>"

    typeToText {
      val t = new TypeFunc()
      t.name = "@a"
      t.sig = "@b"
      t
    } shouldEqual ".typedef @a = func<@b>"

    typeToText {
      val t = new TypeThread()
      t.name = "@a"
      t
    } shouldEqual ".typedef @a = thread"

    typeToText {
      val t = new TypeStack()
      t.name = "@a"
      t
    } shouldEqual ".typedef @a = stack"

    typeToText {
      val t = new TypeTagRef64()
      t.name = "@a"
      t
    } shouldEqual ".typedef @a = tagref64"

    typeToText {
      val t = new TypePtr()
      t.name = "@a"
      t.ty = "@b"
      t
    } shouldEqual ".typedef @a = ptr<@b>"

    typeToText {
      val t = new TypeFuncPtr()
      t.name = "@a"
      t.sig = "@b"
      t
    } shouldEqual ".typedef @a = funcptr<@b>"
  }

  it should "correctly write function signature definitions" in {
    funcSigToText {
      val s = new FuncSigDef()
      s.name = "@sig"
      s.retTy = "@r"
      s.paramTy ++= Seq("@a", "@b", "@c", "@d")
      s
    } shouldEqual ".funcsig @sig = @r (@a @b @c @d)"
  }

  it should "correctly write constant definitions" in {
    constToText {
      val c = new ConstInt()
      c.name = "@C"
      c.ty = "@i64"
      c.num = 1357924680123456789L
      c
    } shouldEqual ".const @C <@i64> = 1357924680123456789"

    constToText {
      val c = new ConstFloat()
      c.name = "@C"
      c.ty = "@float"
      c.num = java.lang.Float.intBitsToFloat(0x7fc00001)
      c
    } shouldEqual ".const @C <@float> = bitsf(0x7fc00001)"

    constToText {
      val c = new ConstDouble()
      c.name = "@C"
      c.ty = "@double"
      c.num = java.lang.Double.longBitsToDouble(0x3ff0000000000000L)
      c
    } shouldEqual ".const @C <@double> = bitsd(0x3ff0000000000000)"

    constToText {
      val c = new ConstStruct()
      c.name = "@C"
      c.ty = "@foo"
      c.fields ++= Seq("@a", "@b", "@c")
      c
    } shouldEqual ".const @C <@foo> = {@a @b @c}"

    constToText {
      val c = new ConstVector()
      c.name = "@C"
      c.ty = "@foo"
      c.elems ++= Seq("@a", "@b", "@c")
      c
    } shouldEqual ".const @C <@foo> = VEC{@a @b @c}"

    constToText {
      val c = new ConstNull()
      c.name = "@C"
      c.ty = "@r"
      c
    } shouldEqual ".const @C <@r> = NULL"

    constToText {
      val c = new ConstPointer()
      c.name = "@C"
      c.ty = "@p"
      c.addr = 1357924680123456789L
      c
    } shouldEqual ".const @C <@p> = 1357924680123456789"
  }

  it should "correctly write global cell definitions" in {
    globalToText {
      val g = new GlobalCellDef()
      g.name = "@g"
      g.ty = "@i64"
      g
    } shouldEqual ".global @g <@i64>"
  }

  it should "correctly write function declarations" in {
    funcDeclToText {
      val f = new FuncDecl()
      f.name = "@f"
      f.sig = "@s"
      f
    } shouldEqual ".funcdecl @f <@s>"
  }

  it should "correctly write function exposing definitions" in {
    funcExpToText {
      val e = new FuncExpDef()
      e.name = "@e"
      e.func = "@f"
      e.callConv = "#DEFAULT"
      e.cookie = "@C"
      e
    } shouldEqual ".expose @e = @f #DEFAULT @C"
  }

  it should "correctly write instructions" in {
    instToText {
      val i = new InstBinOp()
      i.name = "@i"
      i.op = "SDIV"
      i.opndTy = "@i32"
      i.op1 = "@a"
      i.op2 = "@b"
      i.excClause = ExcClause("@nor", "@exc")
      i
    } shouldEqual "@i = SDIV <@i32> @a @b EXC(@nor @exc)"

    instToText {
      val i = new InstCmp()
      i.name = "@i"
      i.op = "EQ"
      i.opndTy = "@i32"
      i.op1 = "@a"
      i.op2 = "@b"
      i
    } shouldEqual "@i = EQ <@i32> @a @b"

    instToText {
      val i = new InstConv()
      i.name = "@i"
      i.op = "TRUNC"
      i.fromTy = "@i32"
      i.toTy = "@i16"
      i.opnd = "@a"
      i
    } shouldEqual "@i = TRUNC <@i32 @i16> @a"

    instToText {
      val i = new InstSelect()
      i.name = "@i"
      i.condTy = "@v4i1"
      i.opndTy = "@v4i32"
      i.cond = "@cond"
      i.ifTrue = "@val1"
      i.ifFalse = "@val2"
      i
    } shouldEqual "@i = SELECT <@v4i1 @v4i32> @cond @val1 @val2"

    instToText {
      val i = new InstBranch()
      i.name = "@i"
      i.dest = "@d"
      i
    } shouldEqual "@i = BRANCH @d"

    instToText {
      val i = new InstBranch2()
      i.name = "@i"
      i.cond = "@c"
      i.ifTrue = "@d1"
      i.ifFalse = "@d2"
      i
    } shouldEqual "@i = BRANCH2 @c @d1 @d2"

    instToText {
      val i = new InstSwitch()
      i.name = "@i"
      i.opndTy = "@T"
      i.opnd = "@o"
      i.defDest = "@d0"
      i.cases ++= Seq(
        SwitchCase("@v1", "@d1"),
        SwitchCase("@v2", "@d2"),
        SwitchCase("@v3", "@d3"))
      i
    }.replaceAll("\\s+", "") shouldEqual "@i = SWITCH <@T> @o @d0 {@v1:@d1;@v2:@d2;@v3:@d3;}".replaceAll("\\s+", "")

    instToText {
      val i = new InstPhi()
      i.name = "@i"
      i.opndTy = "@T"
      i.cases ++= Seq(
        PhiCase("@s1", "@v1"),
        PhiCase("@s2", "@v2"),
        PhiCase("@s3", "@v3"))
      i
    }.replaceAll("\\s+", "") shouldEqual "@i = PHI <@T> {@s1:@v1;@s2:@v2;@s3:@v3;}".replaceAll("\\s+", "")

    instToText {
      val i = new InstCall()
      i.name = "@i"
      i.sig = "@s"
      i.callee = "@c"
      i.argList ++= Seq("@x", "@y", "@z")
      i.excClause = ExcClause("@nor", "@exc")
      i.keepAlives ++= Seq("@u", "@v")
      i
    } shouldEqual "@i = CALL <@s> @c (@x @y @z) EXC(@nor @exc) KEEPALIVE(@u @v)"

    instToText {
      val i = new InstTailCall()
      i.name = "@i"
      i.sig = "@s"
      i.callee = "@c"
      i.argList ++= Seq("@x", "@y", "@z")
      i
    } shouldEqual "@i = TAILCALL <@s> @c (@x @y @z)"

    instToText {
      val i = new InstRet()
      i.name = "@i"
      i.retTy = "@t"
      i.retVal = "@v"
      i
    } shouldEqual "@i = RET <@t> @v"

    instToText {
      val i = new InstRetVoid()
      i.name = "@i"
      i
    } shouldEqual "@i = RETVOID"

    instToText {
      val i = new InstThrow()
      i.name = "@i"
      i.excVal = "@e"
      i
    } shouldEqual "@i = THROW @e"

    instToText {
      val i = new InstLandingPad()
      i.name = "@i"
      i
    } shouldEqual "@i = LANDINGPAD"

    instToText {
      val i = new InstExtractValue()
      i.name = "@i"
      i.strTy = "@s"
      i.index = 3
      i.opnd = "@x"
      i
    } shouldEqual "@i = EXTRACTVALUE <@s 3> @x"

    instToText {
      val i = new InstInsertValue()
      i.name = "@i"
      i.strTy = "@s"
      i.index = 3
      i.opnd = "@x"
      i.newVal = "@y"
      i
    } shouldEqual "@i = INSERTVALUE <@s 3> @x @y"

    instToText {
      val i = new InstExtractElement()
      i.name = "@i"
      i.vecTy = "@v"
      i.indTy = "@i32"
      i.opnd = "@x"
      i.index = "@n"
      i
    } shouldEqual "@i = EXTRACTELEMENT <@v @i32> @x @n"

    instToText {
      val i = new InstInsertElement()
      i.name = "@i"
      i.vecTy = "@v"
      i.indTy = "@i32"
      i.opnd = "@x"
      i.index = "@n"
      i.newVal = "@y"
      i
    } shouldEqual "@i = INSERTELEMENT <@v @i32> @x @n @y"

    instToText {
      val i = new InstShuffleVector()
      i.name = "@i"
      i.vecTy = "@v"
      i.maskTy = "@m"
      i.vec1 = "@x"
      i.vec2 = "@y"
      i.mask = "@z"
      i
    } shouldEqual "@i = SHUFFLEVECTOR <@v @m> @x @y @z"

    instToText {
      val i = new InstNew()
      i.name = "@i"
      i.allocTy = "@t"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = NEW <@t> EXC(@n @e)"

    instToText {
      val i = new InstNewHybrid()
      i.name = "@i"
      i.allocTy = "@t"
      i.lenTy = "@i32"
      i.length = "@l"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = NEWHYBRID <@t @i32> @l EXC(@n @e)"

    instToText {
      val i = new InstAlloca()
      i.name = "@i"
      i.allocTy = "@t"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = ALLOCA <@t> EXC(@n @e)"

    instToText {
      val i = new InstAllocaHybrid()
      i.name = "@i"
      i.allocTy = "@t"
      i.lenTy = "@i32"
      i.length = "@l"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = ALLOCAHYBRID <@t @i32> @l EXC(@n @e)"

    instToText {
      val i = new InstGetIRef()
      i.name = "@i"
      i.referentTy = "@t"
      i.opnd = "@r"
      i
    } shouldEqual "@i = GETIREF <@t> @r"

    instToText {
      val i = new InstGetFieldIRef()
      i.name = "@i"
      i.ptr = true
      i.referentTy = "@t"
      i.index = 3
      i.opnd = "@r"
      i
    } shouldEqual "@i = GETFIELDIREF PTR <@t 3> @r"

    instToText {
      val i = new InstGetElemIRef()
      i.name = "@i"
      i.ptr = true
      i.referentTy = "@t"
      i.indTy = "@i32"
      i.opnd = "@r"
      i.index = "@i"
      i
    } shouldEqual "@i = GETELEMIREF PTR <@t @i32> @r @i"

    instToText {
      val i = new InstShiftIRef()
      i.name = "@i"
      i.ptr = true
      i.referentTy = "@t"
      i.offTy = "@i32"
      i.opnd = "@r"
      i.offset = "@i"
      i
    } shouldEqual "@i = SHIFTIREF PTR <@t @i32> @r @i"

    instToText {
      val i = new InstGetFixedPartIRef()
      i.name = "@i"
      i.ptr = true
      i.referentTy = "@t"
      i.opnd = "@r"
      i
    } shouldEqual "@i = GETFIXEDPARTIREF PTR <@t> @r"

    instToText {
      val i = new InstGetVarPartIRef()
      i.name = "@i"
      i.ptr = true
      i.referentTy = "@t"
      i.opnd = "@r"
      i
    } shouldEqual "@i = GETVARPARTIREF PTR <@t> @r"

    instToText {
      val i = new InstLoad()
      i.name = "@i"
      i.ptr = true
      i.ord = "SEQ_CST"
      i.referentTy = "@t"
      i.loc = "@r"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = LOAD PTR SEQ_CST <@t> @r EXC(@n @e)"

    instToText {
      val i = new InstStore()
      i.name = "@i"
      i.ptr = true
      i.ord = "SEQ_CST"
      i.referentTy = "@t"
      i.loc = "@r"
      i.newVal = "@n"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = STORE PTR SEQ_CST <@t> @r @n EXC(@n @e)"

    instToText {
      val i = new InstCmpXchg()
      i.name = "@i"
      i.ptr = true
      i.weak = true
      i.ordSucc = "ACQ_REL"
      i.ordFail = "ACQUIRE"
      i.referentTy = "@t"
      i.loc = "@r"
      i.expected = "@ex"
      i.desired = "@de"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = CMPXCHG PTR WEAK ACQ_REL ACQUIRE <@t> @r @ex @de EXC(@n @e)"

    instToText {
      val i = new InstAtomicRMW()
      i.name = "@i"
      i.ptr = true
      i.ord = "SEQ_CST"
      i.op = "XOR"
      i.referentTy = "@t"
      i.loc = "@r"
      i.opnd = "@o"
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = ATOMICRMW PTR SEQ_CST XOR <@t> @r @o EXC(@n @e)"

    instToText {
      val i = new InstFence()
      i.name = "@i"
      i.ord = "SEQ_CST"
      i
    } shouldEqual "@i = FENCE SEQ_CST"

    instToText {
      val i = new InstTrap()
      i.name = "@i"
      i.retTy = "@t"
      i.excClause = ExcClause("@n", "@e")
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = TRAP <@t> EXC(@n @e) KEEPALIVE(@a @b)"

    instToText {
      val i = new InstWatchPoint()
      i.name = "@i"
      i.wpid = 42
      i.retTy = "@t"
      i.dis = "@dis"
      i.ena = "@ena"
      i.exc = "@exc"
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = WATCHPOINT 42 <@t> @dis @ena WPEXC(@exc) KEEPALIVE(@a @b)"

    instToText {
      val i = new InstCCall()
      i.name = "@i"
      i.callConv = "#DEFAULT"
      i.calleeTy = "@ct"
      i.sig = "@s"
      i.callee = "@c"
      i.argList ++= Seq("@x", "@y", "@z")
      i.keepAlives ++= Seq("@u", "@v")
      i
    } shouldEqual "@i = CCALL #DEFAULT <@ct @s> @c (@x @y @z) KEEPALIVE(@u @v)"

    instToText {
      val i = new InstNewStack()
      i.name = "@i"
      i.sig = "@s"
      i.callee = "@c"
      i.argList ++= Seq("@x", "@y", "@z")
      i.excClause = ExcClause("@n", "@e")
      i
    } shouldEqual "@i = NEWSTACK <@s> @c (@x @y @z) EXC(@n @e)"

    instToText {
      val i = new InstSwapStack()
      i.name = "@i"
      i.swappee = "@s"
      i.curStackClause = RetWith("@t")
      i.newStackClause = PassValue("@t2", "@v")
      i.excClause = ExcClause("@n", "@e")
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = SWAPSTACK @s RET_WITH (@t) PASS_VALUE <@t2> @v EXC(@n @e) KEEPALIVE(@a @b)"

    instToText {
      val i = new InstSwapStack()
      i.name = "@i"
      i.swappee = "@s"
      i.curStackClause = KillOld()
      i.newStackClause = PassVoid()
      i.excClause = ExcClause("@n", "@e")
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = SWAPSTACK @s KILL_OLD PASS_VOID EXC(@n @e) KEEPALIVE(@a @b)"

    instToText {
      val i = new InstSwapStack()
      i.name = "@i"
      i.swappee = "@s"
      i.curStackClause = KillOld()
      i.newStackClause = ThrowExc("@exc")
      i.excClause = ExcClause("@n", "@e")
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = SWAPSTACK @s KILL_OLD THROW_EXC(@exc) EXC(@n @e) KEEPALIVE(@a @b)"

    instToText {
      val i = new InstCommInst()
      i.name = "@i"
      i.inst = "@foo.bar"
      i.flagList ++= Seq("#A", "#B")
      i.typeList ++= Seq("@t1", "@t2")
      i.funcSigList ++= Seq("@s1", "@s2")
      i.argList ++= Seq("@a1", "@a2")
      i.excClause = ExcClause("@n", "@e")
      i.keepAlives ++= Seq("@a", "@b")
      i
    } shouldEqual "@i = COMMINST @foo.bar [#A #B] <@t1 @t2> <[@s1 @s2]> (@a1 @a2) EXC(@n @e) KEEPALIVE(@a @b)"
  }

  "Instruction name" should "be optinal" in {
    instToText {
      val i = new InstRetVoid()
      i.name = null
      i
    } shouldEqual "RETVOID"
  }

  "Exception clause" should "be optional" in {
    maybeExcClause {
      val i = new InstBinOp()
      i.excClause = null
      i
    } shouldEqual ""
  }

  "Keep-alive clause" should "be omitted when empty" in {
    maybeKeepAlives {
      val i = new InstCall()
      i
    } shouldEqual ""
  }
  
  "The written bundle" should "be parseable by the Mu's UIR parser" in {
    val bundle = new Bundle()
    
    val t = new TypeInt()
    t.name = "@i64"
    t.len = 64
    bundle.typeDefs += t
    
    val s = new FuncSigDef()
    s.name = "@s"
    s.retTy = "@i64"
    s.paramTy ++= Seq("@i64", "@i64")
    bundle.funcSigDefs += s
    
    val c = new ConstInt()
    c.name = "@c"
    c.ty = "@i64"
    c.num = 42L
    bundle.constDefs += c
    
    val g = new GlobalCellDef()
    g.name = "@g"
    g.ty = "@i64"
    bundle.globalCellDefs += g
    
    val fDecl = new FuncDecl()
    fDecl.name = "@f1"
    fDecl.sig = "@s"
    bundle.funcDecls += fDecl
    
    val fExp = new FuncExpDef()
    fExp.name = "@nf"
    fExp.func = "@f1"
    fExp.callConv = "#DEFAULT"
    fExp.cookie = "@c"
    bundle.funcExpDefs += fExp
    
    val fdb = new FuncDefBuilder("@f2", "@f2.v1", "@s")
    fdb.addParam("x")
    fdb.addParam("y")
    
    val i = new InstBranch()
    val bb2 = fdb.newBB("n")
    i.dest = bb2.name
    fdb.emit(i)
    
    fdb.setCurBB(bb2)
    val j = new InstRet()
    j.retTy = "@i64"
    j.retVal = "@c"
    fdb.emit(j)
    
    bundle.funcDefs += fdb.getFuncDef()
    
    val ir = TextIRWriter.bundleToText(bundle)
    
    println(ir)
    
    val muBundle = new UIRTextReader(new IDFactory).read(ir, new uvm.GlobalBundle())
  }
}