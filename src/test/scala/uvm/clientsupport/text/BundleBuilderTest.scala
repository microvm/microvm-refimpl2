package uvm.clientsupport.text

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import TextOutputMatchers._
import uvm.ir.textinput.UIRTextReader
import uvm.utils.IDFactory
import uvm.LogSetter
import uvm.UvmTestBase

class BundleBuilderTest extends UvmTestBase {

  val x = TypeName("x")
  val y = TypeName("y")
  val z = TypeName("z")
  val s = FuncSigName("s")

  behavior of "BundleBuilder"

  it should "correctly write types" in {
    val a = TypeName("a")
    def typeDef(ctor: TypeCtor): String = {
      val builder = new BundleBuilder("test")
      builder.typeDef(a, ctor)
      builder.build().toString.trim
    }

    typeDef(TypeCtor.Int(32)) shouldEqual ".typedef @a = int<32>"
    typeDef(TypeCtor.Float) shouldEqual ".typedef @a = float"
    typeDef(TypeCtor.Double) shouldEqual ".typedef @a = double"
    typeDef(TypeCtor.Ref(x)) shouldEqual ".typedef @a = ref<@x>"
    typeDef(TypeCtor.IRef(x)) shouldEqual ".typedef @a = iref<@x>"
    typeDef(TypeCtor.WeakRef(x)) shouldEqual ".typedef @a = weakref<@x>"
    typeDef(new TypeCtor.Struct(IList(x, y, z))) shouldEqual ".typedef @a = struct<@x @y @z>"
    typeDef(TypeCtor.Array(x, 123456789012345L)) shouldEqual ".typedef @a = array<@x 123456789012345>"
    typeDef(TypeCtor.Hybrid(x, y)) shouldEqual ".typedef @a = hybrid<@x @y>"
    typeDef(TypeCtor.FuncRef(s)) shouldEqual ".typedef @a = funcref<@s>"
    typeDef(TypeCtor.ThreadRef) shouldEqual ".typedef @a = threadref"
    typeDef(TypeCtor.StackRef) shouldEqual ".typedef @a = stackref"
    typeDef(TypeCtor.TagRef64) shouldEqual ".typedef @a = tagref64"
    typeDef(TypeCtor.UPtr(x)) shouldEqual ".typedef @a = uptr<@x>"
    typeDef(TypeCtor.UFuncPtr(s)) shouldEqual ".typedef @a = ufuncptr<@s>"
  }

  it should "correctly write function signature definitions" in {
    val r = TypeName("r")
    val builder = new BundleBuilder("test")
    builder.funcSig(s, IList(x, y, z), IList(r))
    builder.build().toString.trim shouldEqual ".funcsig @s = (@x @y @z) -> (@r)"
  }

  it should "correctly write constant definitions" in {
    val c = GlobalVarName("C")
    val abc = IList(GlobalVarName("a"), GlobalVarName("b"), GlobalVarName("c"))
    def const(constDef: Const): String = {
      val builder = new BundleBuilder("test")
      builder.constDef(c, constDef)
      builder.build().toString.trim
    }

    const(Const.Int(x, 1357924680123456789L)) shouldEqual ".const @C <@x> = 1357924680123456789"
    const(Const.Float(x, java.lang.Float.intBitsToFloat(0x7fc00001))) shouldEqual
      ".const @C <@x> = bitsf(0x7fc00001)"
    const(Const.Double(x, java.lang.Double.longBitsToDouble(0x3ff0000000000000L))) shouldEqual
      ".const @C <@x> = bitsd(0x3ff0000000000000)"
    const(new Const.List(x, abc)) shouldEqual ".const @C <@x> = {@a @b @c}"
    const(new Const.Null(x)) shouldEqual ".const @C <@x> = NULL"
    const(new Const.Pointer(x, 1357924680123456789L)) shouldEqual ".const @C <@x> = 1357924680123456789"
  }

  it should "correctly write global cell definitions" in {
    val g = GlobalVarName("g")
    val builder = new BundleBuilder("test")
    builder.globalCell(g, x)
    builder.build().toString.trim shouldEqual ".global @g <@x>"
  }

  it should "correctly write function declarations" in {
    val f = GlobalVarName("f")
    val builder = new BundleBuilder("test")
    builder.funcDecl(f, s)
    builder.build().toString.trim shouldEqual ".funcdecl @f <@s>"
  }

  it should "correctly write function exposing definitions" in {
    val e = GlobalVarName("e")
    val f = GlobalVarName("f")
    val c = GlobalVarName("C")
    val builder = new BundleBuilder("test")
    builder.exposeFunc(e, f, new Flag("DEFAULT"), c)
    builder.build().toString.trim shouldEqual ".expose @e = @f #DEFAULT @C"
  }

  it should "have a correct string format for instructions" in {
    val a = LocalVarName("a")
    val b = LocalVarName("b")
    val c = LocalVarName("c")
    val f = GlobalVarName("f")
    val l1 = LabelName("l1")
    val l2 = LabelName("l2")
    val dest1 = DestClause(l1, IList(a, b))
    val dest2 = DestClause(l2, IList(c))
    val nor = DestClause(LabelName("nor"), IList())
    val exc = DestClause(LabelName("exc"), IList())
    import MemoryOrder._

    PostExcClause(Inst.BinOp(Inst.BinOp.SignDiv, x, a, b), nor, exc).toString shouldEqual
      "SDIV <@x> %a %b EXC(%nor() %exc())"
    Inst.Cmp(Inst.Cmp.Eq, x, a, b).toString shouldEqual "EQ <@x> %a %b"
    Inst.Conv(Inst.Conv.Trunc, x, y, a).toString shouldEqual "TRUNC <@x @y> %a"
    Inst.Select(x, y, a, b, c).toString shouldEqual "SELECT <@x @y> %a %b %c"
    Inst.PostBranch(dest1).toString shouldEqual "BRANCH %l1(%a %b)"
    Inst.PostBranch2(a, dest1, dest2).toString shouldEqual "BRANCH2 %a %l1(%a %b) %l2(%c)"
    Inst.PreSwitch(x, a, l1, IList(
      SwitchCase(LocalVarName("v1"), LabelName("d1")),
      SwitchCase(LocalVarName("v2"), LabelName("d2")),
      SwitchCase(LocalVarName("v3"), LabelName("d3")))).toString shouldEqual "SWITCH <@x> %a %l1 { %v1: %d1; %v2: %d2; %v3: %d3; }"
    PostExcClause(Inst.Call(s, f, IList(a, b, c), Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "CALL <@s> @f (%a %b %c) EXC(%nor() %exc()) KEEPALIVE(%a %b)"
    Inst.TailCall(s, f, IList(a, b, c)).toString shouldEqual
      "TAILCALL <@s> @f (%a %b %c)"
    Inst.Ret(IList(a)).toString shouldEqual "RET (%a)"
    Inst.RetVoid().toString shouldEqual "RETVOID"
    Inst.Throw(a).toString shouldEqual "THROW %a"
    Inst.LandingPad().toString shouldEqual "LANDINGPAD"
    Inst.ExtractValue(x, 3, a).toString shouldEqual "EXTRACTVALUE <@x 3> %a"
    Inst.InsertValue(x, 3, a, b).toString shouldEqual "INSERTVALUE <@x 3> %a %b"
    Inst.ExtractElement(x, y, a, b).toString shouldEqual "EXTRACTELEMENT <@x @y> %a %b"
    Inst.InsertElement(x, y, a, b, c).toString shouldEqual "INSERTELEMENT <@x @y> %a %b %c"
    Inst.ShuffleVector(x, y, a, b, c).toString shouldEqual "SHUFFLEVECTOR <@x @y> %a %b %c"
    PostExcClause(Inst.New(x), nor, exc).toString shouldEqual "NEW <@x> EXC(%nor() %exc())"
    PostExcClause(Inst.NewHybrid(x, 91, a), nor, exc).toString shouldEqual "NEWHYBRID <@x 91> %a EXC(%nor() %exc())"
    PostExcClause(Inst.Alloca(x), nor, exc).toString shouldEqual "ALLOCA <@x> EXC(%nor() %exc())"
    PostExcClause(Inst.AllocaHybrid(x, 91, a), nor, exc).toString shouldEqual "ALLOCAHYBRID <@x 91> %a EXC(%nor() %exc())"
    Inst.GetIRef(x, a).toString shouldEqual "GETIREF <@x> %a"
    Inst.GetFieldIRef(ptr = true, x, 3, a).toString shouldEqual "GETFIELDIREF PTR <@x 3> %a"
    Inst.GetElemIRef(ptr = true, x, y, a, b).toString shouldEqual "GETELEMIREF PTR <@x @y> %a %b"
    Inst.ShiftIRef(ptr = true, x, y, a, b).toString shouldEqual "SHIFTIREF PTR <@x @y> %a %b"
    Inst.GetFixedPartIRef(ptr = true, x, a).toString shouldEqual "GETFIXEDPARTIREF PTR <@x> %a"
    Inst.GetVarPartIRef(ptr = true, x, a).toString shouldEqual "GETVARPARTIREF PTR <@x> %a"
    PostExcClause(Inst.Load(ptr = true, SeqConsistent, x, a), nor, exc).toString shouldEqual
      "LOAD PTR SEQ_CST <@x> %a EXC(%nor() %exc())"
    PostExcClause(Inst.Store(ptr = true, SeqConsistent, x, a, b), nor, exc).toString shouldEqual
      "STORE PTR SEQ_CST <@x> %a %b EXC(%nor() %exc())"
    PostExcClause(Inst.CmpXchg(ptr = true, weak = true, AcquireRelease, Acquire, x, a, b, c), nor, exc).toString shouldEqual
      "CMPXCHG PTR WEAK ACQ_REL ACQUIRE <@x> %a %b %c EXC(%nor() %exc())"
    PostExcClause(Inst.AtomicRMW(ptr = true, SeqConsistent, AtomicRMWOptr.Xor, x, a, b), nor, exc).toString shouldEqual
      "ATOMICRMW PTR SEQ_CST XOR <@x> %a %b EXC(%nor() %exc())"
    Inst.Fence(SeqConsistent).toString shouldEqual "FENCE SEQ_CST"
    PostExcClause(Inst.Trap(IList(x), Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "TRAP <@x> EXC(%nor() %exc()) KEEPALIVE(%a %b)"
    Inst.PostWatchPoint(42, IList(x), dest1, dest2, Some(dest1), Some(KeepAliveClause(IList(a, b)))).toString shouldEqual
      "WATCHPOINT 42 <@x> %l1(%a %b) %l2(%c) WPEXC(%l1(%a %b)) KEEPALIVE(%a %b)"
    Inst.CCall(new Flag("DEFAULT"), x, s, f, IList(a, b, c), Some(KeepAliveClause(IList(a, b)))).toString shouldEqual
      "CCALL #DEFAULT <@x @s> @f (%a %b %c) KEEPALIVE(%a %b)"
    PostExcClause(Inst.NewThread(f, NewStackClause.PassVoid()), nor, exc).toString shouldEqual
      "NEWTHREAD @f PASS_VOID EXC(%nor() %exc())"
    PostExcClause(Inst.SwapStack(a,
      CurStackClause.RetWith(x),
      NewStackClause.PassValue(y, b),
      Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "SWAPSTACK %a RET_WITH <@x> PASS_VALUE <@y> %b EXC(%nor() %exc()) KEEPALIVE(%a %b)"
    PostExcClause(Inst.SwapStack(a,
      CurStackClause.KillOld(),
      NewStackClause.PassVoid(),
      Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "SWAPSTACK %a KILL_OLD PASS_VOID EXC(%nor() %exc()) KEEPALIVE(%a %b)"
    PostExcClause(Inst.SwapStack(a,
      CurStackClause.KillOld(),
      NewStackClause.ThrowExc(b),
      Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "SWAPSTACK %a KILL_OLD THROW_EXC %b EXC(%nor() %exc()) KEEPALIVE(%a %b)"
    PostExcClause(Inst.CommInst(GlobalVarName("foo.bar"),
      Some(IList(new Flag("A"), new Flag("B"))),
      Some(IList(x, y)),
      Some(IList(s, FuncSigName("s2"))),
      Some(IList(a, b)),
      Some(KeepAliveClause(IList(a, b)))), nor, exc).toString shouldEqual
      "COMMINST @foo.bar [#A #B] <@x @y> <[@s @s2]> (%a %b) EXC(%nor() %exc()) KEEPALIVE(%a %b)"
  }

  it should "translate simple functions to SSA form" in {
    import TypeCtor.Int
    def buildFn(name: String, paramTy: Seq[TypeCtor], retTy: Seq[TypeCtor])(fn: FunctionBuilder => Unit): BundleBuilder = {
      val builder = new BundleBuilder("test")
      fn(builder.newFuncVersion(
        GlobalVarName(name),
        new IList(paramTy map builder.typeDef),
        new IList(retTy map builder.typeDef)))
      builder
    }

    buildFn("add", Seq(Int(32), Int(32)), Seq(Int(32))) { fbuilder =>
      val addRes = fbuilder inst Inst.BinOp(
        Inst.BinOp.Add,
        fbuilder typeDef Int(32),
        fbuilder paramNames 0,
        fbuilder paramNames 1)
      fbuilder.inst(IList(), Inst.Ret(IList(addRes)))
    } should matchIRTemplate("""
      .typedef @$int = int<32>
      .funcsig @$sig = (@$int @$int) -> (@$int)
      .funcdef @add VERSION @$ver <@$sig> {
        %entry(<@$int> %$a <@$int> %$b):
          (%$c) = ADD <@$int> %$a %$b
          () = RET (%$c)
      }
    """)

    buildFn("pow4", Seq(Int(32)), Seq(Int(32))) { fbuilder =>
      val v = fbuilder.paramNames(0)
      fbuilder.inst(IList(v), Inst.BinOp(Inst.BinOp.Mul, fbuilder typeDef Int(32), v, v))
      fbuilder.inst(IList(v), Inst.BinOp(Inst.BinOp.Mul, fbuilder typeDef Int(32), v, v))
      fbuilder.inst(IList(), Inst.Ret(IList(v)))
    } should matchIRTemplate("""
      .typedef @$int = int<32>
      .funcsig @$sig = (@$int) -> (@$int)
      .funcdef @pow4 VERSION @$ver <@$sig> {
        %entry(<@$int> %$a):
          (%$b) = MUL <@$int> %$a %$a
          (%$c) = MUL <@$int> %$b %$b
          () = RET (%$c)
      }
    """)

    buildFn("addjump", Seq(Int(32), Int(32)), Seq(Int(32))) { fbuilder =>
      val addRes = fbuilder inst Inst.BinOp(
        Inst.BinOp.Add,
        fbuilder typeDef Int(32),
        fbuilder paramNames 0,
        fbuilder paramNames 1)
      val next = fbuilder.newLabelName()
      fbuilder.inst(IList(), Inst.PreBranch(next))
      fbuilder.startNewBlock(next)
      fbuilder.inst(IList(), Inst.Ret(IList(addRes)))
    } should matchIRTemplate("""
      .typedef @$int = int<32>
      .funcsig @$sig = (@$int @$int) -> (@$int)
      .funcdef @addjump VERSION @$ver <@$sig> {
        %entry(<@$int> %$a <@$int> %$b):
          (%$c) = ADD <@$int> %$a %$b
          () = BRANCH %$next(%$c)
        %$next(<@$int> %$d):
          () = RET (%$d)
      }
    """)

    buildFn("gt5", Seq(Int(32)), Seq(Int(1))) { fbuilder =>
      val a = fbuilder paramNames 0
      val t = fbuilder constDef Const.Int(fbuilder typeDef Int(1), 1)
      val f = fbuilder constDef Const.Int(fbuilder typeDef Int(1), 0)
      val five = fbuilder constDef Const.Int(fbuilder typeDef Int(32), 5)
      val b = fbuilder.inst(Inst.Cmp(Inst.Cmp.SignGt, fbuilder typeDef Int(32), a, five))
      val l1 = fbuilder.newLabelName()
      val l2 = fbuilder.newLabelName()
      fbuilder.inst(IList(), Inst.PreBranch2(b, l1, l2))
      fbuilder.startNewBlock(l1)
      fbuilder.inst(IList(), Inst.Ret(IList(t)))
      fbuilder.startNewBlock(l2)
      fbuilder.inst(IList(), Inst.Ret(IList(f)))
    } should matchIRTemplate("""
      .typedef @$int = int<32>
      .typedef @$bool = int<1>
      .funcsig @$sig = (@$int) -> (@$bool)
      .const @$true <@$bool> = 1
      .const @$false <@$bool> = 0
      .const @$five <@$int> = 5
      .funcdef @gt5 VERSION @$ver <@$sig> {
        %entry(<@$int> %$a):
          (%$b) = SGT <@$int> %$a @$five
          () = BRANCH2 %$b %$trueBranch() %$falseBranch()
        %$trueBranch():
          () = RET (@$true)
        %$falseBranch():
          () = RET (@$false)
      }
    """)
  }

  "The written bundle" should "be parseable by Mu's UIR parser" in {
    val builder = new BundleBuilder("test")
    val t = builder.typeDef(TypeCtor.Int(64))
    val s = builder.funcSig(IList(t, t), IList(t))
    val c = builder.constDef(Const.Int(t, 42L))
    val g = builder.globalCell(t)
    val fDecl = builder.funcDecl(builder.newVarName("f1"), s)
    builder.exposeFunc(builder.newVarName("nf"), fDecl, new Flag("DEFAULT"), c)

    val funcBuilder =
      builder.newFuncVersion(builder.newVarName("f2"), s, IList("x", "y"))
    val bb2 = funcBuilder.newLabelName()
    funcBuilder.inst(IList(), Inst.Ret(IList(c)))

    val ir = builder.build().toString

    println(ir)

    val muBundle = new UIRTextReader(new IDFactory(0)).read(ir, new uvm.GlobalBundle())
  }
}