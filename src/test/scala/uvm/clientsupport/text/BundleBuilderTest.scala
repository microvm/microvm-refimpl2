package uvm.clientsupport.text

import scala.collection.JavaConversions._
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import uvm.ir.textinput.UIRTextReader
import uvm.ir.textinput.IDFactory

class BundleBuilderTest extends FlatSpec with Matchers {

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
    typeDef(TypeCtor.Float()) shouldEqual ".typedef @a = float"
    typeDef(TypeCtor.Double()) shouldEqual ".typedef @a = double"
    typeDef(TypeCtor.Ref(x)) shouldEqual ".typedef @a = ref<@x>"
    typeDef(TypeCtor.IRef(x)) shouldEqual ".typedef @a = iref<@x>"
    typeDef(TypeCtor.WeakRef(x)) shouldEqual ".typedef @a = weakref<@x>"
    typeDef(new TypeCtor.Struct(Seq(x, y, z))) shouldEqual ".typedef @a = struct<@x @y @z>"
    typeDef(TypeCtor.Array(x, 123456789012345L)) shouldEqual ".typedef @a = array<@x 123456789012345>"
    typeDef(TypeCtor.Hybrid(x, y)) shouldEqual ".typedef @a = hybrid<@x @y>"
    typeDef(TypeCtor.Func(s)) shouldEqual ".typedef @a = func<@s>"
    typeDef(TypeCtor.Thread()) shouldEqual ".typedef @a = thread"
    typeDef(TypeCtor.Stack()) shouldEqual ".typedef @a = stack"
    typeDef(TypeCtor.TagRef64()) shouldEqual ".typedef @a = tagref64"
    typeDef(TypeCtor.Ptr(x)) shouldEqual ".typedef @a = ptr<@x>"
    typeDef(TypeCtor.FuncPtr(s)) shouldEqual ".typedef @a = funcptr<@s>"
  }

  it should "correctly write function signature definitions" in {
    val r = TypeName("r")
    val builder = new BundleBuilder("test")
    builder.funcSig(s, r, Seq(x, y, z))
    builder.build().toString.trim shouldEqual ".funcsig @s = @r (@x @y @z)"
  }

  it should "correctly write constant definitions" in {
    val c = GlobalVarName("C")
    val abc = Seq(GlobalVarName("a"), GlobalVarName("b"), GlobalVarName("c"))
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
    const(new Const.Struct(x, abc)) shouldEqual ".const @C <@x> = {@a @b @c}"
    const(new Const.Vector(x, abc)) shouldEqual ".const @C <@x> = VEC{@a @b @c}"
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

  it should "correctly write instructions" in {
    val i = LocalVarName("i")
    val a = LocalVarName("a")
    val b = LocalVarName("b")
    val c = LocalVarName("c")
    val f = GlobalVarName("f")
    val v = FuncVerName("v")
    val l1 = LabelName("l1")
    val l2 = LabelName("l2")
    val exc = ExcClause(LabelName("nor"), LabelName("exc"))
    def inst(inst: Inst): List[String] = {
      val builder = new BundleBuilder("test")
      val fbuilder = builder.newFuncVersion(f, s, v, Seq.empty)
      fbuilder.inst(i, inst)
      builder.build().toString.split('\n').toList.map(_.trim.replaceAll("\\s+", " ")).filterNot(_.isEmpty)
    }
    def functionWith(line: String): List[String] =
      List(".funcdef @f VERSION @v <@s> () {", "%entry:", line.replaceAll("\\s+", " "), "}")

    inst(new Inst.BinOp(Inst.BinOp.SignDiv, x, a, b, exc)) shouldEqual functionWith(
      "%i = SDIV <@x> %a %b EXC(%nor %exc)"
    )
    inst(new Inst.Cmp(Inst.Cmp.Eq, x, a, b)) shouldEqual functionWith("%i = EQ <@x> %a %b")
    inst(new Inst.Conv(Inst.Conv.Trunc, x, y, a)) shouldEqual functionWith("%i = TRUNC <@x @y> %a")
    inst(new Inst.Select(x, y, a, b, c)) shouldEqual functionWith("%i = SELECT <@x @y> %a %b %c")
    inst(new Inst.Branch(l1)) shouldEqual functionWith("%i = BRANCH %l1")
    inst(new Inst.Branch2(a, l1, l2)) shouldEqual functionWith("%i = BRANCH2 %a %l1 %l2")
    inst(new Inst.Switch(x, a, l1, Seq(
      new SwitchCase(LocalVarName("v1"), LabelName("d1")),
      new SwitchCase(LocalVarName("v2"), LabelName("d2")),
      new SwitchCase(LocalVarName("v3"), LabelName("d3"))
    ))) shouldEqual functionWith("%i = SWITCH <@x> %a %l1 { %v1: %d1; %v2: %d2; %v3: %d3; }")
    inst(new Inst.Phi(x, Seq(
      new PhiCase(LabelName("s1"), LocalVarName("v1")),
      new PhiCase(LabelName("s2"), LocalVarName("v2")),
      new PhiCase(LabelName("s3"), LocalVarName("v3"))
    ))) shouldEqual functionWith("%i = PHI <@x> { %s1: %v1; %s2: %v2; %s3: %v3; }")
    inst(new Inst.Call(s, f, Seq(a, b, c), exc, new KeepAliveClause(Seq(a, b)))) shouldEqual
      functionWith("%i = CALL <@s> @f (%a %b %c) EXC(%nor %exc) KEEPALIVE(%a %b)")
    inst(new Inst.TailCall(s, f, Seq(a, b, c))) shouldEqual
      functionWith("%i = TAILCALL <@s> @f (%a %b %c)")
    inst(new Inst.Ret(x, a)) shouldEqual functionWith("%i = RET <@x> %a")
    inst(new Inst.RetVoid()) shouldEqual functionWith("%i = RETVOID")
    inst(new Inst.Throw(a)) shouldEqual functionWith("%i = THROW %a")
    inst(new Inst.LandingPad()) shouldEqual functionWith("%i = LANDINGPAD")
    inst(new Inst.ExtractValue(x, 3, a)) shouldEqual functionWith("%i = EXTRACTVALUE <@x 3> %a")
    inst(new Inst.InsertValue(x, 3, a, b)) shouldEqual functionWith("%i = INSERTVALUE <@x 3> %a %b")
    inst(new Inst.ExtractElement(x, y, a, b)) shouldEqual functionWith("%i = EXTRACTELEMENT <@x @y> %a %b")
    inst(new Inst.InsertElement(x, y, a, b, c)) shouldEqual functionWith("%i = INSERTELEMENT <@x @y> %a %b %c")
    inst(new Inst.ShuffleVector(x, y, a, b, c)) shouldEqual functionWith("%i = SHUFFLEVECTOR <@x @y> %a %b %c")
    inst(new Inst.New(x, Some(exc))) shouldEqual functionWith("%i = NEW <@x> EXC(%nor %exc)")
    inst(new Inst.NewHybrid(x, y, a, Some(exc))) shouldEqual functionWith("%i = NEWHYBRID <@x @y> %a EXC(%nor %exc)")
    inst(new Inst.Alloca(x, Some(exc))) shouldEqual functionWith("%i = ALLOCA <@x> EXC(%nor %exc)")
    inst(new Inst.AllocaHybrid(x, y, a, Some(exc))) shouldEqual functionWith("%i = ALLOCAHYBRID <@x @y> %a EXC(%nor %exc)")
    inst(new Inst.GetIRef(x, a)) shouldEqual functionWith("%i = GETIREF <@x> %a")
    inst(new Inst.GetFieldIRef(true, x, 3, a)) shouldEqual functionWith("%i = GETFIELDIREF PTR <@x 3> %a")
    inst(new Inst.GetElemIRef(true, x, y, a, b)) shouldEqual functionWith("%i = GETELEMIREF PTR <@x @y> %a %b")
    inst(new Inst.ShiftIRef(true, x, y, a, b)) shouldEqual functionWith("%i = SHIFTIREF PTR <@x @y> %a %b")
    inst(new Inst.GetFixedPartIRef(true, x, a)) shouldEqual functionWith("%i = GETFIXEDPARTIREF PTR <@x> %a")
    inst(new Inst.GetVarPartIRef(true, x, a)) shouldEqual functionWith("%i = GETVARPARTIREF PTR <@x> %a")
    inst(new Inst.Load(true, MemoryOrder.SeqConsistent, x, a, Some(exc))) shouldEqual
      functionWith("%i = LOAD PTR SEQ_CST <@x> %a EXC(%nor %exc)")
    inst(new Inst.Store(true, MemoryOrder.SeqConsistent, x, a, b, Some(exc))) shouldEqual
      functionWith("%i = STORE PTR SEQ_CST <@x> %a %b EXC(%nor %exc)")
    inst(new Inst.CmpXchg(true, true, MemoryOrder.AcquireRelease, MemoryOrder.Acquire, x, a, b, c, Some(exc))) shouldEqual
      functionWith("%i = CMPXCHG PTR WEAK ACQ_REL ACQUIRE <@x> %a %b %c EXC(%nor %exc)")
    inst(new Inst.AtomicRMW(true, MemoryOrder.SeqConsistent, AtomicRMWOptr.Xor, x, a, b, Some(exc))) shouldEqual
      functionWith("%i = ATOMICRMW PTR SEQ_CST XOR <@x> %a %b EXC(%nor %exc)")
    inst(new Inst.Fence(MemoryOrder.SeqConsistent)) shouldEqual functionWith("%i = FENCE SEQ_CST")
    inst(new Inst.Trap(x, Some(exc), Some(new KeepAliveClause(Seq(a, b))))) shouldEqual
      functionWith("%i = TRAP <@x> EXC(%nor %exc) KEEPALIVE(%a %b)")
    inst(new Inst.WatchPoint(42, x, l1, l2, Some(LabelName("exc")), Some(new KeepAliveClause(Seq(a, b))))) shouldEqual
      functionWith("%i = WATCHPOINT 42 <@x> %l1 %l2 WPEXC(%exc) KEEPALIVE(%a %b)")
    inst(new Inst.CCall(new Flag("DEFAULT"), x, s, f, Seq(a, b, c), new KeepAliveClause(Seq(a, b)))) shouldEqual
      functionWith("%i = CCALL #DEFAULT <@x @s> @f (%a %b %c) KEEPALIVE(%a %b)")
    inst(new Inst.NewStack(s, f, Seq(a, b, c), exc)) shouldEqual
      functionWith("%i = NEWSTACK <@s> @f (%a %b %c) EXC(%nor %exc)")
    inst(new Inst.SwapStack(a,
      CurStackClause.RetWith(x),
      NewStackClause.PassValue(y, b),
      Some(exc),
      Some(new KeepAliveClause(Seq(a, b)))
    )) shouldEqual functionWith(
      "%i = SWAPSTACK %a RET_WITH <@x> PASS_VALUE <@y> %b EXC(%nor %exc) KEEPALIVE(%a %b)")
    inst(new Inst.SwapStack(a,
      CurStackClause.KillOld(),
      NewStackClause.PassVoid(),
      Some(exc),
      Some(new KeepAliveClause(Seq(a, b)))
    )) shouldEqual functionWith(
      "%i = SWAPSTACK %a KILL_OLD PASS_VOID EXC(%nor %exc) KEEPALIVE(%a %b)")
    inst(new Inst.SwapStack(a,
      CurStackClause.KillOld(),
      NewStackClause.ThrowExc(b),
      Some(exc),
      Some(new KeepAliveClause(Seq(a, b)))
    )) shouldEqual functionWith(
      "%i = SWAPSTACK %a KILL_OLD THROW_EXC %b EXC(%nor %exc) KEEPALIVE(%a %b)")
    inst(new Inst.CommInst(GlobalVarName("foo.bar"),
      Some(Seq(new Flag("A"), new Flag("B"))),
      Some(Seq(x, y)),
      Some(Seq(s, FuncSigName("s2"))),
      Some(Seq(a, b)),
      Some(exc),
      Some(new KeepAliveClause(Seq(a, b)))
    )) shouldEqual functionWith(
      "%i = COMMINST @foo.bar [#A #B] <@x @y> <[@s @s2]> (%a %b) EXC(%nor %exc) KEEPALIVE(%a %b)")
  }

  "The written bundle" should "be parseable by Mu's UIR parser" in {
    val builder = new BundleBuilder("test")
    val t = builder.typeDef(TypeCtor.Int(64))
    val s = builder.funcSig(t, Seq(t, t))
    val c = builder.constDef(Const.Int(t, 42L))
    val g = builder.globalCell(t)
    val fDecl = builder.funcDecl(builder.newVarName("f1"), s)
    builder.exposeFunc(builder.newVarName("nf"), fDecl, new Flag("DEFAULT"), c)

    val funcBuilder = builder.newFuncVersion(builder.newVarName("f2"), s, Seq("x", "y"))
    val bb2 = funcBuilder.newLabelName()
    funcBuilder.inst(new Inst.Branch(bb2))
    funcBuilder.startNewBlock(bb2)
    funcBuilder.inst(new Inst.Ret(t, c))

    val ir = builder.build().toString
    
    println(ir)
    
    val muBundle = new UIRTextReader(new IDFactory).read(ir, new uvm.Bundle())
  }
}