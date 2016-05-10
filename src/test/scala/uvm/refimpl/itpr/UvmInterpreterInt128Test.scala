package uvm.refimpl.itpr

import org.scalatest._

import ch.qos.logback.classic.Level._
import uvm._
import uvm.refimpl._
import uvm.refimpl.HowToResume.PassValues
import uvm.refimpl.TrapHandlerResult.Rebind
import uvm.refimpl.itpr._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.MemoryOrder._

class UvmInterpreterInt128Test extends UvmBundleTesterBase {

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl.mem.simpleimmix.SimpleImmixCollector$" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  override def makeMicroVM = MicroVM()

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/int128-test.uir")
    
  "The constant pool" should "contain appropriate constant values" in {
    val ctx = microVM.newContext()
    
    def constInt(id: Int): MuIntValue = ctx.handleFromConst(id).asInstanceOf[MuIntValue]
    def valueOfConstInt(id: Int): BigInt = ctx.handleToUInt(constInt(id))

    valueOfConstInt("@I128_0") shouldEqual 0
    valueOfConstInt("@I128_1") shouldEqual 1
    valueOfConstInt("@I128_big") shouldEqual theBig

    ctx.closeContext()
  }
    
  val mask128 = (BigInt(1)<<128) - 1
  val theBig = BigInt("fedcba9876543210123456789abcdef0", 16)
  val big0 = BigInt("fedcba98765432100123456789abcdef", 16)
  val big1 = BigInt("123456789abcdef0fedcba9876543210", 16)
  val big0n = big0 | ~mask128

  "Binary  operations" should "work on int<128>" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@binops128")

    val arg0 = ctx.handleFromInt(big0, 128)
    val arg1 = ctx.handleFromInt(big1, 128)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)
      val Seq(add, sub, mul, udiv, sdiv, urem, srem, shl, lshr, ashr, and, or, xor) = kas

      def valueOfInt(h: MuValue): BigInt = ctx.handleToInt(h.asInstanceOf[MuIntValue], false)
      
      def to128(bi: BigInt): BigInt = bi & mask128

      valueOfInt(add) shouldEqual to128(big0+big1)
      valueOfInt(sub) shouldEqual to128(big0-big1)
      valueOfInt(mul) shouldEqual to128(big0*big1)
      valueOfInt(udiv) shouldEqual to128(big0/big1)
      valueOfInt(sdiv) shouldEqual to128(big0n/big1)
      valueOfInt(urem) shouldEqual to128(big0%big1)
      valueOfInt(srem) shouldEqual to128(big0n%big1)
      valueOfInt(shl) shouldEqual to128(big0<<(big1 % 128).toInt)
      valueOfInt(lshr) shouldEqual to128(big0>>(big1 % 128).toInt)
      valueOfInt(ashr) shouldEqual to128(big0n>>(big1 % 128).toInt)
      valueOfInt(and) shouldEqual to128(big0 & big1)
      valueOfInt(or) shouldEqual to128(big0 | big1)
      valueOfInt(xor) shouldEqual to128(big0 ^ big1)
      
      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "Comparing operations" should "work on int<128>" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@cmp128")

    val arg0 = ctx.handleFromInt(big0, 128)
    val arg1 = ctx.handleFromInt(big1, 128)

    testFunc(ctx, func, Seq(arg0, arg1)) { (ctx, th, st, wp) =>
      val kas = ctx.dumpKeepalives(st, 0)
      val Seq(eq, ne, ult, ule, ugt, uge, slt, sle, sgt, sge) = kas

      def valueOfBool(h: MuValue): Boolean = ctx.handleToInt(h.asInstanceOf[MuIntValue], false) != 0
      
      valueOfBool(eq) shouldEqual (big0 == big1)
      valueOfBool(ne) shouldEqual (big0 != big1)
      valueOfBool(ult) shouldEqual (big0 < big1)
      valueOfBool(ule) shouldEqual (big0 <= big1)
      valueOfBool(ugt) shouldEqual (big0 > big1)
      valueOfBool(uge) shouldEqual (big0 >= big1)
      valueOfBool(slt) shouldEqual (big0n < big1)
      valueOfBool(sle) shouldEqual (big0n <= big1)
      valueOfBool(sgt) shouldEqual (big0n > big1)
      valueOfBool(sge) shouldEqual (big0n >= big1)

      Rebind(st, PassValues(Seq()))
    }

    ctx.closeContext()
  }

  "LOAD and STORE" should "work with int<128>" in {
    val ctx = microVM.newContext()
    val func = ctx.handleFromFunc("@memAccessing128")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(l0, l1) = ctx.dumpKeepalives(st, 0)

      def valueOfInt(h: MuValue): BigInt = ctx.handleToInt(h.asInstanceOf[MuIntValue], false)
      
      valueOfInt(l0) shouldEqual 0
      valueOfInt(l1) shouldEqual theBig

      Rebind(st, PassValues(Seq()))
    }
    ctx.closeContext()
  }
  
  "Object layout of int<128>" should "be as defined" in {
    val ctx = microVM.newContext()
    
    val hobj = ctx.newFixed("@sbig")
    val hir = ctx.getIRef(hobj)
    val hf0 = ctx.getFieldIRef(hir, 0)
    val hf1 = ctx.getFieldIRef(hir, 1)
    val hf2 = ctx.getFieldIRef(hir, 2)
    
    val a0 = hf0.vb.asIRefAddr 
    val a1 = hf1.vb.asIRefAddr 
    val a2 = hf2.vb.asIRefAddr 
    
    (a1-a0) shouldBe 16
    (a2-a0) shouldBe 32
    
    val hbig0 = ctx.handleFromInt(big0, 128)
    val hsmall0 = ctx.handleFromInt(0xff, 8)
    
    ctx.store(NOT_ATOMIC, hf1, hbig0)
    ctx.store(NOT_ATOMIC, hf0, hsmall0)
    ctx.store(NOT_ATOMIC, hf2, hsmall0)
    
    val hbig1 = ctx.load(NOT_ATOMIC, hf1).asInstanceOf[MuIntValue]
    val hbig1v = ctx.handleToUInt(hbig1)
    
    hbig1v shouldEqual big0

    ctx.closeContext()
  }
}