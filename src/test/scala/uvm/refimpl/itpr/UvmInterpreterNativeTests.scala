package uvm.refimpl.itpr

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import MemoryOrder._
import AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word
import ch.qos.logback.classic.Level._
import uvm.refimpl.UvmBundleTesterBase
import com.kenai.jffi.Library
import jnr.posix.POSIXFactory

class UvmInterpreterNativeTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/native-tests.uir")

  "The CCALL instruction" should "call the getpid() function" in {
    val ctx = microVM.newContext()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("getpid")

    val posix = POSIXFactory.getPOSIX
    val actualPID = posix.getpid

    println("actualPID = %d".format(actualPID))

    val func = ctx.handleFromFunc("@getpidtest")

    val a0 = ctx.handleFromInt64( funcAddr)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      val Seq(fp, rv) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asSInt(32) shouldEqual actualPID

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "The CCALL instruction" should "call the write() function" in {
    val ctx = microVM.newContext()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("write")

    val func = ctx.handleFromFunc("@writetest")

    val a0 = ctx.handleFromFP("@write_fp", funcAddr)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      val Seq(fp, rv, buf, bufV0P) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asSInt(64) shouldEqual 6

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "The CCALL instruction" should "call the memcpy() function" in {
    val ctx = microVM.newContext()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("memcpy")

    val hgfp = ctx.handleFromGlobal("@FP_MEMCPY")
    val hfp = ctx.handleFromFP("@memcpy_fp", funcAddr)
    ctx.store(MemoryOrder.NOT_ATOMIC, hgfp, hfp)

    val func = ctx.handleFromFunc("@memcpytest")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(fp, rv, ob, b0, b1, b2, b3, b4, b5) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asPointer shouldEqual ob.vb.asPointer

      b0.vb.asSInt(8) shouldEqual 'H'
      b1.vb.asSInt(8) shouldEqual 'e'
      b2.vb.asSInt(8) shouldEqual 'l'
      b3.vb.asSInt(8) shouldEqual 'l'
      b4.vb.asSInt(8) shouldEqual 'o'
      b5.vb.asSInt(8) shouldEqual '\n'

      returnFromTrap(st)
    }

    ctx.closeContext()
  }
}