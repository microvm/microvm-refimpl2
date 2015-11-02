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
import jnr.ffi.LibraryLoader
import uvm.utils.HexDump

class UvmInterpreterNativeTestsExtra extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.nat" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  val fileName = "tests/c-snippets/structtest.so"
  if (!new java.io.File(fileName).isFile()) {
    throw new RuntimeException("Need to compile the structtest.so library. cd into tests/c-snippets and invoke 'make'.")
  }

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/native-tests.uir")

  "The CCALL instruction" should "handle struct parameters in foo" in {
    val ctx = microVM.newContext()

    val lib = Library.openLibrary(fileName, Library.NOW)
    val funcAddr = lib.getSymbolAddress("foo_func")

    val func = ctx.handleFromFunc("@foo_func_test")

    val a0 = ctx.handleFromInt64( funcAddr)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      val Seq(fp, rv, a, b, c, d) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr

      // println("%x".format(a.vb.asUInt(64).toLong))

      a.vb.asUInt(64) shouldEqual 0x55aa55aa55aa55aaL
      b.vb.asUInt(32) shouldEqual 0x5a5a5a5a
      c.vb.asUInt(16) shouldEqual 0xa5a5
      d.vb.asUInt(8) shouldEqual 0x61

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "The CCALL instruction" should "handle struct parameters in bar involving pointers" in {
    val ctx = microVM.newContext()

    val lib = Library.openLibrary(fileName, Library.NOW)
    val funcAddr = lib.getSymbolAddress("bar_func")

    val func = ctx.handleFromFunc("@bar_func_test")

    val a0 = ctx.handleFromInt64( funcAddr)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      val Seq(fp, rv, a, b) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr

      // println("%x".format(a.vb.asUInt(64).toLong))

      a.vb.asPointer shouldEqual 0x123456789abcdef0L
      b.vb.asPointer shouldEqual 0xfedcba9876543210L

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "The CCALL instruction" should "handle struct parameters and return value in baz" in {
    val ctx = microVM.newContext()

    val lib = Library.openLibrary(fileName, Library.NOW)
    val funcAddr = lib.getSymbolAddress("baz_func")

    val func = ctx.handleFromFunc("@baz_func_test")

    val a0 = ctx.handleFromInt64( funcAddr)

    testFunc(ctx, func, Seq(a0)) { (ctx, th, st, wp) =>
      val Seq(fp, rv, a, b, c, pextra, aextra) = ctx.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr

      val Seq(rab, rbb) = rv.vb.asStruct
      val Seq(raxb, rayb) = rab.asStruct

      raxb.asFloat shouldEqual 4.0f
      rayb.asSInt(32) shouldEqual 5
      rbb.asDouble shouldEqual 6.0

      // println("%x".format(a.vb.asUInt(64).toLong))

      a.vb.asFloat shouldEqual 1.0f
      b.vb.asSInt(32) shouldEqual 2
      c.vb.asDouble shouldEqual 3.0
      
      val ptr = pextra.vb.asPointer
      println("ptr is 0x%x".format(ptr))
      
      val (oref, off) = aextra.vb.asIRef
      
      println("oref, off is 0x%x 0x%x".format(oref, off))
      
      print(HexDump.dumpMemory(ptr-32, 16+64))

      val m = microVM.memoryManager.memorySupport.theMemory
      m.getFloat(ptr) shouldEqual 4.0f
      m.getInt(ptr+4) shouldEqual 5
      m.getDouble(ptr+8) shouldEqual 6.0
      
      returnFromTrap(st)
    }

    ctx.closeContext()
  }
}