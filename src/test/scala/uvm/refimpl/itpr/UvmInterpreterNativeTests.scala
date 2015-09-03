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

  preloadBundles("tests/uvm-refimpl-test/native-tests.uir")

  "The CCALL instruction" should "call the getpid() function" in {
    val ca = microVM.newClientAgent()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("getpid")

    val posix = POSIXFactory.getPOSIX
    val actualPID = posix.getpid

    println("actualPID = %d".format(actualPID))

    val func = ca.putFunction("@getpidtest")

    val a0 = ca.putInt("@i64", funcAddr)

    testFunc(ca, func, Seq(a0)) { (ca, th, st, wp) =>
      val Seq(fp, rv) = ca.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asSInt(32) shouldEqual actualPID

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "The CCALL instruction" should "call the write() function" in {
    val ca = microVM.newClientAgent()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("write")

    val func = ca.putFunction("@writetest")

    val a0 = ca.putInt("@i64", funcAddr)

    testFunc(ca, func, Seq(a0)) { (ca, th, st, wp) =>
      val Seq(fp, rv, buf, bufV0P) = ca.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asSInt(64) shouldEqual 6

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "The CCALL instruction" should "call the memcpy() function" in {
    val ca = microVM.newClientAgent()

    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("memcpy")

    val func = ca.putFunction("@memcpytest")

    val a0 = ca.putInt("@i64", funcAddr)

    testFunc(ca, func, Seq(a0)) { (ca, th, st, wp) =>
      val Seq(fp, rv, ob, b0, b1, b2, b3, b4, b5) = ca.dumpKeepalives(st, 0)

      fp.vb.asPointer shouldEqual funcAddr
      rv.vb.asPointer shouldEqual ob.vb.asPointer

      b0.vb.asSInt(8) shouldEqual 'H'
      b1.vb.asSInt(8) shouldEqual 'e'
      b2.vb.asSInt(8) shouldEqual 'l'
      b3.vb.asSInt(8) shouldEqual 'l'
      b4.vb.asSInt(8) shouldEqual 'o'
      b5.vb.asSInt(8) shouldEqual '\n'

      TrapRebindPassVoid(st)
    }

    ca.close()
  }
}