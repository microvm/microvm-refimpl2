package uvm.refimpl.hail

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import MemoryOrder._
import AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.refimpl.TrapHandlerResult.{ ThreadExit, Rebind }
import uvm.refimpl.HowToResume.{ PassValues, ThrowExc }
import ch.qos.logback.classic.Level._
import uvm.refimpl.RichMuCtx._
import com.kenai.jffi.Library

class UvmHailHelloWorldTest extends UvmHailTesterBase {
  setLogLevels(ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.hail" -> DEBUG //"uvm.refimpl.mem" -> DEBUG,
    //"uvm.refimpl.itpr" -> DEBUG
    )

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-hail-test/hello-world-test.uir")

  behavior of "The HAIL reader and the unsafe native interface"

  it should "create string constants and print it using the 'write' syscall" in {
    val mc = microVM.newContext()

    loadHailFromFile(mc, "tests/uvm-hail-test/hello-world-test.hail")

    // Resolve the function address for 'write'
    val lib = Library.getDefault()
    val funcAddr = lib.getSymbolAddress("write")
    val hFuncAddr = mc.handleFromFP("@write.fp", funcAddr)
    val hWrite = mc.handleFromGlobal("@write")
    mc.store(NOT_ATOMIC, hWrite, hFuncAddr)

    val func = mc.handleFromFunc("@main")

    testFunc(mc, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(rv: MuIntValue) = ctx.dumpKeepalives(st, 0)

      val rvInt = ctx.handleToSInt(rv).toLong
      
      printf("[trap handler] %d bytes written\n", rvInt)
      
      rvInt shouldBe 13

      returnFromTrap(st)
    }

    mc.closeContext()
  }
}