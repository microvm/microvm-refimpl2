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
import uvm.refimpl.nat.NativeLibraryTestHelper

class UvmInterpreterNativeCallbackTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG,
    "uvm.refimpl.nat" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/native-callback-tests.uir")

  val libCallbacktest = NativeLibraryTestHelper.loadTestLibrary("callbacktest")

  "The exposing definition" should "allow a native function to call it" in {
    val ca = microVM.newClientAgent()

    val hCB = ca.putExpFunc("@square.exposed")
    val hCBAddr = ca.toPointer(hCB)
    println("Callback address: 0x%x".format(hCBAddr))
    hCBAddr should not be 0L
    
    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    assert(nativeFuncAddr != 0L)
    
    val hFP = ca.putPointer("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ca.putGlobal("@one_level.global")
    ca.store(MemoryOrder.SEQ_CST, nativeFuncGlobal, hFP)

    val muFunc = ca.putFunction("@one_level_test")

    var callbackCalled: Int = 0

    testFunc(ca, muFunc, Seq()) { (ca, th, st, wp) =>
      ca.nameOf(ca.currentInstruction(st, 0)) match {
        case "@square.v1.trap" => {
          val Seq(value, cok) = ca.dumpKeepalives(st, 0)

          ca.toDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
          })
          
          ca.toInt(cok).toLong shouldBe 42L

          callbackCalled += 1

          TrapRebindPassVoid(st)
        }
        case "@one_level_test.v1.pretrap" => {
          val Seq(fp) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr

          TrapRebindPassVoid(st)
        }
        case "@one_level_test.v1.trap" => {
          val Seq(fp, rv) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr
          ca.toDouble(rv) shouldEqual 25.0

          TrapRebindPassVoid(st)
        }
      }

    }

    ca.close()
  }

}