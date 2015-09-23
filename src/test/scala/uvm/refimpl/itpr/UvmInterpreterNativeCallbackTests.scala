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
    ca.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

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

    callbackCalled shouldBe 2

    ca.close()
  }

  "The @uvm.native.expose instruction" should "allow a native function to call it" in {
    val ca = microVM.newClientAgent()

    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    assert(nativeFuncAddr != 0L)

    val hFP = ca.putPointer("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ca.putGlobal("@one_level.global")
    ca.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

    val muFunc = ca.putFunction("@one_level_test2")

    var callbackCalled: Int = 0

    testFunc(ca, muFunc, Seq()) { (ca, th, st, wp) =>
      ca.nameOf(ca.currentInstruction(st, 0)) match {
        case "@square.v1.trap" => {
          val Seq(value, cok) = ca.dumpKeepalives(st, 0)

          ca.toDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
            case 2 => 3.0
            case 3 => 4.0
          })

          ca.toInt(cok).toLong shouldBe (callbackCalled match {
            case 0 => 84L
            case 1 => 84L
            case 2 => 126L
            case 3 => 126L
          })

          callbackCalled += 1

          TrapRebindPassVoid(st)
        }
        case "@one_level_test2.v1.pretrap" => {
          val Seq(fp, cb1, cb2) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr

          val cb1Ptr = ca.toPointer(cb1)
          val cb2Ptr = ca.toPointer(cb2)
          cb1Ptr should not equal cb2Ptr

          val cb1Rec = microVM.nativeCallHelper.exposedFuncs(cb1Ptr)
          val cb2Rec = microVM.nativeCallHelper.exposedFuncs(cb2Ptr)

          cb1Rec.muFunc shouldBe microVM.globalBundle.funcNs("@square")
          cb2Rec.muFunc shouldBe microVM.globalBundle.funcNs("@square")

          cb1Rec.cookie shouldBe 84L
          cb2Rec.cookie shouldBe 126L

          TrapRebindPassVoid(st)
        }
        case "@one_level_test2.v1.trap" => {
          val Seq(fp, rv1, rv2, cb1, cb2) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr
          ca.toDouble(rv1) shouldEqual 25.0
          ca.toDouble(rv2) shouldEqual 25.0

          val cb1Ptr = ca.toPointer(cb1)
          val cb2Ptr = ca.toPointer(cb2)

          microVM.nativeCallHelper.exposedFuncs should not contain cb1Ptr
          microVM.nativeCallHelper.exposedFuncs should not contain cb2Ptr

          TrapRebindPassVoid(st)
        }
      }
    }

    callbackCalled shouldBe 4

    ca.close()
  }

  "The expose and unexpose API calls" should "allow a native function to call it" in {
    val ca = microVM.newClientAgent()

    val hCBF = ca.putFunction("@square")
    val hCok = ca.putInt("@i64", 168L)
    val hCB = ca.expose(hCBF, Flag("#DEFAULT"), hCok)
    val hCBAddr = ca.toPointer(hCB)
    println("Callback address: 0x%x".format(hCBAddr))
    hCBAddr should not be 0L

    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    assert(nativeFuncAddr != 0L)

    val hFP = ca.putPointer("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ca.putGlobal("@one_level.global")
    ca.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

    val muFunc = ca.putFunction("@one_level_test3")

    var callbackCalled: Int = 0

    testFunc(ca, muFunc, Seq(hCB)) { (ca, th, st, wp) =>
      ca.nameOf(ca.currentInstruction(st, 0)) match {
        case "@square.v1.trap" => {
          val Seq(value, cok) = ca.dumpKeepalives(st, 0)

          ca.toDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
          })

          ca.toInt(cok).toLong shouldBe 168L

          callbackCalled += 1

          TrapRebindPassVoid(st)
        }
        case "@one_level_test3.v1.pretrap" => {
          val Seq(fp, cb) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr
          ca.toPointer(cb) shouldEqual hCBAddr

          TrapRebindPassVoid(st)
        }
        case "@one_level_test3.v1.trap" => {
          val Seq(fp, rv) = ca.dumpKeepalives(st, 0)

          ca.toPointer(fp) shouldEqual nativeFuncAddr
          ca.toDouble(rv) shouldEqual 25.0

          TrapRebindPassVoid(st)
        }
      }

    }

    callbackCalled shouldBe 2

    ca.unexpose(Flag("#DEFAULT"), hCB)
    microVM.nativeCallHelper.exposedFuncs should not contain hCBAddr

    ca.close()
  }
}