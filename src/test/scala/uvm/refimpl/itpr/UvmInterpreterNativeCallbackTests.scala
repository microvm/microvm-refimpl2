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

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/native-callback-tests.uir")

  val libCallbacktest = NativeLibraryTestHelper.loadTestLibrary("callbacktest")

  "The exposing definition" should "allow a native function to call it" in {
    val ctx = microVM.newContext()

    val hCB = ctx.handleFromExpose("@square.exposed")
    val hCBAddr = ctx.handleToFP(hCB)
    println("Callback address: 0x%x".format(hCBAddr))
    hCBAddr should not be 0L

    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    nativeFuncAddr should not be 0L

    val hFP = ctx.handleFromFP("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ctx.handleFromGlobal("@one_level.global")
    ctx.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

    val muFunc = ctx.handleFromFunc("@one_level_test")

    var callbackCalled: Int = 0

    testFunc(ctx, muFunc, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@square.v1.entry.trap" => {
          val Seq(value: MuDoubleValue, cok: MuIntValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
          })

          ctx.handleToUInt(cok).toLong shouldBe 42L

          callbackCalled += 1

          returnFromTrap(st)
        }
        case "@one_level_test.v1.entry.pretrap" => {
          val Seq(fp: MuUFPValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr

          returnFromTrap(st)
        }
        case "@one_level_test.v1.entry.trap" => {
          val Seq(fp: MuUFPValue, rv: MuDoubleValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr
          ctx.handleToDouble(rv) shouldEqual 25.0

          returnFromTrap(st)
        }
      }

    }

    callbackCalled shouldBe 2

    ctx.closeContext()
  }

  "The @uvm.native.expose instruction" should "allow a native function to call it" in {
    val ctx = microVM.newContext()

    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    nativeFuncAddr should not be 0L

    val hFP = ctx.handleFromFP("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ctx.handleFromGlobal("@one_level.global")
    ctx.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

    val muFunc = ctx.handleFromFunc("@one_level_test2")

    var callbackCalled: Int = 0

    testFunc(ctx, muFunc, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@square.v1.entry.trap" => {
          val Seq(value: MuDoubleValue, cok: MuIntValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
            case 2 => 3.0
            case 3 => 4.0
          })

          ctx.handleToUInt(cok.asInstanceOf[MuIntValue]).toLong shouldBe (callbackCalled match {
            case 0 => 84L
            case 1 => 84L
            case 2 => 126L
            case 3 => 126L
          })

          callbackCalled += 1

          returnFromTrap(st)
        }
        case "@one_level_test2.v1.entry.pretrap" => {
          val Seq(fp: MuUFPValue, cb1: MuUFPValue, cb2: MuUFPValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr

          val cb1Ptr = ctx.handleToFP(cb1)
          val cb2Ptr = ctx.handleToFP(cb2)
          cb1Ptr should not equal cb2Ptr

          val cb1Rec = microVM.nativeCallHelper.addrToRec(cb1Ptr)
          val cb2Rec = microVM.nativeCallHelper.addrToRec(cb2Ptr)

          cb1Rec.muFunc shouldBe microVM.globalBundle.funcNs("@square")
          cb2Rec.muFunc shouldBe microVM.globalBundle.funcNs("@square")

          cb1Rec.cookie shouldBe 84L
          cb2Rec.cookie shouldBe 126L

          returnFromTrap(st)
        }
        case "@one_level_test2.v1.entry.trap" => {
          val Seq(fp: MuUFPValue, rv1: MuDoubleValue, rv2: MuDoubleValue, cb1: MuUFPValue, cb2: MuUFPValue) =
            ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr
          ctx.handleToDouble(rv1) shouldEqual 25.0
          ctx.handleToDouble(rv2) shouldEqual 25.0

          val cb1Ptr = ctx.handleToFP(cb1)
          val cb2Ptr = ctx.handleToFP(cb2)

          microVM.nativeCallHelper.addrToRec should not contain cb1Ptr
          microVM.nativeCallHelper.addrToRec should not contain cb2Ptr

          returnFromTrap(st)
        }
      }
    }

    callbackCalled shouldBe 4

    ctx.closeContext()
  }

  "The expose and unexpose API calls" should "allow a native function to call it" in {
    val ctx = microVM.newContext()

    val hCBF = ctx.handleFromFunc("@square")
    val hCok = ctx.handleFromInt64(168L)
    val hCB = ctx.expose(hCBF, Flag("#DEFAULT"), hCok)
    val hCBAddr = ctx.handleToFP(hCB)
    println("Callback address: 0x%x".format(hCBAddr))
    hCBAddr should not be 0L

    val nativeFuncAddr = libCallbacktest.getSymbolAddress("one_level")
    nativeFuncAddr should not be 0L

    val hFP = ctx.handleFromFP("@one_level.fp", nativeFuncAddr)

    val nativeFuncGlobal = ctx.handleFromGlobal("@one_level.global")
    ctx.store(MemoryOrder.NOT_ATOMIC, nativeFuncGlobal, hFP)

    val muFunc = ctx.handleFromFunc("@one_level_test3")

    var callbackCalled: Int = 0

    testFunc(ctx, muFunc, Seq(hCB)) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@square.v1.entry.trap" => {
          val Seq(value: MuDoubleValue, cok) = ctx.dumpKeepalives(st, 0)

          ctx.handleToDouble(value) shouldBe (callbackCalled match {
            case 0 => 3.0
            case 1 => 4.0
          })

          ctx.handleToUInt(cok.asInstanceOf[MuIntValue]).toLong shouldBe 168L

          callbackCalled += 1

          returnFromTrap(st)
        }
        case "@one_level_test3.v1.entry.pretrap" => {
          val Seq(fp: MuUFPValue, cb: MuUFPValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr
          ctx.handleToFP(cb) shouldEqual hCBAddr

          returnFromTrap(st)
        }
        case "@one_level_test3.v1.entry.trap" => {
          val Seq(fp: MuUFPValue, rv: MuDoubleValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToFP(fp) shouldEqual nativeFuncAddr
          ctx.handleToDouble(rv) shouldEqual 25.0

          returnFromTrap(st)
        }
      }

    }

    callbackCalled shouldBe 2

    ctx.unexpose(Flag("#DEFAULT"), hCB)
    microVM.nativeCallHelper.addrToRec should not contain hCBAddr

    ctx.closeContext()
  }

  "The exposing definition" should "allow multi-level nested/recursive Mu-native calls" in {
    val ctx = microVM.newContext()

    val hPongFP = ctx.handleFromExpose("@pong.exposed")
    val pongAddr = ctx.handleToFP(hPongFP)
    println("Exposed Mu func (pong) addr: 0x%x".format(pongAddr))
    pongAddr should not be 0L

    val pingAddr = libCallbacktest.getSymbolAddress("ping")
    println("Native func (ping) addr: 0x%x".format(pingAddr))
    pingAddr should not be 0L

    val hPingFP = ctx.handleFromFP("@PingPong.fp", pingAddr)

    val hPongTest = ctx.handleFromFunc("@pong_test")
    val initialV = ctx.handleFromInt32(10)

    var pongCalled: Int = 0

    testFunc(ctx, hPongTest, Seq(initialV, hPingFP)) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@pong.v1.entry.entrytrap" => {
          val Seq(v: MuIntValue, peer: MuUFPValue) = ctx.dumpKeepalives(st, 0)
          val vInt = ctx.handleToUInt(v).toInt
          val peerAddr = ctx.handleToFP(peer)

          pongCalled += 1

          printf("[Mu:@pong]: Pong called. v=%d, peer=0x%x\n", vInt, peerAddr)

          vInt shouldBe (pongCalled match {
            case 1 => 10
            case 2 => 8
            case 3 => 6
            case 4 => 4
            case 5 => 2
            case 6 => 0
          })

          ctx.handleToFP(peer) shouldEqual pingAddr

          returnFromTrap(st)
        }
        case "@pong.v1.not_zero.resptrap" => {
          val Seq(v: MuIntValue, resp: MuIntValue) = ctx.dumpKeepalives(st, 0)
          val vInt = ctx.handleToUInt(v).toInt
          val respInt = ctx.handleToUInt(resp).toInt

          respInt shouldBe (vInt match {
            case 10 => 362880
            case 8  => 5040
            case 6  => 120
            case 4  => 6
            case 2  => 1
          })

          returnFromTrap(st)
        }
        case "@pong_test.v1.entry.entrytrap" => {
          val Seq(v: MuIntValue, peer: MuUFPValue) = ctx.dumpKeepalives(st, 0)

          ctx.handleToUInt(v).toInt shouldEqual 10
          ctx.handleToFP(peer) shouldEqual pingAddr

          returnFromTrap(st)
        }
        case "@pong_test.v1.entry.exittrap" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)

          ctx.handleToUInt(rv.asInstanceOf[MuIntValue]).toInt shouldEqual 3628800

          returnFromTrap(st)
        }
      }
    }

    pongCalled shouldBe 6

    ctx.closeContext()
  }

  "The Mu micro VM" should "be able to swap between stacks with native frames" in {
    val ctx = microVM.newContext()

    val giverAddr = libCallbacktest.getSymbolAddress("giver")
    println("Native func (giver) addr: 0x%x".format(giverAddr))
    giverAddr should not be 0L

    val takerAddr = libCallbacktest.getSymbolAddress("taker")
    println("Native func (taker) addr: 0x%x".format(takerAddr))
    takerAddr should not be 0L

    val hGiverFP = ctx.handleFromFP("@giver.fp", giverAddr)
    val hGiverGlobal = ctx.handleFromGlobal("@giver.global")
    ctx.store(MemoryOrder.NOT_ATOMIC, hGiverGlobal, hGiverFP)

    val hTakerFP = ctx.handleFromFP("@taker.fp", takerAddr)
    val hTakerGlobal = ctx.handleFromGlobal("@taker.global")
    ctx.store(MemoryOrder.NOT_ATOMIC, hTakerGlobal, hTakerFP)

    val muFunc = ctx.handleFromFunc("@native_sched_test")

    testFunc(ctx, muFunc, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@native_sched_test.v1.body.inspect" => {
          val Seq(rv) = ctx.dumpKeepalives(st, 0)
          val rvInt = ctx.handleToUInt(rv.asInstanceOf[MuIntValue])
          printf("@native_sched_test: rv = %d\n", rvInt)

          returnFromTrap(st)
        }
        case "@take_from_mu.v1.entry.inspect" => {
          val Seq(ss) = ctx.dumpKeepalives(st, 0)
          val rvInt = ctx.handleToUInt(ss.asInstanceOf[MuIntValue])
          printf("@take_from_mu: ss = %d\n", rvInt)

          returnFromTrap(st)
        }
        case "@native_sched_test.v1.exit.exittrap" => {
          try {
            val Seq(rvTaker) = ctx.dumpKeepalives(st, 0)

            ctx.handleToUInt(rvTaker.asInstanceOf[MuIntValue]).toInt shouldEqual 3628800

            returnFromTrap(st)
          } catch {
            case e: Exception =>
              e.printStackTrace()
              // Have to immediately stop the process because those native frames will not return.
              System.exit(1)
              throw new Exception("Unreachable")
          }
        }
      }
    }

    ctx.closeContext()
  }
}