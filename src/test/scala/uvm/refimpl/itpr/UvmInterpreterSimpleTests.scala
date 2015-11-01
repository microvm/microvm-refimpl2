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

class UvmInterpreterSimpleTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/simple-tests.uir")

  "Factorial functions" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@test_fac")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val Seq(r1, r2, r3) = ctx.dumpKeepalives(st, 0)

      r1.vb.asInt shouldEqual 3628800
      r2.vb.asInt shouldEqual 3628800
      r3.vb.asInt shouldEqual 3628800

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "Fibonacci functions" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@test_fib")

    val watch = true

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@fibonacci_mat_v1.head.watch" => {
          if (watch) {
            val vhs = ctx.dumpKeepalives(st, 0)
            val vs = vhs.map(_.vb.asInt)
            println("watch " + vs)
          }
          returnFromTrap(st)
        }
        case "@test_fib_v1.entry.checktrap" => {
          val Seq(r1, r2) = ctx.dumpKeepalives(st, 0)

          r1.vb.asInt shouldEqual 55
          r2.vb.asInt shouldEqual 55

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()
  }

  "Coroutine test" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@test_coroutine")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@test_coroutine_v1.body.trap_body" => {
          val Seq(v) = ctx.dumpKeepalives(st, 0)

          println(v.vb.asSInt(64))

          returnFromTrap(st)
        }
        case "@test_coroutine_v1.exit.trap_exit" => {
          val Seq(exc) = ctx.dumpKeepalives(st, 0)

          val hsi = ctx.handleFromGlobal("@StopIteration")
          val hrsi = ctx.load(MemoryOrder.NOT_ATOMIC, hsi)

          exc.vb.asRef shouldEqual hrsi.vb.asRef

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()
  }

  "Multi-threading test" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@test_multithreading")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      val trapName = nameOf(ctx.curInst(st, 0))

      trapName match {
        case "@test_multithreading_v1.getresult.trap_result" => {
          val Seq(v) = ctx.dumpKeepalives(st, 0)

          v.vb.asSInt(64) shouldEqual 4950

          returnFromTrap(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ctx.closeContext()
  }
}