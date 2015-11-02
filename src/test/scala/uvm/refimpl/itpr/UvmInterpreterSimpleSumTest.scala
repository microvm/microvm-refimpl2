package uvm.refimpl.itpr

import org.scalatest._

import ch.qos.logback.classic.Level._
import uvm._
import uvm.refimpl._
import uvm.refimpl.UvmBundleTesterBase
import uvm.refimpl.itpr._
import uvm.ssavariables._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.ssavariables.MemoryOrder._
import uvm.types._

class UvmInterpreterSimpleSumTest extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> INFO)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/simple-sum.uir")

  "Simple sum" should "work" in {
    val ctx = microVM.newContext()

    val from = 1L
    val to = 1000000L
    val expectedSum = (from + to) * (to - from + 1L) / 2L

    val hFrom = ctx.handleFromInt64(from)
    val hTo = ctx.handleFromInt64(to)

    val func = ctx.handleFromFunc("@simplesum")

    var t1: Long = 0L
    var t2: Long = 0L

    testFunc(ctx, func, Seq(hFrom, hTo)) { (ctx, th, st, wp) =>
      nameOf(ctx.curInst(st, 0)) match {
        case "@simplesum_v1.entry.starttrap" => {
          t1 = System.currentTimeMillis()
          returnFromTrap(st)
        }
        case "@simplesum_v1.exit.exittrap" => {
          t2 = System.currentTimeMillis()

          val Seq(sum) = ctx.dumpKeepalives(st, 0)

          sum.vb.asSInt(64) shouldBe expectedSum

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()

    val timeDiff = t2 - t1
    printf("Time: %d ms".format(timeDiff))
  }
}