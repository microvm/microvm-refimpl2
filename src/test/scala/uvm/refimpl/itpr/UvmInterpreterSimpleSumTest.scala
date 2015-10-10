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

class UvmInterpreterSimpleSumTest extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> INFO)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/simple-sum.uir")

  "Simple sum" should "work" in {
    val ca = microVM.newClientAgent()

    val from = 1L
    val to = 1000000L
    val expectedSum = (from + to) * (to - from + 1L) / 2L

    val hFrom = ca.putInt("@i64", from)
    val hTo = ca.putInt("@i64", to)

    val func = ca.putFunction("@simplesum")

    var t1: Long = 0L
    var t2: Long = 0L

    testFunc(ca, func, Seq(hFrom, hTo)) { (ca, th, st, wp) =>
      nameOf(ca.currentInstruction(st, 0)) match {
        case "@simplesum_v1.entry.starttrap" => {
          t1 = System.currentTimeMillis()
          TrapRebindPassVoid(st)
        }
        case "@simplesum_v1.exit.exittrap" => {
          t2 = System.currentTimeMillis()

          val Seq(sum) = ca.dumpKeepalives(st, 0)

          sum.vb.asSInt(64) shouldBe expectedSum

          TrapRebindPassVoid(st)
        }
      }
    }

    ca.close()

    val timeDiff = t2 - t1
    printf("Time: %d ms".format(timeDiff))
  }
}