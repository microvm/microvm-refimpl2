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
import java.io.StringReader

class UvmInterpreterTestBigFunc extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/extra-big-func.uir")

  "The extra big function" should "execute properly" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@big")
    val hParam = ctx.handleFromInt64(0)

    testFunc(ctx, func, Seq(hParam)) { (ctx, th, st, wp) =>
      val Seq(i) = ctx.dumpKeepalives(st, 0)

      ctx.handleToSInt(i.asInstanceOf[MuIntValue]) shouldEqual 200

      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "The Micro VM" should "sustain frequent bundle loading" in {
    val ctx = microVM.newContext()

    for (i <- 0 until 1000) {
      val miniBundle = s".global @h${i} <@i64>"
      ctx.loadBundle(miniBundle)
    }

    val sb = new StringBuilder()
    sb ++= ".funcdef @bigger VERSION @bigger.v1 <@big.sig> {\n"
    sb ++= "     %entry(<@i64> %p):\n"
    for (i <- 0 until 1000) {
      sb ++= s"        [%s${i}] STORE <@i64> @h${i} %p\n"
    }
    sb ++= "        TRAP <>\n"
    sb ++= "        COMMINST @uvm.thread_exit\n"
    sb ++= "}"

    ctx.loadBundle(sb.toString())

    val func = ctx.handleFromFunc("@bigger")
    val hParam = ctx.handleFromInt64(42)

    testFunc(ctx, func, Seq(hParam)) { (ctx, th, st, wp) =>
      val hr = ctx.handleFromGlobal("@h12")
      val hv = ctx.load(MemoryOrder.NOT_ATOMIC, hr)
      val v = ctx.handleToSInt(hv.asInstanceOf[MuIntValue]).toInt

      v shouldEqual 42

      returnFromTrap(st)
    }

    ctx.closeContext()
  }
}