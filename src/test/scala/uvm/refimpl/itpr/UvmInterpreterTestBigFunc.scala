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
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@big")
    val hParam = ca.putInt("@i64", 0)

    testFunc(ca, func, Seq(hParam)) { (ca, th, st, wp) =>
      val Seq(i) = ca.dumpKeepalives(st, 0)

      ca.toInt(i, true) shouldEqual 200

      TrapRebindPassVoid(st)
    }

    ca.close()
  }
  
  "The Micro VM" should "sustain frequent bundle loading" in {
    val ca = microVM.newClientAgent()
    
    for(i <- 0 until 100) {
      val miniBundle = s".global @h${i} <@i64>"
      ca.loadBundle(miniBundle)
    }
    
    val sb = new StringBuilder()
    sb ++= ".funcdef @bigger VERSION @bigger.v1 <@big.sig> (%p) {\n"
    sb ++= "     %entry:\n"
    for(i <- 0 until 100) {
      sb ++= s"        %r${i} = STORE <@i64> @h${i} %p\n"
    }
    sb ++= "        TRAP <@void>\n"
    sb ++= "        COMMINST @uvm.thread_exit\n"
    sb ++= "}"
 
    ca.loadBundle(sb.toString())
    
    val func = ca.putFunction("@bigger")
    val hParam = ca.putInt("@i64", 42)

    testFunc(ca, func, Seq(hParam)) { (ca, th, st, wp) =>
      val hr = ca.putGlobal("@h12")
      val hv = ca.load(MemoryOrder.NOT_ATOMIC, hr)
      val v = ca.toInt(hv, true).toInt
      
      v shouldEqual 42

      TrapRebindPassVoid(st)
    }

    
    ca.close()
  }
}