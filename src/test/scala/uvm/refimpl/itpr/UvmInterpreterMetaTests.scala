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
import uvm.refimpl.TrapHandlerResult.{ ThreadExit, Rebind }
import uvm.refimpl.HowToResume.{ PassValues, ThrowExc }

class UvmInterpreterMetaTests extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/meta-tests.uir")

  "id_of and name_of" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@id_name")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curInst(st, 0)) match {
        case "@id_name.v1.entry.trap" => {
          val Seq(id: MuIntValue, name: MuRefValue) = ctx.dumpKeepalives(st, 0)
          
          ctx.handleToSInt(id) shouldBe idOf("@foo")

          returnFromTrap(st)
        }
      }
    }

    ctx.closeContext()
  }
}