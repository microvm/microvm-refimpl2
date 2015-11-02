package uvm.refimpl.misc

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
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm.refimpl.TrapHandlerResult.Rebind
import uvm.refimpl.HowToResume.ThrowExc

object FuncRedefTests {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

class FuncRedefTests extends UvmBundleTesterBase {
  import FuncRedefTests._

  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    "uvm.refimpl.misc" -> DEBUG,
    "uvm.refimpl.itpr" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir", "tests/uvm-refimpl-test/redef-file1.uir")

  def loadBundleFromFile(ctx: MuCtx, fileName: String): Unit = {
    val r = new FileReader(fileName)
    ctx.loadBundle(r)
    r.close()
  }

  "Function redefinition" should "work" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@main")

    logger.debug("Starting...")

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curFunc(st, 0)) match {
        case "@main" => {
          ctx.nameOf(ctx.curInst(st, 0)) match {
            case "@main.v1.entry.checkpoint1" => {
              val Seq(curMeaning: MuIntValue) = ctx.dumpKeepalives(st, 0)
              ctx.handleToSInt(curMeaning) shouldBe 42
            }
            case "@main.v1.entry.checkpoint2" => {
              val Seq(fox: MuIntValue) = ctx.dumpKeepalives(st, 0)
              ctx.handleToSInt(fox) shouldBe 99
            }
            case "@main.v1.entry.change_meaning" => {
              logger.debug("Changing meaning...")
              loadBundleFromFile(ctx, "tests/uvm-refimpl-test/redef-file3.uir")
            }
            case "@main.v1.entry.checkpoint3" => {
              val Seq(newMeaning: MuIntValue) = ctx.dumpKeepalives(st, 0)
              ctx.handleToSInt(newMeaning) shouldBe 43
            }
          }
        }
        case "@foxsay" => {
          logger.debug("Undefined @foxsay trapped.")
          ctx.curFuncVer(st, 0) shouldBe 0
          ctx.curInst(st, 0) shouldBe 0
          loadBundleFromFile(ctx, "tests/uvm-refimpl-test/redef-file2.uir")
        }
      }
      returnFromTrap(st)
    }

    ctx.closeContext()
  }

  "Exceptions thrown in undefined function frames" should "be rethrown" in {
    val ctx = microVM.newContext()

    val func = ctx.handleFromFunc("@main2")

    logger.debug("Starting2...")

    var excReached = false

    testFunc(ctx, func, Seq()) { (ctx, th, st, wp) =>
      ctx.nameOf(ctx.curFunc(st, 0)) match {
        case "@main2" => {
          ctx.nameOf(ctx.curInst(st, 0)) match {
            case "@main2.v1.nor.trap" => {
              fail("Should not continue normally.")
            }
            case "@main2.v1.exc.trap" => {
              excReached = true
              
              val Seq(e: MuRefValue) = ctx.dumpKeepalives(st, 0)
              val hExc = ctx.handleFromConst("@NULLREF").asInstanceOf[MuRefValue]
              
              val eq = ctx.refEq(e, hExc)
              
              eq shouldBe true
            }
          }
          returnFromTrap(st)
        }
        case "@lonelyfox" => {
          logger.debug("Undefined @lonelyfox trapped.")
          ctx.curFuncVer(st, 0) shouldBe 0
          ctx.curInst(st, 0) shouldBe 0

          val hExc = ctx.handleFromConst("@NULLREF").asInstanceOf[MuRefValue]
          Rebind(st, ThrowExc(hExc))
        }
      }
    }
    
    excReached shouldBe true

    ctx.closeContext()
  }
}