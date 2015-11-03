package uvm.refimpl

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import uvm.ssavariables.MemoryOrder._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.{ Logger => LLogger }
import ch.qos.logback.classic.Level
import uvm.refimpl.TrapHandlerResult.Rebind
import uvm.refimpl.HowToResume.PassValues

object UvmBundleTesterBase {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))

  implicit class MagicalBox(val vb: ValueBox) extends AnyVal {
    def asInt: BigInt = vb.asInstanceOf[BoxInt].value
    def asSInt(l: Int): BigInt = OpHelper.prepareSigned(vb.asInstanceOf[BoxInt].value, l)
    def asUInt(l: Int): BigInt = OpHelper.prepareUnsigned(vb.asInstanceOf[BoxInt].value, l)
    def asFloat: Float = vb.asInstanceOf[BoxFloat].value
    def asDouble: Double = vb.asInstanceOf[BoxDouble].value
    def asRef: Word = vb.asInstanceOf[BoxRef].objRef
    def asIRef: (Word, Word) = { val b = vb.asInstanceOf[BoxIRef]; (b.objRef, b.offset) }
    def asIRefAddr: Word = { val b = vb.asInstanceOf[BoxIRef]; b.objRef + b.offset }
    def asStruct: Seq[ValueBox] = vb.asInstanceOf[BoxSeq].values
    def asFunc: Option[Function] = vb.asInstanceOf[BoxFunc].func
    def asThread: Option[InterpreterThread] = vb.asInstanceOf[BoxThread].thread
    def asStack: Option[InterpreterStack] = vb.asInstanceOf[BoxStack].stack
    def asTR64Box: BoxTagRef64 = vb.asInstanceOf[BoxTagRef64]
    def asTR64Raw: Long = vb.asInstanceOf[BoxTagRef64].raw
    def asSeq: Seq[ValueBox] = vb.asInstanceOf[BoxSeq].values
    def asVec: Seq[ValueBox] = vb.asInstanceOf[BoxSeq].values
    def asPointer: Word = vb.asInstanceOf[BoxPointer].addr
  }
}

abstract class UvmBundleTesterBase extends FlatSpec with Matchers {
  val ROOT_LOGGER_NAME = org.slf4j.Logger.ROOT_LOGGER_NAME

  def setLogLevels(settings: (String, Level)*): Unit = { // Configure logger
    import org.slf4j.LoggerFactory
    import org.slf4j.{ Logger => SLogger }
    import ch.qos.logback.classic.{ Logger => LLogger, Level }
    import ch.qos.logback.classic.Level._

    def setLevel(name: String, level: Level): Unit = {
      LoggerFactory.getLogger(name).asInstanceOf[LLogger].setLevel(level)
    }

    for ((name, lvl) <- settings) {
      setLevel(name, lvl)
    }
  }

  def makeMicroVM(): MicroVM = new MicroVM()

  val microVM = makeMicroVM()

  implicit def idOf(name: String): Int = microVM.idOf(name)
  implicit def nameOf(id: Int): String = microVM.nameOf(id)

  def preloadBundles(fileNames: String*): Unit = {
    val ctx = microVM.newContext()

    for (fn <- fileNames) {
      val r = new FileReader(fn)
      ctx.loadBundle(r)
      r.close()
    }

    ctx.closeContext()
  }

  type TrapHandlerFunction = (MuCtx, MuThreadRefValue, MuStackRefValue, Int) => TrapHandlerResult

  class MockTrapHandler(thf: TrapHandlerFunction) extends TrapHandler {
    def handleTrap(ctx: MuCtx, thread: MuThreadRefValue, stack: MuStackRefValue, watchPointID: Int): TrapHandlerResult = {
      thf(ctx, thread, stack, watchPointID)
    }
  }

  def testFunc(ctx: MuCtx, func: MuFuncRefValue, args: Seq[MuValue])(handler: TrapHandlerFunction): Unit = {
    microVM.trapManager.trapHandler = new MockTrapHandler(handler)
    val hStack = ctx.newStack(func)
    val hThread = ctx.newThread(hStack, HowToResume.PassValues(args))
    microVM.execute()
  }

  import UvmBundleTesterBase._

  implicit def magicalMuValue(mv: MuValue): MagicalBox = MagicalBox(mv.vb)
  implicit def magicalValueBox(vb: ValueBox): MagicalBox = MagicalBox(vb)

  implicit def richMuCtx(ctx: MuCtx) = RichMuCtx.RichMuCtx(ctx)

  def returnFromTrap(st: MuStackRefValue) = Rebind(st, PassValues(Seq()))
}
