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

object UvmBundleTesterBase {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
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
    val ca = microVM.newClientAgent()

    for (fn <- fileNames) {
      val r = new FileReader(fn)
      ca.loadBundle(r)
      r.close()
    }

    ca.close()
  }

  type TrapHandlerFunction = (ClientAgent, Handle, Handle, Int) => TrapHandlerResult

  class MockTrapHandler(thf: TrapHandlerFunction) extends TrapHandler {
    def handleTrap(ca: ClientAgent, thread: Handle, stack: Handle, watchPointID: Int): TrapHandlerResult = {
      thf(ca, thread, stack, watchPointID)
    }
  }

  def testFunc(ca: ClientAgent, func: Handle, args: Seq[Handle])(handler: TrapHandlerFunction): Unit = {
    microVM.trapManager.trapHandler = new MockTrapHandler(handler)
    val hStack = ca.newStack(func)
    val hThread = ca.newThread(hStack)
    microVM.execute()
  }

  implicit class MagicalBox(vb: ValueBox) {
    def asInt: BigInt = vb.asInstanceOf[BoxInt].value
    def asSInt(l: Int): BigInt = OpHelper.prepareSigned(vb.asInstanceOf[BoxInt].value, l)
    def asUInt(l: Int): BigInt = OpHelper.prepareUnsigned(vb.asInstanceOf[BoxInt].value, l)
    def asFloat: Float = vb.asInstanceOf[BoxFloat].value
    def asDouble: Double = vb.asInstanceOf[BoxDouble].value
    def asRef: Word = vb.asInstanceOf[BoxRef].objRef
    def asIRef: (Word, Word) = { val b = vb.asInstanceOf[BoxIRef]; (b.objRef, b.offset) }
    def asIRefAddr: Word = { val b = vb.asInstanceOf[BoxIRef]; b.objRef + b.offset }
    def asStruct: Seq[ValueBox] = vb.asInstanceOf[BoxStruct].values
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