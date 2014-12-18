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

class UvmInterpreterSimpleTests extends FlatSpec with Matchers {

  { // Configure logger
    import org.slf4j.LoggerFactory
    import org.slf4j.{ Logger => SLogger }
    import ch.qos.logback.classic.{ Logger => LLogger, Level }
    import ch.qos.logback.classic.Level._

    def setLevel(name: String, level: Level): Unit = {
      LoggerFactory.getLogger(name).asInstanceOf[LLogger].setLevel(level)
    }

    setLevel(SLogger.ROOT_LOGGER_NAME, INFO)
    setLevel("uvm.refimpl.itpr", DEBUG)
  }

  val microVM = new MicroVM();

  implicit def idOf(name: String): Int = microVM.globalBundle.allNs(name).id
  implicit def nameOf(id: Int): String = microVM.globalBundle.allNs(id).name.get

  {
    val ca = microVM.newClientAgent()

    val r = new FileReader("tests/uvm-refimpl-test/primitives.uir")
    ca.loadBundle(r)
    r.close()

    val r2 = new FileReader("tests/uvm-refimpl-test/simple-tests.uir")
    ca.loadBundle(r2)
    r2.close()

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
    val hStack = ca.newStack(func, args)
    val hThread = ca.newThread(hStack)
    microVM.threadStackManager.joinAll()
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
    def asVec: Seq[ValueBox] = vb.asInstanceOf[BoxVector].values
  }

  "Factorial functions" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@test_fac")

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val Seq(r1, r2, r3) = ca.dumpKeepalives(st, 0)

      r1.vb.asInt shouldEqual 3628800
      r2.vb.asInt shouldEqual 3628800
      r3.vb.asInt shouldEqual 3628800

      TrapRebindPassVoid(st)
    }

    ca.close()
  }

  "Fibonacci functions" should "work" in {
    val ca = microVM.newClientAgent()

    val func = ca.putFunction("@test_fib")

    val watch = true

    testFunc(ca, func, Seq()) { (ca, th, st, wp) =>
      val trapName = nameOf(ca.currentInstruction(st, 0))

      trapName match {
        case "@fibonacci_mat_v1.watch" => {
          if (watch) {
            val vhs = ca.dumpKeepalives(st, 0)
            val vs = vhs.map(_.vb.asInt)
            println("watch " + vs)
          }
          TrapRebindPassVoid(st)
        }
        case "@test_fib_v1.checktrap" => {
          val Seq(r1, r2) = ca.dumpKeepalives(st, 0)

          r1.vb.asInt shouldEqual 55
          r2.vb.asInt shouldEqual 55

          TrapRebindPassVoid(st)
        }
        case _ => fail("Should not hit " + trapName)
      }
    }

    ca.close()
  }
}