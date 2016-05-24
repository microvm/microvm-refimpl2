package uvm.refimpl.nat

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.ClosureManager
import com.kenai.jffi.{ Type => JType }

import uvm.FuncSig
import uvm.{ Function => MFunc }
import uvm.ir.textinput.ExtraMatchers
import uvm.refimpl.itpr._
import uvm.types._
import uvm.UvmTestBase

class NativeStackKeeperTest extends UvmTestBase with ExtraMatchers {
  behavior of "NativeStackKeeper"

  val lib = NativeLibraryTestHelper.loadTestLibrary("callbacktest")

  implicit val nch = new NativeCallHelper()

  def autoClose[T](nsk: NativeStackKeeper)(f: => T) = {
    try {
      f
    } finally {
      nsk.close()
    }
  }

  it should "stop the slave thread when closed" in {
    val nsk = new NativeStackKeeper()

    nsk.close()

    nsk.slaveThread.isAlive() shouldBe false
  }

  it should "call non-callback native functions" in {
    val addr = lib.getSymbolAddress("add")

    val nsk = new NativeStackKeeper()

    autoClose(nsk) {

      val i32 = TypeInt(32)
      val sig = FuncSig(Seq(i32, i32), Seq(i32))

      val box1 = BoxInt(3)
      val box2 = BoxInt(4)

      val result = nsk.callNative(sig, addr, Seq(box1, box2))

      result shouldBeA[NativeCallResult.ReturnToMu] { its => 
        its.maybeRvb shouldBe Some(BoxInt(7))
      }
    }
  }

  it should "handle one-level callback" in {
    val addr = lib.getSymbolAddress("one_level")

    addr should not be 0

    val nsk = new NativeStackKeeper()

    autoClose(nsk) {

      val d = TypeDouble()
      val dtdSig = FuncSig(Seq(d), Seq(d))
      val dtd = TypeUFuncPtr(dtdSig)
      val sig = FuncSig(Seq(d, dtd), Seq(d))

      val mockMuCallbackFunc = new MFunc()
      mockMuCallbackFunc.sig = dtdSig

      val mockClosAddr = nch.exposeFuncDynamic(mockMuCallbackFunc, 42L)

      mockClosAddr should not be 0

      val b1 = BoxDouble(3.0)
      val b2 = BoxPointer(mockClosAddr)

      val r1 = nsk.callNative(sig, addr, Seq(b1, b2))

      println("Hello. I received r1")

      r1 shouldBeA[NativeCallResult.CallMu] { its =>
        its.func shouldBe mockMuCallbackFunc
        its.cookie shouldBe 42
        its.args.size shouldBe 1
        its.args(0) shouldBe BoxDouble(3.0)
      }
      
      val r2 = nsk.returnToNative(Some(BoxDouble(9.0)))
      
      println("Hello. I received r2")
      
      r2 shouldBeA[NativeCallResult.CallMu] { its =>
        its.func shouldBe mockMuCallbackFunc
        its.cookie shouldBe 42
        its.args.size shouldBe 1
        its.args(0) shouldBe BoxDouble(4.0)
      }      
      
      val r3 = nsk.returnToNative(Some(BoxDouble(16.0)))
      
      println("Hello. I received r3")
      
      r3 shouldBe NativeCallResult.ReturnToMu(Some(BoxDouble(25.0)))
    }
  }

  "The 'one_level' function" should "work on usual callback closures" in {
    val addr = lib.getSymbolAddress("one_level")

    addr should not be 0

    val nsk = new NativeStackKeeper()

    autoClose(nsk) {

      val d = TypeDouble()
      val dtdSig = FuncSig(Seq(d), Seq(d))
      val dtd = TypeUFuncPtr(dtdSig)
      val sig = FuncSig(Seq(d, dtd), Seq(d))

      val clos = new Closure() {
        def invoke(buf: Buffer): Unit = {
          val value = buf.getDouble(0)
          (value == 3.0 || value == 4.0) shouldBe true
          val rv = value * value
          buf.setDoubleReturn(rv)
        }
      }

      val closHandle = ClosureManager.getInstance.newClosure(clos, JType.DOUBLE, Array(JType.DOUBLE, JType.POINTER), CallingConvention.DEFAULT)

      closHandle.getAddress should not be 0

      val b1 = BoxDouble(3.0)
      val b2 = BoxPointer(closHandle.getAddress)
      val br = BoxDouble(-1.0)

      val result = nsk.callNative(sig, addr, Seq(b1, b2))

      closHandle.dispose()

      result shouldBe NativeCallResult.ReturnToMu(Some(BoxDouble(25.0)))
    }
  }
}