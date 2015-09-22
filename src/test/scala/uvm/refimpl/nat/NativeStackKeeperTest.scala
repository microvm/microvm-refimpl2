package uvm.refimpl.nat

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.ClosureManager
import com.kenai.jffi.{ Type => JType }
import uvm.{ Function => MFunc }
import uvm.FuncSig
import uvm.refimpl.itpr.BoxDouble
import uvm.refimpl.itpr.BoxInt
import uvm.refimpl.itpr.BoxPointer
import uvm.types.TypeDouble
import uvm.types.TypeFuncPtr
import uvm.types.TypeInt
import uvm.refimpl.MicroVM
import uvm.ir.textinput.ExtraMatchers
import uvm.refimpl.itpr.BoxDouble

class NativeStackKeeperTest extends FlatSpec with Matchers with ExtraMatchers {
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
      val sig = FuncSig(i32, Seq(i32, i32))

      val box1 = BoxInt(3)
      val box2 = BoxInt(4)
      val boxRv = BoxInt(-1)

      val result = nsk.callNative(sig, addr, Seq(box1, box2), boxRv)

      result shouldBe NativeCallResult.Return()

      boxRv.value shouldBe 7
    }
  }

  it should "handle one-level callback" in {
    val addr = lib.getSymbolAddress("one_level")

    addr should not be 0

    val nsk = new NativeStackKeeper()

    autoClose(nsk) {

      val d = TypeDouble()
      val dtdSig = FuncSig(d, Seq(d))
      val dtd = TypeFuncPtr(dtdSig)
      val sig = FuncSig(d, Seq(d, dtd))

      val mockMuCallbackFunc = new MFunc()
      mockMuCallbackFunc.sig = dtdSig

      val mockClosAddr = nch.exposeFunc(mockMuCallbackFunc, 42L, true)

      mockClosAddr should not be 0

      val b1 = BoxDouble(3.0)
      val b2 = BoxPointer(mockClosAddr)
      val br = BoxDouble(-1.0)

      val r1 = nsk.callNative(sig, addr, Seq(b1, b2), br)

      println("Hello. I received r1")

      r1 shouldBeA[NativeCallResult.CallBack] { its =>
        its.func shouldBe mockMuCallbackFunc
        its.cookie shouldBe 42
        its.args.size shouldBe 1
        its.args(0) shouldBeA[BoxDouble] {whose =>
          whose.value shouldBe 3.0
        }
        
        its.retBox shouldBeA[BoxDouble] { b =>
          b.value = 9.0
        }
      }
      
      val r2 = nsk.returnToCallBack()
      println("Hello. I received r2")
      r2 shouldBeA[NativeCallResult.CallBack] { its =>
        its.func shouldBe mockMuCallbackFunc
        its.cookie shouldBe 42
        its.args.size shouldBe 1
        its.args(0) shouldBeA[BoxDouble] {whose =>
          whose.value shouldBe 4.0
        }
        
        its.retBox shouldBeA[BoxDouble] { b =>
          b.value = 16.0
        }
      }      
      val r3 = nsk.returnToCallBack()
      println("Hello. I received r3")
      r3 shouldBe NativeCallResult.Return()

      br.value shouldBe 25.0
    }
  }

  "The 'one_level' function" should "work on usual callback closures" in {
    val addr = lib.getSymbolAddress("one_level")

    addr should not be 0

    val nsk = new NativeStackKeeper()

    autoClose(nsk) {

      val d = TypeDouble()
      val dtdSig = FuncSig(d, Seq(d))
      val dtd = TypeFuncPtr(dtdSig)
      val sig = FuncSig(d, Seq(d, dtd))

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

      nsk.callNative(sig, addr, Seq(b1, b2), br)

      closHandle.dispose()

      br.value shouldBe 25.0
    }
  }
}