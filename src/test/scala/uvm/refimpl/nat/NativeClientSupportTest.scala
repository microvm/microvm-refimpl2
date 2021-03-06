package uvm.refimpl.nat

import scala.reflect.runtime.universe
import ch.qos.logback.classic.Level._
import uvm.refimpl.UvmBundleTesterBase
import jnr.ffi.LibraryLoader
import PlatformConstants._
import NativeSupport._

class NativeClientSupportTest extends UvmBundleTesterBase {
  setLogLevels(
    ROOT_LOGGER_NAME -> INFO,
    //"uvm.refimpl" -> DEBUG,
    "uvm.refimpl.nat" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir",
    "tests/uvm-refimpl-test/native-client-test.uir")

  val fileName = "./tests/c-snippets/ncs_tests.so"
  if (!new java.io.File(fileName).isFile()) {
    throw new RuntimeException("Need to compile the ncs_tests.so library. cd into tests/c-snippets and invoke 'make'.")
  }

  type CBool = Byte

  trait NcsTestsLib {
    def test_basic(mvm: Word, theID: Int, theName: String): CBool
    def test_with_ctx(mvm: Word, theID: Int, theName: String): CBool
    def test_basic_conv(mvm: Word): CBool
    def test_global_vars(mvm: Word, the_plus_one_fp: Word): CBool
    def test_traps(mvm: Word): CBool
    def test_load_bundle(mvm: Word): CBool
    def test_comp_types(mvm: Word): CBool
    def test_memory_ops(mvm: Word): CBool
    def test_osr(mvm: Word): CBool
    def test_tr64(mvm: Word): CBool
    def test_wp(mvm: Word): CBool
    def test_native(mvm: Word): CBool
  }

  val ncs_tests = LibraryLoader.create(classOf[NcsTestsLib]).load(fileName)

  val microVMFuncTableAddr = NativeClientSupport.exposeMicroVM(microVM)

  def assertNativeSuccess(result: CBool): Unit = {
    if (result == 0) {
      fail("Failed in the native program.")
    }
  }

  behavior of "The ClientAccessibleClassExposer"

  it should "be able to access the exposed MicroVM" in {
    val funcTablePtr = jnrMemoryManager.newPointer(microVMFuncTableAddr)
    val header = funcTablePtr.getAddress(0)
    val mvm = NativeClientSupport.microVMs.get(jnrMemoryManager.newPointer(header))

    mvm shouldBe microVM

    val theName = "@i64"
    val theID = microVM.idOf(theName)

    val result = ncs_tests.test_basic(microVMFuncTableAddr, theID, theName)
    assertNativeSuccess(result)
  }

  it should "be able to create MuCtx and use it" in {
    val theName = "@double"
    val theID = microVM.idOf(theName)

    val result = ncs_tests.test_with_ctx(microVMFuncTableAddr, theID, theName)
    assertNativeSuccess(result)
  }

  it should "convert C types to Mu types and back, with the same value" in {
    val result = ncs_tests.test_basic_conv(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "get values from global variables" in {
    val ctx = microVM.newContext()
    val hThePlusOneFP = ctx.handleFromExpose(ctx.idOf("@plus_one_native"))
    val thePlusOneFP = ctx.handleToFP(hThePlusOneFP)
    ctx.closeContext()

    val result = ncs_tests.test_global_vars(microVMFuncTableAddr, thePlusOneFP)
    assertNativeSuccess(result)
  }

  it should "support traps" in {
    val result = ncs_tests.test_traps(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "load bundles and HAIL" in {
    val result = ncs_tests.test_load_bundle(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "handle composite values" in {
    val result = ncs_tests.test_comp_types(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "perform memory operations" in {
    val result = ncs_tests.test_memory_ops(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "perform OSR" in {
    val result = ncs_tests.test_osr(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }

  it should "handle tagref64" in {
    val result = ncs_tests.test_tr64(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }
  
  it should "enable and disable watchPoints" in {
    val result = ncs_tests.test_wp(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }
  
  it should "pin and unpin objects, and expose Mu functions." in {
    val result = ncs_tests.test_native(microVMFuncTableAddr)
    assertNativeSuccess(result)
  }
}