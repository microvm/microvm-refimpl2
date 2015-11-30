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
    "uvm.refimpl.nat" -> DEBUG)

  preloadBundles("tests/uvm-refimpl-test/primitives.uir")

  val fileName = "./tests/c-snippets/ncs_tests.so"
  if (!new java.io.File(fileName).isFile()) {
    throw new RuntimeException("Need to compile the ncs_tests.so library. cd into tests/c-snippets and invoke 'make'.")
  }

  trait NcsTestsLib {
    def test_basic(mvm: Word, theID: Int, theName: String): Int
  }

  val ncs_tests = LibraryLoader.create(classOf[NcsTestsLib]).load(fileName)

  "The ClientAccessibleClassExposer" should "enumerate declared methods" in {
    val cace = new ClientAccessibleClassExposer(NativeMuVM)

    val microVMAddr = NativeClientSupport.exposeMicroVM(microVM)

    val funcTable = cace.makeFuncTable(microVMAddr)

    val funcTablePtr = NativeClientSupport.funcTableToPointer(funcTable)
    val header = funcTablePtr.getAddress(0)
    val mvm = NativeClientSupport.microVMs.get(jnrMemoryManager.newPointer(header))

    mvm shouldBe microVM

    val theName = "@i64"
    val theID = microVM.idOf(theName)

    val result = ncs_tests.test_basic(funcTable, theID, theName)
    if (result == 0) {
      fail("Failed in the native program.")
    }
  }
}