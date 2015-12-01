package uvm.refimpl.nat

import com.kenai.jffi.Library

object NativeLibraryTestHelper {
  def loadTestLibrary(name: String): Library = {
    val relPath = s"tests/c-snippets/${name}.so"
    
    if (!new java.io.File(relPath).isFile()) {
      throw new RuntimeException(s"Need to compile the ${name}.so library. cd into tests/c-snippets and invoke 'make'.")
    }

    Library.openLibrary(relPath, Library.NOW)
  }
}