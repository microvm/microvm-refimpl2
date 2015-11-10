package uvm.refimpl.hail

import java.io.FileReader
import uvm.refimpl.MuCtx
import uvm.refimpl.UvmBundleTesterBase

class UvmHailTesterBase extends UvmBundleTesterBase {
  def loadHailFromFile(ctx: MuCtx, fileName: String): Unit = {
    val r = new FileReader(fileName)
    ctx.loadHail(r)
    r.close()
  }
}