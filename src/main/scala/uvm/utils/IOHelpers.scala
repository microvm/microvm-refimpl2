package uvm.utils

import java.io.Reader

object IOHelpers {
  def slurp(r: Reader): String = {
    val sb = new StringBuilder()
    val cb = new Array[Char](4096)

    var finished = false
    while (!finished) {
      val actualRead = r.read(cb, 0, 4096)
      if (actualRead > 0) {
        sb.appendAll(cb, 0, actualRead)
      } else {
        finished = true
      }
    }

    sb.toString()
  }
}