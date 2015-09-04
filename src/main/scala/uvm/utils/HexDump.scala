package uvm.utils

import jnr.ffi.Pointer
import jnr.ffi.Runtime
import uvm.refimpl.mem.TypeSizes._
import scala.annotation.elidable
import scala.annotation.elidable.ASSERTION
import java.nio.ByteBuffer

/**
 * Show binary data in hexadecimal bytes notations.
 */
object HexDump {
  val mem = Pointer.wrap(Runtime.getSystemRuntime, 0L)

  val LINE_SIZE = 16
  
  def dumpByteBuffer(buf: ByteBuffer): String = dumpByteBuffer(buf, 0, buf.limit())
  
  def dumpByteBuffer(buf: ByteBuffer, begin: Int, size: Int): String = {
    val hd = new HexDump(begin)
    for (addr <- begin until (begin + size)) {
      val b = buf.get(addr)
      hd.addByte(b)
    }
    hd.finish()
  }

  def dumpMemory(begin: Long, size: Long): String = {
    val hd = new HexDump(begin)
    for (addr <- begin until (begin + size)) {
      val b = mem.getByte(addr)
      hd.addByte(b)
    }
    hd.finish()
  }
}

class HexDump(beginAddr: Long) {
  import HexDump._

  val buf = new Array[Byte](16)
  var curLineAddr: Long = alignDown(beginAddr, 16)
  var curLineSize: Int = (beginAddr - curLineAddr).toInt
  var linePreSkip: Int = curLineSize
  var linePostSkip: Int = 0
  val sb = new StringBuilder()

  def addByte(b: Byte) {
    buf(curLineSize) = b
    curLineSize += 1
    if (curLineSize == LINE_SIZE) {
      processLine()
    }
  }

  def processLine() {
    assert(curLineSize == LINE_SIZE, "Line not full: current: %d, expected: %d".format(curLineSize, LINE_SIZE))

    sb ++= "%16x".format(curLineAddr)
    sb ++= ":   "

    for ((b, i) <- buf.zipWithIndex) {
      val padding = i match {
        case 0 => ""
        case 8 => "  "
        case _ => " "
      }
      sb ++= padding

      val num = if (i < linePreSkip || i + linePostSkip >= LINE_SIZE) "  "
      else "%02x".format(b)

      sb ++= num
    }

    sb ++= "    "

    sb ++= "|"
    for ((b, i) <- buf.zipWithIndex) {
      val chr = if (i < linePreSkip || i + linePostSkip >= LINE_SIZE) " "
      else if (32 <= b && b <= 126)  b.toChar.toString() else "."

      sb ++= chr

    }
    sb ++= "|"

    sb ++= "\n"

    curLineSize = 0
    linePreSkip = 0
    linePostSkip = 0
    curLineAddr += LINE_SIZE
  }

  def finish(): String = {
    if (curLineSize > 0) {
      if (curLineSize < 16) {
        linePostSkip = (alignUp(curLineSize, 16) - curLineSize).toInt
        curLineSize = 16
      }
      processLine()

    }
    sb.toString()
  }
}