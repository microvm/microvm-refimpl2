package uvm.refimpl.mem

import uvm.refimpl.mem.TypeSizes.Word
import Space._

object Space {
  private var spaces: List[Space] = Nil

  private def addSpace(space: Space) {
    spaces = space :: spaces
  }

  def getSpaceForAddress(addr: Long): Option[Space] = {
    spaces.find(_.isInSpace(addr))
  }
}

class Space(val name: String, val begin: Long, val extend: Long) {
  addSpace(this)

  def isInSpace(addr: Word): Boolean = begin <= addr && addr < begin + extend
}
