package uvm.refimpl.mem

import uvm.refimpl.mem.TypeSizes.Word
import Space._

object Space {
  private var spaces: List[Space] = Nil

  private def addSpace(space: Space) {
    spaces = space :: spaces
  }

  def getSpaceForAddress(addr: Word): Option[Space] = {
    spaces.find(_.isInSpace(addr))
  }
}

class Space(val name: String, val begin: Word, val extend: Word) {
  addSpace(this)

  def isInSpace(addr: Word): Boolean = begin <= addr && addr < begin + extend
}
