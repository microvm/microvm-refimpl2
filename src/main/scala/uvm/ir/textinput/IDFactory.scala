package uvm.ir.textinput

class IDFactory(val initialID: Int = 65536) {
  private var id: Int = initialID

  def getID(): Int = {
    val myID = id
    id = id + 1
    return myID
  }
}