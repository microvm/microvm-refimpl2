package uvm.utils

class IDFactory(initialID: Int) {
  private var id: Int = initialID

  def getID(): Int = {
    val myID = id
    id = id + 1
    return myID
  }
}