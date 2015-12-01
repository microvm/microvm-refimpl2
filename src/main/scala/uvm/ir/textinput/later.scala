package uvm.ir.textinput

class Later {
  var tooLate = false
  var jobs: List[() => Unit] = Nil
  def apply(job: () => Unit) {
    if (tooLate) {
      job()
    } else {
      jobs = job :: jobs
    }
  }

  def doAll() {
    tooLate = true
    while (!jobs.isEmpty) {
      val job = jobs.head
      jobs = jobs.tail
      job()
    }
  }

  def isEmpty: Boolean = jobs.isEmpty
}

object Later {
  implicit class Laterable[T](val anything: T) extends AnyVal {
    def later(lat: Later)(job: T => Unit): T = {
      lat(() => job(anything))
      anything
    }
  }
}