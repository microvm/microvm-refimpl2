package uvm

import org.slf4j.LoggerFactory
import org.slf4j.{ Logger => SLogger }
import ch.qos.logback.classic.{ Logger => LLogger, Level }
import ch.qos.logback.classic.Level._

object LogSetter {
  def setLevel(name: String, level: Level): Unit = {
    LoggerFactory.getLogger(name).asInstanceOf[LLogger].setLevel(level)
  }

  val isTravis = {
    System.getenv("TRAVIS") == "true"
  }
}

trait LogSetter {
  import LogSetter._

  val ROOT_LOGGER_NAME = org.slf4j.Logger.ROOT_LOGGER_NAME
  
  if (isTravis) { // Travis does not like logging, so just heighten the log level whenever we run tests.
    setLevel(ROOT_LOGGER_NAME, WARN)
  }

  def setLogLevels(settings: (String, Level)*): Unit = if (isTravis) {
    // Travis does not like logging, so just make it a no-op.
  } else { // Configure logger
    for ((name, lvl) <- settings) {
      setLevel(name, lvl)
    }
  }

}