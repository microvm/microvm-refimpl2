/**
 * This class allows the MicroVM to be configured from a text string. It increases the flexibility of language bindings.
 * Other programming languages can set heap/global sizes and logging via this interface. It is less static than a fixed
 * MicroVM constructor signature, but more options can be added/removed without changing the C binding API.
 */
package uvm.refimpl

import uvm.refimpl.mem.TypeSizes._

object VMConf {
  val DEFAULT_CONF = new VMConf()
  
  val ReComment = """^\s*#.*$""".r
  val ReBlank = """^\s*$""".r
  val ReConf = """^\s*(\w+)=(\w+)\s*$""".r

  def apply(confStr: String): VMConf = {
    var sosSize = DEFAULT_CONF.sosSize
    var losSize = DEFAULT_CONF.losSize
    var globalSize = DEFAULT_CONF.globalSize
    var stackSize = DEFAULT_CONF.stackSize
    var staticCheck = DEFAULT_CONF.staticCheck
    var sourceInfo = DEFAULT_CONF.sourceInfo
    confStr.lines foreach {
      case ReComment() => 
      case ReBlank() =>
      case ReConf(key, value) => {
        key match {
          case "sosSize" => sosSize = value.toLong
          case "losSize" => losSize = value.toLong
          case "globalSize" => globalSize = value.toLong
          case "stackSize" => stackSize = value.toLong
          case "staticCheck" => staticCheck = value.toLowerCase().toBoolean
          case "sourceInfo" => sourceInfo = value.toLowerCase().toBoolean
          case "vmLog" => setLog("uvm", value)
          case "gcLog" => setLog("uvm.refimpl.mem", value)
          case _ => throw new UvmRefImplException("Unrecognized option %s".format(key))
        }
      }
    }
    new VMConf(sosSize, losSize, globalSize, stackSize, staticCheck, sourceInfo)
  }
  
  def setLog(name: String, levelStr: String): Unit = {
    import org.slf4j.LoggerFactory
    import org.slf4j.{ Logger => SLogger }
    import ch.qos.logback.classic.{ Logger => LLogger, Level }
    import ch.qos.logback.classic.Level._

    val level = Level.toLevel(levelStr)

    LoggerFactory.getLogger(name).asInstanceOf[LLogger].setLevel(level)
  }
}

class VMConf(
  val sosSize: Word = MicroVM.DEFAULT_SOS_SIZE,
  val losSize: Word = MicroVM.DEFAULT_LOS_SIZE,
  val globalSize: Word = MicroVM.DEFAULT_GLOBAL_SIZE,
  val stackSize: Word = MicroVM.DEFAULT_STACK_SIZE,
  val staticCheck: Boolean = true,
  val sourceInfo: Boolean = true)

