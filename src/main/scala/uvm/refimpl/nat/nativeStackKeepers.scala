package uvm.refimpl.nat

import uvm.FuncSig
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes.Word
import uvm.{ Function => MFunc }
import uvm.refimpl.UvmRuntimeException
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object PoorManAgent {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

/**
 * An Agent is something that has a mailbox. It allows Erlang-like usage pattern. But it does not automatically
 * provide a thread.
 */
trait PoorManAgent[T] {
  import PoorManAgent._
  val inBox = new java.util.concurrent.ArrayBlockingQueue[T](1)

  def send(msg: T) {
    inBox.add(msg)
  }

  def receive[T]() = {
    inBox.take()
  }
}

abstract class NativeCallResult
object NativeCallResult {
  case class CallBack(func: MFunc, cookie: Long, args: Seq[ValueBox], retBox: ValueBox) extends NativeCallResult
  case class Return() extends NativeCallResult
}

object NativeStackKeeper {
  val logger = Logger(LoggerFactory.getLogger(getClass.getName))
}

/**
 * This class (the master) contains a slave: a Java thread which contains many native frames.
 * <p>
 * A NativeStackKeeper is allocated for each Mu stack. All native frames are kept on the slave.
 * Calling native function is done by sending the slave a message so the slave calls the native
 * function on behalf of the master, i.e. the interpreter thread.
 * <p>
 * The slave calls the native function and may have two results. It either returns directly, or
 * calls back to Java (Scala) one or more times before returning, i.e. it eventually returns.
 * <ol>
 * <li>If the native function returns, the slave sends the Return message to the master.
 * <li>If the native function calls back, the slave sends the CallBack message to the caller and
 * wait for the master to supply the return value of the call-back function.
 * </ol>
 * When the Mu stack is killed, the master asks the slave to stop by sending the Stop message.
 *
 * this
 * thread sends back a message to the interpreter thread * while keeping the native frame on itself, "freezing" the native frame until the interpreter thread returns.
 * This allows the interpreter thread to swap between multiple Mu stacks (including native frames).
 */
class NativeStackKeeper(implicit nativeCallHelper: NativeCallHelper) extends PoorManAgent[NativeCallResult] {
  import NativeStackKeeper._

  abstract class ToSlave
  object ToSlave {
    case class CallNative(sig: FuncSig, func: Word, args: Seq[ValueBox], retBox: ValueBox) extends ToSlave
    case class ReturnToCallBack() extends ToSlave
    case class Stop() extends ToSlave
  }

  private val master = this

  class Slave extends Runnable with PoorManAgent[ToSlave] {

    def run(): Unit = {
      while (true) {
        val received = try {
          receive()
        } catch {
          case e: InterruptedException =>
            logger.debug("Native stack keeper slave thread interrupted. No native frames. Stopping slave...")
            return
        }
        received match {
          case ToSlave.CallNative(sig, func, args, retBox) => {
            nativeCallHelper.callNative(master, sig, func, args, retBox)
            master.send(NativeCallResult.Return())
          }

          case ToSlave.ReturnToCallBack() => {
            throw new UvmNativeCallException("Attempt to return to native function, but no native function called back before")
          }

          case ToSlave.Stop() => {
            logger.debug("Received Stop msg. Stopping slave...")
            return
          }
        }
      }
    }

    def onCallBack(func: MFunc, cookie: Long, args: Seq[ValueBox], retBox: ValueBox): Unit = {
      logger.debug("sending master the CallBack message...")
      master.send(NativeCallResult.CallBack(func, cookie, args, retBox))
      logger.debug("msg sent. Waiting for master's reply...")
      try {
        while (true) {
          val received = try {
            receive()
          } catch {
            case e: InterruptedException =>
              logger.debug("Native stack keeper slave thread interrupted while there are native frames alive. " +
                "This may happen when the micro VM itself is killed but the stack is still alive. " +
                "Prepare for undefined behaviours in native frames (or JVM frames if the native calls back again).")
              throw e
          }
          received match {
            case ToSlave.CallNative(sig, func, args, retBox) => {
              nativeCallHelper.callNative(master, sig, func, args, retBox)
              master.send(NativeCallResult.Return())
            }

            case ToSlave.ReturnToCallBack() => {
              return
            }

            case ToSlave.Stop() => {
              val msg = "Attempt to kill the native stack, but there are still native frames alive. " +
                "This has implementation-defined behaviour. Although not forbidden, it is almost always dangerous."
              logger.debug(msg)
              throw new UvmNativeCallException(msg)
            }
          }
        }
      } catch {
        case e: Exception =>
          logger.debug("Exception occured in the slave thread when there are native threads alive. " +
            "Prepare for undefined behaviours in native frames (or JVM frames if the native calls back again).")
          throw e
      }

      logger.debug("returning...")
    }
  }

  val slave = new Slave()
  val slaveThread = new Thread(slave)
  slaveThread.start()

  def callNative(sig: FuncSig, func: Word, args: Seq[ValueBox], retBox: ValueBox): NativeCallResult = {
    slave.send(ToSlave.CallNative(sig, func, args, retBox))
    val result = receive()
    result
  }

  def returnToCallBack(): NativeCallResult = {
    slave.send(ToSlave.ReturnToCallBack())
    val result = receive()
    result
  }

  def close(): Unit = {
    slave.send(ToSlave.Stop())
    slaveThread.join()
  }
}

/**
 * Thrown when an error happens which involves calling native functions.
 */
class UvmNativeCallException(message: String = null, cause: Throwable = null) extends UvmRuntimeException(message, cause)
