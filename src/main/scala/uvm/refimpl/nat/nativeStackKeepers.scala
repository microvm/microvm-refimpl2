package uvm.refimpl.nat

import uvm.FuncSig
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes.Word
import uvm.{ Function => MFunc }
import uvm.refimpl.UvmRuntimeException
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.annotation.tailrec

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
  /** Native calls back to Mu. */
  case class CallMu(func: MFunc, cookie: Long, args: Seq[ValueBox]) extends NativeCallResult
  /** Native returns to Mu. maybeRvb holds the return value. */
  case class ReturnToMu(maybeRvb: Option[ValueBox]) extends NativeCallResult
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
    /** Mu wants to call a native function. */
    case class CallNative(sig: FuncSig, func: Word, args: Seq[ValueBox]) extends ToSlave
    /** Mu wants to return to a native function. maybeRvb holds the return value. */
    case class ReturnToNative(maybeRvb: Option[ValueBox]) extends ToSlave
    /** Mu wants the slave to stop. */
    case class Stop() extends ToSlave
  }

  private val master = this

  class Slave extends Runnable with PoorManAgent[ToSlave] {

    def run(): Unit = {
      @tailrec
      def receiving(): Unit = {
        val received = try {
          receive()
        } catch {
          case e: InterruptedException =>
            logger.debug("Native stack keeper slave thread interrupted. No native frames. Stopping slave...")
            return
        }
        received match {
          case ToSlave.CallNative(sig, func, args) => {
            val maybeRvb = nativeCallHelper.callNative(master, sig, func, args)
            master.send(NativeCallResult.ReturnToMu(maybeRvb))
            receiving()
          }

          case ToSlave.ReturnToNative(maybeRvb) => {
            throw new UvmNativeCallException("Attempt to return to native function, but no native function called back before")
          }

          case ToSlave.Stop() => {
            logger.debug("Received Stop msg. Stopping slave...")
            return
          }
        }
      }

      receiving()
    }

    def onCallBack(func: MFunc, cookie: Long, args: Seq[ValueBox]): Option[ValueBox] = {
      logger.debug("sending master the CallBack message...")
      master.send(NativeCallResult.CallMu(func, cookie, args))
      logger.debug("msg sent. Waiting for master's reply...")
      try {
        @tailrec
        def receiving(): Option[ValueBox] = {
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
            case ToSlave.CallNative(sig, func, args) => {
              val maybeRvb = nativeCallHelper.callNative(master, sig, func, args)
              master.send(NativeCallResult.ReturnToMu(maybeRvb))
              receiving()
            }

            case ToSlave.ReturnToNative(maybeRvb) => {
              maybeRvb
            }

            case ToSlave.Stop() => {
              val msg = "Attempt to kill the native stack, but there are still native frames alive. " +
                "This has implementation-defined behaviour. Although not forbidden, it is almost always dangerous."
              logger.debug(msg)
              throw new UvmNativeCallException(msg)
            }
          }

        }
        receiving()
      } catch {
        case e: Throwable =>
          logger.debug("Exception occured in the slave thread when there are native threads alive. " +
            "Prepare for undefined behaviours in native frames (or JVM frames if the native calls back again).", e)
          throw e
      }
    }
  }

  val slave = new Slave()
  val slaveThread = new Thread(slave)
  slaveThread.start()

  def callNative(sig: FuncSig, func: Word, args: Seq[ValueBox]): NativeCallResult = {
    slave.send(ToSlave.CallNative(sig, func, args))
    val result = receive()
    result
  }

  def returnToNative(maybeRvBox: Option[ValueBox]): NativeCallResult = {
    slave.send(ToSlave.ReturnToNative(maybeRvBox))
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
