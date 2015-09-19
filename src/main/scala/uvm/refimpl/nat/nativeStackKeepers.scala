package uvm.refimpl.nat

import uvm.FuncSig
import uvm.refimpl.itpr.ValueBox
import uvm.refimpl.mem.TypeSizes.Word
import uvm.{ Function => MFunc }
import uvm.refimpl.UvmRuntimeException

/**
 * An Agent is something that has a mailbox. It allows Erlang-like usage pattern. But it does not automatically
 * provide a thread.
 */
trait PoorManAgent[T] {
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
  case class CallBack(sig: FuncSig, func: MFunc, args: Seq[ValueBox], retBox: ValueBox) extends NativeCallResult
  case class Return() extends NativeCallResult
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
        receive() match {
          case ToSlave.CallNative(sig, func, args, retBox) => {
            nativeCallHelper.callNative(master, sig, func, args, retBox)
            master.send(NativeCallResult.Return())
          }

          case ToSlave.ReturnToCallBack() => {
            throw new UvmNativeCallException("Attempt to return to callback functions when there is no native function calling back")
          }

          case ToSlave.Stop() => {
            return
          }
        }
      }
    }

    def onCallBack(): Unit = {

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
