package uvm.refimpl.itpr
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm._
import uvm.comminsts._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables._
import uvm.types._
import uvm.refimpl.nat.NativeCallResult
/**
 * A part of the InterpreterThread that interprets common instructions
 */
trait CommInstExecutor extends InterpreterActions with ObjectPinner with IRBuilderCommInstExecutor {
  import InterpreterThread.logger

  implicit protected def mutator: Mutator
  implicit protected def memorySupport: MemorySupport

  override def interpretCurrentCommonInstruction(): Unit = {
    assert(curInst.isInstanceOf[InstCommInst])
    val InstCommInst(ci, flagList, typeList, sigList, argList, excClause, keepalives) = curInst
    
    if (ci.name.get.startsWith("@uvm.irbuilder")) {
      return interpretCurrentIRBuilderCommonInstruction()
    }

    ci.name.get match {
      // Thread and stack operations
      case "@uvm.new_stack" => {
        val Seq(sig) = sigList
        val Seq(func) = argList
        val funcVal = func.asFunc.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to create new thread for NULL Mu function.")
        }

        val sta = microVM.threadStackManager.newStack(funcVal, mutator)
        resultBox(0).asStack = Some(sta)
        continueNormally()
      }

      case "@uvm.kill_stack" => {
        val Seq(s) = argList
        val sta = s.asStack.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to kill NULL stack.")
        }
        sta.kill()
        continueNormally()
      }

      case "@uvm.thread_exit" => {
        threadExit()
      }

      case "@uvm.current_stack" => {
        results(0).asStack = Some(curStack)
        continueNormally()
      }

      case "@uvm.set_threadlocal" => {
        val Seq(tl) = argList
        threadLocal copyFrom tl
        continueNormally()
      }

      case "@uvm.get_threadlocal" => {
        results(0) copyFrom threadLocal
        continueNormally()
      }

      // 64-bit Tagged Reference

      case "@uvm.tr64.is_fp" => {
        val Seq(tr) = argList
        results(0).asBoolean = OpHelper.tr64IsFP(tr.asTR64Raw)
        continueNormally()
      }

      case "@uvm.tr64.is_int" => {
        val Seq(tr) = argList
        results(0).asBoolean = OpHelper.tr64IsInt(tr.asTR64Raw)
        continueNormally()
      }

      case "@uvm.tr64.is_ref" => {
        val Seq(tr) = argList
        results(0).asBoolean = OpHelper.tr64IsRef(tr.asTR64Raw)
        continueNormally()
      }

      case "@uvm.tr64.from_fp" => {
        val Seq(v) = argList
        results(0).asTR64Raw = OpHelper.fpToTr64(v.asDouble)
        continueNormally()
      }

      case "@uvm.tr64.from_int" => {
        val Seq(v) = argList
        results(0).asTR64Raw = OpHelper.intToTr64(v.getUInt(52).longValue)
        continueNormally()
      }

      case "@uvm.tr64.from_ref" => {
        val Seq(ref, tag) = argList
        val vRef = ref.asRef
        val vTag = tag.getSInt(6).longValue
        resultBox(0).asTR64Raw = OpHelper.refToTr64(vRef, vTag)
        continueNormally()
      }

      case "@uvm.tr64.to_fp" => {
        val Seq(tr) = argList
        val raw = tr.asTR64Raw
        if (OpHelper.tr64IsFP(raw)) {
          val result = OpHelper.tr64ToFP(raw)
          results(0).asDouble = result
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract double from a tagref64 which is not holding a double")
        }
      }

      case "@uvm.tr64.to_int" => {
        val Seq(tr) = argList
        val raw = tr.asTR64Raw
        if (OpHelper.tr64IsInt(raw)) {
          val result = OpHelper.tr64ToInt(raw)
          results(0).setInt(result, 52)
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract int from a tagref64 which is not holding a int")
        }
      }

      case "@uvm.tr64.to_ref" => {
        val Seq(tr) = argList
        val raw = tr.asTR64Raw
        if (OpHelper.tr64IsRef(raw)) {
          val result = OpHelper.tr64ToRef(raw)
          results(0).asRef = result
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract ref from a tagref64 which is not holding a ref")
        }
      }

      case "@uvm.tr64.to_tag" => {
        val Seq(tr) = argList
        val raw = tr.asTR64Raw
        if (OpHelper.tr64IsRef(raw)) {
          val result = OpHelper.tr64ToTag(raw)
          results(0).setInt(result, 6)
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract tag from a tagref64 which is not holding a ref")
        }
      }

      case "@uvm.futex.wait" => {
        val Seq(ty: TypeInt) = typeList
        val Seq(loc, v) = argList

        val len = ty.length
        val (objRef, offset) = loc.asIRef
        val locWord = objRef + offset
        val exp = v.asIntRaw

        val equal = MemoryOperations.cmpInt(len, locWord, exp)

        if (equal) {
          microVM.threadStackManager.futexManager.futexWaitNoCheck(objRef, offset, curThread, None)
          logger.debug(ctx + "Waiting in the futex waiting queue.")
        } else {
          logger.debug(ctx + "Memory location does not contain expected value. Don't wait.")
          futexReturn(-1)
        }
      }

      case "@uvm.futex.wait_timeout" => {
        val Seq(ty: TypeInt) = typeList
        val Seq(loc, v, timeout) = argList

        val len = ty.length
        val (objRef, offset) = loc.asIRef
        val locWord = objRef + offset
        val toVal = timeout.asSInt64.longValue
        val exp = v.asIntRaw

        if (toVal < 0L) throw new UvmRefImplException(ctx + "This refimpl treats timeout as signed due to restriction of Java.")

        val equal = MemoryOperations.cmpInt(len, locWord, exp)

        if (equal) {
          microVM.threadStackManager.futexManager.futexWaitNoCheck(objRef, offset, curThread, Some(toVal))
          logger.debug(ctx + "Waiting in the futex waiting queue.")
        } else {
          logger.debug(ctx + "Memory location does not contain expected value. Don't wait.")
          futexReturn(-1)
        }
      }

      case "@uvm.futex.wake" => {
        val Seq(ty: TypeInt) = typeList
        val Seq(loc, nthread) = argList

        val len = ty.length
        val (objRef, offset) = loc.asIRef
        val locWord = objRef + offset
        val nth = nthread.asSInt32.intValue

        if (nth < 0) throw new UvmRuntimeException(ctx + "nthread must not be negative")

        val nWoken = microVM.threadStackManager.futexManager.futexWake(objRef, offset, nth)
        futexReturn(nWoken)
      }

      case "@uvm.futex.cmp_requeue" => {
        val Seq(ty) = typeList
        val Seq(locSrc, locDst, expected, nthread) = argList

        val len = ty.asInstanceOf[TypeInt].length
        val (objRefSrc, offsetSrc) = locSrc.asIRef
        val (objRefDst, offsetDst) = locDst.asIRef
        val exp = expected.asIntRaw
        val nth = nthread.asSInt32.intValue

        if (nth < 0) throw new UvmRuntimeException(ctx + "nthread must not be negative")

        val equal = MemoryOperations.cmpInt(len, objRefSrc + offsetSrc, exp)

        if (equal) {
          val nWoken = microVM.threadStackManager.futexManager.futexRequeue(objRefSrc, offsetSrc, objRefDst, offsetDst, nth)
          futexReturn(nWoken)
        } else {
          futexReturn(-1)
        }
      }

      case "@uvm.kill_dependency" => {
        val Seq(v) = argList
        val vBox = boxOf(v)
        resultBox(0).copyFrom(vBox)
        continueNormally()
      }

      case "@uvm.native.pin" => {
        val Seq(ty) = typeList
        val Seq(r) = argList

        val (addr, offset) = ty match {
          case TypeRef(_)  => (r.asRef, 0L)
          case TypeIRef(_) => r.asIRef
        }

        pin(addr)

        resultBox(0).asInstanceOf[BoxPointer].addr = addr + offset
        continueNormally()
      }

      case "@uvm.native.unpin" => {
        val Seq(ty) = typeList
        val Seq(r) = argList

        val addr = ty match {
          case TypeRef(_)  => r.asRef
          case TypeIRef(_) => r.asIRef._1
        }

        unpin(addr)

        continueNormally()
      }

      case "@uvm.native.expose" => {
        val Seq(callConv) = flagList
        val Seq(sig) = sigList
        val Seq(func, cookie) = argList

        val f = func.asFunc.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to expose NULL Mu function")
        }

        val c = boxOf(cookie).asSInt64.longValue

        val addr = microVM.nativeCallHelper.exposeFuncDynamic(f, c)

        resultBox(0).asInstanceOf[BoxPointer].addr = addr

        continueNormally()
      }

      case "@uvm.native.unexpose" => {
        val Seq(callConv) = flagList
        val Seq(addr) = argList

        val a = addr.asPtr

        microVM.nativeCallHelper.unexposeFunc(a)

        continueNormally()
      }

      case "@uvm.native.get_cookie" => {
        val cookie = topDefMu.cookie
        results(0).asInt64 = cookie
        continueNormally()
      }

      case "@uvm.meta.id_of" => {
        val Seq(name) = argList
        val nameStr = MemoryOperations.bytesToStr(name.asRef)
        val theID = microVM.idOf(nameStr)

        results(0).asInt32 = theID

        continueNormally()
      }

      case "@uvm.meta.name_of" => {
        val Seq(theID) = argList
        val idInt = theID.asSInt32.toInt
        val name = microVM.nameOf(idInt)
        val bytesRef = MemoryOperations.strToBytes(name)

        results(0).asRef = bytesRef

        continueNormally()
      }

      case "@uvm.meta.load_bundle" => {
        val Seq(bundle) = argList
        val bundleStr = MemoryOperations.bytesToStr(bundle.asRef)

        MetaOperations.loadBundle(bundleStr)

        continueNormally()
      }

      case "@uvm.meta.load_hail" => {
        val Seq(hailScript) = argList
        val hailStr = MemoryOperations.bytesToStr(hailScript.asRef)

        MetaOperations.loadHail(hailStr)

        continueNormally()
      }

      case "@uvm.meta.cur_func" => {
        val Seq(stack) = argList
        ???
      }

      case "@uvm.meta.cur_func_ver"       => ???
      case "@uvm.meta.cur_inst"           => ???
      case "@uvm.meta.dump_keepalives"    => ???

      case "@uvm.meta.pop_frame"          => ???
      case "@uvm.meta.push_frame"         => ???

      case "@uvm.meta.enable_watchPoint"  => ???
      case "@uvm.meta.disable_watchPoint" => ???

      case "@uvm.meta.set_trap_handler"   => ???

      // Insert more CommInsts here.

      case ciName => {
        throw new UvmRefImplException("Unimplemented common instruction %s".format(ciName))
      }

    }

  }
}