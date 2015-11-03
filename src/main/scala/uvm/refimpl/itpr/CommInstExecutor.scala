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
trait CommInstExecutor extends InterpreterActions with ObjectPinner {
  import InterpreterThread.logger

  implicit protected def mutator: Mutator
  implicit protected def memorySupport: MemorySupport

  override def interpretCurrentCommonInstruction(): Unit = {
    assert(curInst.isInstanceOf[InstCommInst])
    val InstCommInst(ci, flagList, typeList, sigList, argList, excClause, keepAlives) = curInst

    ci.name.get match {
      // Thread and stack operations
      case "@uvm.new_stack" => {
        val Seq(sig) = sigList
        val Seq(func) = argList
        val funcVal = boxOf(func).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to create new thread for NULL Mu function.")
        }

        val sta = microVM.threadStackManager.newStack(funcVal, mutator)
        resultBox(0).asInstanceOf[BoxStack].stack = Some(sta)
        continueNormally()
      }

      case "@uvm.kill_stack" => {
        val Seq(s) = argList
        val sta = boxOf(s).asInstanceOf[BoxStack].stack.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to kill NULL stack.")
        }
        sta.kill()
        continueNormally()
      }

      case "@uvm.thread_exit" => {
        threadExit()
      }

      case "@uvm.current_stack" => {
        val bi = resultBox(0)
        bi.asInstanceOf[BoxStack].stack = stack
        continueNormally()
      }

      // 64-bit Tagged Reference

      case "@uvm.tr64.is_fp" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        val result = OpHelper.tr64IsFp(raw)
        writeBooleanResult(result, resultBox(0))
        continueNormally()
      }

      case "@uvm.tr64.is_int" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        val result = OpHelper.tr64IsInt(raw)
        writeBooleanResult(result, resultBox(0))
        continueNormally()
      }

      case "@uvm.tr64.is_ref" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        val result = OpHelper.tr64IsRef(raw)
        writeBooleanResult(result, resultBox(0))
        continueNormally()
      }

      case "@uvm.tr64.from_fp" => {
        val Seq(v) = argList
        val vFP = boxOf(v).asInstanceOf[BoxDouble].value
        val raw = OpHelper.fpToTr64(vFP)
        resultBox(0).asInstanceOf[BoxTagRef64].raw = raw
        continueNormally()
      }

      case "@uvm.tr64.from_int" => {
        val Seq(v) = argList
        val vInt = OpHelper.prepareUnsigned(boxOf(v).asInstanceOf[BoxInt].value, 52)
        val raw = OpHelper.intToTr64(vInt.longValue())
        resultBox(0).asInstanceOf[BoxTagRef64].raw = raw
        continueNormally()
      }

      case "@uvm.tr64.from_ref" => {
        val Seq(ref, tag) = argList
        val vRef = boxOf(ref).asInstanceOf[BoxRef].objRef
        val vTag = OpHelper.prepareUnsigned(boxOf(tag).asInstanceOf[BoxInt].value, 6)
        val raw = OpHelper.refToTr64(vRef, vTag.longValue())
        resultBox(0).asInstanceOf[BoxTagRef64].raw = raw
        continueNormally()
      }

      case "@uvm.tr64.to_fp" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        if (OpHelper.tr64IsFp(raw)) {
          val result = OpHelper.tr64ToFp(raw)
          resultBox(0).asInstanceOf[BoxDouble].value = result
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract double from a tagref64 which is not holding a double")
        }
      }

      case "@uvm.tr64.to_int" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        if (OpHelper.tr64IsInt(raw)) {
          val result = OpHelper.tr64ToInt(raw)
          resultBox(0).asInstanceOf[BoxInt].value = OpHelper.unprepare(result, 52)
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract int from a tagref64 which is not holding a int")
        }
      }

      case "@uvm.tr64.to_ref" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        if (OpHelper.tr64IsRef(raw)) {
          val result = OpHelper.tr64ToRef(raw)
          resultBox(0).asInstanceOf[BoxRef].objRef = result
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract ref from a tagref64 which is not holding a ref")
        }
      }

      case "@uvm.tr64.to_tag" => {
        val Seq(tr) = argList
        val raw = boxOf(tr).asInstanceOf[BoxTagRef64].raw
        if (OpHelper.tr64IsRef(raw)) {
          val result = OpHelper.tr64ToTag(raw)
          resultBox(0).asInstanceOf[BoxInt].value = OpHelper.unprepare(result, 6)
          continueNormally()
        } else {
          throw new UvmRuntimeException(ctx + "Attempt to extract tag from a tagref64 which is not holding a ref")
        }
      }

      case "@uvm.futex.wait" => {
        val Seq(ty) = typeList
        val Seq(loc, v) = argList

        val len = ty.asInstanceOf[TypeInt].length
        val bLoc = boxOf(loc).asInstanceOf[BoxIRef]
        val objRef = bLoc.objRef
        val offset = bLoc.offset
        val locWord = objRef + offset
        val bv = boxOf(v).asInstanceOf[BoxInt]

        val equal = MemoryOperations.cmpInt(len, locWord, bv)

        if (equal) {
          microVM.threadStackManager.futexManager.futexWaitNoCheck(objRef, offset, curThread, None)
          logger.debug(ctx + "Waiting in the futex waiting queue.")
        } else {
          logger.debug(ctx + "Memory location does not contain expected value. Don't wait.")
          futexReturn(-1)
        }
      }

      case "@uvm.futex.wait_timeout" => {
        val Seq(ty) = typeList
        val Seq(loc, v, timeout) = argList

        val len = ty.asInstanceOf[TypeInt].length
        val bLoc = boxOf(loc).asInstanceOf[BoxIRef]
        val objRef = bLoc.objRef
        val offset = bLoc.offset
        val locWord = objRef + offset
        val bv = boxOf(v).asInstanceOf[BoxInt]
        val bto = boxOf(timeout).asInstanceOf[BoxInt]
        val toVal = OpHelper.prepareSigned(bto.value, 64).longValue

        if (toVal < 0L) throw new UvmRefImplException(ctx + "This refimpl treats timeout as signed due to restriction of Java.")

        val equal = MemoryOperations.cmpInt(len, locWord, bv)

        if (equal) {
          microVM.threadStackManager.futexManager.futexWaitNoCheck(objRef, offset, curThread, Some(toVal))
          logger.debug(ctx + "Waiting in the futex waiting queue.")
        } else {
          logger.debug(ctx + "Memory location does not contain expected value. Don't wait.")
          futexReturn(-1)
        }
      }

      case "@uvm.futex.wake" => {
        val Seq(ty) = typeList
        val Seq(loc, nthread) = argList

        val len = ty.asInstanceOf[TypeInt].length
        val bLoc = boxOf(loc).asInstanceOf[BoxIRef]
        val objRef = bLoc.objRef
        val offset = bLoc.offset
        val locWord = objRef + offset
        val nth = OpHelper.prepareSigned(boxOf(nthread).asInstanceOf[BoxInt].value, 32).intValue

        if (nth < 0) throw new UvmRuntimeException(ctx + "nthread must not be negative")

        val nWoken = microVM.threadStackManager.futexManager.futexWake(objRef, offset, nth)
        futexReturn(nWoken)
      }

      case "@uvm.futex.cmp_requeue" => {
        val Seq(ty) = typeList
        val Seq(locSrc, locDst, expected, nthread) = argList

        val len = ty.asInstanceOf[TypeInt].length
        val (objRefSrc, offsetSrc) = boxOf(locSrc).asInstanceOf[BoxIRef].oo
        val (objRefDst, offsetDst) = boxOf(locDst).asInstanceOf[BoxIRef].oo
        val bExp = boxOf(expected).asInstanceOf[BoxInt]
        val nth = OpHelper.prepareSigned(boxOf(nthread).asInstanceOf[BoxInt].value, 32).intValue

        if (nth < 0) throw new UvmRuntimeException(ctx + "nthread must not be negative")

        val equal = MemoryOperations.cmpInt(len, objRefSrc + offsetSrc, bExp)

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
          case TypeRef(_) => (boxOf(r).asInstanceOf[BoxRef].objRef, 0L)
          case TypeIRef(_) => boxOf(r).asInstanceOf[BoxIRef].oo
        }

        pin(addr)

        resultBox(0).asInstanceOf[BoxPointer].addr = addr + offset
        continueNormally()
      }

      case "@uvm.native.unpin" => {
        val Seq(ty) = typeList
        val Seq(r) = argList

        val addr = ty match {
          case TypeRef(_) => boxOf(r).asInstanceOf[BoxRef].objRef
          case TypeIRef(_) => boxOf(r).asInstanceOf[BoxIRef].objRef
        }

        unpin(addr)

        continueNormally()
      }

      case "@uvm.native.expose" => {
        val Seq(callConv) = flagList
        val Seq(sig) = sigList
        val Seq(func, cookie) = argList

        val f = boxOf(func).asInstanceOf[BoxFunc].func.getOrElse {
          throw new UvmRuntimeException(ctx + "Attempt to expose NULL Mu function")
        }

        val c = boxOf(cookie).asInstanceOf[BoxInt].value.toLong

        val addr = microVM.nativeCallHelper.exposeFuncDynamic(f, c)

        resultBox(0).asInstanceOf[BoxPointer].addr = addr

        continueNormally()
      }

      case "@uvm.native.unexpose" => {
        val Seq(callConv) = flagList
        val Seq(addr) = argList

        val a = boxOf(addr).asInstanceOf[BoxPointer].addr

        microVM.nativeCallHelper.unexposeFunc(a)

        continueNormally()
      }

      case "@uvm.native.get_cookie" => {
        val cookie = topDefMu.cookie
        resultBox(0).asInstanceOf[BoxInt].value = OpHelper.trunc(cookie, 64)
        continueNormally()
      }

      case "@uvm.meta.id_of" => {
        val Seq(name) = argList
        val nameStr = MemoryOperations.bytesToStr(boxOf(name).asInstanceOf[BoxRef].objRef)
        val theID = microVM.idOf(nameStr)

        resultBox(0).asInstanceOf[BoxInt].value = OpHelper.unprepare(theID, 32)

        continueNormally()
      }

      case "@uvm.meta.name_of" => {
        val Seq(theID) = argList
        val idInt = boxOf(theID).asInstanceOf[BoxInt].value.toInt
        val name = microVM.nameOf(idInt)
        val bytesRef = MemoryOperations.strToBytes(name)

        resultBox(0).asInstanceOf[BoxRef].objRef = bytesRef

        continueNormally()
      }

      case "@uvm.meta.load_bundle" => {
        val Seq(bundle) = argList
        val bundleStr = MemoryOperations.bytesToStr(boxOf(bundle).asInstanceOf[BoxRef].objRef)

        MetaOperations.loadBundle(bundleStr)

        continueNormally()
      }

      case "@uvm.meta.load_hail" => {
        val Seq(hailScript) = argList
        val hailStr = MemoryOperations.bytesToStr(boxOf(hailScript).asInstanceOf[BoxRef].objRef)

        MetaOperations.loadHail(hailStr)

        continueNormally()
      }

      case "@uvm.meta.cur_func" => {
        val Seq(stack) = argList
        ???
      }

      case "@uvm.meta.cur_func_ver" => ???
      case "@uvm.meta.cur_inst" => ???
      case "@uvm.meta.dump_keepalives" => ???

      case "@uvm.meta.pop_frame" => ???
      case "@uvm.meta.push_frame" => ???

      case "@uvm.meta.enable_watchpoint" => ???
      case "@uvm.meta.disable_watchpoint" => ???

      case "@uvm.meta.set_trap_handler" => ???

      // Insert more CommInsts here.

      case ciName => {
        throw new UvmRefImplException("Unimplemented common instruction %s".format(ciName))
      }

    }

  }
}