package uvm.refimpl

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer

import uvm.ssavariables.MemoryOrder._

object RichMuCtx {
  /**
   * Allow you to use the `val h = x << ctx.makeSomeHandle()` or `val h = x(ctx.makeSomeHandle())`
   *  syntax to dispose it later
   */
  class DelayedDisposer(garbageList: Buffer[MuValue]) {
    def apply[T <: MuValue](v: T): T = {
      garbageList += v
      v
    }
    def <<[T <: MuValue](v: T): T = {
      garbageList += v
      v
    }
  }

  /** Extensions to the MuCtx interface. Not officially part of the client API. */
  implicit class RichMuCtx(val ctx: MuCtx) extends AnyVal {
    def handleFromBoolean(b: Boolean) = ctx.handleFromInt(if (b) 1 else 0, 1)
    def handleFromInt1(num: BigInt) = ctx.handleFromInt(num, 1)
    def handleFromInt6(num: BigInt) = ctx.handleFromInt(num, 6)
    def handleFromInt8(num: BigInt) = ctx.handleFromInt(num, 8)
    def handleFromInt16(num: BigInt) = ctx.handleFromInt(num, 16)
    def handleFromInt32(num: BigInt) = ctx.handleFromInt(num, 32)
    def handleFromInt52(num: BigInt) = ctx.handleFromInt(num, 52)
    def handleFromInt64(num: BigInt) = ctx.handleFromInt(num, 64)
    def deleteValue(vs: MuValue*) = vs.foreach(ctx.deleteValue)

    def autoDispose[T](f: DelayedDisposer => T) = {
      val garbages = ArrayBuffer[MuValue]()
      val dd = new DelayedDisposer(garbages)
      val rv = f(dd)
      garbages.foreach(ctx.deleteValue)
      rv
    }

    def loadInt(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuIntValue]
    def loadFloat(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuFloatValue]
    def loadDouble(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuDoubleValue]
    def loadRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuRefValue]
    def loadIRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuIRefValue]
    def loadFuncRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuFuncRefValue]
    def loadThreadRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuThreadRefValue]
    def loadStackRef(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuStackRefValue]
    def loadTagRef64(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuTagRef64Value]
    def loadVector(ord: MemoryOrder, loc: MuIRefValue) = ctx.load(ord, loc).asInstanceOf[MuVectorValue]

    def storeInt(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuIntValue]
    def storeFloat(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuFloatValue]
    def storeDouble(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuDoubleValue]
    def storeRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuRefValue]
    def storeIRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuIRefValue]
    def storeFuncRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuFuncRefValue]
    def storeThreadRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuThreadRefValue]
    def storeStackRef(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuStackRefValue]
    def storeTagRef64(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuTagRef64Value]
    def storeVector(ord: MemoryOrder, loc: MuIRefValue, newval: MuValue) = ctx.store(ord, loc, newval).asInstanceOf[MuVectorValue]

    def bytesToStr(bytes: MuRefValue): String = ctx.autoDispose { x =>
      val hIR = x << ctx.getIRef(bytes)
      val hFix = x << ctx.getFieldIRef(hIR, 0)
      val hLen = x << ctx.loadInt(NOT_ATOMIC, hFix)
      val len = ctx.handleToSInt(hLen).toLong
      val hVar = x << ctx.getVarPartIRef(hIR)

      val byteValues = for (i <- 0L until len) yield ctx.autoDispose { x =>
        val hI = x << ctx.handleFromInt64(i)
        val hVarI = x << ctx.shiftIRef(hVar, hI)
        val hByte = x << ctx.loadInt(NOT_ATOMIC, hVarI)
        val byte = ctx.handleToSInt(hByte).toByte
        byte
      }
      val bytesArray = byteValues.toArray
      val str = new String(bytesArray, MuCtx.US_ASCII)
      str
    }

    // legacy support

    /** Get the ID of the current function of a frame. Return 0 for native frames. */
    def curFunc(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curFunc(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /**
     * Get the ID of the current function version of a frame. Return 0 for native frames
     *  or Mu frames of undefined functions
     */
    def curFuncVer(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curFuncVer(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /**
     * Get the ID of the current instruction of a frame. Return 0 for native frames, Mu frames for undefined
     *  functions, or if the frame is just created by newStack or pushFrame.
     */
    def curInst(stack: MuStackRefValue, frame: Int): Int = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val id = ctx.curInst(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      id
    }

    /** Dump keep-alive variables of the current instruction. */
    def dumpKeepalives(stack: MuStackRefValue, frame: Int): Seq[MuValue] = {
      val cursor = ctx.newCursor(stack)
      for (i <- 0 until frame) {
        ctx.nextFrame(cursor)
      }
      val kas = ctx.dumpKeepalives(cursor)
      ctx.closeCursor(cursor)
      ctx.deleteValue(cursor)
      kas
    }
  }
}