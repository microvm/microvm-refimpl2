package uvm.refimpl.nat

import com.kenai.jffi.CallingConvention
import com.kenai.jffi.Closure
import com.kenai.jffi.Closure.Buffer
import com.kenai.jffi.{ Type => JType }
import NativeSupport._
import PlatformConstants._
import uvm.refimpl._
import uvm.ssavariables.{BinOptr, CmpOptr, ConvOptr, MemoryOrder, AtomicRMWOptr}
import uvm.ir.irbuilder.DestKind

import CDefsHelperFunctions._
import uvm.refimpl.MicroVM
import uvm.ssavariables.Flag

class ExposedMethod(jRetTy: JType, jParamTys: Array[JType], invokeFunc: Buffer => Unit) {
  val closure = new SimpleClosure(invokeFunc)
  val handle = jffiClosureManager.newClosure(closure, jRetTy, jParamTys, CallingConvention.DEFAULT)
  def address = handle.getAddress()
}

class SimpleClosure(f: Buffer => Unit) extends Closure {
  def invoke(buffer: Buffer): Unit = f(buffer)
}

private object CDefsHelperFunctions {

  def exposedMethod(jRetTy: JType, jParamTys: Array[JType])(invokeFunc: Buffer => Unit) = {
    new ExposedMethod(jRetTy, jParamTys, invokeFunc)
  }
  
  def getMuVM(ptr: Long): MicroVM = NativeClientSupport.getMicroVM(ptr)
  def getMuCtx(ptr: Long): MuCtx = NativeClientSupport.getMuCtx(ptr)
  def getMuValueNotNull(ptr: Long): MuValue = NativeClientSupport.getMuValueNotNull(ptr)
  def getMuValueNullable(ptr: Long): Option[MuValue] = NativeClientSupport.getMuValueNullable(ptr)
  
  def readIntArray(base: Long, len: Long): IndexedSeq[Int] = {
    if (base == 0L) {
      IndexedSeq[Int]()
    } else {
      for (i <- 0L until len) yield {
        val addr = base + i * 4L
        val v = theMemory.getInt(addr)
        v
      }
    }
  }
  
  def readLongArray(base: Long, len: Long): IndexedSeq[Long] = {
    if (base == 0L) {
      IndexedSeq[Long]()
    } else {
      for (i <- 0L until len) yield {
        val addr = base + i * 8L
        val v = theMemory.getLong(addr)
        v
      }
    }
  }

  def readMuValueArray(base: Long, len: Long): IndexedSeq[MuValue] = {
    readLongArray(base, len).map(getMuValueNotNull)
  }
  
  def readFlagArray(base: Long, len: Long): IndexedSeq[Flag] = {
    readIntArray(base, len).map(toFlag)
  }
  
  def toFlag(cval: Int): Flag = cval match {
    case 0x00 => Flag("#DEFAULT")
    case _ => throw new IllegalArgumentException("Unknown calling convention %d (0x%x)".format(cval, cval))
  }
  
}