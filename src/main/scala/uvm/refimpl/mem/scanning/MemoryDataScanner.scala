package uvm.refimpl.mem.scanning

import uvm.refimpl._
import uvm.refimpl.itpr._
import uvm.refimpl.mem._
import uvm.types._
import com.typesafe.scalalogging.StrictLogging
import TypeSizes._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

object MemoryDataScanner extends StrictLogging {

  val paranoiaLogger = Logger(LoggerFactory.getLogger(getClass.getName() + ".paranoia"))

  /**
   * Scan an allocation unit. In this implementation, heap objects, alloca cells and global
   * cells all have the same layout.
   */
  def scanAllocUnit(objRef: Word, iRef: Word, microVM: MicroVM, handler: RefFieldHandler) {
    val tag = HeaderUtils.getTag(objRef)
    logger.debug("Obj 0x%x, tag 0x%x".format(objRef, tag))
    val ty = HeaderUtils.getType(microVM, tag)
    scanField(ty, objRef, objRef, handler)
  }

  def scanField(ty: Type, objRef: Word, iRef: Word, handler: RefFieldHandler) {
    ty match {
      case t: TypeRef => {
        val toObj = MemorySupport.loadLong(iRef)
        logger.debug(s"Ref field ${iRef} -> ${toObj}")
        handler.fromMem(objRef, iRef, toObj, false, false)
      }
      case t: TypeIRef => {
        val toObj = MemorySupport.loadLong(iRef)
        logger.debug(s"IRef field ${iRef} -> ${toObj}")
        handler.fromMem(objRef, iRef, toObj, false, false)
      }
      case t: TypeWeakRef => {
        val toObj = MemorySupport.loadLong(iRef)
        logger.debug(s"WeakRef field ${iRef} -> ${toObj}")
        handler.fromMem(objRef, iRef, toObj, true, false)
      }
      case t: TypeTagRef64 => {
        val bits = MemorySupport.loadLong(iRef)
        if (paranoiaLogger.underlying.isDebugEnabled()) {
          paranoiaLogger.debug(s"Tagref bits ${bits}")
          if (OpHelper.tr64IsFp(bits)) {
            paranoiaLogger.debug(s"Tagref is FP: ${OpHelper.tr64ToFp(bits)}")
          } else if (OpHelper.tr64IsInt(bits)) {
            paranoiaLogger.debug(s"Tagref is Int: ${OpHelper.tr64ToInt(bits)}")
          } else if (OpHelper.tr64IsRef(bits)) {
            paranoiaLogger.debug(s"Tagref is Ref: ${OpHelper.tr64ToRef(bits)} tag: ${OpHelper.tr64ToTag(bits)}")
          }
        }
        if (OpHelper.tr64IsRef(bits)) {
          val toObj = OpHelper.tr64ToRef(bits)
          logger.debug(s"TagRef64 field ${iRef} -> ${toObj} tag: ${OpHelper.tr64ToTag(bits)}")
          handler.fromMem(objRef, iRef, toObj, false, true)
        }
      }
      case t: TypeStruct => {
        var fieldAddr = iRef
        for (fieldTy <- t.fieldTy) {
          val fieldAlign = TypeSizes.alignOf(fieldTy)
          fieldAddr = TypeSizes.alignUp(fieldAddr, fieldAlign)
          scanField(fieldTy, objRef, fieldAddr, handler)
          fieldAddr += TypeSizes.sizeOf(fieldTy)
        }
      }
      case t: TypeArray => {
        val elemTy = t.elemTy
        val elemSize = TypeSizes.sizeOf(elemTy)
        val elemAlign = TypeSizes.alignOf(elemTy)
        var elemAddr = iRef
        for (i <- 0L until t.len) {
          scanField(elemTy, objRef, elemAddr, handler)
          elemAddr = TypeSizes.alignUp(elemAddr + elemSize, elemAlign)
        }

      }
      case t: TypeHybrid => {
        val fixedTy = t.fixedTy
        val varTy = t.varTy
        val fixedSize = TypeSizes.sizeOf(fixedTy)
        val fixedAlign = TypeSizes.alignOf(fixedTy)
        val varSize = TypeSizes.sizeOf(varTy)
        val varAlign = TypeSizes.alignOf(varTy)
        var curAddr = iRef
        val varLength = HeaderUtils.getVarLength(iRef)
        scanField(fixedTy, objRef, curAddr, handler)
        curAddr = TypeSizes.alignUp(curAddr + fixedSize, fixedAlign)
        for (i <- 0L until varLength) {
          scanField(varTy, objRef, curAddr, handler)
          curAddr = TypeSizes.alignUp(curAddr + varSize, varAlign)
        }
      }
      case _ => // Ignore non-reference fields.
    }
  }
}
