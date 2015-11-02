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
  def scanAllocUnit(objRef: Word, iRef: Word, handler: RefFieldHandler)(implicit microVM: MicroVM, memorySupport: MemorySupport) {
    val tag = HeaderUtils.getTag(objRef)
    logger.debug("Obj 0x%x, tag 0x%x".format(objRef, tag))
    val ty = HeaderUtils.getType(microVM, tag)
    logger.debug {
      if (ty.isInstanceOf[TypeHybrid]) {
        val len = HeaderUtils.getVarLength(objRef)
        val size = TypeSizes.hybridSizeOf(ty.asInstanceOf[TypeHybrid], len)
        "Type: %s, varLen: %d, hybridSizeOf: %d".format(ty.repr, len, size)
      } else {
        "Type: %s".format(ty.repr)
      }
    }
    scanField(ty, objRef, objRef, handler)
  }

  def scanField(ty: Type, objRef: Word, iRef: Word, handler: RefFieldHandler)(implicit microVM: MicroVM, memorySupport: MemorySupport) {
    def logField(kind: String, toObj: Word): String = "%s field [0x%x + 0x%x] = 0x%x -> 0x%x".format(kind, objRef, iRef - objRef, iRef, toObj)
    ty match {
      case t: TypeRef => {
        val toObj = memorySupport.loadLong(iRef)
        logger.debug(logField("Ref", toObj))
        handler.memToHeap(objRef, iRef, toObj, false, false)
      }
      case t: TypeIRef => {
        val toObj = memorySupport.loadLong(iRef)
        logger.debug(logField("IRef", toObj))
        handler.memToHeap(objRef, iRef, toObj, false, false)
      }
      case t: TypeWeakRef => {
        val toObj = memorySupport.loadLong(iRef)
        logger.debug(logField("WeakRef", toObj))
        handler.memToHeap(objRef, iRef, toObj, true, false)
      }
      case t: TypeTagRef64 => {
        val bits = memorySupport.loadLong(iRef)
        if (paranoiaLogger.underlying.isDebugEnabled()) {
          paranoiaLogger.debug(s"Tagref bits ${bits}")
          if (OpHelper.tr64IsFp(bits)) {
            paranoiaLogger.debug("Tagref is FP: %f".format(OpHelper.tr64ToFp(bits)))
          } else if (OpHelper.tr64IsInt(bits)) {
            paranoiaLogger.debug("Tagref is Int: %d".format(OpHelper.tr64ToInt(bits)))
          } else if (OpHelper.tr64IsRef(bits)) {
            paranoiaLogger.debug("Tagref is Ref: 0x%x tag: %d".format(OpHelper.tr64ToRef(bits), OpHelper.tr64ToTag(bits)))
          }
        }
        if (OpHelper.tr64IsRef(bits)) {
          val toObj = OpHelper.tr64ToRef(bits)
          logger.debug(logField("TagRef64", toObj))
          handler.memToHeap(objRef, iRef, toObj, false, true)
        }
      }
      case t: TypeStruct => {
        var fieldAddr = iRef
        for (fieldTy <- t.fieldTys) {
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
        val varTy = t.varTy
        val varSize = TypeSizes.sizeOf(varTy)
        val varAlign = TypeSizes.alignOf(varTy)
        val varLength = HeaderUtils.getVarLength(iRef)
        var curAddr = iRef
        for (fieldTy <- t.fieldTys) {
          val fieldAlign = TypeSizes.alignOf(fieldTy)
          curAddr = TypeSizes.alignUp(curAddr, fieldAlign)
          scanField(fieldTy, objRef, curAddr, handler)
          curAddr += TypeSizes.sizeOf(fieldTy)
        }
        curAddr = TypeSizes.alignUp(curAddr, varAlign)
        for (i <- 0L until varLength) {
          scanField(varTy, objRef, curAddr, handler)
          curAddr = TypeSizes.alignUp(curAddr + varSize, varAlign)
        }
      }
      case t: TypeStackRef => {
        val toStackID = memorySupport.loadLong(iRef)
        val maybeToStack = if (toStackID == 0) {
          None
        } else {
          val toStack = microVM.threadStackManager.getStackByID(toStackID.toInt).getOrElse {
            throw new UvmRefImplException("Memory location 0x%x referring to non-existing stack %d".format(iRef, toStackID))
          }
          Some(toStack)
        }
        handler.memToStack(objRef, iRef, maybeToStack)
      }
      case _ => // Ignore non-reference fields.
    }
  }
}
