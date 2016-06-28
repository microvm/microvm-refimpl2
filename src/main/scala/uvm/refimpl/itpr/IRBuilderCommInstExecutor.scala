package uvm.refimpl.itpr
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.Logger
import uvm._
import uvm.comminsts._
import uvm.ir.irbuilder._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.refimpl.mem.TypeSizes.Word
import uvm.ssavariables._
import uvm.types._
import uvm.refimpl.nat.NativeCallResult
import uvm.refimpl.nat.CDefs

object IRBuilderCommInstExecutor {
  implicit def nodeToChildNode[T <: Identified](n: IRNode): ChildNode[T] = n.asInstanceOf[ChildNode[T]]
  implicit def optNodeToChildOptNode[T <: Identified](ns: Option[IRNode]): Option[ChildNode[T]] = ns.asInstanceOf[Option[ChildNode[T]]]
  implicit def seqNodeToChildSeqNode[T <: Identified](ns: Seq[IRNode]): Seq[ChildNode[T]] = ns.asInstanceOf[Seq[ChildNode[T]]]
  implicit def toBinOptr(i: Int): BinOptr.Value = CDefs.toBinOptr(i)
  implicit def toCmpOptr(i: Int): CmpOptr.Value = CDefs.toCmpOptr(i)
  implicit def toConvOptr(i: Int): ConvOptr.Value = CDefs.toConvOptr(i)
  implicit def toMemoryOrder(i: Int): MemoryOrder.Value = CDefs.toMemoryOrder(i)
  implicit def toAtomicRMWOptr(i: Int): AtomicRMWOptr.Value = CDefs.toAtomicRMWOptr(i)
  implicit def toDestKind(i: Int): DestKind.Value = CDefs.toDestKind(i)
  implicit def toFlag(i: Int): Flag = i match {
    case CDefs.MU_CC_DEFAULT => Flag("#DEFAULT")
  }

  def unsignedLongSeqToBigInt(nums: Seq[Long]): BigInt = {
      var bigNum = BigInt(0)
      for (num <- nums) {
        bigNum = (bigNum << 64) | (BigInt(num) & 0xffffffffffffffffL)
      }
      bigNum
  }
}

/**
 * A part of the InterpreterThread that interprets common instructions
 */
trait IRBuilderCommInstExecutor extends InterpreterActions with ObjectPinner {
  import IRBuilderCommInstExecutor._
  
  import InterpreterThread.logger

  implicit protected def mutator: Mutator
  implicit protected def memorySupport: MemorySupport
  
  protected def irBuilder = microVM.irBuilder

  protected def loadIRNodeArray(ir: (Word, Word), sz: Word): IndexedSeq[IRNode] = {
    val (obj,off) = ir
    val loc = obj + off
    MemoryOperations.loadIRNodeArray(loc, sz)
  }

  protected def loadInt64Array(ir: (Word, Word), sz: Word): IndexedSeq[Long] = {
    val (obj,off) = ir
    val loc = obj + off
    MemoryOperations.loadInt64Array(loc, sz)
  }

  protected def loadFlagArray(ir: (Word, Word), sz: Word): IndexedSeq[Flag] = {
    val (obj,off) = ir
    val loc = obj + off
    MemoryOperations.loadInt32Array(loc, sz).map(toFlag)
  } 

  def interpretCurrentIRBuilderCommonInstruction(): Unit = {
    assert(curInst.isInstanceOf[InstCommInst])
    val InstCommInst(ci, _, _, _, argList, _, _) = curInst
    
    assert(ci.name.get.startsWith("@uvm.irbuilder"))

    ci.name.get match {
      case "@uvm.irbuilder.load_bundle_from_node" => {
        val _param0 = argList(0).asIRNode.get.asInstanceOf[BundleNode].bundle
        microVM.addBundle(_param0)
        continueNormally()
      }
      case "@uvm.irbuilder.abort_bundle_node" => {
        // no-op
        continueNormally()
      }
      case "@uvm.irbuilder.set_name" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val node = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %node must not be null"))
        val name = argList(2).asIRef
        val nameStr = MemoryOperations.bytesToStr(name._1 + name._2)
        irBuilder.setName(b, node, nameStr)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_int_ex" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val values = argList(2).asIRef
        val nvalues = argList(3).asInt64.toLong
        val _ary_values = loadInt64Array(values, nvalues)
        val _value_big = unsignedLongSeqToBigInt(_ary_values)
        val _rv = irBuilder.newConstInt(b, ty, _value_big)
        continueNormally()
      }
      
      // Auto-generated implementations go here
      /// GEN:BEGIN:IRBUILDER_IMPL
      case "@uvm.irbuilder.new_bundle" => {
        val _rv = irBuilder.newBundle()
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.get_node" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val id = argList(1).asInt32.toInt
        val _rv = irBuilder.getNode(b, id)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.get_id" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val node = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %node must not be null"))
        val _rv = irBuilder.getID(b, node)
        results(0).asInt32 = _rv
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_int" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val len = argList(1).asInt32.toInt
        val _rv = irBuilder.newTypeInt(b, len)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_float" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeFloat(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_double" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeDouble(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_uptr" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeUPtr(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_uptr" => {
        val uptr = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %uptr must not be null"))
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.setTypeUPtr(uptr, ty)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_ufuncptr" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeUFuncPtr(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_ufuncptr" => {
        val ufuncptr = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ufuncptr must not be null"))
        val sig = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val _rv = irBuilder.setTypeUFuncPtr(ufuncptr, sig)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_struct" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val fieldtys = argList(1).asIRef
        val nfieldtys = argList(2).asInt64.toLong
        val _ary_fieldtys = loadIRNodeArray(fieldtys, nfieldtys)
        val _rv = irBuilder.newTypeStruct(b, _ary_fieldtys)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_hybrid" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val fixedtys = argList(1).asIRef
        val nfixedtys = argList(2).asInt64.toLong
        val varty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %varty must not be null"))
        val _ary_fixedtys = loadIRNodeArray(fixedtys, nfixedtys)
        val _rv = irBuilder.newTypeHybrid(b, _ary_fixedtys, varty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_array" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val elemty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %elemty must not be null"))
        val len = argList(2).asInt64.toLong
        val _rv = irBuilder.newTypeArray(b, elemty, len)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_vector" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val elemty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %elemty must not be null"))
        val len = argList(2).asInt64.toLong
        val _rv = irBuilder.newTypeVector(b, elemty, len)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_void" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeVoid(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_ref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_ref" => {
        val ref = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ref must not be null"))
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.setTypeRef(ref, ty)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_iref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeIRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_iref" => {
        val iref = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %iref must not be null"))
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.setTypeIRef(iref, ty)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_weakref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeWeakRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_weakref" => {
        val weakref = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %weakref must not be null"))
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.setTypeWeakRef(weakref, ty)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_funcref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeFuncRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_type_funcref" => {
        val funcref = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %funcref must not be null"))
        val sig = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val _rv = irBuilder.setTypeFuncRef(funcref, sig)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_tagref64" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeTagRef64(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_threadref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeThreadRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_stackref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeStackRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_framecursorref" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeFrameCursorRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_type_irnoderef" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val _rv = irBuilder.newTypeIRNodeRef(b)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_funcsig" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val paramtys = argList(1).asIRef
        val nparamtys = argList(2).asInt64.toLong
        val rettys = argList(3).asIRef
        val nrettys = argList(4).asInt64.toLong
        val _ary_paramtys = loadIRNodeArray(paramtys, nparamtys)
        val _ary_rettys = loadIRNodeArray(rettys, nrettys)
        val _rv = irBuilder.newFuncSig(b, _ary_paramtys, _ary_rettys)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_int" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val value = argList(2).asInt64.toLong
        val _rv = irBuilder.newConstInt(b, ty, value)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_float" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val value = argList(2).asFloat
        val _rv = irBuilder.newConstFloat(b, ty, value)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_double" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val value = argList(2).asDouble
        val _rv = irBuilder.newConstDouble(b, ty, value)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_null" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.newConstNull(b, ty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_const_seq" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val elems = argList(2).asIRef
        val nelems = argList(3).asInt64.toLong
        val _ary_elems = loadIRNodeArray(elems, nelems)
        val _rv = irBuilder.newConstSeq(b, ty, _ary_elems)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_global_cell" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.newGlobalCell(b, ty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_func" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val sig = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val _rv = irBuilder.newFunc(b, sig)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_func_ver" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val func = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %func must not be null"))
        val _rv = irBuilder.newFuncVer(b, func)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_exp_func" => {
        val b = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %b must not be null")).asInstanceOf[BundleNode]
        val func = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %func must not be null"))
        val callconv = argList(2).asInt32.toInt
        val cookie = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %cookie must not be null"))
        val _rv = irBuilder.newExpFunc(b, func, callconv, cookie)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_bb" => {
        val fv = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %fv must not be null"))
        val _rv = irBuilder.newBB(fv)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_nor_param" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val _rv = irBuilder.newNorParam(bb, ty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_exc_param" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val _rv = irBuilder.newExcParam(bb)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_inst_res" => {
        val inst = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %inst must not be null"))
        val _rv = irBuilder.newInstRes(inst)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.add_dest" => {
        val inst = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %inst must not be null"))
        val kind = argList(1).asInt32.toInt
        val dest = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %dest must not be null"))
        val vars = argList(3).asIRef
        val nvars = argList(4).asInt64.toLong
        val _ary_vars = loadIRNodeArray(vars, nvars)
        val _rv = irBuilder.addDest(inst, kind, dest, _ary_vars)
        continueNormally()
      }
      case "@uvm.irbuilder.add_keepalives" => {
        val inst = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %inst must not be null"))
        val vars = argList(1).asIRef
        val nvars = argList(2).asInt64.toLong
        val _ary_vars = loadIRNodeArray(vars, nvars)
        val _rv = irBuilder.addKeepalives(inst, _ary_vars)
        continueNormally()
      }
      case "@uvm.irbuilder.new_binop" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val optr = argList(1).asInt32.toInt
        val ty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val opnd1 = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd1 must not be null"))
        val opnd2 = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd2 must not be null"))
        val _rv = irBuilder.newBinOp(bb, optr, ty, opnd1, opnd2)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_cmp" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val optr = argList(1).asInt32.toInt
        val ty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %ty must not be null"))
        val opnd1 = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd1 must not be null"))
        val opnd2 = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd2 must not be null"))
        val _rv = irBuilder.newCmp(bb, optr, ty, opnd1, opnd2)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_conv" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val optr = argList(1).asInt32.toInt
        val from_ty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %from_ty must not be null"))
        val to_ty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %to_ty must not be null"))
        val opnd = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _rv = irBuilder.newConv(bb, optr, from_ty, to_ty, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_select" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val cond_ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %cond_ty must not be null"))
        val opnd_ty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd_ty must not be null"))
        val cond = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %cond must not be null"))
        val if_true = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %if_true must not be null"))
        val if_false = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %if_false must not be null"))
        val _rv = irBuilder.newSelect(bb, cond_ty, opnd_ty, cond, if_true, if_false)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_branch" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val _rv = irBuilder.newBranch(bb)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_branch2" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val cond = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %cond must not be null"))
        val _rv = irBuilder.newBranch2(bb, cond)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_switch" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val opnd_ty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd_ty must not be null"))
        val opnd = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _rv = irBuilder.newSwitch(bb, opnd_ty, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.add_switch_dest" => {
        val sw = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sw must not be null"))
        val key = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %key must not be null"))
        val dest = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %dest must not be null"))
        val vars = argList(3).asIRef
        val nvars = argList(4).asInt64.toLong
        val _ary_vars = loadIRNodeArray(vars, nvars)
        val _rv = irBuilder.addSwitchDest(sw, key, dest, _ary_vars)
        continueNormally()
      }
      case "@uvm.irbuilder.new_call" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val sig = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val callee = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %callee must not be null"))
        val args = argList(3).asIRef
        val nargs = argList(4).asInt64.toLong
        val _ary_args = loadIRNodeArray(args, nargs)
        val _rv = irBuilder.newCall(bb, sig, callee, _ary_args)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_tailcall" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val sig = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val callee = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %callee must not be null"))
        val args = argList(3).asIRef
        val nargs = argList(4).asInt64.toLong
        val _ary_args = loadIRNodeArray(args, nargs)
        val _rv = irBuilder.newTailCall(bb, sig, callee, _ary_args)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_ret" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val rvs = argList(1).asIRef
        val nrvs = argList(2).asInt64.toLong
        val _ary_rvs = loadIRNodeArray(rvs, nrvs)
        val _rv = irBuilder.newRet(bb, _ary_rvs)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_throw" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val exc = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %exc must not be null"))
        val _rv = irBuilder.newThrow(bb, exc)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_extractvalue" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val strty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %strty must not be null"))
        val index = argList(2).asInt32.toInt
        val opnd = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _rv = irBuilder.newExtractValue(bb, strty, index, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_insertvalue" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val strty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %strty must not be null"))
        val index = argList(2).asInt32.toInt
        val opnd = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val newval = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %newval must not be null"))
        val _rv = irBuilder.newInsertValue(bb, strty, index, opnd, newval)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_extractelement" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val seqty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %seqty must not be null"))
        val indty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %indty must not be null"))
        val opnd = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val index = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %index must not be null"))
        val _rv = irBuilder.newExtractElement(bb, seqty, indty, opnd, index)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_insertelement" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val seqty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %seqty must not be null"))
        val indty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %indty must not be null"))
        val opnd = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val index = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %index must not be null"))
        val newval = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %newval must not be null"))
        val _rv = irBuilder.newInsertElement(bb, seqty, indty, opnd, index, newval)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_shufflevector" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val vecty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %vecty must not be null"))
        val maskty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %maskty must not be null"))
        val vec1 = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %vec1 must not be null"))
        val vec2 = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %vec2 must not be null"))
        val mask = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %mask must not be null"))
        val _rv = irBuilder.newShuffleVector(bb, vecty, maskty, vec1, vec2, mask)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_new" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val allocty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %allocty must not be null"))
        val _rv = irBuilder.newNew(bb, allocty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_newhybrid" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val allocty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %allocty must not be null"))
        val lenty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %lenty must not be null"))
        val length = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %length must not be null"))
        val _rv = irBuilder.newNewHybrid(bb, allocty, lenty, length)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_alloca" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val allocty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %allocty must not be null"))
        val _rv = irBuilder.newAlloca(bb, allocty)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_allocahybrid" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val allocty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %allocty must not be null"))
        val lenty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %lenty must not be null"))
        val length = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %length must not be null"))
        val _rv = irBuilder.newAllocaHybrid(bb, allocty, lenty, length)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_getiref" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val refty = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val opnd = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _rv = irBuilder.newGetIRef(bb, refty, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_getfieldiref" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val refty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val index = argList(3).asInt32.toInt
        val opnd = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newGetFieldIRef(bb, _bool_is_ptr, refty, index, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_getelemiref" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val refty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val indty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %indty must not be null"))
        val opnd = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val index = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %index must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newGetElemIRef(bb, _bool_is_ptr, refty, indty, opnd, index)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_shiftiref" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val refty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val offty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %offty must not be null"))
        val opnd = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val offset = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %offset must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newShiftIRef(bb, _bool_is_ptr, refty, offty, opnd, offset)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_getvarpartiref" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val refty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val opnd = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newGetVarPartIRef(bb, _bool_is_ptr, refty, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_load" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val ord = argList(2).asInt32.toInt
        val refty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val loc = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %loc must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newLoad(bb, _bool_is_ptr, ord, refty, loc)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_store" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val ord = argList(2).asInt32.toInt
        val refty = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val loc = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %loc must not be null"))
        val newval = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %newval must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newStore(bb, _bool_is_ptr, ord, refty, loc, newval)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_cmpxchg" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val is_weak = argList(2).asInt32.toInt
        val ord_succ = argList(3).asInt32.toInt
        val ord_fail = argList(4).asInt32.toInt
        val refty = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refty must not be null"))
        val loc = argList(6).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %loc must not be null"))
        val expected = argList(7).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %expected must not be null"))
        val desired = argList(8).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %desired must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _bool_is_weak = is_weak != 0
        val _rv = irBuilder.newCmpXchg(bb, _bool_is_ptr, _bool_is_weak, ord_succ, ord_fail, refty, loc, expected, desired)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_atomicrmw" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val is_ptr = argList(1).asInt32.toInt
        val ord = argList(2).asInt32.toInt
        val optr = argList(3).asInt32.toInt
        val refTy = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %refTy must not be null"))
        val loc = argList(5).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %loc must not be null"))
        val opnd = argList(6).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %opnd must not be null"))
        val _bool_is_ptr = is_ptr != 0
        val _rv = irBuilder.newAtomicRMW(bb, _bool_is_ptr, ord, optr, refTy, loc, opnd)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_fence" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val ord = argList(1).asInt32.toInt
        val _rv = irBuilder.newFence(bb, ord)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_trap" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val rettys = argList(1).asIRef
        val nrettys = argList(2).asInt64.toLong
        val _ary_rettys = loadIRNodeArray(rettys, nrettys)
        val _rv = irBuilder.newTrap(bb, _ary_rettys)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_watchpoint" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val wpid = argList(1).asInt32.toInt
        val rettys = argList(2).asIRef
        val nrettys = argList(3).asInt64.toLong
        val _ary_rettys = loadIRNodeArray(rettys, nrettys)
        val _rv = irBuilder.newWatchPoint(bb, wpid, _ary_rettys)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_wpbranch" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val wpid = argList(1).asInt32.toInt
        val _rv = irBuilder.newWPBranch(bb, wpid)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_ccall" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val callconv = argList(1).asInt32.toInt
        val callee_ty = argList(2).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %callee_ty must not be null"))
        val sig = argList(3).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %sig must not be null"))
        val callee = argList(4).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %callee must not be null"))
        val args = argList(5).asIRef
        val nargs = argList(6).asInt64.toLong
        val _ary_args = loadIRNodeArray(args, nargs)
        val _rv = irBuilder.newCCall(bb, callconv, callee_ty, sig, callee, _ary_args)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_newthread" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val stack = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %stack must not be null"))
        val threadlocal = argList(2).asIRNode
        val _rv = irBuilder.newNewThread(bb, stack, threadlocal)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_swapstack_ret" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val swappee = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %swappee must not be null"))
        val ret_tys = argList(2).asIRef
        val nret_tys = argList(3).asInt64.toLong
        val _ary_ret_tys = loadIRNodeArray(ret_tys, nret_tys)
        val _rv = irBuilder.newSwapStackRet(bb, swappee, _ary_ret_tys)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.new_swapstack_kill" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val swappee = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %swappee must not be null"))
        val _rv = irBuilder.newSwapStackKill(bb, swappee)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      case "@uvm.irbuilder.set_newstack_pass_values" => {
        val inst = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %inst must not be null"))
        val tys = argList(1).asIRef
        val vars = argList(2).asIRef
        val nvars = argList(3).asInt64.toLong
        val _ary_tys = loadIRNodeArray(tys, nvars)
        val _ary_vars = loadIRNodeArray(vars, nvars)
        val _rv = irBuilder.setNewStackPassValues(inst, _ary_tys, _ary_vars)
        continueNormally()
      }
      case "@uvm.irbuilder.set_newstack_throw_exc" => {
        val inst = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %inst must not be null"))
        val exc = argList(1).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %exc must not be null"))
        val _rv = irBuilder.setNewStackThrowExc(inst, exc)
        continueNormally()
      }
      case "@uvm.irbuilder.new_comminst" => {
        val bb = argList(0).asIRNode.getOrElse(throw new UvmNullGenRefException("CommInst arg %bb must not be null"))
        val opcode = argList(1).asInt32.toInt
        val flags = argList(2).asIRef
        val nflags = argList(3).asInt64.toLong
        val tys = argList(4).asIRef
        val ntys = argList(5).asInt64.toLong
        val sigs = argList(6).asIRef
        val nsigs = argList(7).asInt64.toLong
        val args = argList(8).asIRef
        val nargs = argList(9).asInt64.toLong
        val _ary_flags = loadFlagArray(flags, nflags)
        val _ary_tys = loadIRNodeArray(tys, ntys)
        val _ary_sigs = loadIRNodeArray(sigs, nsigs)
        val _ary_args = loadIRNodeArray(args, nargs)
        val _rv = irBuilder.newCommInst(bb, opcode, _ary_flags, _ary_tys, _ary_sigs, _ary_args)
        results(0).asIRNode = Some(_rv)
        continueNormally()
      }
      /// GEN:END:IRBUILDER_IMPL

      case ciName => {
        throw new UvmRefImplException("Unimplemented IR builder common instruction %s".format(ciName))
      }

    }

  }
}
