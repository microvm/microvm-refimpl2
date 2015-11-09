package uvm.refimpl.hail

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext

import uvm.Function
import uvm.ir.textinput.TextIRParsingException
import uvm.ir.textinput.gen.HAILLexer
import uvm.ir.textinput.gen.HAILParser
import uvm.ir.textinput.gen.HAILParser._
import uvm.refimpl._
import uvm.refimpl.mem.HeaderUtils
import uvm.refimpl.mem.MemorySupport
import uvm.ssavariables._
import uvm.ssavariables.MemoryOrder.NOT_ATOMIC
import uvm.types._
import uvm.utils.AntlrHelpers._

class HailScriptLoader(implicit microVM: MicroVM, memorySupport: MemorySupport) {
  def loadHail(hailScript: String): Unit = {
    val ais = new ANTLRInputStream(hailScript)
    val ea = new AccumulativeAntlrErrorListener(hailScript)

    val lexer = new HAILLexer(ais)
    lexer.removeErrorListeners()
    lexer.addErrorListener(ea)
    val tokens = new CommonTokenStream(lexer)
    val parser = new HAILParser(tokens)
    parser.removeErrorListeners()
    parser.addErrorListener(ea)
    val ast = parser.hail()
    if (ea.hasError) {
      throw new TextIRParsingException("Syntax error:\n" + ea.getMessages)
    }

    loadTopLevel(ast)
  }

  private type HailObjMap = HashMap[String, MuRefValue]

  private def loadTopLevel(ast: HailContext): Unit = {
    implicit val mc = microVM.newContext()
    try {
      implicit val hailObjMap = new HailObjMap

      ast.topLevelDef.map(_.getChild(0)) foreach {
        case tl: FixedAllocContext => {
          val hailName = tl.nam.toString
          val ty = resTy(tl.ty)
          if (ty.isInstanceOf[TypeHybrid]) {
            throw new UvmHailParsingException(inCtx(tl, "Cannot allocate hybrid using '.new'. Found: %s".format(ty)))
          }
          val obj = mc.newFixed(ty.id)
          if (hailObjMap.contains(hailName)) {
            throw new UvmHailParsingException(inCtx(tl, "HAIL name %s already used.".format(hailName)))
          }
          hailObjMap(hailName) = obj
        }
        case tl: HybridAllocContext => {
          val hailName = tl.nam.toString
          val ty = resTy(tl.ty)
          if (!ty.isInstanceOf[TypeHybrid]) {
            throw new UvmHailParsingException(inCtx(tl, "hybrid required. Found %s".format(ty)))
          }
          val len: Long = evalIntExpr(tl.len).toLong
          val obj = mc.newFixed(ty.id)
          if (hailObjMap.contains(hailName)) {
            throw new UvmHailParsingException(inCtx(tl, "HAIL name %s already used.".format(hailName)))
          }
          hailObjMap(hailName) = obj
        }
        case init: MemInitContext => {
          val lv = evalLValue(init.lv)
          assign(lv, init.rv)
        }
      }

    } finally {
      mc.closeContext()
    }
  }

  class LValue private (val iref: MuIRefValue, val varLen: Option[Long], val baseCtx: ParserRuleContext, val curCtx: ParserRuleContext) {
    def indexInto(index: Long, ctx: ParserRuleContext)(implicit mc: MuCtx): LValue = {
      val (newIRef, newVarLen) = varLen match {
        case None => { // not in the var-part of a hybrid
          iref.ty.ty match {
            case t: TypeStruct => {
              val ii = index.toInt
              if (ii < 0 || ii >= t.fieldTys.length) {
                throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Struct %s has %d fields. Found index: %d".format(
                  t, t.fieldTys.length, ii)))
              }
              val nir = mc.getFieldIRef(iref, index.toInt)
              (nir, None)
            }
            case t: TypeHybrid => {
              val ii = index.toInt
              if (ii < 0 || ii > t.fieldTys.length) {
                throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Hybrid %s has %d fields. Found index: %d".format(
                  t, t.fieldTys.length, ii)))
              }
              if (ii == t.fieldTys.length) {
                val nir = mc.getVarPartIRef(iref)
                // For debug purpose, we keep the upperbound recorded. Out-of-bound access has undefined behaviour.
                val len = HeaderUtils.getVarLength(iref.vb.objRef)
                (nir, Some(len))
              } else {
                val nir = mc.getFieldIRef(iref, index.toInt)
                (nir, None)
              }
            }
            case t: AbstractSeqType => {
              val ii = index.toLong
              if (ii < 0 || ii >= t.len) {
                throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Sequence type %s has %d elements. Found index: %d".format(
                  t, t.len, ii)))
              }
              val hII = mc.handleFromInt(ii, 64)
              val nir = mc.getElemIRef(iref, hII)
              mc.deleteValue(hII)
              (nir, None)
            }
          }
        }
        case Some(l) => { // in the var-part of a hybrid
          val ii = index.toLong
          if (ii < 0 || ii >= l) {
            throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Hybrid %s has %d actual var-part elements. Found index: %d".format(
              iref.ty, l, ii)))
          }
          val hII = mc.handleFromInt(ii, 64)
          val nir = mc.shiftIRef(iref, hII)
          mc.deleteValue(hII)
          (nir, None)
        }
      }

      new LValue(newIRef, newVarLen, baseCtx, ctx)
    }
  }

  object LValue {
    def forName(name: String, baseCtx: ParserRuleContext)(implicit mc: MuCtx, hailObjMap: HailObjMap): LValue = {
      val base = name.charAt(0) match {
        case '@' => {
          val global = microVM.globalBundle.globalCellNs.get(name).getOrElse {
            throw new UvmHailParsingException(inCtx(baseCtx, "Global cell %s not found".format(name)))
          }
          val gc = mc.handleFromGlobal(global.id)
          gc
        }
        case '$' => {
          val ref = hailObjMap.getOrElse(name, {
            throw new UvmHailParsingException(inCtx(baseCtx, "HAIL name %s not defined. It needs to be defined BEFORE this".format(name)))
          })
          val iref = mc.getIRef(ref)
          iref
        }
      }

      new LValue(base, None, baseCtx, baseCtx)
    }
  }

  def evalLValue(lv: LValueContext)(implicit mc: MuCtx, hailObjMap: HailObjMap): LValue = {
    val base = LValue.forName(lv.nam.getText, lv)

    var cur: LValue = base

    for (ic <- lv.indices) {
      val index = evalIntExpr(ic.intExpr()).toLong
      val newCur = cur.indexInto(index, ic)
      val oldCur = cur
      cur = newCur
      mc.deleteValue(oldCur.iref)
    }

    cur
  }

  def assign(lv: LValue, rv: RValueContext)(implicit mc: MuCtx, hailObjMap: HailObjMap): Unit = {
    val lir = lv.iref
    val lty = lir.ty.ty // LValue referent type.

    // error reporting

    def unexpectedRValueError(): Nothing = {
      throw new UvmHailParsingException(inCtx(rv, "Unsuitable RValue for LValue type %s".format(lty)))
    }

    def unexpectedGlobalTypeError(gv: GlobalVariable): Nothing = {
      throw new UvmHailParsingException(inCtx(rv, "Unsuitable global variable type. Expected: %s, Found: %s".format(
        lty, TypeInferer.inferType(gv))))
    }

    // Reused by their types as well as tagref64

    def resForDoubleType(rv: RValueContext)(implicit mc: MuCtx): MuDoubleValue = {
      rv match {
        case fl: RVDoubleContext         => mc.handleFromDouble(fl.doubleLiteral)
        case RVGlobalOf(gv: ConstDouble) => mc.handleFromConst(gv.id).asInstanceOf[MuDoubleValue]
        case RVGlobalOf(gv)              => unexpectedGlobalTypeError(gv)
        case _                           => unexpectedRValueError()
      }
    }

    def resForIntType(rv: RValueContext, len: Int)(implicit mc: MuCtx): MuIntValue = {
      rv match {
        case il: RVIntContext                         => mc.handleFromInt(il.intLiteral, len)
        case RVGlobalOf(gv @ ConstInt(_: TypeInt, _)) => mc.handleFromConst(gv.id).asInstanceOf[MuIntValue]
        case RVGlobalOf(gv)                           => unexpectedGlobalTypeError(gv)
        case _                                        => unexpectedRValueError()
      }
    }

    def resForRefType(rv: RValueContext)(implicit mc: MuCtx, hailObjMap: HailObjMap): (MuRefValue, Boolean) = {
      rv match {
        case nu: RVNullContext    => (mc.handleFromConst(InternalTypes.NULL_REF_VOID.id).asInstanceOf[MuRefValue], true)
        case hr: RVHailRefContext => (resRVHailRef(hr), false)
        case _                    => unexpectedRValueError()
      }
    }

    def resForTagRef64Type(rv: RValueContext)(implicit mc: MuCtx, hailObjMap: HailObjMap): MuTagRef64Value = {
      val (kind, hv) = rv match {
        case fl: RVDoubleContext         => (1, mc.handleFromDouble(fl.doubleLiteral))
        case RVGlobalOf(gv: ConstDouble) => (1, mc.handleFromConst(gv.id).asInstanceOf[MuDoubleValue])
        case il: RVIntContext            => (2, mc.handleFromInt(il.intLiteral, 52))
        case RVGlobalOf(gv: ConstInt)    => (2, mc.handleFromConst(gv.id).asInstanceOf[MuIntValue])
        case lst: RVListContext => {
          val elems = lst.list.rv.toSeq
          elems match {
            case Seq(rv1, rv2) => {
              val (hr, del) = resForRefType(rv1)
              val ht = resForIntType(rv2, 6)
              val r = mc.tr64FromRef(hr, ht)
              if (del) mc.deleteValue(hr)
              mc.deleteValue(ht)
              (3, r)
            }
            case _ => unexpectedRValueError()
          }
        }
        case RVGlobalOf(gv) => unexpectedGlobalTypeError(gv)
        case _              => unexpectedRValueError()
      }

      kind match {
        case 1 => { val r = mc.tr64FromFp(hv.asInstanceOf[MuDoubleValue]); mc.deleteValue(hv); r }
        case 2 => { val r = mc.tr64FromInt(hv.asInstanceOf[MuIntValue]); mc.deleteValue(hv); r }
        case 3 => { hv.asInstanceOf[MuTagRef64Value] }
      }
    }

    // actual assigning

    lty match {
      case TypeInt(len) => {
        val hi = resForIntType(rv, len)
        mc.store(NOT_ATOMIC, lir, hi)
        mc.deleteValue(hi)
      }

      case TypeUPtr(_) => {
        val hi = rv match {
          case il: RVIntContext                          => mc.handleFromInt(il.intLiteral, 64)
          case RVGlobalOf(gv @ ConstInt(_: TypeUPtr, _)) => mc.handleFromConst(gv.id).asInstanceOf[MuUPtrValue]
          case RVGlobalOf(gv)                            => unexpectedGlobalTypeError(gv)
          case _                                         => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hi)
        mc.deleteValue(hi)
      }
      case TypeUFuncPtr(_) => {
        val hi = rv match {
          case il: RVIntContext => mc.handleFromInt(il.intLiteral, 64)
          case RVGlobalOf(gv @ ConstInt(_: TypeUFuncPtr, _)) => mc.handleFromConst(gv.id).asInstanceOf[MuUFPValue]
          case RVGlobalOf(gv) => unexpectedGlobalTypeError(gv)
          case _ => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hi)
        mc.deleteValue(hi)
      }

      case TypeFloat() => {
        val hf = rv match {
          case fl: RVFloatContext         => mc.handleFromFloat(fl.floatLiteral)
          case RVGlobalOf(gv: ConstFloat) => mc.handleFromConst(gv.id)
          case RVGlobalOf(gv)             => unexpectedGlobalTypeError(gv)
          case _                          => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hf)
        mc.deleteValue(hf)
      }
      case TypeDouble() => {
        val hf = resForDoubleType(rv)
        mc.store(NOT_ATOMIC, lir, hf)
        mc.deleteValue(hf)
      }
      case _: AbstractObjRefType => {
        val (hr, del) = resForRefType(rv)
        mc.store(NOT_ATOMIC, lir, hr)
        if (del) mc.deleteValue(hr)
      }
      case t: TypeIRef => {
        val hr = rv match {
          case nu: RVNullContext          => mc.handleFromConst(InternalTypes.NULL_IREF_VOID.id)
          case io: RVIRefOfContext        => evalLValue(io.lValue()).iref
          case RVGlobalOf(gv: GlobalCell) => mc.handleFromConst(gv.id)
          case RVGlobalOf(gv)             => unexpectedGlobalTypeError(gv)
          case _                          => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hr)
        mc.deleteValue(hr)
      }
      case t: TypeFuncRef => {
        val hr = rv match {
          case nu: RVNullContext        => mc.handleFromConst(InternalTypes.NULL_FUNCREF_VV.id)
          case RVGlobalOf(gv: Function) => mc.handleFromConst(gv.id)
          case RVGlobalOf(gv)           => unexpectedGlobalTypeError(gv)
          case _                        => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hr)
        mc.deleteValue(hr)
      }
      case t: TypeThreadRef => {
        val hr = rv match {
          case nu: RVNullContext => mc.handleFromConst(InternalTypes.NULL_THREADREF.id)
          case _                 => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hr)
        mc.deleteValue(hr)
      }
      case t: TypeStackRef => {
        val hr = rv match {
          case nu: RVNullContext => mc.handleFromConst(InternalTypes.NULL_STACKREF.id)
          case _                 => unexpectedRValueError()
        }
        mc.store(NOT_ATOMIC, lir, hr)
        mc.deleteValue(hr)
      }
      case t: TypeTagRef64 => {
        val hr = resForTagRef64Type(rv)
        mc.store(NOT_ATOMIC, lir, hr)
        mc.deleteValue(hr)
      }
      case _: AbstractCompositeType => {
        rv match {
          case lst: RVListContext => {
            val elems = lst.list.rv.toSeq
            for ((innerRv, i) <- elems.zipWithIndex) {
              val innerLv = lv.indexInto(i, innerRv)
              assign(innerLv, innerRv)
            }
          }
          case _ => unexpectedRValueError()
        }
      }
      case _ => unexpectedRValueError()
    }
  }

  def evalIntExpr(ie: IntExprContext): BigInt = {
    ie match {
      case i: IntLitContext    => IntLiteralToBigInt(i.intLiteral())
      case i: IntGlobalContext => resConstInt(i)
    }
  }

  val IntRe = """([+-]?)(0x|0|)([0-9a-fA-F]*)""".r

  implicit def IntLiteralToBigInt(il: IntLiteralContext): BigInt = {
    val txt = il.getText()

    txt match {
      case IntRe(sign, prefix, nums) => {
        val neg = sign match {
          case "+" => false
          case "-" => true
          case ""  => false
        }
        val abs = prefix match {
          case "0x" => BigInt(nums, 16)
          case "0"  => if (nums == "") BigInt(0) else BigInt(nums, 8)
          case ""   => BigInt(nums, 10)
        }
        return if (neg) -abs else abs
      }
    }
  }

  implicit def floatLiteralToFloat(fl: FloatLiteralContext): Float = fl match {
    case num: FloatNumberContext => num.FP_NUM.getText.toFloat
    case fi: FloatInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Float.NEGATIVE_INFINITY
      else java.lang.Float.POSITIVE_INFINITY
    }
    case _: FloatNanContext     => java.lang.Float.NaN
    case bits: FloatBitsContext => java.lang.Float.intBitsToFloat(bits.intLiteral().intValue())
  }

  implicit def doubleLiteralToDouble(dl: DoubleLiteralContext): Double = dl match {
    case num: DoubleNumberContext => num.FP_NUM.getText.toDouble
    case fi: DoubleInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Double.NEGATIVE_INFINITY
      else java.lang.Double.POSITIVE_INFINITY
    }
    case _: DoubleNanContext     => java.lang.Double.NaN
    case bits: DoubleBitsContext => java.lang.Double.longBitsToDouble(bits.intLiteral().longValue())
  }

  def catchIn[T](ctx: ParserRuleContext, s: String)(func: => T): T = try {
    func
  } catch {
    case e: UvmHailParsingException => throw new UvmHailParsingException(inCtx(ctx, e.getMessage), e)
    case e: Exception               => throw new UvmHailParsingException(inCtx(ctx, s), e)
  }

  implicit def resTy(ctx: TypeContext): Type = catchIn(ctx, "Unable to resolve type") { resTyByName(ctx.getText) }
  private def resTyByName(name: String): Type = microVM.globalBundle.typeNs(name)

  implicit def resConstInt(ctx: IntGlobalContext): BigInt = catchIn(ctx, "Unable to resolve constant int") { resConstIntByName(ctx.getText) }
  private def resConstIntByName(name: String): BigInt = {
    val const = microVM.globalBundle.constantNs.get(name).getOrElse {
      throw new UvmHailParsingException("Type %s not found".format(name))
    }

    const match {
      case ConstInt(ty, num) => num
      case _                 => throw new UvmHailParsingException("Expected constant int. Found %s: ty=".format(const.repr, const.constTy))
    }
  }

  def resRVGlobal(ctx: RVGlobalContext): GlobalVariable = {
    val name = ctx.GLOBAL_NAME.getText
    val gv = microVM.globalBundle.globalVarNs.get(name).getOrElse {
      throw new UvmHailParsingException(inCtx(ctx, "Global variable %s not found".format(name)))
    }
    gv
  }

  def resRVHailRef(hrc: RVHailRefContext)(implicit mc: MuCtx, hailObjMap: HailObjMap) = {
    val name = hrc.HAIL_NAME().getText
    val href = hailObjMap.getOrElse(name, {
      throw new UvmHailParsingException(inCtx(hrc, "HAIL name %s not defined. It needs to be defined BEFORE this".format(name)))
    })
    href
  }

  def rvListCtxToSeq(rvc: RVListContext): Seq[RValueContext] = {
    rvc.list.rv.toSeq
  }

  object RVGlobalOf {
    def unapply(g: RVGlobalContext): Option[GlobalVariable] = {
      val gv = resRVGlobal(g)
      Some(gv)
    }
  }

}