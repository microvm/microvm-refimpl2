package uvm.refimpl.hail

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.ParserRuleContext
import uvm.ir.textinput.TextIRParsingException
import uvm.ir.textinput.gen.HAILLexer
import uvm.ir.textinput.gen.HAILParser
import uvm.ir.textinput.gen.HAILParser.DoubleBitsContext
import uvm.ir.textinput.gen.HAILParser.DoubleInfContext
import uvm.ir.textinput.gen.HAILParser.DoubleLiteralContext
import uvm.ir.textinput.gen.HAILParser.DoubleNanContext
import uvm.ir.textinput.gen.HAILParser.DoubleNumberContext
import uvm.ir.textinput.gen.HAILParser.FixedAllocContext
import uvm.ir.textinput.gen.HAILParser.FloatBitsContext
import uvm.ir.textinput.gen.HAILParser.FloatInfContext
import uvm.ir.textinput.gen.HAILParser.FloatLiteralContext
import uvm.ir.textinput.gen.HAILParser.FloatNanContext
import uvm.ir.textinput.gen.HAILParser.FloatNumberContext
import uvm.ir.textinput.gen.HAILParser.HailContext
import uvm.ir.textinput.gen.HAILParser.HybridAllocContext
import uvm.ir.textinput.gen.HAILParser.IntGlobalContext
import uvm.ir.textinput.gen.HAILParser.IntLitContext
import uvm.ir.textinput.gen.HAILParser.IntLiteralContext
import uvm.ir.textinput.gen.HAILParser.LValueContext
import uvm.ir.textinput.gen.HAILParser.MemInitContext
import uvm.ir.textinput.gen.HAILParser.RValueContext
import uvm.ir.textinput.gen.HAILParser.TypeContext
import uvm.refimpl.MicroVM
import uvm.refimpl.MuCtx
import uvm.refimpl.MuIRefValue
import uvm.refimpl.MuRefValue
import uvm.refimpl.UvmHailParsingException
import uvm.refimpl.UvmHailParsingException
import uvm.refimpl.UvmHailParsingException
import uvm.refimpl.UvmHailParsingException
import uvm.ssavariables.ConstInt
import uvm.types.Type
import uvm.types.TypeHybrid
import uvm.utils.AntlrHelpers.AccumulativeAntlrErrorListener
import uvm.utils.AntlrHelpers.inCtx
import uvm.ir.textinput.gen.HAILParser.IntExprContext
import uvm.refimpl.MuStructValue
import uvm.types.TypeStruct
import uvm.refimpl.UvmHailParsingException
import uvm.types.AbstractSeqType
import uvm.refimpl.mem.HeaderUtils
import uvm.refimpl.itpr.MemoryOperations
import uvm.refimpl.mem.MemorySupport

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

  def catchIn[T](ctx: ParserRuleContext, s: String)(func: => T): T = try {
    func
  } catch {
    case e: UvmHailParsingException => throw new UvmHailParsingException(inCtx(ctx, e.getMessage), e)
    case e: Exception => throw new UvmHailParsingException(inCtx(ctx, s), e)
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
      case _ => throw new UvmHailParsingException("Expected constant int. Found %s: ty=".format(const.repr, const.constTy))
    }
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

  /**
   * Index into an iref in a general way.
   * @param varLen: None if base is not the var part. Some(l) if base is the 0-th elem of the var part of a hybrid whose actual length is l.
   */
  private def generalIndex(base: MuIRefValue, index: Long, varLen: Option[Long])(
    implicit mc: MuCtx, ctx: ParserRuleContext): MuIRefValue = {
    varLen match {
      case None => { // not in the var-part of a hybrid
        base.ty.ty match {
          case t: TypeStruct => {
            val ii = index.toInt
            if (ii < 0 || ii >= t.fieldTys.length) {
              throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Struct %s has %d fields. Found index: %d".format(
                t, t.fieldTys.length, ii)))
            }
            mc.getFieldIRef(base, index.toInt)
          }
          case t: TypeHybrid => {
            val ii = index.toInt
            if (ii < 0 || ii > t.fieldTys.length) {
              throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Hybrid %s has %d fields. Found index: %d".format(
                t, t.fieldTys.length, ii)))
            }
            mc.getFieldIRef(base, index.toInt)
          }
          case t: AbstractSeqType => {
            val ii = index.toLong
            if (ii < 0 || ii >= t.len) {
              throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Sequence type %s has %d elements. Found index: %d".format(
                t, t.len, ii)))
            }
            val hII = mc.handleFromInt(ii, 64)
            val nc = mc.getElemIRef(base, hII)
            mc.deleteValue(hII)
            nc
          }
        }
      }
      case Some(l) => { // in the var-part of a hybrid
        val ii = index.toLong
        if (ii < 0 || ii >= l) {
          throw new UvmHailParsingException(inCtx(ctx, "Index out of bound. Hybrid %s has %d actual var-part elements. Found index: %d".format(
            base.ty, l, ii)))
        }
        val hII = mc.handleFromInt(ii, 64)
        val nc = mc.shiftIRef(base, hII)
        mc.deleteValue(hII)
        nc
      }
    }
  }

  def assign(lv: LValue, rv: RValueContext): Unit = {
    ???
  }

  def evalIntExpr(ie: IntExprContext): BigInt = {
    ie match {
      case i: IntLitContext => IntLiteralToBigInt(i.intLiteral())
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
          case "" => false
        }
        val abs = prefix match {
          case "0x" => BigInt(nums, 16)
          case "0" => if (nums == "") BigInt(0) else BigInt(nums, 8)
          case "" => BigInt(nums, 10)
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
    case _: FloatNanContext => java.lang.Float.NaN
    case bits: FloatBitsContext => java.lang.Float.intBitsToFloat(bits.intLiteral().intValue())
  }

  implicit def doubleLiteralToDouble(dl: DoubleLiteralContext): Double = dl match {
    case num: DoubleNumberContext => num.FP_NUM.getText.toDouble
    case fi: DoubleInfContext => {
      if (fi.getText.startsWith("-"))
        java.lang.Double.NEGATIVE_INFINITY
      else java.lang.Double.POSITIVE_INFINITY
    }
    case _: DoubleNanContext => java.lang.Double.NaN
    case bits: DoubleBitsContext => java.lang.Double.longBitsToDouble(bits.intLiteral().longValue())
  }
}