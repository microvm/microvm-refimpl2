package uvm.ir.textinput

import scala.collection.JavaConversions._

import uvm._
import uvm.types._
import uvm.ssavalues._
import uvm.ir.textinput.gen._
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.TerminalNode
import uIRParser._

object AntlrUvmIRReader {
  import Later.Laterable

  def read(ir: String, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast, globalBundle)
  }

  def read(ir: java.io.Reader, globalBundle: Bundle): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast, globalBundle)
  }

  object IDFactory {
    private var id: Int = 65536
    def getID(): Int = {
      val myID = id
      id = id + 1
      return myID
    }
  }

  implicit def terminalToString(tn: TerminalNode): String = tn.getText()

  implicit def IntLiteralToBigInt(il: IntLiteralContext): BigInt = {
    val txt = il.getText()
    val (neg, beg) = txt(0) match {
      case '+' => (false, 1)
      case '-' => (true, 1)
      case _ => (false, 0)
    }
    val abs = il match {
      case dec: DecIntLiteralContext => BigInt(txt.substring(beg), 10)
      case oct: OctIntLiteralContext => BigInt(txt.substring(beg), 8)
      case hex: HexIntLiteralContext => BigInt(txt.substring(beg + 2), 16)
    }
    return if (neg) -abs else abs
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

  def read(ir: IrContext, globalBundle: Bundle): Bundle = {

    val bundle = new Bundle()

    val phase1 = new Later()

    def resTy(te: TypeContext): Type = te match {
      case rt: ReferencedTypeContext => bundle.typeNs.get(rt.GLOBAL_ID()).getOrElse(globalBundle.typeNs(rt.GLOBAL_ID()))
      case it: InLineTypeContext => mkType(it.typeConstructor())
    }

    def mkType(tc: TypeConstructorContext): Type = {
      val ty = tc match {
        case it: IntTypeContext => TypeInt(it.intLiteral().intValue())
        case ft: FloatTypeContext => TypeFloat()
        case dt: DoubleTypeContext => TypeDouble()
        case rt: RefTypeContext => TypeRef(null).later(phase1) { _.ty = resTy(rt.`type`()) }
        case irt: IRefTypeContext => TypeIRef(null).later(phase1) { _.ty = resTy(irt.`type`()) }
        case wrt: WeakRefTypeContext => TypeWeakRef(null).later(phase1) { _.ty = resTy(wrt.`type`()) }
        case st: StructTypeContext => TypeStruct(null).later(phase1) { _.fieldTy = st.`type`().map(resTy) }
        case at: ArrayTypeContext => TypeArray(null, at.intLiteral().longValue()).later(phase1) { _.elemTy = resTy(at.`type`()) }
        case ht: HybridTypeContext => TypeHybrid(null, null).later(phase1) { t => t.fixedPart = resTy(ht.`type`(0)); t.varPart = resTy(ht.`type`(1)) }
        case vt: VoidTypeContext => TypeVoid()
        case ft: FuncTypeContext => TypeFunc(null).later(phase1) { _.sig = resSig(ft.funcSig()) }
        case thr: ThreadTypeContext => TypeThread()
        case sta: StackTypeContext => TypeStack()
        case tr64: TagRef64TypeContext => TypeTagRef64()
        case _ => throw new TextIRParsingException("foo")
      }
      ty.id = IDFactory.getID()
      return ty
    }

    def resSig(fs: FuncSigContext): FuncSig = fs match {
      case rfs: ReferencedFuncSigContext => bundle.funcSigNs.get(rfs.GLOBAL_ID()).getOrElse(globalBundle.funcSigNs(rfs.GLOBAL_ID()))
      case ilfs: InLineFuncSigContext => mkSig(ilfs.funcSigConstructor())
    }

    def mkSig(fsc: FuncSigConstructorContext): FuncSig = {
      val sig = FuncSig(null, null).later(phase1) { sig =>
        sig.retTy = resTy(fsc.`type`().head)
        sig.paramTy = fsc.`type`().tail.map(resTy)
      }
      sig.id = IDFactory.getID()
      return sig
    }

    ir.metaData.map(_.getChild(0)).foreach {
      case td: TypeDefContext => {
        val ty = mkType(td.typeConstructor)
        ty.name = Some(td.GLOBAL_ID)
        bundle.typeNs.add(ty)
      }
      case fsd: FuncSigDefContext => {
        val sig = mkSig(fsd.funcSigConstructor)
        sig.name = Some(fsd.GLOBAL_ID)
        bundle.funcSigNs.add(sig)
      }
      case _ =>
    }

    phase1.doAll()

    val phase2 = new Later()

    def resGV(t: Type, ce: ConstantContext): GlobalValue = ce match {
      case rcc: ReferencedConstContext => bundle.globalValueNs.get(rcc.GLOBAL_ID).getOrElse(globalBundle.globalValueNs(rcc.GLOBAL_ID))
      case icc: InLineConstContext => mkConst(t, icc.constExpr)
    }

    def mkConst(t: Type, c: ConstExprContext): DeclaredConstant = {
      val con = c match {
        case icc: IntConstContext => ConstInt(t, icc.intLiteral)
        case fcc: FloatConstContext => ConstFloat(t, fcc.floatLiteral)
        case dcc: DoubleConstContext => ConstDouble(t, dcc.doubleLiteral)
        case scc: StructConstContext => ConstStruct(t, null).later(phase2) {
          _.fields = for ((ft, f) <- t.asInstanceOf[TypeStruct].fieldTy.zip(scc.constant)) yield resGV(ft, f)
        }
        case _: NullConstContext => ConstNull(t)
      }
      con.id = IDFactory.getID()
      return con
    }

    def mkGlobalData(t: Type): GlobalData = {
      val gd = GlobalData(t)
      gd.id = IDFactory.getID()
      return gd
    }

    def mkGlobalDataConst(gd: GlobalData): ConstGlobalData = {
      val gdc = ConstGlobalData(gd)
      return gdc
    }

    def mkFunc(sig: FuncSig): Function = {
      val func = new Function()
      func.sig = sig
      return func
    }

    def mkFuncConst(func: Function): ConstFunc = {
      val fc = ConstFunc(func)
      return fc
    }

    def tryReuseFuncID(name: String): Option[Int] = {
      globalBundle.funcNs.get(name).map(_.id)
    }

    def declFunc(n: String, s: FuncSigContext): Function = {
      val sig = resSig(s)
      val func = mkFunc(sig)
      val maybeOldID = tryReuseFuncID(n)
      func.id = maybeOldID.getOrElse(IDFactory.getID())
      func.name = Some(n)
      bundle.funcNs.add(func)

      if (maybeOldID == None) {
        val fc = mkFuncConst(func)
        bundle.globalValueNs.add(fc)
      }

      return func
    }

    var funcDefs: List[(Function, Seq[String], FuncBodyContext)] = Nil

    ir.metaData.map(_.getChild(0)).foreach {
      case cdctx: ConstDefContext => {
        val ty = resTy(cdctx.`type`)
        val con = mkConst(ty, cdctx.constExpr)
        con.name = Some(cdctx.GLOBAL_ID)
        bundle.declConstNs.add(con)
        bundle.globalValueNs.add(con)
      }
      case gdctx: GlobalDefContext => {
        val ty = resTy(gdctx.`type`)
        val gd = mkGlobalData(ty)
        gd.name = Some(gdctx.GLOBAL_ID)
        bundle.globalDataNs.add(gd)

        val gdc = mkGlobalDataConst(gd)
        bundle.globalValueNs.add(gdc)
      }
      case fdecl: FuncDeclContext => {
        declFunc(fdecl.GLOBAL_ID, fdecl.funcSig)
      }
      case fdef: FuncDefContext => {
        val func = declFunc(fdef.GLOBAL_ID, fdef.funcSig)
        funcDefs = (func, fdef.paramList.LOCAL_ID.map(_.getText), fdef.funcBody) :: funcDefs
      }
      case _ => {}
    }

    phase2.doAll()

    return bundle
  }
}