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

  def read(ir: String): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast)
  }

  def read(ir: java.io.Reader): Bundle = {
    val input = new ANTLRInputStream(ir)
    val lexer = new uIRLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new uIRParser(tokens)
    val ast = parser.ir();
    read(ast)
  }

  object IDFactory {
    private var id: Int = 65536
    def getID(): Int = {
      val myID = id
      id = id + 1
      return myID
    }
  }

  class Later {
    var jobs: List[() => Unit] = Nil
    def apply(job: () => Unit) {
      jobs = job :: jobs
    }

    def doAll() {
      while (!jobs.isEmpty) {
        val job = jobs.head
        jobs = jobs.tail
        job()
      }
    }

    def isEmpty: Boolean = jobs.isEmpty
  }

  implicit class Laterable[T](val anything: T) {
    def later(lat: Later)(job: T => Unit): T = {
      lat(() => job(anything))
      anything
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
    il match {
      case dec: DecIntLiteralContext => BigInt(txt.substring(beg), 10)
      case oct: OctIntLiteralContext => BigInt(txt.substring(beg + 1), 8)
      case hex: HexIntLiteralContext => BigInt(txt.substring(beg + 2), 16)
    }
  }

  implicit def floatLiteralToFloat(fl: FloatLiteralContext): Float = fl match {
    case num: FloatNumberContext => num.FP_NUM().getText().toFloat
    case bits: FloatBitsContext => java.lang.Float.intBitsToFloat(bits.intLiteral().intValue())
    case _ => fl.getText() match {
      case "nan" => java.lang.Float.NaN
      case "-inf" => java.lang.Float.NEGATIVE_INFINITY
      case "+inf" => java.lang.Float.POSITIVE_INFINITY
    }
  }
  implicit def doubleLiteralToDouble(dl: DoubleLiteralContext): Double = dl match {
    case num: DoubleNumberContext => num.FP_NUM().getText().toDouble
    case bits: DoubleBitsContext => java.lang.Double.longBitsToDouble(bits.intLiteral().longValue())
    case _ => dl.getText() match {
      case "nan" => java.lang.Double.NaN
      case "-inf" => java.lang.Double.NEGATIVE_INFINITY
      case "+inf" => java.lang.Double.POSITIVE_INFINITY
    }
  }

  def read(ir: IrContext): Bundle = {

    val bundle = new Bundle()

    val phase1 = new Later()

    def resTy(te: TypeContext): Type = te match {
      case rt: ReferencedTypeContext => bundle.typeNs(rt.GLOBAL_ID().getText())
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
      case rfs: ReferencedFuncSigContext => bundle.funcSigNs(rfs.GLOBAL_ID())
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

    ir.metaData().foreach { rule =>
      rule.getChild(0) match {
        case td: TypeDefContext => {
          val ty = mkType(td.typeConstructor())
          ty.name = Some(td.GLOBAL_ID())
          bundle.typeNs.add(ty)
        }
        case fsd: FuncSigDefContext => {
          val sig = mkSig(fsd.funcSigConstructor())
          sig.name = Some(fsd.GLOBAL_ID())
          bundle.funcSigNs.add(sig)
        }
        case _ =>
      }
    }

    phase1.doAll()

    return bundle
  }
}