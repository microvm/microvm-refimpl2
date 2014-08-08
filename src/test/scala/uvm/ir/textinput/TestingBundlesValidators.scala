package uvm.ir.textinput

import org.scalatest._

import uvm._
import uvm.types._
import uvm.ssavalues._
import uvm.ifuncs._

trait TestingBundlesValidators extends Matchers with ExtraMatchers {
  
  implicit class MagicalOur(b: Bundle) {
    def ty = b.typeNs
    def const = b.globalValueNS
    def globalValue = b.globalValueNS
    def globalData = b.globalDataNS
    def sig = b.funcSigNs
    def func = b.funcNs
  }

  implicit class MagicalMy(c: CFG) {
    def bb = c.bbNs
    def localValue = c.lvNs
    def param = c.lvNs
    def inst = c.lvNs
  }

  def validateTypes(bundle: Bundle) {
    val our = bundle
    
    our ty "@i1" shouldBeATypeIntOf(1)
    our ty "@i8" shouldBeATypeIntOf(8)
    our ty "@i16" shouldBeATypeIntOf(16)
    our ty "@i32" shouldBeATypeIntOf(32)
    our ty "@i64" shouldBeATypeIntOf(64)
    
    our ty "@f" shouldBeA[TypeFloat] thatsIt
    our ty "@d" shouldBeA[TypeDouble] thatsIt
    
    our ty "@rv" shouldBeA[TypeRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    our ty "@irv" shouldBeA[TypeIRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    our ty "@wrv" shouldBeA[TypeWeakRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    
    our ty "@ri16" shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf(16)}
    our ty "@ri16_2" shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf(16)}

    our ty "@s0" shouldBeA[TypeStruct] { _.fieldTy shouldBe empty }
    our ty "@s1" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBeATypeIntOf(8)
      its fieldTy 1 shouldBeATypeIntOf(16)
      its fieldTy 2 shouldBeATypeIntOf(32)
      its fieldTy 3 shouldBeATypeIntOf(64)
      its fieldTy 4 shouldBeA[TypeFloat] thatsIt
      its fieldTy 5 shouldBeA[TypeDouble] thatsIt
      its fieldTy 6 shouldBeA[TypeRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 7 shouldBeA[TypeIRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 8 shouldBeA[TypeWeakRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 9 shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf(16)}
      its fieldTy 10 shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf(16)}
    }
    
    our ty "@cons" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBeATypeIntOf(64)
      its fieldTy 1 shouldBeA[TypeRef] { _.ty shouldBe (our ty "@cons")}
    }
    
    our ty "@a0" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBeATypeIntOf(8)
      its.len shouldEqual 100
    }
    our ty "@a1" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBeA[TypeDouble] thatsIt
        whose fieldTy 1 shouldBeATypeIntOf(64)
      }
      its.len shouldEqual 10
    }
    our ty "@a2" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBe (our ty "@a1")
      its.len shouldEqual 10
    }
    
    our ty "@h0" shouldBeA[TypeHybrid] { its =>
      its.fixedPart shouldBeA[TypeVoid] thatsIt
      its.varPart shouldBeATypeIntOf(8)
    }
    our ty "@h1" shouldBeA[TypeHybrid] { its =>
      its.fixedPart shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBe (our ty "@i32")
        whose fieldTy 1 shouldBe (our ty "@i32")
        whose fieldTy 2 shouldBeA[TypeFloat] thatsIt
      }
      its.varPart shouldBeATypeIntOf(64)
    }
    
    our ty "@v" shouldBeA[TypeVoid] thatsIt
    
    our ty "@f0" shouldBeA[TypeFunc] { its =>
      its.sig.retTy shouldBeA[TypeVoid] thatsIt
      its.sig.paramTy shouldBe empty
    }
    our ty "@f1" shouldBeA[TypeFunc] { its =>
      its.sig.retTy shouldBeATypeIntOf(32)
      its.sig paramTy 0 shouldBeATypeIntOf(32)
      its.sig.paramTy(1) shouldBeA[TypeIRef] { _.ty shouldBeA[TypeIRef] {
        _.ty shouldBeATypeIntOf(8)}
      }
    }
    
    our ty "@th" shouldBeA[TypeThread] thatsIt
    our ty "@st" shouldBeA[TypeStack] thatsIt
    our ty "@tr64" shouldBeA[TypeTagRef64] thatsIt
  }
  
  def validateConstants(bundle: Bundle) {
    val our = bundle

    our const "@ci8" shouldBeAConstIntOf (8, 127)
    our const "@ci16" shouldBeAConstIntOf (16, 32767)
    our const "@ci32" shouldBeAConstIntOf (32, 2147483647)
    our const "@ci64" shouldBeAConstIntOf (64, 9223372036854775807L)
    
    our const "@ci64neg" shouldBeAConstIntOf (64, BigInt(-42L) & 0xffffffffffffffffL)
    
    our const "@cio64" shouldBeAConstIntOf (64, BigInt("777", 8))
    
    our const "@cix64" shouldBeAConstIntOf (64, BigInt("123456789abcdef0", 16))
    our const "@cixovf" shouldBeAConstIntOf (64, BigInt("ffffffffffffffff", 16))
    our const "@cixovf2" shouldBeAConstIntOf (64, BigInt("8000000000000000", 16))
    
    our const "@cf" shouldBeAConstFloatOf 3.14F
    our const "@cfnan" shouldBeAConstFloatOf nan
    our const "@cfninf" shouldBeAConstFloatOf Float.NegativeInfinity 
    our const "@cfpinf" shouldBeAConstFloatOf Float.PositiveInfinity 
    our const "@cfbits" shouldBeAConstFloatOf exactly(bitsf(0x12345678))
  
    our const "@cd" shouldBeAConstDoubleOf 6.28D
    our const "@cdnan" shouldBeAConstDoubleOf nan
    our const "@cdninf" shouldBeAConstDoubleOf Double.NegativeInfinity 
    our const "@cdpinf" shouldBeAConstDoubleOf Double.PositiveInfinity 
    our const "@cdbits" shouldBeAConstDoubleOf exactly(bitsd(0xfedcba9876543210L))
 
    our const "@cs1" shouldBeA[ConstStruct] { its=>
      its.constTy shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBeATypeIntOf(64)
        whose fieldTy 1 shouldBeA[TypeDouble] thatsIt
      }
      its fields 0 shouldBeAConstIntOf (64, 100)
      its fields 1 shouldBeAConstDoubleOf 200.0D
    }
    
    our const "@cs2" shouldBeA[ConstStruct] { its=>
      its.constTy shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBeA[TypeDouble] thatsIt
        whose fieldTy 1 shouldBeA[TypeStruct] { where =>
          where fieldTy 0 shouldBeA[TypeFloat] thatsIt
          where fieldTy 1 shouldBeATypeIntOf(64)
        }
        whose fieldTy 2 shouldBeATypeIntOf(32)
      }
      its fields 0 shouldBeAConstDoubleOf 1.0D
      its fields 1 shouldBeA[ConstStruct] { whose =>
        whose fields 0 shouldBeAConstFloatOf 2.0F
        whose fields 1 shouldBeAConstIntOf (64, 3)
      }
      its fields 2 shouldBeAConstIntOf (32, 4)
    }
    
    our const "@cons" shouldBeA[ConstStruct] { its =>
      its.constTy shouldBe (our ty "@Cons")
      its fields 0 shouldBeAConstIntOf (64, 42)
      its fields 1 shouldBeA[ConstNull] { _.ty shouldBeA[TypeRef] { _.ty shouldBe (our ty "@Cons") }}
    }
    
    our const "@cr" shouldBeA[ConstNull] { _.ty shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf(64) }}
    our const "@cir" shouldBeA[ConstNull] { _.ty shouldBeA[TypeIRef] { _.ty shouldBeA[TypeFloat] thatsIt }}
    our const "@cwr" shouldBeA[ConstNull] { _.ty shouldBeA[TypeWeakRef] { _.ty shouldBe (our ty "@Cons") }}
    our const "@cfu" shouldBeA[ConstNull] { _.ty shouldBeA[TypeFunc] { _.sig shouldBeA[FuncSig] { its =>
      its.retTy shouldBeA[TypeVoid] thatsIt
      its.paramTy shouldBe empty
    }}}
    our const "@cth" shouldBeA[ConstNull] { _.ty shouldBeA[TypeThread] thatsIt }
    our const "@cst" shouldBeA[ConstNull] { _.ty shouldBeA[TypeStack] thatsIt }
    
    our globalData "@gi64" shouldBeA[GlobalData] { _.ty shouldBeATypeIntOf(64) }

    our func "@fdummy" shouldBeA[Function] { _.sig shouldBeA[FuncSig] { its =>
      its.retTy shouldBeA[TypeVoid] thatsIt
      its.paramTy shouldBe empty
    }}
  }
  def validateFunctions(bundle: Bundle) {
    val our = bundle
    
    our sig "@foo" shouldBeA[FuncSig] { its =>
      its.retTy shouldBeA[TypeVoid] thatsIt
      its.paramTy shouldBe empty
    }

    our sig "@bar" shouldBeA[FuncSig] { its =>
      its.retTy shouldBeATypeIntOf 64
      its paramTy 0 shouldBeATypeIntOf 32
      its paramTy 1 shouldBeATypeIntOf 16
    }
    
    our sig "@baz" shouldBeA[FuncSig] { its =>
      its.retTy shouldBeATypeIntOf 32
      its paramTy 0 shouldBeATypeIntOf 32
      its paramTy 1 shouldBeA[TypeIRef] { _.ty shouldBeA[TypeIRef] {
        _.ty shouldBeATypeIntOf 8
      }}
    }
    
    our sig "@sig_fs" shouldBeA[FuncSig] { its =>
      its.retTy shouldBeA[TypeVoid] thatsIt
      its paramTy 0 shouldBeATypeIntOf 32
    }
    
    our ty "@sig_t" shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@sig_fs") }

    our func "@signal" shouldBeA[Function] { its =>
      its.cfg shouldBe None
      its.sig shouldBeA[FuncSig] { whose =>
        whose.retTy shouldBe (our ty "@sig_t")
        whose paramTy 0 shouldBeATypeIntOf 32
        whose paramTy 1 shouldBe (our ty "@sig_t")
      }
    }
    
    our const "@zero" shouldBeA[ConstInt] { its =>
      its.constTy shouldBeATypeIntOf 32
      its.num shouldBe 0
    }
    
    our func "@main" shouldBeA[Function] { theFunc =>
      val its = theFunc
      its.sig shouldBe (our sig "@baz")
      its.cfg shouldNot be(None)
      its.cfg.get.func shouldBe theFunc
      its.cfg.get.params(0).name shouldBe Some("%argc")
      its.cfg.get.params(0).sig shouldBe its.sig
      its.cfg.get.params(0).index shouldBe 0
      its.cfg.get.params(1).name shouldBe Some("%argv")
      its.cfg.get.params(1).sig shouldBe its.sig
      its.cfg.get.params(1).index shouldBe 1
      its.cfg.get.entry.name shouldBe Some("%entry")
      its.cfg.get.bbs.size shouldBe 1
      
    }
    
    our func "@implicitentry" shouldBeA[Function] { its =>
      its.cfg shouldNot be(None)
      its.sig shouldBe (our sig "@foo")
      its.cfg.get.params shouldBe empty
      its.cfg.get.entry.name shouldBe None
      its.cfg.get.bbs.size shouldBe 1
    }
  }
  
  def in(func: Function)(f: (Function, CFG) => Unit) {
    val cfg = func.cfg.get
    f(func, cfg)
  }
  
  def validateInstructions(bundle: Bundle) {
    val our = bundle
    
    in (our func "@intBinOpTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%add"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.ADD }
      my inst "%sub"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.SUB }
      my inst "%mul"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.MUL }
      my inst "%udiv" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.UDIV}
      my inst "%sdiv" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.SDIV}
      my inst "%urem" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.UREM}
      my inst "%srem" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.SREM}
      my inst "%shl"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.SHL }
      my inst "%lshr" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.LSHR}
      my inst "%ashr" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.ASHR}
      my inst "%and"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.AND }
      my inst "%or"   shouldBeA[InstBinOp] { _.op shouldBe BinOptr.OR  }
      my inst "%xor"  shouldBeA[InstBinOp] { _.op shouldBe BinOptr.XOR }
      
      for (i <- my.inst.all; if i.isInstanceOf[InstBinOp]) {
        i.asInstanceOf[InstBinOp].opndTy shouldBeATypeIntOf 32
        i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
        i.asInstanceOf[InstBinOp].ty shouldBeATypeIntOf 32
      }
    }   
 
    in (our func "@fpBinOpTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%fadd" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FADD }
      my inst "%fsub" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FSUB }
      my inst "%fmul" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FMUL }
      my inst "%fdiv" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FDIV }
      my inst "%frem" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FREM }
      
      for (i <- my.inst.all; if i.isInstanceOf[InstBinOp]) {
        i.asInstanceOf[InstBinOp].opndTy shouldBeA[TypeDouble] thatsIt
        i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
        i.asInstanceOf[InstBinOp].ty shouldBeA[TypeDouble] thatsIt
      }
    }   
    
    in (our func "@intCmpTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%eq"  shouldBeA[InstCmp] { _.op shouldBe CmpOptr.EQ  }
      my inst "%ne"  shouldBeA[InstCmp] { _.op shouldBe CmpOptr.NE  }
      my inst "%ult" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.ULT }
      my inst "%ule" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.ULE }
      my inst "%ugt" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.UGT }
      my inst "%uge" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.UGE }
      my inst "%slt" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.SLT }
      my inst "%sle" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.SLE }
      my inst "%sgt" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.SGT }
      my inst "%sge" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.SGE }
      
      for (i <- my.inst.all; if i.isInstanceOf[InstCmp]) {
        i.asInstanceOf[InstCmp].opndTy shouldBeATypeIntOf 64
        i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
        i.asInstanceOf[InstCmp].ty shouldBeATypeIntOf 1
      }
    }   
    
    in (our func "@fpCmpTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%ftrue"  shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FTRUE }
      my inst "%ffalse" shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FFALSE }
      my inst "%ford"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FORD }
      my inst "%foeq"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FOEQ }
      my inst "%fone"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FONE }
      my inst "%folt"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FOLT }
      my inst "%fole"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FOLE }
      my inst "%fogt"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FOGT }
      my inst "%foge"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FOGE }
      my inst "%funo"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FUNO }
      my inst "%fueq"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FUEQ }
      my inst "%fune"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FUNE }
      my inst "%fult"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FULT }
      my inst "%fule"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FULE }
      my inst "%fugt"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FUGT }
      my inst "%fuge"   shouldBeA[InstCmp] { _.op shouldBe CmpOptr.FUGE }
      
      for (i <- my.inst.all; if i.isInstanceOf[InstCmp]) {
        i.asInstanceOf[InstCmp].opndTy shouldBeA[TypeFloat] thatsIt
        i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
        i.asInstanceOf[InstCmp].ty shouldBeATypeIntOf 1
      }
    }
    
    in (our func "@convTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%trunc" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.TRUNC
        its.fromTy shouldBeATypeIntOf 64
        its.toTy   shouldBeATypeIntOf 32
        its.opnd   shouldBe (my param "%p1")
        its.ty     shouldBeATypeIntOf 32
      }
      my inst "%zext" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.ZEXT 
        its.fromTy shouldBeATypeIntOf 32
        its.toTy   shouldBeATypeIntOf 64
        its.opnd   shouldBe (my param "%p0")
        its.ty     shouldBeATypeIntOf 64
      }
      my inst "%sext" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.SEXT
        its.fromTy shouldBeATypeIntOf 32
        its.toTy   shouldBeATypeIntOf 64
        its.opnd   shouldBe (my param "%p0")
        its.ty     shouldBeATypeIntOf 64
      }
      my inst "%fptrunc" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.FPTRUNC
        its.fromTy shouldBeA[TypeDouble] thatsIt
        its.toTy   shouldBeA[TypeFloat] thatsIt
        its.opnd   shouldBe (my param "%p3")
        its.ty     shouldBeA[TypeFloat] thatsIt
      }
      my inst "%fpext" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.FPEXT 
        its.fromTy shouldBeA[TypeFloat] thatsIt
        its.toTy   shouldBeA[TypeDouble] thatsIt
        its.opnd   shouldBe (my param "%p2")
        its.ty     shouldBeA[TypeDouble] thatsIt
      }
      my inst "%fptoui" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.FPTOUI
        its.fromTy shouldBeA[TypeDouble] thatsIt
        its.toTy   shouldBeATypeIntOf 64
        its.opnd   shouldBe (my param "%p3")
        its.ty     shouldBeATypeIntOf 64
      }
      my inst "%fptosi" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.FPTOSI
        its.fromTy shouldBeA[TypeDouble] thatsIt
        its.toTy   shouldBeATypeIntOf 64
        its.opnd   shouldBe (my param "%p3")
        its.ty     shouldBeATypeIntOf 64
      }
      my inst "%uitofp" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.UITOFP
        its.fromTy shouldBeATypeIntOf 64
        its.toTy   shouldBeA[TypeDouble] thatsIt
        its.opnd   shouldBe (my param "%p1")
        its.ty     shouldBeA[TypeDouble] thatsIt
      }
      my inst "%sitofp" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.SITOFP 
        its.fromTy shouldBeATypeIntOf 64
        its.toTy   shouldBeA[TypeDouble] thatsIt
        its.opnd   shouldBe (my param "%p1")
        its.ty     shouldBeA[TypeDouble] thatsIt
      }
      my inst "%bitcast0" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.BITCAST 
        its.fromTy shouldBeATypeIntOf 32
        its.toTy   shouldBeA[TypeFloat] thatsIt
        its.opnd   shouldBe (my param "%p0")
        its.ty     shouldBeA[TypeFloat] thatsIt
      }
      my inst "%bitcast1" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.BITCAST 
        its.fromTy shouldBeATypeIntOf 64
        its.toTy   shouldBeA[TypeDouble] thatsIt
        its.opnd   shouldBe (my param "%p1")
        its.ty     shouldBeA[TypeDouble] thatsIt
      }
      my inst "%bitcast2" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.BITCAST 
        its.fromTy shouldBeA[TypeFloat] thatsIt
        its.toTy   shouldBeATypeIntOf 32
        its.opnd   shouldBe (my param "%p2")
        its.ty     shouldBeATypeIntOf 32
      }
      my inst "%bitcast3" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.BITCAST 
        its.fromTy shouldBeA[TypeDouble] thatsIt
        its.toTy   shouldBeATypeIntOf 64
        its.opnd   shouldBe (my param "%p3")
        its.ty     shouldBeATypeIntOf 64
      }
    }

    in (our func "@refCastTest") { (func, cfg) =>
      val my = cfg
      
      my inst "%refcast" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.REFCAST
        its.fromTy shouldBeA[TypeRef] { _.ty shouldBeA[TypeVoid] thatsIt }
        its.toTy   shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf 32 }
        its.opnd   shouldBe (my param "%p0")
        its.ty     shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf 32 }
      }
      my inst "%irefcast" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.IREFCAST
        its.fromTy shouldBeA[TypeIRef] { _.ty shouldBeA[TypeVoid] thatsIt }
        its.toTy   shouldBeA[TypeIRef] { _.ty shouldBeATypeIntOf 64 }
        its.opnd   shouldBe (my param "%p1")
        its.ty     shouldBeA[TypeIRef] { _.ty shouldBeATypeIntOf 64 }
      }
      my inst "%funccast" shouldBeA[InstConv] { its => 
        its.op     shouldBe ConvOptr.FUNCCAST
        its.fromTy shouldBeA[TypeFunc] { _.sig shouldBeA[FuncSig] { its =>
          its.retTy shouldBeA[TypeVoid] thatsIt
          its.paramTy shouldBe empty
        }}
        its.toTy   shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@iiisig") }
        its.opnd   shouldBe (my param "%p2")
        its.ty     shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@iiisig") }
      }
    }
    
    in (our func "@ctrlFlow") { (func, cfg) =>
      val my = cfg
      
      my inst "%br1" shouldBeA[InstBranch] { _.dest shouldBe (my bb "%head") }
      my inst "%br3" shouldBeA[InstBranch] { _.dest shouldBe (my bb "%next") }
      my inst "%br4" shouldBeA[InstBranch] { _.dest shouldBe (my bb "%next") }
      my inst "%br5" shouldBeA[InstBranch] { _.dest shouldBe (my bb "%next") }
      my inst "%br6" shouldBeA[InstBranch] { _.dest shouldBe (my bb "%head") }
      
      my inst "%br2" shouldBeA[InstBranch2] { its =>
        its.cond shouldBe (my inst "%zero")
        its.ifTrue shouldBe (my bb "%body")
        its.ifFalse shouldBe (my bb "%exit")
      }
      
      my inst "%switch" shouldBeA[InstSwitch] { its =>
        its.opndTy shouldBeATypeIntOf 32
        its.opnd shouldBe (my inst "%phi")
        its.defDest shouldBe (my bb "%other")
        for ((c, b) <- its.cases) {
          c shouldBeA[ConstInt] { _.num.longValue match {
            case 1 => b shouldBe (my bb "%one")
            case 2 => b shouldBe (my bb "%two")
          }}
        }
      }
      
      my inst "%phi" shouldBeA[InstPhi] { its =>
        its.opndTy shouldBeATypeIntOf 32
        for ((b, c) <- its.cases) {
          if (b == (my bb "%entry")) { c shouldBeAConstIntOf(32, 0) }
          if (b == (my bb "%next")) { c shouldBe (my inst "%i2") }
        }
      }
      
      (my inst "%br1").ty    shouldBeA[TypeVoid] thatsIt
      (my inst "%br2").ty    shouldBeA[TypeVoid] thatsIt
      (my inst "%switch").ty shouldBeA[TypeVoid] thatsIt
      (my inst "%phi").ty    shouldBeATypeIntOf 32
    }
  
    in (our func "@caller1") { (func, cfg) =>
      val my = cfg
      
      my inst "%v1" shouldBeA[InstCall] { its =>
        its.sig.retTy shouldBeA[TypeVoid] thatsIt
        its.sig.paramTy shouldBe empty
        its.callee shouldBe (our globalValue "@callee1")
        its.args shouldBe empty
        its.ty shouldBeA[TypeVoid] thatsIt
      }
      
      my inst "%v2" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@iiisig")
        its.callee shouldBe (our globalValue "@callee2")
        its.args(0) shouldBeAConstIntOf(64, 1)
        its.args(1) shouldBeAConstIntOf(64, 2)
        its.args.size shouldBe 2
        its.ty shouldBeATypeIntOf 64
      }
      
      my inst "%v4" shouldBeA[InstCall] { its =>
        its.sig.retTy shouldBeA[TypeVoid] thatsIt
        its.sig.paramTy shouldBe empty
        its.callee shouldBe (our globalValue "@callee1")
        its.args shouldBe empty
        its.keepAlives(0) shouldBe (my inst "%v2")
        its.keepAlives(1) shouldBe (my inst "%v3")
        its.keepAlives.size shouldBe 2
        its.ty shouldBeA[TypeVoid] thatsIt
      }
      
      my inst "%v3" shouldBeA[InstInvoke] { its =>
        its.sig shouldBe (our sig "@iiisig")
        its.callee shouldBe (our globalValue "@callee3")
        its.args(0) shouldBeAConstIntOf(64, 3)
        its.args(1) shouldBeAConstIntOf(64, 4)
        its.args.size shouldBe 2
        its.nor shouldBe (my bb "%cont")
        its.exc shouldBe (my bb "%catch")
        its.ty shouldBeATypeIntOf 64
      }      

      my inst "%v5" shouldBeA[InstInvoke] { its =>
        its.sig shouldBe (our sig "@iiisig")
        its.callee shouldBe (our globalValue "@callee3")
        its.args(0) shouldBe (my inst "%v3")
        its.args(1) shouldBe (my inst "%v3")
        its.args.size shouldBe 2
        its.nor shouldBe (my bb "%cont2")
        its.exc shouldBe (my bb "%catch")
        its.keepAlives(0) shouldBe (my inst "%v2")
        its.keepAlives.size shouldBe 1
        its.ty shouldBeATypeIntOf 64
      }  
      
      my inst "%retv" shouldBeA[InstRetVoid] { _.ty shouldBeA[TypeVoid] thatsIt }
      my inst "%exc" shouldBeA[InstLandingpad] { _.ty shouldBeA[TypeRef] {
        _.ty shouldBeA[TypeVoid] thatsIt
      }}
    }
    
    in (our func "@caller2") { (func, cfg) =>
      val my = cfg
      
      my inst "%tc" shouldBeA[InstTailCall] { its =>
        its.sig shouldBe (our sig "@iiisig")
        its.callee shouldBe (our globalValue "@callee2")
        its.args(0) shouldBe (my inst "%p0")
        its.args(1) shouldBe (my inst "%p1")
        its.args.size shouldBe 2
        its.ty shouldBeA[TypeVoid] thatsIt
      }
    }
 
    in (our func "@callee2") { (func, cfg) =>
      val my = cfg
      
      my inst "%ret" shouldBeA[InstRet] { its =>
        its.retTy shouldBeATypeIntOf 64
        its.retVal shouldBe (my inst "%rv")
        its.ty shouldBeA[TypeVoid] thatsIt
      }
    }     
   
 
    in (our func "@callee3") { (func, cfg) =>
      val my = cfg
      
      my inst "%throw" shouldBeA[InstThrow] { its =>
        its.excVal shouldBe (my inst "%exc")
        its.ty shouldBeA[TypeVoid] thatsIt
      }
    }
    
    in (our func "@aggregate") { (func, cfg) =>
      val my = cfg
      
      my inst "%e0" shouldBeA[InstExtractValue] { its =>
        its.strTy shouldBe (our ty "@sid")
        its.index shouldBe 0
        its.opnd shouldBe (our const "@sid1")
        its.ty shouldBeATypeIntOf 64
      }
      
      my inst "%e1" shouldBeA[InstExtractValue] { its =>
        its.strTy shouldBe (our ty "@sid")
        its.index shouldBe 1
        its.opnd shouldBe (our const "@sid1")
        its.ty shouldBeA[TypeDouble] thatsIt
      }
      
      my inst "%i0" shouldBeA[InstInsertValue] { its =>
        its.strTy shouldBe (our ty "@sid")
        its.index shouldBe 0
        its.opnd shouldBe (our const "@sid1")
        its.newVal shouldBeAConstIntOf(64, 40)
        its.ty shouldBe (our ty "@sid")
      }
      
      my inst "%i1" shouldBeA[InstInsertValue] { its =>
        its.strTy shouldBe (our ty "@sid")
        its.index shouldBe 1
        its.opnd shouldBe (our const "@sid1")
        its.newVal shouldBeAConstDoubleOf 40.0d
        its.ty shouldBe (our ty "@sid")
      }
    }
    
    in (our func "@memops") { (func, cfg) =>
      val my = cfg
      
      my inst "%new" shouldBeA[InstNew] { its =>
        its.allocTy shouldBeATypeIntOf 64
        its.ty shouldBeA[TypeRef] { _.ty shouldBeATypeIntOf 64 }
      }
      
      my inst "%newhybrid" shouldBeA[InstNewHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.length shouldBe (my param "%p0")
        its.ty shouldBeA[TypeRef] { _.ty shouldBe (our ty "@hic") }
      }
      
      my inst "%alloca" shouldBeA[InstAlloca] { its =>
        its.allocTy shouldBeATypeIntOf 64
        its.ty shouldBeA[TypeIRef] { _.ty shouldBeATypeIntOf 64 }
      }
      
      my inst "%allocahybrid" shouldBeA[InstAllocaHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.length shouldBe (my param "%p0")
        its.ty shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@hic") }
      }

      my inst "%new2" shouldBeA[InstNew] { its =>
        its.allocTy shouldBe (our ty "@sid")
        its.ty shouldBeA[TypeRef] { _.ty shouldBe (our ty "@sid") }
      }
      
      my inst "%alloca2" shouldBeA[InstAlloca] { its =>
        its.allocTy shouldBe (our ty "@al")
        its.ty shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@al") }
      }
      
      my inst "%getiref" shouldBeA[InstGetIRef] { its =>
        its.referentTy shouldBe (our ty "@sid")
        its.opnd shouldBe (my inst "%new2")
        its.ty shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@sid") }
      }
    }
  }
}