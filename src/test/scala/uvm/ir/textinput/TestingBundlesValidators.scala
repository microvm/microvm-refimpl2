package uvm.ir.textinput

import org.scalatest._

import uvm._
import uvm.comminsts._
import uvm.ssavariables._
import uvm.types._

object TestingBundlesValidators {
  implicit class RichStringContext(val sc: StringContext) extends AnyVal {
    def qw(): Seq[String] = sc.parts(0).split("\\s+")
  }
  
  implicit class MagicalOur(val b: Bundle) extends AnyVal {
    def anything(s: String) = b.allNs(s)
    def ty(s: String) = b.typeNs(s)
    def const(s: String) = b.constantNs (s)
    def value(s: String) = b.globalVarNs(s)
    def globalValue(s: String) = b.globalVarNs(s)
    def globalCell(s: String) = b.globalCellNs(s)
    def sig(s: String) = b.funcSigNs(s)
    def func(s: String) = b.funcNs(s)
    def funcVer(s: String) = b.funcVerNs(s)
    def expFunc(s: String) = b.expFuncNs(s)
    def inst(s: String) = b.instNs(s)
  }
  
  implicit class MagicalThe(val c: FuncVer) extends AnyVal {
    def globalName(s: String) = UIRTextReader.globalize(s, c.name.get)
    def bb(s: String) = c.bbNs(UIRTextReader.globalize(s, c.name.get))
  }
  
  implicit class MagicalMy(val d: BasicBlock) extends AnyVal {
    def globalName(s: String) = UIRTextReader.globalize(s, d.name.get)
    def value(s: String) = d.localVarNs(UIRTextReader.globalize(s, d.name.get))
    def param(s: String) = d.localVarNs(UIRTextReader.globalize(s, d.name.get))
    def ires(s: String) = d.localVarNs(UIRTextReader.globalize(s, d.name.get))
    def inst(s: String) = d.localInstNs(UIRTextReader.globalize(s, d.name.get))
  }
}

trait TestingBundlesValidators extends Matchers with ExtraMatchers {
  import TestingBundlesValidators._

  def validateTypes(bundle: GlobalBundle) {
    val our = bundle
    
    our ty "@i1" shouldBeATypeIntOf(1)
    our ty "@i8" shouldBeATypeIntOf(8)
    our ty "@i16" shouldBeATypeIntOf(16)
    our ty "@i32" shouldBeATypeIntOf(32)
    our ty "@i64" shouldBeATypeIntOf(64)
    
    our ty "@float" shouldBeA[TypeFloat] thatsIt
    our ty "@double" shouldBeA[TypeDouble] thatsIt
    
    our ty "@rv" shouldBeA[TypeRef] { _.ty shouldBe (our ty "@void") }
    our ty "@irv" shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@void") }
    our ty "@wrv" shouldBeA[TypeWeakRef] { _.ty shouldBe (our ty "@void") }
    
    our ty "@ri16" shouldBeA[TypeRef] { _.ty shouldBe (our ty "@i16")}

    our ty "@s1" shouldBeA[TypeStruct] { its =>
      its fieldTys 0 shouldBe (our ty "@i8")
      its fieldTys 1 shouldBe (our ty "@i16")
      its fieldTys 2 shouldBe (our ty "@i32")
      its fieldTys 3 shouldBe (our ty "@i64")
      its fieldTys 4 shouldBe (our ty "@float")
      its fieldTys 5 shouldBe (our ty "@double")
      its fieldTys 6 shouldBe (our ty "@rv")
      its fieldTys 7 shouldBe (our ty "@irv")
      its fieldTys 8 shouldBe (our ty "@wrv")
      its fieldTys 9 shouldBe (our ty "@ri16")
    }
    
    our ty "@Cons" shouldBeA[TypeStruct] { its =>
      its fieldTys 0 shouldBe (our ty "@i64")
      its fieldTys 1 shouldBe (our ty "@RefCons")
    }
    
    our ty "@RefCons" shouldBeA[TypeRef] { _.ty should be (our ty "@Cons") }

    our ty "@foo" shouldBeA[TypeStruct] { its =>
      its fieldTys 0 shouldBe (our ty "@double")
      its fieldTys 1 shouldBe (our ty "@i64")
    }

    our ty "@a0" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBe (our ty "@i8")
      its.len shouldEqual 100
    }
    our ty "@a1" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBe (our ty "@foo")
      its.len shouldEqual 10
    }
    our ty "@a2" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBe (our ty "@a1")
      its.len shouldEqual 10
    }
    
    our ty "@h0" shouldBeA[TypeHybrid] { its =>
      its.fieldTys shouldBe empty
      its.varTy shouldBe (our ty "@i8")
    }
    our ty "@h1" shouldBeA[TypeHybrid] { its =>
      its.fieldTys shouldBe Seq(our ty "@foo")
      its.varTy shouldBe (our ty "@i64")
    }
    our ty "@h2" shouldBeA[TypeHybrid] { its =>
      its.fieldTys shouldBe Seq("@i8", "@i16", "@float").map(our.ty)
      its.varTy shouldBe (our ty "@i64")
    }
    
    our ty "@void" shouldBeA[TypeVoid] thatsIt
    
    our sig "@sig0" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe empty
      its.retTys shouldBe empty
    }
    
    our ty "@ii8" shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@i8") }
    our ty "@iii8" shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@ii8") }
    
    our sig "@sig1" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe Seq("@i32", "@iii8").map(our.ty)
      its.retTys shouldBe Seq(our ty "@i32")
    }
    
    our sig "@sig2" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe Seq("@i32", "@iii8").map(our.ty)
      its.retTys shouldBe Seq("@i32", "@i64").map(our.ty)
    }
    
    our ty "@f0" shouldBeA[TypeFuncRef] { _.sig shouldBe (our sig "@sig0") }
    our ty "@f1" shouldBeA[TypeFuncRef] { _.sig shouldBe (our sig "@sig1") }
    
    our ty "@th" shouldBeA[TypeThreadRef] thatsIt
    our ty "@st" shouldBeA[TypeStackRef] thatsIt
    our ty "@tr64" shouldBeA[TypeTagRef64] thatsIt
    
    our ty "@4xfloat" shouldBeA[TypeVector] { its =>
      its.elemTy shouldBe (our ty "@float")
      its.len shouldEqual 4
    }
    our ty "@4xi32" shouldBeA[TypeVector] { its =>
      its.elemTy shouldBe (our ty "@i32")
      its.len shouldEqual 4
    }
    our ty "@2xdouble" shouldBeA[TypeVector] { its =>
      its.elemTy shouldBe (our ty "@double")
      its.len shouldEqual 2
    }
    
    our ty "@i32_p" shouldBeA[TypeUPtr] { its => its.ty shouldBe (our ty "@i32") }
    our ty "@i64_p" shouldBeA[TypeUPtr] { its => its.ty shouldBe (our ty "@i64") }
    our ty "@sig0_fp" shouldBeA[TypeUFuncPtr] { its => its.sig shouldBe (our sig "@sig0") }
    our ty "@sig1_fp" shouldBeA[TypeUFuncPtr] { its => its.sig shouldBe (our sig "@sig1") }

    // Testing namespaces
    val i8 = our ty "@i8"
    our anything "@i8" shouldBe i8
    
    val sig0 = our sig "@sig0"
    our anything "@sig0" shouldBe sig0
  }

  def validateConstants(bundle: GlobalBundle) {
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
    our const "@cfnan" shouldBeAConstFloatOf NaN
    our const "@cfninf" shouldBeAConstFloatOf Float.NegativeInfinity 
    our const "@cfpinf" shouldBeAConstFloatOf Float.PositiveInfinity 
    our const "@cfbits" shouldBeAConstFloatOf exactly(bitsf(0x12345678))
  
    our const "@cd" shouldBeAConstDoubleOf 6.28D
    our const "@cdnan" shouldBeAConstDoubleOf NaN
    our const "@cdninf" shouldBeAConstDoubleOf Double.NegativeInfinity 
    our const "@cdpinf" shouldBeAConstDoubleOf Double.PositiveInfinity 
    our const "@cdbits" shouldBeAConstDoubleOf exactly(bitsd(0xfedcba9876543210L))
 
    our const "@cs1" shouldBeA[ConstSeq] { its=>
      its.constTy shouldBe (our ty "@s1")
      its.elems shouldBe qw"@ci64 @cd".map(our.const)
    }
    
    our const "@cs2" shouldBeA[ConstSeq] { its=>
      its.constTy shouldBe (our ty "@s2")
      its.elems shouldBe qw"@cf @ci64".map(our.const)
    }
    
    our const "@cs3" shouldBeA[ConstSeq] { its=>
      its.constTy shouldBe (our ty "@s3")
      its.elems shouldBe qw"@cd @cs2 @ci32".map(our.const)
    }
      
    our const "@cr" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@rv") }
    our const "@cir" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@irv") }
    our const "@cfu" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@func0") }
    our const "@cth" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@thread") }
    our const "@cst" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@stack") }

    our const "@cv4f" shouldBeA[ConstSeq] { its =>
      its.constTy shouldBe (our ty "@4xfloat")
      its.elems shouldBe qw"@F_1 @F_2 @F_3 @F_4".map(our.const)
    }
    
    our const "@cv4i" shouldBeA[ConstSeq] { its =>
      its.constTy shouldBe (our ty "@4xi32")
      its.elems shouldBe qw"@I32_1 @I32_2 @I32_3 @I32_4".map(our.const)
    }
 
    our const "@cv4d" shouldBeA[ConstSeq] { its =>
      its.constTy shouldBe (our ty "@2xdouble")
      its.elems shouldBe qw"@D_1 @D_2".map(our.const)
    }

    our globalCell "@gi64" shouldBeA[GlobalCell] { _.cellTy shouldBe (our ty "@i64") }

    our func "@fdummy" shouldBeA[Function] { _.sig shouldBe (our sig "@sig0") }
    
    our const "@sgf" shouldBeA[ConstSeq] { its =>
      its.constTy shouldBe (our ty "@sgf_t")
      its.elems shouldBe qw"@gi64 @fdummy".map(our.globalValue)
    }
    
    our const "@I32P_PTR1" shouldBeA[ConstInt] { its =>
      its.constTy shouldBe (our ty "@i32_p")
      its.num.toLong shouldBe 0x123456789abcdef0L
    }
    
    our const "@SIG0FP_PTR1" shouldBeA[ConstInt] { its =>
      its.constTy shouldBe (our ty "@sig0_fp")
      its.num.toLong shouldBe 0xfedcba9876543210L
    }
    
    our const "@ary1" shouldBeA[ConstSeq] { its =>
      its.constTy shouldBe (our ty "@i32_3_ary")
      its.elems shouldBe qw"@I32_1 @I32_2 @I32_3".map(our.const)
    }
    
    // Testing namespaces
    val ci8 = our const "@ci8"
    our globalValue "@ci8" shouldBe ci8
    our value "@ci8" shouldBe ci8
    our anything "@ci8" shouldBe ci8
    
    val gi64 = our globalCell "@gi64"
    our globalValue "@gi64" shouldBe gi64
    our value "@gi64" shouldBe gi64
    our anything "@gi64" shouldBe gi64
  }

  def validateFunctions(bundle: GlobalBundle) {
    val our = bundle
    
    our sig "@foo" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe empty
      its.retTys shouldBe empty
    }

    our sig "@bar" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe qw"@i32 @i16".map(our.ty)
      its.retTys shouldBe qw"@i64".map(our.ty)
    }
    
    our sig "@baz" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe qw"@i32 @iii8".map(our.ty)
      its.retTys shouldBe qw"@i32".map(our.ty)
    }
    
    our sig "@sig_fs" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe qw"@i32".map(our.ty)
      its.retTys shouldBe empty
    }
    
    our ty "@sig_t" shouldBeA[TypeFuncRef] { _.sig shouldBe (our sig "@sig_fs") }

    our sig "@signal_sig" shouldBeA[FuncSig] { its =>
      its.paramTys shouldBe qw"@i32 @sig_t".map(our.ty)
      its.retTys shouldBe qw"@sig_t".map(our.ty)
    }
    
    our func "@signal" shouldBeA[Function] { its =>
      its.sig shouldBe (our sig "@signal_sig")
    }
    
    our const "@zero" shouldBeA[ConstInt] { its =>
      its.constTy shouldBe (our ty "@i32")
      its.num shouldBe 0
    }
    
    our func "@main" shouldBeA[Function] { its =>
      its.sig shouldBe (our sig "@baz")
    }
    
    our funcVer "@main.v1" shouldBeA[FuncVer] { its =>
      val theFuncVer = its
      its.sig shouldBe (our sig "@baz")
      
      in (its bb "%entry") { my =>
        my norParams 0 shouldBeA[NorParam] { whose =>
          whose.ty shouldBe (our ty "@i32")
          whose.name.get shouldEqual "@main.v1.entry.argc"
        }
        my norParams 1 shouldBeA[NorParam] { whose =>
          whose.ty shouldBe (our ty "@iii8")
          whose.name.get shouldEqual "@main.v1.entry.argv"
        }
        
        (my inst "%add").results shouldBe Seq(my ires "%sum")
        (my inst "%call").results shouldBe qw"%x %y %z".map(my.ires)
        (my inst "%ret").results shouldBe empty
      }
      
    }
    
    our expFunc "@main_native" shouldBeA[ExposedFunc] { its =>
      its.func shouldBe (our func "@main")
      its.callConv shouldEqual Flag("#DEFAULT")
      its.cookie shouldBe (our const "@zero64").asInstanceOf[ConstInt]
    }
    
    // Testing namespaces
    val main = our func "@main"
    our globalValue "@main" shouldBe main
    our value "@main" shouldBe main
    our anything "@main" shouldBe main
    
    val mainV1 = our funcVer "@main.v1"
    our anything "@main.v1" shouldBe mainV1
    
    val mainNativeGN = "@main_native"
    val mainNative = our expFunc mainNativeGN
    our globalValue mainNativeGN shouldBe mainNative
    our value mainNativeGN shouldBe mainNative
    our anything mainNativeGN shouldBe mainNative 
  }

  def in(func: Function)(f: (Function, FuncVer) => Unit)(implicit bundle: GlobalBundle) {
    val ver = bundle.funcToVers(func).head
    f(func, ver)
  }

  def in(funcVer: FuncVer)(f: FuncVer => Unit) {
    f(funcVer)
  }
  
   def in(b: BasicBlock)(f: BasicBlock => Unit) {
    f(b)
  }
  
  def validateInstructions(bundle: GlobalBundle) {
    implicit val _bundle = bundle
    val our = bundle
    
    in (our func "@intBinOpTest") { (func, the) =>
      in (the bb "%entry") { my =>
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
        
        for (i <- my.localInstNs.all; if i.isInstanceOf[InstBinOp]) {
          i.asInstanceOf[InstBinOp].opndTy shouldBe (our ty "@i32")
          i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
          i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
        }
      }
    }
     
    in (our func "@fpBinOpTest") { (func, the) =>
      in(the bb "%entry") {my =>
      
        my inst "%fadd" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FADD }
        my inst "%fsub" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FSUB }
        my inst "%fmul" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FMUL }
        my inst "%fdiv" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FDIV }
        my inst "%frem" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FREM }
        
        for (i <- my.localInstNs.all; if i.isInstanceOf[InstBinOp]) {
          i.asInstanceOf[InstBinOp].opndTy shouldBe (our ty "@double")
          i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
          i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
        }
      }
    }   

    in (our func "@intCmpTest") { (func, the) =>
      in(the bb "%entry") { my =>
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
        
        for (i <- my.localInstNs.all; if i.isInstanceOf[InstCmp]) {
          i.asInstanceOf[InstCmp].opndTy shouldBe (our ty "@i64")
          i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
          i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
        }
      }
    }   
    
    in (our func "@fpCmpTest") { (func, the) =>
      in(the bb "%entry") { my =>
      
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
        
        for (i <- my.localInstNs.all; if i.isInstanceOf[InstCmp]) {
          i.asInstanceOf[InstCmp].opndTy shouldBe (our ty "@float")
          i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
          i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
        }
      }
    }

    in (our func "@convTest") { (func, the) =>
      in(the bb "%entry") { my =>
      
        my inst "%trunc" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.TRUNC
          its.fromTy shouldBe (our ty "@i64")
          its.toTy   shouldBe (our ty "@i32")
          its.opnd   shouldBe (my param "%p1")
        }
        my inst "%zext" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.ZEXT 
          its.fromTy shouldBe (our ty "@i32")
          its.toTy   shouldBe (our ty "@i64")
          its.opnd   shouldBe (my param "%p0")
  
        }
        my inst "%sext" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.SEXT
          its.fromTy shouldBe (our ty "@i32")
          its.toTy   shouldBe (our ty "@i64")
          its.opnd   shouldBe (my param "%p0")
        }
        my inst "%fptrunc" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.FPTRUNC
          its.fromTy shouldBe (our ty "@double")
          its.toTy   shouldBe (our ty "@float")
          its.opnd   shouldBe (my param "%p3")
        }
        my inst "%fpext" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.FPEXT 
          its.fromTy shouldBe (our ty "@float")
          its.toTy   shouldBe (our ty "@double")
          its.opnd   shouldBe (my param "%p2")
        }
        my inst "%fptoui" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.FPTOUI
          its.fromTy shouldBe (our ty "@double")
          its.toTy   shouldBe (our ty "@i64")
          its.opnd   shouldBe (my param "%p3")
        }
        my inst "%fptosi" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.FPTOSI
          its.fromTy shouldBe (our ty "@double")
          its.toTy   shouldBe (our ty "@i64")
          its.opnd   shouldBe (my param "%p3")
        }
        my inst "%uitofp" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.UITOFP
          its.fromTy shouldBe (our ty "@i64")
          its.toTy   shouldBe (our ty "@double")
          its.opnd   shouldBe (my param "%p1")
        }
        my inst "%sitofp" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.SITOFP 
          its.fromTy shouldBe (our ty "@i64")
          its.toTy   shouldBe (our ty "@double")
          its.opnd   shouldBe (my param "%p1")
        }
        my inst "%bitcast0" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.BITCAST 
          its.fromTy shouldBe (our ty "@i32")
          its.toTy   shouldBe (our ty "@float")
          its.opnd   shouldBe (my param "%p0")
        }
        my inst "%bitcast1" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.BITCAST 
          its.fromTy shouldBe (our ty "@i64")
          its.toTy   shouldBe (our ty "@double")
          its.opnd   shouldBe (my param "%p1")
        }
        my inst "%bitcast2" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.BITCAST 
          its.fromTy shouldBe (our ty "@float")
          its.toTy   shouldBe (our ty "@i32")
          its.opnd   shouldBe (my param "%p2")
        }
        my inst "%bitcast3" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.BITCAST 
          its.fromTy shouldBe (our ty "@double")
          its.toTy   shouldBe (our ty "@i64")
          its.opnd   shouldBe (my param "%p3")
        }
      }
    }

    in (our func "@refCastTest") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%refcast" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.REFCAST
          its.fromTy shouldBe (our ty "@rv")
          its.toTy   shouldBe (our ty "@ri32")
          its.opnd   shouldBe (my param "%p0")
        }
        my inst "%irefcast" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.REFCAST
          its.fromTy shouldBe (our ty "@irv")
          its.toTy   shouldBe (our ty "@ii64")
          its.opnd   shouldBe (my param "%p1")
        }
        my inst "%funccast" shouldBeA[InstConv] { its => 
          its.op     shouldBe ConvOptr.REFCAST
          its.fromTy shouldBe (our ty "@npnr_func")
          its.toTy   shouldBe (our ty "@iii_func")
          its.opnd   shouldBe (my param "%p2")
        }
      }
    }
    
    in (our func "@ptrCastTest") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%ptrcast" shouldBeA[InstConv] { its =>
          its.op shouldBe ConvOptr.PTRCAST
          its.fromTy shouldBe (our ty "@i64")
          its.toTy shouldBe (our ty "@pi64")
          its.opnd shouldBe (my param "%p0")
        } 
      }
    }

    in (our func "@select") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%select" shouldBeA[InstSelect] { its =>
          its.condTy shouldBe (our ty "@i1")
          its.opndTy shouldBe (our ty "@i32")
          its.cond shouldBe (our const "@TRUE")
          its.ifTrue shouldBe (our const "@I32_0")
          its.ifFalse shouldBe (our const "@I32_1")
        } 
      }
    }
    
    in (our func "@ctrlFlow") { (func, the) =>
      in(the bb "%entry") { my =>
        my inst "%br1" shouldBeA[InstBranch] { its =>
          its.dest.bb shouldBe (the bb "%head")
          its.dest.args shouldBe Seq(my value "%p0")
        }
      }
        
      in(the bb "%head") { my =>
        my inst "%br2" shouldBeA[InstBranch2] { its =>
          its.cond shouldBe (my ires "%zero")
          its.ifTrue.bb shouldBe (the bb "%body")
          its.ifTrue.args shouldBe Seq(my value "%x")
          its.ifFalse.bb shouldBe (the bb "%exit")
          its.ifFalse.args shouldBe empty
        }
      }
      
      in(the bb "%body") { my =>
        my inst "%switch" shouldBeA[InstSwitch] { its =>
          its.opndTy shouldBe (our ty "@i32")
          its.opnd shouldBe (my value "%x")
          its.defDest.bb shouldBe (the bb "%other")
          its.cases(0)._1 shouldBe (our value "@I32_1")
          its.cases(0)._2.bb shouldBe (the bb "%one")  
          its.cases(1)._1 shouldBe (our value "@I32_2")
          its.cases(1)._2.bb shouldBe (the bb "%two")  
        }
      }
      
      in(the bb "%one") { my =>
        my inst "%br3" shouldBeA[InstBranch] { its =>
          its.dest.bb shouldBe (the bb "%next")
          its.dest.args shouldBe Seq(my value "%x")
        }
      }
      in(the bb "%two") { my =>
        my inst "%br4" shouldBeA[InstBranch] { its =>
          its.dest.bb shouldBe (the bb "%next")
          its.dest.args shouldBe Seq(my value "%x")
        }
      }
      in(the bb "%other") { my =>
        my inst "%br5" shouldBeA[InstBranch] { its =>
          its.dest.bb shouldBe (the bb "%next")
          its.dest.args shouldBe Seq(my value "%x")
        }
      }
      in(the bb "%next") { my =>
        my inst "%br6" shouldBeA[InstBranch] { _.dest shouldBe DestClause(the bb "%head", Seq(my ires "%i2")) }
      }

    }
    
    in (our func "@callee2") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%ret" shouldBeA[InstRet] { its =>
          its.funcVer shouldBe the
          its.retVals shouldBe Seq(my ires "%rv")
        }
      }
    }     
 
    in (our func "@callee3") { (func, the) =>
      in(the bb "%entry") { my =>
      
        my inst "%throw" shouldBeA[InstThrow] { its =>
          its.excVal shouldBe (my ires "%exc")
        }
      }
    }

    in (our func "@caller1") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%call1" shouldBeA[InstCall] { its =>
          its.sig shouldBe (our sig "@npnr_sig")
          its.callee shouldBe (our value "@callee1")
          its.argList shouldBe empty
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
        
        my inst "%call2" shouldBeA[InstCall] { its =>
          its.sig shouldBe (our sig "@iii_sig")
          its.callee shouldBe (our value "@callee2")
          its.argList shouldEqual Seq("@I64_1", "@I64_2").map(our.value)
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
  
        my inst "%call3" shouldBeA[InstCall] { its =>
          its.sig shouldBe (our sig "@iii_sig")
          its.callee shouldBe (our value "@callee3")
          its.argList shouldEqual Seq("@I64_1", "@I64_2").map(our.value)
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%cont", qw"%v2 %v3".map(my.ires)),
              DestClause(the bb "%catch", Seq())))
          its.keepAlives shouldBe empty
        }  
      }
      
      in(the bb "%cont") { my =>
        my inst "%call4" shouldBeA[InstCall] { its =>
          its.sig shouldBe (our sig "@npnr_sig")
          its.callee shouldBe (our globalValue "@callee1")
          its.argList shouldBe empty
          its.excClause shouldBe None
          its.keepAlives shouldBe Seq("%v2", "%v3").map(my.value)
        }
  
        my inst "%call5" shouldBeA[InstCall] { its =>
          its.sig shouldBe (our sig "@iii_sig")
          its.callee shouldBe (our globalValue "@callee3")
          its.argList shouldBe Seq("%v3", "%v3").map(my.value)
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%cont2", Seq()),
              DestClause(the bb "%catch", Seq())))
          its.keepAlives shouldEqual Seq(my ires "%v2")
        }  
      }
      
      in(the bb "%cont2") { my =>
        my inst "%retv" shouldBeA[InstRet] {its =>
          its.retVals shouldBe empty
        }
      }
      
      in(the bb "%catch") { my =>
        my.excParam.get shouldBeA[ExcParam] {its =>
        }
      }
    }
    
    in (our func "@caller2") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%tc" shouldBeA[InstTailCall] { its =>
          its.sig shouldBe (our sig "@iii_sig")
          its.callee shouldBe (our globalValue "@callee2")
          its.argList shouldEqual Seq("%p0", "%p1").map(my.value)
        }
      }
    }

    in (our func "@aggregate") { (func, the) =>
      in(the bb "%entry") { my =>
      
        my inst "%e0" shouldBeA[InstExtractValue] { its =>
          its.strTy shouldBe (our ty "@sid")
          its.index shouldBe 0
          its.opnd shouldBe (our value "@sid1")
        }
        
        my inst "%e1" shouldBeA[InstExtractValue] { its =>
          its.strTy shouldBe (our ty "@sid")
          its.index shouldBe 1
          its.opnd shouldBe (our value "@sid1")
        }
        
        my inst "%i0" shouldBeA[InstInsertValue] { its =>
          its.strTy shouldBe (our ty "@sid")
          its.index shouldBe 0
          its.opnd shouldBe (our value "@sid1")
          its.newVal shouldBe (our value "@I64_0")
        }
        
        my inst "%i1" shouldBeA[InstInsertValue] { its =>
          its.strTy shouldBe (our ty "@sid")
          its.index shouldBe 1
          its.opnd shouldBe (our value "@sid1")
          its.newVal shouldBe (our value "@D_0")
        }
        
        my inst "%ee0" shouldBeA[InstExtractElement] { its =>
          its.seqTy shouldBe (our ty "@4xfloat")
          its.indTy shouldBe (our ty "@i32")
          its.opnd shouldBe (our value "@v1")
          its.index shouldBe (our value "@I32_0")
        }
        
        my inst "%ie0" shouldBeA[InstInsertElement] { its =>
          its.seqTy shouldBe (our ty "@4xfloat")
          its.indTy shouldBe (our ty "@i32")
          its.opnd shouldBe (our value "@v1")
          its.index shouldBe (our value "@I32_1")
          its.newVal shouldBe (our value "@F_1")
        }
        
        my inst "%sv0" shouldBeA[InstShuffleVector] { its =>
          its.vecTy shouldBe (our ty "@4xfloat")
          its.maskTy shouldBe (our ty "@4xi32")
          its.vec1 shouldBe (our value "@v1")
          its.vec2 shouldBe (our value "@v2")
          its.mask shouldBe (our value "@vshf")
        }
      }
    }

    in (our func "@memops") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%new" shouldBeA[InstNew] { its =>
          its.allocTy shouldBe (our ty "@i64")
          its.excClause shouldBe None
        }
        
        my inst "%newhybrid" shouldBeA[InstNewHybrid] { its =>
          its.allocTy shouldBe (our ty "@hic")
          its.lenTy shouldBe (our ty "@i64")
          its.length shouldBe (our const "@I64_43")
          its.excClause shouldBe None
        }
        
        my inst "%alloca" shouldBeA[InstAlloca] { its =>
          its.allocTy shouldBe (our ty "@i64")
          its.excClause shouldBe None
        }

        my inst "%new_s" shouldBeA[InstNew] { its =>
          its.allocTy shouldBe (our ty "@i64")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb2", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb2") { my =>
        
        my inst "%newhybrid_s" shouldBeA[InstNewHybrid] { its =>
          its.allocTy shouldBe (our ty "@hic")
          its.lenTy shouldBe (our ty "@i64")
          its.length shouldBe (our const "@I64_43")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb3", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb3") { my =>
        
        my inst "%alloca_s" shouldBeA[InstAlloca] { its =>
          its.allocTy shouldBe (our ty "@i64")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb4", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb4") { my =>
        
        my inst "%allocahybrid_s" shouldBeA[InstAllocaHybrid] { its =>
          its.allocTy shouldBe (our ty "@hic")
          its.lenTy shouldBe (our ty "@i64")
          its.length shouldBe (our const "@I64_43")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb5", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb5") { my =>
  
        my inst "%new2" shouldBeA[InstNew] { its =>
          its.allocTy shouldBe (our ty "@sid")
          its.excClause shouldBe None
        }
        
        my inst "%alloca2" shouldBeA[InstAlloca] { its =>
          its.allocTy shouldBe (our ty "@al")
          its.excClause shouldBe None
        }
        
        my inst "%getiref" shouldBeA[InstGetIRef] { its =>
          its.referentTy shouldBe (our ty "@sid")
          its.opnd shouldBe (my value "%r9")
        }
        
        my inst "%getfieldiref" shouldBeA[InstGetFieldIRef] { its =>
          its.ptr shouldBe false
          its.referentTy shouldBe (our ty "@sid")
          its.index shouldBe 0
          its.opnd shouldBe (my value "%r11")
        }
        
        my inst "%getelemiref" shouldBeA[InstGetElemIRef] { its =>
          its.ptr shouldBe false
          its.referentTy shouldBe (our ty "@al")
          its.indTy shouldBe (our ty "@i64")
          its.opnd shouldBe (my value "%r10")
          its.index shouldBe (our const "@I64_1")
        }
        
        my inst "%allocahybrid" shouldBeA[InstAllocaHybrid] { its =>
          its.allocTy shouldBe (our ty "@hic")
          its.lenTy shouldBe (our ty "@i64")
          its.length shouldBe (our const "@I64_43")
          its.excClause shouldBe None
        }

        my inst "%getvarpartiref" shouldBeA[InstGetVarPartIRef] { its =>
          its.ptr shouldBe false
          its.referentTy shouldBe (our ty "@hic")
          its.opnd shouldBe (my value "%r4")
        }
        
        my inst "%shiftiref" shouldBeA[InstShiftIRef] { its =>
          its.ptr shouldBe false
          its.referentTy shouldBe (our ty "@i8")
          its.offTy shouldBe (our ty "@i64")
          its.opnd shouldBe (my value "%r14")
          its.offset shouldBe (our const "@I64_1")
        }        
        
        my inst "%load" shouldBeA[InstLoad] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.excClause shouldBe None
        }
        
        my inst "%store" shouldBeA[InstStore] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.newVal shouldBe (our const "@I64_42")
          its.excClause shouldBe None
        }
        
        my inst "%cmpxchg" shouldBeA[InstCmpXchg] { its =>
          its.ptr shouldBe false
          its.weak shouldBe false
          its.ordSucc shouldBe MemoryOrder.SEQ_CST
          its.ordFail shouldBe MemoryOrder.SEQ_CST
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.expected shouldBe (our const "@I64_42")
          its.desired shouldBe (our const "@I64_0")
          its.excClause shouldBe None
        }
        
        my inst "%cmpxchg_w" shouldBeA[InstCmpXchg] { its =>
          its.ptr shouldBe false
          its.weak shouldBe true
          its.ordSucc shouldBe MemoryOrder.SEQ_CST
          its.ordFail shouldBe MemoryOrder.SEQ_CST
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.expected shouldBe (our const "@I64_42")
          its.desired shouldBe (our const "@I64_0")
          its.excClause shouldBe None
        }
      
        my inst "%atomicrmw" shouldBeA[InstAtomicRMW] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.SEQ_CST
          its.op shouldBe AtomicRMWOptr.ADD
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.opnd shouldBe (our const "@I64_43")
          its.excClause shouldBe None
        }
     
        my inst "%load_s" shouldBeA[InstLoad] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb6", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb6") { my =>
        
        my inst "%store_s" shouldBeA[InstStore] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.newVal shouldBe (our const "@I64_42")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb7", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb7") { my =>
        
        my inst "%cmpxchg_s" shouldBeA[InstCmpXchg] { its =>
          its.ptr shouldBe false
          its.ordSucc shouldBe MemoryOrder.SEQ_CST
          its.ordFail shouldBe MemoryOrder.SEQ_CST
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.expected shouldBe (our const "@I64_42")
          its.desired shouldBe (our const "@I64_0")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb8", Seq()),
              DestClause(the bb "%handler", Seq())))
       }
      }
      in(the bb "%bb8") { my =>
      
        my inst "%atomicrmw_s" shouldBeA[InstAtomicRMW] { its =>
          its.ptr shouldBe false
          its.ord shouldBe MemoryOrder.SEQ_CST
          its.op shouldBe AtomicRMWOptr.ADD
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (our globalCell "@i64loc")
          its.opnd shouldBe (our const "@I64_43")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%bb9", Seq()),
              DestClause(the bb "%handler", Seq())))
        }
      }
      in(the bb "%bb9") { my =>
        
        my inst "%fence" shouldBeA[InstFence] { its =>
          its.ord shouldBe (MemoryOrder.SEQ_CST)
        }
      }
    }

    in (our func "@memops_ptr") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%pin" shouldBeA[InstCommInst] { _.argList(0) shouldBe (my value "%o")}
        
        my inst "%getfieldiref" shouldBeA[InstGetFieldIRef] { its =>
          its.ptr shouldBe true
          its.referentTy shouldBe (our ty "@sid")
          its.index shouldBe 0
          its.opnd shouldBe (my value "%p2")
        }
        
        my inst "%getelemiref" shouldBeA[InstGetElemIRef] { its =>
          its.ptr shouldBe true
          its.referentTy shouldBe (our ty "@al")
          its.indTy shouldBe (our ty "@i64")
          its.opnd shouldBe (my value "%p3")
          its.index shouldBe (my param "%p1")
        }
        
        my inst "%shiftiref" shouldBeA[InstShiftIRef] { its =>
          its.ptr shouldBe true
          its.referentTy shouldBe (our ty "@i8")
          its.offTy shouldBe (our ty "@i64")
          its.opnd shouldBe (my value "%pv")
          its.offset shouldBe (my param "%p1")
        }
        
        my inst "%getvarpartiref" shouldBeA[InstGetVarPartIRef] { its =>
          its.ptr shouldBe true
          its.referentTy shouldBe (our ty "@hic")
          its.opnd shouldBe (my value "%ph")
        }
        
        my inst "%load" shouldBeA[InstLoad] { its =>
          its.ptr shouldBe true
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (my value "%p")
          its.excClause shouldBe None
        }
        
        my inst "%store" shouldBeA[InstStore] { its =>
          its.ptr shouldBe true
          its.ord shouldBe MemoryOrder.NOT_ATOMIC
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (my value "%p")
          its.newVal shouldBe (our const "@I64_42")
          its.excClause shouldBe None
        }
        
        my inst "%cmpxchg" shouldBeA[InstCmpXchg] { its =>
          its.ptr shouldBe true
          its.ordSucc shouldBe MemoryOrder.SEQ_CST
          its.ordFail shouldBe MemoryOrder.SEQ_CST
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (my value "%p")
          its.expected shouldBe (our const "@I64_42")
          its.desired shouldBe (our const "@I64_0")
          its.excClause shouldBe None
        }
      
        my inst "%atomicrmw" shouldBeA[InstAtomicRMW] { its =>
          its.ptr shouldBe true
          its.ord shouldBe MemoryOrder.SEQ_CST
          its.op shouldBe AtomicRMWOptr.ADD
          its.referentTy shouldBe (our ty "@i64")
          its.loc shouldBe (my value "%p")
          its.opnd shouldBe (our const "@I64_43")
          its.excClause shouldBe None
        }
      }
    }
    
    in (our func "@memorder") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%l0" shouldBeA[InstLoad] { _.ord shouldBe MemoryOrder.NOT_ATOMIC }
        my inst "%l1" shouldBeA[InstLoad] { _.ord shouldBe MemoryOrder.RELAXED }
        my inst "%l2" shouldBeA[InstLoad] { _.ord shouldBe MemoryOrder.CONSUME }
        my inst "%l3" shouldBeA[InstLoad] { _.ord shouldBe MemoryOrder.ACQUIRE }
        my inst "%s4" shouldBeA[InstStore] { _.ord shouldBe MemoryOrder.RELEASE }
        my inst "%c5" shouldBeA[InstCmpXchg] { its =>
          its.ordSucc shouldBe MemoryOrder.ACQ_REL
          its.ordFail shouldBe MemoryOrder.ACQUIRE
        }
        my inst "%l6" shouldBeA[InstLoad] { _.ord shouldBe MemoryOrder.SEQ_CST }
      }
    }
    
    in (our func "@atomicrmwops") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%rmw0" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.XCHG }
        my inst "%rmw1" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.ADD }
        my inst "%rmw2" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.SUB }
        my inst "%rmw3" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.AND }
        my inst "%rmw4" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.NAND }
        my inst "%rmw5" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.OR }
        my inst "%rmw6" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.XOR }
        my inst "%rmw7" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.MAX }
        my inst "%rmw8" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.MIN }
        my inst "%rmw9" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.UMAX }
        my inst "%rmwa" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.UMIN }
      }
    }

    in (our func "@traps") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%t" shouldBeA[InstTrap] { its =>
          its.retTys shouldBe qw"@i32".map(our.ty)
          its.excClause shouldBe None
          its.keepAlives shouldBe Seq(my value "%a")
        }
        
        my inst "%t0" shouldBeA[InstTrap] { its =>
          its.retTys shouldBe empty
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
         
        my inst "%ts" shouldBeA[InstTrap] { its =>
          its.retTys shouldBe qw"@i64 @float".map(our.ty)
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%tp_s_cont", Seq("%a", "%b").map(my.value)),
              DestClause(the bb "%tp_s_exc", Seq())))
          its.keepAlives shouldBe Seq(my value "%b")
        }
      }
      
      in(the bb "%tp_s_cont") { my =>
        
        my inst "%wp" shouldBeA[InstWatchPoint] { its =>
          its.wpID shouldBe 1
          its.retTys shouldBe empty
          its.dis shouldBe DestClause(the bb "%wp_dis_cont", Seq(my value "%b"))
          its.ena shouldBe DestClause(the bb "%wp_ena_cont", Seq())
          its.exc shouldBe None
          its.keepAlives shouldBe Seq(my value "%a")
        }
      }
      
      in(the bb "%wp_dis_cont") { my =>
        my inst "%wp_s" shouldBeA[InstWatchPoint] { its =>
          its.wpID shouldBe 2
          its.retTys shouldBe Seq(our ty "@double")
          its.dis shouldBe DestClause(the bb "%wp_s_dis_cont", Seq())
          its.ena shouldBe DestClause(the bb "%wp_s_ena_cont", Seq())
          its.exc shouldBe Some(DestClause(the bb "%wp_s_exc", Seq()))
          its.keepAlives shouldBe Seq(my value "%b")
        }
      }
      
      in(the bb "%wp_s_dis_cont") { my =>
        my inst "%wpbr" shouldBeA[InstWPBranch] { its =>
          its.wpID shouldBe 3
          its.dis shouldBe DestClause(the bb "%wpbr_t", Seq())
          its.ena shouldBe DestClause(the bb "%wpbr_f", Seq())
        }
      }
    }

    in (our func "@ccall") { (func, the) =>
      in(the bb "%entry") { my =>
      
        my inst "%ccall" shouldBeA[InstCCall] { its =>
          its.callConv shouldBe Flag("#DEFAULT")
          its.funcTy shouldBe (our ty "@ccall_callee_fp")
          its.sig shouldBe (our sig "@ccall_callee_sig")
          its.callee shouldBe (my param "%p0")
          its.argList shouldBe Seq(our value "@D_1")
        }
      }
    }
    
    in (our func "@gen") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%ss1" shouldBeA[InstSwapStack] { its =>
          its.swappee shouldBe (my value "%main")
          its.curStackAction shouldBe RetWith(Seq())
          its.newStackAction shouldBe PassValues(Seq(our ty "@i64"), Seq(our value "@I64_0"))
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
        my inst "%ss2" shouldBeA[InstSwapStack] { its =>
          its.swappee shouldBe (my value "%main")
          its.curStackAction shouldBe KillOld()
          its.newStackAction shouldBe ThrowExc(our value "@NULLREF")
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
      }
    }
   
    in (our func "@newthread") { (func, the) =>
      in(the bb "%entry") { my =>
        
        my inst "%nt1" shouldBeA[InstNewThread] { its =>
          its.stack shouldBe (my value "%s1")
          its.threadLocal shouldBe None
          its.newStackAction shouldBe PassValues(qw"@i64 @i64".map(our.ty), qw"@I64_0 @I64_1".map(our.value))
          its.excClause shouldBe None
        }
        my inst "%nt2" shouldBeA[InstNewThread] { its =>
          its.stack shouldBe (my value "%s2")
          its.threadLocal shouldBe None
          its.newStackAction shouldBe ThrowExc(our value "@NULLREF")
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%nor", Seq()),
              DestClause(the bb "%exc", Seq())
              ))
        }
        my inst "%nt3" shouldBeA[InstNewThread] { its =>
          its.stack shouldBe (my value "%s1")
          its.threadLocal.isDefined shouldBe true
          its.threadLocal.get shouldBe (my value "%tl")
          its.newStackAction shouldBe PassValues(qw"@i64 @i64".map(our.ty), qw"@I64_0 @I64_1".map(our.value))
          its.excClause shouldBe None
        }
      }
    }
    
    in (our func "@swapstack") { (func, the) =>
 
      in(the bb "%entry") { my =>
        my inst "%ss1" shouldBeA[InstSwapStack] { its =>
          its.swappee shouldBe (my value "%coro")
          its.curStackAction shouldBe RetWith(Seq(our ty "@i64"))
          its.newStackAction shouldBe PassValues(Seq(), Seq())
          its.excClause shouldBe None
          its.keepAlives shouldBe Seq(my value "%curstack")
        }
        my inst "%ss2" shouldBeA[InstSwapStack] { its =>
          its.swappee shouldBe (my value "%coro")
          its.curStackAction shouldBe RetWith(Seq(our ty "@i64"))
          its.newStackAction shouldBe PassValues(Seq(), Seq())
          its.excClause shouldBe Some(ExcClause(
              DestClause(the bb "%nor", Seq()),
              DestClause(the bb "%exc", Seq())))
          its.keepAlives shouldBe empty
        }
      }
    }
    
    in (our func "@comminst") { (func, the) =>
      in(the bb "%entry") { my =>
      
        my inst "%ci2" shouldBeA[InstCommInst] { its =>
          its.inst shouldBe CommInsts("@uvm.new_stack")
          its.flagList shouldBe empty
          its.typeList shouldBe empty
          its.funcSigList shouldBe Seq(our sig "@iii_sig")
          its.argList shouldBe Seq(our value "@callee2")
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        } 
  
        my inst "%ci3" shouldBeA[InstCommInst] { its =>
          its.inst shouldBe CommInsts("@uvm.native.expose")
          its.flagList shouldBe Seq(Flag("#DEFAULT"))
          its.typeList shouldBe empty
          its.funcSigList shouldBe Seq(our sig "@npnr_sig")
          its.argList shouldBe Seq(our func "@swapstack")
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
        
        my inst "%ci4" shouldBeA[InstCommInst] { its =>
          its.inst shouldBe CommInsts("@uvm.thread_exit")
          its.flagList shouldBe empty
          its.typeList shouldBe empty
          its.funcSigList shouldBe empty
          its.argList shouldBe empty
          its.excClause shouldBe None
          its.keepAlives shouldBe empty
        }
      }
    }
  }
  
  def versionsOf(func: Function)(implicit bundle: GlobalBundle): List[FuncVer] = {
    bundle.funcToVers.getOrElse(func, Nil)
  }

  def validateRedef(globalBundle: GlobalBundle, b1: TrantientBundle, b2: TrantientBundle) {
    implicit val ourGlobal = globalBundle
    val ourOld = b1
    val ourNew = b2
    
    in (ourGlobal func "@meaning_of_life") { (func, the) =>
      in (the bb "%entry") { my =>
      
        my inst "%ret" shouldBeA[InstRet] { its =>
          its.retVals shouldBe Seq(ourOld const "@I64_42")
        }
      }
    }
    
    versionsOf(ourGlobal func "@foxsay") shouldBe empty
    versionsOf(ourGlobal func "@meaning_of_life") shouldBe Seq(ourGlobal funcVer "@meaning_of_life.v1")
     
    ourNew.funcNs.get("@foxsay") shouldBe None
    ourNew.funcNs.get("@meaning_of_life") shouldBe None
   
    (ourNew funcVer "@meaning_of_life.v2").func shouldBe (ourGlobal func "@meaning_of_life")
    (ourNew funcVer "@foxsay.v1").func shouldEqual (ourGlobal func "@foxsay")
    
    in (ourNew funcVer "@meaning_of_life.v2") { the =>
      in (the bb "%entry") { my =>
      
        my inst "%ret" shouldBeA[InstRet] { its =>
          its.retVals shouldBe Seq(ourNew const "@I64_43")
        }
      }
    }
    
    in (ourNew funcVer "@foxsay.v1") {  the =>
      in (the bb "%entry") { my =>
      
        my inst "%ret" shouldBeA[InstRet] { its =>
          its.retVals shouldBe Seq(ourNew const "@I64_99")
        }
      }
    }
  }

  def validateRedefAfterMerge(globalBundle: GlobalBundle, bundle: TrantientBundle) {
    implicit val ourGlobal = globalBundle
    val ourNew = bundle
    
    versionsOf(ourGlobal func "@foxsay").head shouldBe (ourNew funcVer "@foxsay.v1")
    versionsOf(ourGlobal func "@meaning_of_life").head shouldBe (ourNew funcVer "@meaning_of_life.v2")
    versionsOf(ourGlobal func "@meaning_of_life").tail.head shouldBe (ourGlobal funcVer "@meaning_of_life.v1")
    
    (ourGlobal funcVer "@meaning_of_life.v1").func shouldBe (ourGlobal func "@meaning_of_life")
    (ourGlobal funcVer "@meaning_of_life.v2").func shouldBe (ourGlobal func "@meaning_of_life")
    
    val foxSay = ourGlobal func "@foxsay"
    ourGlobal value "@foxsay" shouldBe foxSay
    ourGlobal globalValue "@foxsay" shouldBe foxSay
    
    {
      val its = ourGlobal expFunc "@meaning_external1"
      its.func shouldBe (ourGlobal func "@meaning_of_life")
      its.callConv shouldBe Flag("#DEFAULT")
      its.cookie shouldBe (ourGlobal const "@I64_42")
    }

    {
      val its = ourGlobal expFunc "@meaning_external2"
      its.func shouldBe (ourGlobal func "@meaning_of_life")
      its.callConv shouldBe Flag("#DEFAULT")
      its.cookie shouldBe (ourGlobal const "@I64_43")
    }
  }
  
}