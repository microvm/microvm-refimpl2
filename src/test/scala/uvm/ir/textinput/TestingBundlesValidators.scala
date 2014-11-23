package uvm.ir.textinput

import org.scalatest._

import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.comminsts._
import UIRTextReader.globalize

trait TestingBundlesValidators extends Matchers with ExtraMatchers {
  
  implicit class MagicalOur(b: Bundle) {
    def anything(s: String) = b.allNs(s)
    def ty(s: String) = b.typeNs(s)
    def const(s: String) = b.constantNs (s)
    def value(s: String) = b.varNs(s)
    def globalValue(s: String) = b.globalVarNs(s)
    def globalCell(s: String) = b.globalCellNs(s)
    def sig(s: String) = b.funcSigNs(s)
    def func(s: String) = b.funcNs(s)
    def funcVer(s: String) = b.funcVerNs(s)
  }

  implicit class MagicalMy(c: FuncVer) {
    def globalName(s: String) = UIRTextReader.globalize(s, c.name.get)
    def bb(s: String) = c.bbNs(UIRTextReader.globalize(s, c.name.get))
    def value(s: String) = c.localVarNs(UIRTextReader.globalize(s, c.name.get))
    def param(s: String) = c.localVarNs(UIRTextReader.globalize(s, c.name.get))
    def inst(s: String) = c.localVarNs(UIRTextReader.globalize(s, c.name.get))
  }

  def validateTypes(bundle: Bundle) {
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
      its fieldTy 0 shouldBe (our ty "@i8")
      its fieldTy 1 shouldBe (our ty "@i16")
      its fieldTy 2 shouldBe (our ty "@i32")
      its fieldTy 3 shouldBe (our ty "@i64")
      its fieldTy 4 shouldBe (our ty "@float")
      its fieldTy 5 shouldBe (our ty "@double")
      its fieldTy 6 shouldBe (our ty "@rv")
      its fieldTy 7 shouldBe (our ty "@irv")
      its fieldTy 8 shouldBe (our ty "@wrv")
      its fieldTy 9 shouldBe (our ty "@ri16")
    }
    
    our ty "@Cons" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBe (our ty "@i64")
      its fieldTy 1 shouldBe (our ty "@RefCons")
    }
    
    our ty "@RefCons" shouldBeA[TypeRef] { _.ty should be (our ty "@Cons") }

    our ty "@foo" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBe (our ty "@double")
      its fieldTy 1 shouldBe (our ty "@i64")
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
      its.fixedTy shouldBe (our ty "@void")
      its.varTy shouldBe (our ty "@i8")
    }
    our ty "@h1" shouldBeA[TypeHybrid] { its =>
      its.fixedTy shouldBe (our ty "@foo")
      its.varTy shouldBe (our ty "@i64")
    }
    
    our ty "@void" shouldBeA[TypeVoid] thatsIt
    
    our sig "@sig0" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@void")
      its.paramTy shouldBe empty
    }
    
    our ty "@ii8" shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@i8") }
    our ty "@iii8" shouldBeA[TypeIRef] { _.ty shouldBe (our ty "@ii8") }
    
    our sig "@sig1" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@i32")
      its paramTy 0 shouldBe (our ty "@i32")
      its paramTy 1 shouldBe (our ty "@iii8")
    }
    
    our ty "@f0" shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@sig0") }
    our ty "@f1" shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@sig1") }
    
    our ty "@th" shouldBeA[TypeThread] thatsIt
    our ty "@st" shouldBeA[TypeStack] thatsIt
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
    
    // Testing namespaces
    val i8 = our ty "@i8"
    our anything "@i8" shouldBe i8
    
    val sig0 = our sig "@sig0"
    our anything "@sig0" shouldBe sig0
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
      its.constTy shouldBe (our ty "@s1")
      its fields 0 shouldBe (our const "@ci64")
      its fields 1 shouldBe (our const "@cd")
    }
    
    our const "@cs2" shouldBeA[ConstStruct] { its=>
      its.constTy shouldBe (our ty "@s2")
      its fields 0 shouldBe (our const "@cf")
      its fields 1 shouldBe (our const "@ci64")
    }
    
    our const "@cs3" shouldBeA[ConstStruct] { its=>
      its.constTy shouldBe (our ty "@s3")
      its fields 0 shouldBe (our const "@cd")
      its fields 1 shouldBe (our const "@cs2")
      its fields 2 shouldBe (our const "@ci32")
    }
      
    our const "@cr" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@rv") }
    our const "@cir" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@irv") }
    our const "@cfu" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@func0") }
    our const "@cth" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@thread") }
    our const "@cst" shouldBeA[ConstNull] { _.constTy shouldBe (our ty "@stack") }

    our const "@cv4f" shouldBeA[ConstVector] { its =>
      its.constTy shouldBe (our ty "@4xfloat")
      its elems 0 shouldBe (our const "@F_1")
      its elems 1 shouldBe (our const "@F_2")
      its elems 2 shouldBe (our const "@F_3")
      its elems 3 shouldBe (our const "@F_4")
    }
    
    our const "@cv4i" shouldBeA[ConstVector] { its =>
      its.constTy shouldBe (our ty "@4xi32")
      its elems 0 shouldBe (our const "@I32_1")
      its elems 1 shouldBe (our const "@I32_2")
      its elems 2 shouldBe (our const "@I32_3")
      its elems 3 shouldBe (our const "@I32_4")
    }
 
    our const "@cv4d" shouldBeA[ConstVector] { its =>
      its.constTy shouldBe (our ty "@2xdouble")
      its elems 0 shouldBe (our const "@D_1")
      its elems 1 shouldBe (our const "@D_2")
    }

    our globalCell "@gi64" shouldBeA[GlobalCell] { _.cellTy shouldBe (our ty "@i64") }

    our func "@fdummy" shouldBeA[Function] { _.sig shouldBe (our sig "@sig0") }
    
    our const "@sgf" shouldBeA[ConstStruct] { its =>
      its.constTy shouldBe (our ty "@sgf_t")
      its fields 0 shouldBe (our globalValue "@gi64")
      its fields 1 shouldBe (our globalValue "@fdummy")
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
  
  def validateFunctions(bundle: Bundle) {
    val our = bundle
    
    our sig "@foo" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@void")
      its.paramTy shouldBe empty
    }

    our sig "@bar" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@i64")
      its paramTy 0 shouldBe (our ty "@i32")
      its paramTy 1 shouldBe (our ty "@i16")
    }
    
    our sig "@baz" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@i32")
      its paramTy 0 shouldBe (our ty "@i32")
      its paramTy 1 shouldBe (our ty "@iii8")
    }
    
    our sig "@sig_fs" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@void")
      its paramTy 0 shouldBe (our ty "@i32")
    }
    
    our ty "@sig_t" shouldBeA[TypeFunc] { _.sig shouldBe (our sig "@sig_fs") }

    our sig "@signal_sig" shouldBeA[FuncSig] { its =>
      its.retTy shouldBe (our ty "@sig_t")
      its paramTy 0 shouldBe (our ty "@i32")
      its paramTy 1 shouldBe (our ty "@sig_t")
    }
    
    our func "@signal" shouldBeA[Function] { its =>
      its.sig shouldBe (our sig "@signal_sig")
      its.versions shouldBe Nil
    }
    
    our const "@zero" shouldBeA[ConstInt] { its =>
      its.constTy shouldBe (our ty "@i32")
      its.num shouldBe 0
    }
    
    our func "@main" shouldBeA[Function] { its =>
      its.sig shouldBe (our sig "@baz")
      its.versions.head shouldBe (our funcVer "@main_v1")
    }
    
    our funcVer "@main_v1" shouldBeA[FuncVer] { its =>
      val theFuncVer = its
      its.sig shouldBe (our sig "@baz")
      its params 0 shouldBeA[Parameter] { whose =>
        whose.funcVer shouldEqual theFuncVer
        whose.name.get shouldEqual (globalize("%argc", its.name.get))
        whose.index shouldEqual 0
      }
      its params 1 shouldBeA[Parameter] { whose =>
        whose.funcVer shouldEqual theFuncVer
        whose.name.get shouldEqual (globalize("%argv", its.name.get))
        whose.index shouldEqual 1
      }
    }
    
    // Testing namespaces
    val main = our func "@main"
    our globalValue "@main" shouldBe main
    our value "@main" shouldBe main
    our anything "@main" shouldBe main
    
    val mainV1 = our funcVer "@main_v1"
    our anything "@main_v1" shouldBe mainV1

    val argcGN = "@main_v1.argc"
    val argc = mainV1.localVarNs(argcGN)
    our value argcGN shouldBe argc
    our anything argcGN shouldBe argc
    
    val addGN = "@main_v1.add"
    val add = mainV1.localVarNs(addGN)
    our value addGN shouldBe add 
    our anything addGN shouldBe add 
  }

  def in(func: Function)(f: (Function, FuncVer) => Unit) {
    val ver = func.versions.head
    f(func, ver)
  }
  
  def validateInstructions(bundle: Bundle) {
    val our = bundle
    
    in (our func "@intBinOpTest") { (func, ver) =>
      val my = ver
      
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
      
      for (i <- ver.localVarNs.all; if i.isInstanceOf[InstBinOp]) {
        i.asInstanceOf[InstBinOp].opndTy shouldBe (our ty "@i32")
        i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
      }
    }   

    in (our func "@fpBinOpTest") { (func, ver) =>
      val my = ver
      
      my inst "%fadd" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FADD }
      my inst "%fsub" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FSUB }
      my inst "%fmul" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FMUL }
      my inst "%fdiv" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FDIV }
      my inst "%frem" shouldBeA[InstBinOp] { _.op shouldBe BinOptr.FREM }
      
      for (i <- ver.localVarNs.all; if i.isInstanceOf[InstBinOp]) {
        i.asInstanceOf[InstBinOp].opndTy shouldBe (our ty "@double")
        i.asInstanceOf[InstBinOp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstBinOp].op2 shouldBe (my param "%p1")
      }
    }   

    in (our func "@intCmpTest") { (func, ver) =>
      val my = ver
      
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
      
      for (i <- ver.localVarNs.all; if i.isInstanceOf[InstCmp]) {
        i.asInstanceOf[InstCmp].opndTy shouldBe (our ty "@i64")
        i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
      }
    }   
    
    in (our func "@fpCmpTest") { (func, ver) =>
      val my = ver
      
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
      
      for (i <- ver.localVarNs.all; if i.isInstanceOf[InstCmp]) {
        i.asInstanceOf[InstCmp].opndTy shouldBe (our ty "@float")
        i.asInstanceOf[InstCmp].op1 shouldBe (my param "%p0")
        i.asInstanceOf[InstCmp].op2 shouldBe (my param "%p1")
      }
    }

    in (our func "@convTest") { (func, ver) =>
      val my = ver
      
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

    in (our func "@refCastTest") { (func, ver) =>
      val my = ver
      
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

    in (our func "@ctrlFlow") { (func, ver) =>
      val my = ver
      
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
        its.opndTy shouldBe (our ty "@i32")
        its.opnd shouldBe (my inst "%phi")
        its.defDest shouldBe (my bb "%other")
        its cases 0 shouldBe ((our value "@I32_1"), (my bb "%one"))  
        its cases 1 shouldBe ((our value "@I32_2"), (my bb "%two"))  
      }
      
      my inst "%phi" shouldBeA[InstPhi] { its =>
        its.opndTy shouldBe (our ty "@i32")
        its cases 0 shouldBe ((my bb "%entry"), (our value "@I32_0"))
        its cases 1 shouldBe ((my bb "%next"), (my value "%i2"))
      }
    }
    
    in (our func "@callee2") { (func, ver) =>
      val my = ver
      
      my inst "%ret" shouldBeA[InstRet] { its =>
        its.retTy shouldBe (our ty "@i64")
        its.retVal shouldBe (my inst "%rv")
      }
    }     
 
    in (our func "@callee3") { (func, ver) =>
      val my = ver
      
      my inst "%throw" shouldBeA[InstThrow] { its =>
        its.excVal shouldBe (my inst "%exc")
      }
    }

    in (our func "@caller1") { (func, ver) =>
      val my = ver
      
      my inst "%v1" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@npnr_sig")
        its.callee shouldBe (our value "@callee1")
        its.argList shouldBe empty
        its.excClause shouldBe None
        its.keepAlives shouldBe empty
      }
      
      my inst "%v2" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@iii_sig")
        its.callee shouldBe (our value "@callee2")
        its.argList shouldEqual Seq("@I64_1", "@I64_2").map(our.value)
        its.excClause shouldBe None
        its.keepAlives shouldBe empty
      }

      my inst "%v3" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@iii_sig")
        its.callee shouldBe (our value "@callee3")
        its.argList shouldEqual Seq("@I64_1", "@I64_2").map(our.value)
        its.excClause shouldEqual Some(ExcClause((my bb "%cont"), (my bb "%catch")))
        its.keepAlives shouldBe empty
      }     
      
      my inst "%v4" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@npnr_sig")
        its.callee shouldBe (our globalValue "@callee1")
        its.argList shouldBe empty
        its.excClause shouldBe None
        its.keepAlives shouldBe Seq("%v2", "%v3").map(my.value)
      }

      my inst "%v5" shouldBeA[InstCall] { its =>
        its.sig shouldBe (our sig "@iii_sig")
        its.callee shouldBe (our globalValue "@callee3")
        its.argList shouldBe Seq("%v3", "%v3").map(my.value)
        its.excClause shouldEqual Some(ExcClause((my bb "%cont2"), (my bb "%catch")))
        its.keepAlives shouldEqual Seq(my value "%v2")
      }  
      
      my inst "%retv" shouldBeA[InstRetVoid] thatsIt
      my inst "%exc" shouldBeA[InstLandingPad] thatsIt
    }

    in (our func "@caller2") { (func, ver) =>
      val my = ver
      
      my inst "%tc" shouldBeA[InstTailCall] { its =>
        its.sig shouldBe (our sig "@iii_sig")
        its.callee shouldBe (our globalValue "@callee2")
        its.argList shouldEqual Seq("%p0", "%p1").map(my.inst)
      }
    }

    in (our func "@aggregate") { (func, ver) =>
      val my = ver
      
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
        its.vecTy shouldBe (our ty "@4xfloat")
        its.indTy shouldBe (our ty "@i32")
        its.opnd shouldBe (our value "@v1")
        its.index shouldBe (our value "@I32_0")
      }
      
      my inst "%ie0" shouldBeA[InstInsertElement] { its =>
        its.vecTy shouldBe (our ty "@4xfloat")
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

    in (our func "@memops") { (func, ver) =>
      val my = ver
      
      my inst "%new" shouldBeA[InstNew] { its =>
        its.allocTy shouldBe (our ty "@i64")
        its.excClause shouldBe None
      }
      
      my inst "%newhybrid" shouldBeA[InstNewHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.lenTy shouldBe (our ty "@i64")
        its.length shouldBe (my param "%p0")
        its.excClause shouldBe None
      }
      
      my inst "%alloca" shouldBeA[InstAlloca] { its =>
        its.allocTy shouldBe (our ty "@i64")
        its.excClause shouldBe None
      }
      
      my inst "%allocahybrid" shouldBeA[InstAllocaHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.lenTy shouldBe (our ty "@i64")
        its.length shouldBe (my param "%p0")
        its.excClause shouldBe None
      }
     
      my inst "%new_s" shouldBeA[InstNew] { its =>
        its.allocTy shouldBe (our ty "@i64")
        its.excClause shouldBe Some(ExcClause((my bb "%bb2"), (my bb "%handler")))
      }
      
      my inst "%newhybrid_s" shouldBeA[InstNewHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.lenTy shouldBe (our ty "@i64")
        its.length shouldBe (my param "%p0")
        its.excClause shouldBe Some(ExcClause((my bb "%bb3"), (my bb "%handler")))
      }
      
      my inst "%alloca_s" shouldBeA[InstAlloca] { its =>
        its.allocTy shouldBe (our ty "@i64")
        its.excClause shouldBe Some(ExcClause((my bb "%bb4"), (my bb "%handler")))
      }
      
      my inst "%allocahybrid_s" shouldBeA[InstAllocaHybrid] { its =>
        its.allocTy shouldBe (our ty "@hic")
        its.lenTy shouldBe (our ty "@i64")
        its.length shouldBe (my param "%p0")
        its.excClause shouldBe Some(ExcClause((my bb "%bb5"), (my bb "%handler")))
      }

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
        its.opnd shouldBe (my inst "%new2")
      }
      
      my inst "%getfieldiref" shouldBeA[InstGetFieldIRef] { its =>
        its.referentTy shouldBe (our ty "@sid")
        its.index shouldBe 0
        its.opnd shouldBe (my inst "%getiref")
      }
      
      my inst "%getelemiref" shouldBeA[InstGetElemIRef] { its =>
        its.referentTy shouldBe (our ty "@al")
        its.indTy shouldBe (our ty "@i64")
        its.opnd shouldBe (my inst "%alloca2")
        its.index shouldBe (my param "%p1")
      }
      
      my inst "%shiftiref" shouldBeA[InstShiftIRef] { its =>
        its.referentTy shouldBe (our ty "@i8")
        its.offTy shouldBe (our ty "@i64")
        its.opnd shouldBe (my inst "%getvarpartiref")
        its.offset shouldBe (my param "%p1")
      }

      my inst "%getfixedpartiref" shouldBeA[InstGetFixedPartIRef] { its =>
        its.referentTy shouldBe (our ty "@hic")
        its.opnd shouldBe (my inst "%allocahybrid")
      }
      
      my inst "%getvarpartiref" shouldBeA[InstGetVarPartIRef] { its =>
        its.referentTy shouldBe (our ty "@hic")
        its.opnd shouldBe (my inst "%allocahybrid")
      }
      
      my inst "%load" shouldBeA[InstLoad] { its =>
        its.ord shouldBe MemoryOrder.NOT_ATOMIC
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.excClause shouldBe None
      }
      
      my inst "%store" shouldBeA[InstStore] { its =>
        its.ord shouldBe MemoryOrder.NOT_ATOMIC
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.newVal shouldBe (our const "@I64_42")
        its.excClause shouldBe None
      }
      
      my inst "%cmpxchg" shouldBeA[InstCmpXchg] { its =>
        its.ordSucc shouldBe MemoryOrder.SEQ_CST
        its.ordFail shouldBe MemoryOrder.SEQ_CST
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.expected shouldBe (our const "@I64_42")
        its.desired shouldBe (our const "@I64_0")
        its.excClause shouldBe None
      }
    
      my inst "%atomicrmw" shouldBeA[InstAtomicRMW] { its =>
        its.ord shouldBe MemoryOrder.SEQ_CST
        its.op shouldBe AtomicRMWOptr.ADD
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.opnd shouldBe (our const "@I64_43")
        its.excClause shouldBe None
      }
   
      my inst "%load_s" shouldBeA[InstLoad] { its =>
        its.ord shouldBe MemoryOrder.NOT_ATOMIC
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.excClause shouldBe Some(ExcClause((my bb "%bb6"), (my bb "%handler")))
      }
      
      my inst "%store_s" shouldBeA[InstStore] { its =>
        its.ord shouldBe MemoryOrder.NOT_ATOMIC
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.newVal shouldBe (our const "@I64_42")
        its.excClause shouldBe Some(ExcClause((my bb "%bb7"), (my bb "%handler")))
      }
      
      my inst "%cmpxchg_s" shouldBeA[InstCmpXchg] { its =>
        its.ordSucc shouldBe MemoryOrder.SEQ_CST
        its.ordFail shouldBe MemoryOrder.SEQ_CST
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.expected shouldBe (our const "@I64_42")
        its.desired shouldBe (our const "@I64_0")
        its.excClause shouldBe Some(ExcClause((my bb "%bb8"), (my bb "%handler")))
     }
    
      my inst "%atomicrmw_s" shouldBeA[InstAtomicRMW] { its =>
        its.ord shouldBe MemoryOrder.SEQ_CST
        its.op shouldBe AtomicRMWOptr.ADD
        its.referentTy shouldBe (our ty "@i64")
        its.loc shouldBe (my inst "%alloca")
        its.opnd shouldBe (our const "@I64_43")
        its.excClause shouldBe Some(ExcClause((my bb "%bb9"), (my bb "%handler")))
      }
      
      my inst "%fence" shouldBeA[InstFence] { its =>
        its.ord shouldBe (MemoryOrder.SEQ_CST)
      }
    }

    in (our func "@memorder") { (func, ver) =>
      val my = ver
      
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
    
    in (our func "@atomicrmwops") { (func, ver) =>
      val my = ver
      
      my inst "%old0" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.XCHG }
      my inst "%old1" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.ADD }
      my inst "%old2" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.SUB }
      my inst "%old3" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.AND }
      my inst "%old4" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.NAND }
      my inst "%old5" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.OR }
      my inst "%old6" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.XOR }
      my inst "%old7" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.MAX }
      my inst "%old8" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.MIN }
      my inst "%old9" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.UMAX }
      my inst "%olda" shouldBeA[InstAtomicRMW] { _.op shouldBe AtomicRMWOptr.UMIN }
    }

    in (our func "@traps") { (func, ver) =>
      val my = ver
      
      my inst "%tp" shouldBeA[InstTrap] { its =>
        its.retTy shouldBe (our ty "@i32")
        its.excClause shouldBe None
        its.keepAlives shouldBe Seq(my inst "%a")
      }
       
      my inst "%tp_s" shouldBeA[InstTrap] { its =>
        its.retTy shouldBe (our ty "@i64")
        its.excClause shouldBe Some(ExcClause(my bb "%tp_s_cont", my bb "%tp_s_exc"))
        its.keepAlives shouldBe Seq(my inst "%b")
      }
      
      my inst "%wp" shouldBeA[InstWatchPoint] { its =>
        its.wpID shouldBe 1
        its.retTy shouldBe (our ty "@float")
        its.dis shouldBe (my bb "%wp_dis_cont")
        its.ena shouldBe (my bb "%wp_ena_cont")
        its.exc shouldBe None
        its.keepAlives shouldBe Seq(my inst "%a")
      }
      
      my inst "%wp_s" shouldBeA[InstWatchPoint] { its =>
        its.wpID shouldBe 2
        its.retTy shouldBe (our ty "@double")
        its.dis shouldBe (my bb "%wp_s_dis_cont")
        its.ena shouldBe (my bb "%wp_s_ena_cont")
        its.exc shouldBe Some(my bb "%wp_s_exc")
        its.keepAlives shouldBe Seq(my inst "%b")
      }
    }

    in (our func "@ccall") { (func, ver) =>
      val my = ver
      
      my inst "%rv" shouldBeA[InstCCall] { its =>
        its.callConv shouldBe CallConv.DEFAULT
        its.funcTy shouldBe (our ty "@i64")
        its.sig shouldBe (our sig "@ccall_callee_sig")
        its.callee shouldBe (my param "%p0")
        its.argList shouldBe Seq(our value "@D_1")
      }
    }
    
    in (our func "@gen") { (func, ver) =>
      val my = ver
      
      my inst "%ss1" shouldBeA[InstSwapStack] { its =>
        its.swappee shouldBe (my value "%main")
        its.curStackAction shouldBe RetWith(our ty "@void")
        its.newStackAction shouldBe PassValue(our ty "@i64", our value "@I64_0")
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
   
    in (our func "@swapstack") { (func, ver) =>
      val my = ver
      
      my inst "%curstack" shouldBeA[InstCommInst] { its =>
        its.inst shouldBe (CommInsts("@uvm.current_stack"))
        its.typeList shouldBe empty
        its.argList shouldBe empty
        its.excClause shouldBe None
        its.keepAlives shouldBe empty
      }
      
      my inst "%coro" shouldBeA[InstNewStack] { its =>
        its.sig shouldBe (our sig "@iii_sig")
        its.callee shouldBe (our value "@callee2")
        its.argList shouldBe Seq(my value "%curstack")
        its.excClause shouldBe Some(ExcClause(my bb "%cont", my bb "%exc"))
      }
      
      my inst "%ss1" shouldBeA[InstSwapStack] { its =>
        its.swappee shouldBe (my value "%coro")
        its.curStackAction shouldBe RetWith(our ty "@i64")
        its.newStackAction shouldBe PassVoid()
        its.excClause shouldBe None
        its.keepAlives shouldBe Seq(my value "%curstack")
      }
      my inst "%ss2" shouldBeA[InstSwapStack] { its =>
        its.swappee shouldBe (my value "%coro")
        its.curStackAction shouldBe RetWith(our ty "@i64")
        its.newStackAction shouldBe PassVoid()
        its.excClause shouldBe Some(ExcClause(my bb "%nor", my bb "%exc"))
        its.keepAlives shouldBe empty
      }
    }
    
    in (our func "@comminst") { (func, ver) =>
      val my = ver
      
      my inst "%thr" shouldBeA[InstCommInst] { its =>
        its.inst shouldBe CommInsts("@uvm.new_thread")
        its.typeList shouldBe empty
        its.argList shouldBe Seq(my value "%sta")
        its.excClause shouldBe None
        its.keepAlives shouldBe empty
      }
      
      my inst "%th_ex" shouldBeA[InstCommInst] { its =>
        its.inst shouldBe CommInsts("@uvm.thread_exit")
        its.typeList shouldBe empty
        its.argList shouldBe empty
        its.excClause shouldBe None
        its.keepAlives shouldBe empty
      }

    }
  }

  def validateRedef(globalBundle: Bundle, bundle: Bundle) {
    val ourOld = globalBundle
    val ourNew = bundle
    
    in (ourOld func "@meaning_of_life") { (func, ver) =>
      val my = ver
      
      my inst "%ret" shouldBeA[InstRet] { its =>
        its.retTy shouldBe (ourOld ty "@i64")
        its.retVal shouldBe (ourOld const "@I64_42")
      }
    }
    
    (ourOld func "@foxsay").versions shouldBe Nil
    (ourOld func "@meaning_of_life").versions shouldBe Seq(ourOld funcVer "@meaning_of_life_v1")
    
    (ourNew func "@meaning_of_life").id shouldEqual (ourOld func "@meaning_of_life").id
    (ourNew func "@foxsay").id shouldEqual (ourOld func "@foxsay").id
    
    in (ourNew func "@meaning_of_life") { (func, ver) =>
      val my = ver
      
      my inst "%ret" shouldBeA[InstRet] { its =>
        its.retTy shouldBe (ourOld ty "@i64")
        its.retVal shouldBe (ourNew const "@I64_43")
      }
    }
    
    (ourNew func "@foxsay").versions shouldBe Seq(ourNew funcVer "@foxsay_v1")
    (ourNew func "@meaning_of_life").versions shouldBe Seq(ourNew funcVer "@meaning_of_life_v2")
    
    in (ourNew func "@foxsay") { (func, ver) =>
      val my = ver
      
      my inst "%ret" shouldBeA[InstRet] { its =>
        its.retTy shouldBe (ourOld ty "@i64")
        its.retVal shouldBe (ourNew const "@I64_99")
      }
    }
  }
  
  def validateRedefAfterMerge(globalBundle: Bundle, bundle: Bundle) {
    val ourGlobal = globalBundle
    val ourNew = bundle
    
    (ourGlobal func "@foxsay").versions.head shouldBe (ourNew funcVer "@foxsay_v1")
    (ourGlobal func "@meaning_of_life").versions.head shouldBe (ourNew funcVer "@meaning_of_life_v2")
    
    val foxSay = ourGlobal func "@foxsay"
    ourGlobal value "@foxsay" shouldBe foxSay
    ourGlobal globalValue "@foxsay" shouldBe foxSay
  }

}