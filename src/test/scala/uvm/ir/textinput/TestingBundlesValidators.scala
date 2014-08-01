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

  }
  def validateInstructions(bundle: Bundle) {
    val our = bundle

  }
}