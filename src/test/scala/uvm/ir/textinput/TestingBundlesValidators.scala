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
    def sig = b.funcSigNs
    def func = b.funcNs
  }

  def validateTypes(bundle: Bundle) {
    val our = bundle
    
    our ty "@i1" shouldBeA[TypeInt] { _.length shouldEqual 1 }
    our ty "@i8" shouldBeA[TypeInt] { _.length shouldEqual 8 }
    our ty "@i16" shouldBeA[TypeInt] { _.length shouldEqual 16 }
    our ty "@i32" shouldBeA[TypeInt] { _.length shouldEqual 32 }
    our ty "@i64" shouldBeA[TypeInt] { _.length shouldEqual 64 }
    
    our ty "@f" shouldBeA[TypeFloat] thatsIt
    our ty "@d" shouldBeA[TypeDouble] thatsIt
    
    our ty "@rv" shouldBeA[TypeRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    our ty "@irv" shouldBeA[TypeIRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    our ty "@wrv" shouldBeA[TypeWeakRef] { _.ty shouldBeA[TypeVoid] thatsIt }
    
    our ty "@ri16" shouldBeA[TypeRef] { _.ty shouldBeA[TypeInt] {_.length shouldEqual 16}}
    our ty "@ri16_2" shouldBeA[TypeRef] { _.ty shouldBeA[TypeInt] {_.length shouldEqual 16}}

    our ty "@s0" shouldBeA[TypeStruct] { _.fieldTy shouldBe empty }
    our ty "@s1" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBeA[TypeInt] { _.length shouldEqual 8 }
      its fieldTy 1 shouldBeA[TypeInt] { _.length shouldEqual 16 }
      its fieldTy 2 shouldBeA[TypeInt] { _.length shouldEqual 32 }
      its fieldTy 3 shouldBeA[TypeInt] { _.length shouldEqual 64 }
      its fieldTy 4 shouldBeA[TypeFloat] thatsIt
      its fieldTy 5 shouldBeA[TypeDouble] thatsIt
      its fieldTy 6 shouldBeA[TypeRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 7 shouldBeA[TypeIRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 8 shouldBeA[TypeWeakRef] { _.ty shouldBeA[TypeVoid] thatsIt }
      its fieldTy 9 shouldBeA[TypeRef] { _.ty shouldBeA[TypeInt] {_.length shouldEqual 16}}
      its fieldTy 10 shouldBeA[TypeRef] { _.ty shouldBeA[TypeInt] {_.length shouldEqual 16}}
    }
    
    our ty "@cons" shouldBeA[TypeStruct] { its =>
      its fieldTy 0 shouldBeA[TypeInt] { _.length shouldEqual 64 }
      its fieldTy 1 shouldBeA[TypeRef] { _.ty shouldBe (our ty "@cons")}
    }
    
    our ty "@a0" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBeA[TypeInt] { _.length shouldEqual 8}
      its.len shouldEqual 100
    }
    our ty "@a1" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBeA[TypeDouble] thatsIt
        whose fieldTy 1 shouldBeA[TypeInt] { _.length shouldEqual 64}
      }
      its.len shouldEqual 10
    }
    our ty "@a2" shouldBeA[TypeArray] { its =>
      its.elemTy shouldBe (our ty "@a1")
      its.len shouldEqual 10
    }
    
    our ty "@h0" shouldBeA[TypeHybrid] { its =>
      its.fixedPart shouldBeA[TypeVoid] thatsIt
      its.varPart shouldBeA[TypeInt] {_.length shouldEqual 8}
    }
    our ty "@h1" shouldBeA[TypeHybrid] { its =>
      its.fixedPart shouldBeA[TypeStruct] { whose =>
        whose fieldTy 0 shouldBe (our ty "@i32")
        whose fieldTy 1 shouldBe (our ty "@i32")
        whose fieldTy 2 shouldBeA[TypeFloat] thatsIt
      }
      its.varPart shouldBeA[TypeInt] {_.length shouldEqual 64}
    }
    
    our ty "@v" shouldBeA[TypeVoid] thatsIt
    
    our ty "@f0" shouldBeA[TypeFunc] { its =>
      its.sig.retTy shouldBeA[TypeVoid] thatsIt
      its.sig.paramTy shouldBe empty
    }
    our ty "@f1" shouldBeA[TypeFunc] { its =>
      its.sig.retTy shouldBeA[TypeInt] {_.length shouldEqual 32}
      its.sig.paramTy(0) shouldBeA[TypeInt] {_.length shouldEqual 32}
      its.sig.paramTy(1) shouldBeA[TypeIRef] { _.ty shouldBeA[TypeIRef] {
        _.ty shouldBeA[TypeInt] {_.length shouldEqual 8}}
      }
    }
    
    our ty "@th" shouldBeA[TypeThread] thatsIt
    our ty "@st" shouldBeA[TypeStack] thatsIt
    our ty "@tr64" shouldBeA[TypeTagRef64] thatsIt
  }
}