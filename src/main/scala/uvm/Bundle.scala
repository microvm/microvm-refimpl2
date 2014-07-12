package uvm

import uvm.types._
import uvm.ssavalues._

class Bundle {
  val typeNs = new SimpleNamespace[Type]()
  val funcSigNs = new SimpleNamespace[FuncSig]()
  val declConstNs = new SimpleNamespace[DeclaredConstant]()
  val globalDataNS = new SimpleNamespace[GlobalData]()
  val funcNs = new SimpleNamespace[Function]()
  val globalValueNS = new SimpleNamespace[Constant]()
}
