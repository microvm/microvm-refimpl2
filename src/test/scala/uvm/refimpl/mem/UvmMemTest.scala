package uvm.refimpl.mem

import org.scalatest._
import uvm.refimpl._
import java.io.FileReader

class UvmMemSpec extends FlatSpec with Matchers with BeforeAndAfter {

  // The heap size is intentionally reduced to make GC more often
  val microVM = new MicroVM(heapSize = 512L * 1024L);

  {
    microVM.memoryManager.heap.space.debugLogBlockStates()
    
    val ca = microVM.newClientAgent()
    
    val r = new FileReader("tests/uvm-refimpl-test/uvm-mem-test-bundle.uir")
    ca.loadBundle(r)
    
    r.close()
    ca.close()

    microVM.memoryManager.heap.space.debugLogBlockStates()
  }

  behavior of "UVM memory manager"

  it should "allocate scalar objects" in {
     val ca = microVM.newClientAgent()
     
     val h = ca.newFixed(ca.idOf("@i64"))
     val h2 = ca.newFixed(ca.idOf("@a0"))
     
     ca.close()
     
     microVM.memoryManager.heap.space.debugLogBlockStates()
  }
  
  it should "allocate hybrid objects" in {
     val ca = microVM.newClientAgent()
     
     val hlen = ca.putInt(ca.idOf("@i64"), 1024)
     val h = ca.newHybrid(ca.idOf("@h0"), hlen)

     val hlen2 = ca.putInt(ca.idOf("@i64"), 128*1024)
     val h2 = ca.newHybrid(ca.idOf("@h0"), hlen2)
     
     ca.close()
  }
  
  it should "automatically trigger GC if the memory is full" in {
     val ca = microVM.newClientAgent()
     
     val hheld = ca.newFixed(ca.idOf("@i64"))   // Objects held in the CA should survive the GC

     val hlen = ca.putInt(ca.idOf("@i64"), 1024)
     
     val allocCount = 300  // enough to fill the 256KiB small object space at least once.
     
     for (i <- (0 until allocCount)) {
       val h = ca.newHybrid(ca.idOf("@h0"), hlen)
       ca.deleteHandle(h)
     }
     
     ca.close()
    
  }

  it should "defrag heavily fragmented heap" in {
     val ca = microVM.newClientAgent()
     
     val hlen = ca.putInt(ca.idOf("@i64"), 1024)
     
     val allocCount = 300  // enough to fill the 256KiB small object space at least once.
     
     for (i <- (0 until allocCount)) {
       val h = ca.newHybrid(ca.idOf("@h0"), hlen)
       ca.deleteHandle(h)
       val hBreadcrumb = ca.newFixed(ca.idOf("@i64"))
     }
     
     ca.close()
    
  }}