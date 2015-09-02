package uvm.refimpl.mem

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import MemoryOrder._
import AtomicRMWOptr._

class NativeMemoryAccessTest extends FlatSpec with Matchers {

  behavior of "The JNR-FFI-based native memory support"

  it should "Allocate memory and do basic access" in {
    val ms = new MemorySupport(1024L)
    
    val begin = ms.muMemoryBegin
    
    ms.storeI128(begin, (0x55555555aaaaaaaaL, 0xaaaaaaaa55555555L))
    ms.storeLong(begin+16, 0x55aa55aa55aa55aaL)
    ms.storeInt(begin+24, 0x5a5a5a5a)
    ms.storeShort(begin+28, 0x55a5)
    ms.storeByte(begin+30, 0x5a)
    ms.storeDouble(begin+32, 3.14)
    ms.storeFloat(begin+40, 6.28F)
    
    ms.loadDouble(begin+32) shouldBe 3.14
    ms.loadFloat(begin+40) shouldBe 6.28F
    ms.loadByte(begin+30) shouldBe 0x5a
    ms.loadShort(begin+28) shouldBe 0x55a5
    ms.loadInt(begin+24) shouldBe 0x5a5a5a5a
    ms.loadLong(begin+16) shouldBe 0x55aa55aa55aa55aaL
    ms.loadI128(begin) shouldBe (0x55555555aaaaaaaaL, 0xaaaaaaaa55555555L)
  }
  
  it should "Refuse out-of-bound access when the in-mu-memory checking is enabled" in {
    val ms = new MemorySupport(1024L)
    
    val begin = ms.muMemoryBegin
    val end = ms.muMemoryEnd
    
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeI128(end+1024L, (0x55555555aaaaaaaaL, 0xaaaaaaaa55555555L))
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeLong(end+1024L, 0x55aa55aa55aa55aaL)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeInt(end+1024L, 0x5a5a5a5a)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeShort(end+1024L, 0x55a5)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeByte(end+1024L, 0x5a)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeDouble(end+1024L, 3.14)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.storeFloat(end+1024L, 6.28F)
    
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadDouble(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadFloat(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadByte(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadShort(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadInt(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadLong(end+1024L)
    an [UvmIllegalMemoryAccessException] should be thrownBy ms.loadI128(end+1024L)
  }
  
  it should "allow out-of-bound access when the in-mu-memory checking is disabled" in {
    val ms1 = new MemorySupport(1024L)
    val ms2 = new MemorySupport(1024L)
    
    // Use ms1 to access ms2.
    val begin = ms2.muMemoryBegin
    
    ms1.storeI128(begin, (0x55555555aaaaaaaaL, 0xaaaaaaaa55555555L), inMu=false)
    ms1.storeLong(begin+16, 0x55aa55aa55aa55aaL, inMu=false)
    ms1.storeInt(begin+24, 0x5a5a5a5a, inMu=false)
    ms1.storeShort(begin+28, 0x55a5, inMu=false)
    ms1.storeByte(begin+30, 0x5a, inMu=false)
    ms1.storeDouble(begin+32, 3.14, inMu=false)
    ms1.storeFloat(begin+40, 6.28F, inMu=false)
    
    ms1.loadDouble(begin+32, inMu=false) shouldBe 3.14
    ms1.loadFloat(begin+40, inMu=false) shouldBe 6.28F
    ms1.loadByte(begin+30, inMu=false) shouldBe 0x5a
    ms1.loadShort(begin+28, inMu=false) shouldBe 0x55a5
    ms1.loadInt(begin+24, inMu=false) shouldBe 0x5a5a5a5a
    ms1.loadLong(begin+16, inMu=false) shouldBe 0x55aa55aa55aa55aaL
    ms1.loadI128(begin, inMu=false) shouldBe (0x55555555aaaaaaaaL, 0xaaaaaaaa55555555L)
  }
}