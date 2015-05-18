package uvm.refimpl.misc

import org.scalatest._
import java.io.FileReader
import uvm._
import uvm.types._
import uvm.ssavariables._
import uvm.refimpl._
import uvm.refimpl.itpr._
import uvm.ssavariables.MemoryOrder._
import uvm.ssavariables.AtomicRMWOptr._
import uvm.refimpl.mem.TypeSizes.Word
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.{ Logger => LLogger }
import ch.qos.logback.classic.Level
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class HashableSSAValueTest extends FlatSpec with Matchers {
  "HashMap" should "be able to hold many items" in {
    val hm = new HashMap[Int, Int]()

    for (i <- 0 until 100) {
      val maybeOld = hm.put(i, i)
      maybeOld match {
        case None =>
        case Some(old) => {
          println("%d conflicts with %d".format(i, old))
        }
      }
    }
  }

  "GlobalCell" should "be hashable" in {
    val int64Ty = TypeInt(64)
    int64Ty.id = 65536
    int64Ty.name = Some("@i64")

    val buf = new ArrayBuffer[GlobalCell]()

    var nextID = 65537
    for (i <- 0 until 100) {
      val myID = nextID
      nextID += 1
      val gc = GlobalCell(int64Ty)
      gc.id = myID
      gc.name = Some("@g" + i)
      buf += gc
    }

    val hm = new HashMap[GlobalCell, GlobalCell]()

    for (gc <- buf) {
      println("hashCode of gc %s is %d".format(gc.repr, gc.hashCode()))
      val maybeOldGC = hm.put(gc, gc)

      if (!maybeOldGC.isEmpty) {
        val oldGC = maybeOldGC.get
        println("gc %s got oldgc %s".format(gc.repr, oldGC.repr))
        val equ = gc equals oldGC
        println("Equal? %s".format(equ))
      }
    }

    for (gc <- buf) {
      if (!hm.contains(gc)) {
        println("GlobalCell %s not contained!".format(gc.repr))
        fail
      }
    }
  }
}