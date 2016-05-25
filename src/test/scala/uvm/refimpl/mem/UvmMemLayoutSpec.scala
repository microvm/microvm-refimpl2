package uvm.refimpl.mem

import org.scalatest._

import TypeSizes._
import uvm._
import uvm.refimpl._
import uvm.refimpl.mem._
import uvm.types._

class UvmMemLayoutSpec extends UvmTestBase with BeforeAndAfter {
  "The sizes of primitive types" should "be appropriate" in {
    sizeOf(TypeInt(8)) shouldBe 1
    sizeOf(TypeInt(16)) shouldBe 2
    sizeOf(TypeInt(32)) shouldBe 4
    sizeOf(TypeInt(64)) shouldBe 8
    sizeOf(TypeFloat()) shouldBe 4
    sizeOf(TypeDouble()) shouldBe 8
    sizeOf(TypeRef(TypeVoid())) shouldBe 8
    sizeOf(TypeIRef(TypeVoid())) shouldBe 16
    sizeOf(TypeWeakRef(TypeVoid())) shouldBe 8
    sizeOf(TypeVoid()) shouldBe 0
    sizeOf(TypeFuncRef(FuncSig(Seq(), Seq()))) shouldBe 8
    sizeOf(TypeThreadRef()) shouldBe 8
    sizeOf(TypeStackRef()) shouldBe 8
    sizeOf(TypeTagRef64()) shouldBe 8
  }

  "The alignment of primitive types" should "be appropriate" in {
    alignOf(TypeInt(8)) shouldBe 1
    alignOf(TypeInt(16)) shouldBe 2
    alignOf(TypeInt(32)) shouldBe 4
    alignOf(TypeInt(64)) shouldBe 8
    alignOf(TypeFloat()) shouldBe 4
    alignOf(TypeDouble()) shouldBe 8
    alignOf(TypeRef(TypeVoid())) shouldBe 8
    alignOf(TypeIRef(TypeVoid())) shouldBe 16
    alignOf(TypeWeakRef(TypeVoid())) shouldBe 8
    alignOf(TypeVoid()) shouldBe 1
    alignOf(TypeFuncRef(FuncSig(Seq(), Seq()))) shouldBe 8
    alignOf(TypeThreadRef()) shouldBe 8
    alignOf(TypeStackRef()) shouldBe 8
    alignOf(TypeTagRef64()) shouldBe 8
  }

  "Struct types" should "have the size of all members plus padding and the alignment of the most strict member" in {
    val ty = TypeStruct(Seq(TypeInt(8), TypeInt(16), TypeInt(32), TypeInt(64)))
    sizeOf(ty) shouldBe 16
    alignOf(ty) shouldBe 8
  }

  "Struct types which contains other aggregate types" should "recursively calculate sizes and alignments" in {
    val ty1 = TypeStruct(Seq(TypeInt(8), TypeInt(16), TypeInt(32), TypeInt(64)))
    val ty2 = TypeStruct(Seq(TypeInt(16), ty1, TypeInt(32)))
    sizeOf(ty2) shouldBe 28
    alignOf(ty2) shouldBe 8
  }

  "Array types" should "have the size of all elements plus padding and the alignment of its element" in {
    val ty = TypeArray(TypeInt(64), 100)
    sizeOf(ty) shouldBe 800
    alignOf(ty) shouldBe 8
  }

  "Vector types" should "have the size of all elements and the alignment of its own size" in {
    val ty = TypeVector(TypeInt(32), 4)
    sizeOf(ty) shouldBe 16
    alignOf(ty) shouldBe 16
  }

  "The offset of struct fields" should "go past all previous fields and align to the current field" in {
    val ty = TypeStruct(Seq(TypeInt(8), TypeInt(16), TypeInt(32), TypeInt(64)))
    fieldOffsetOf(ty, 0) shouldBe 0
    fieldOffsetOf(ty, 1) shouldBe 2
    fieldOffsetOf(ty, 2) shouldBe 4
    fieldOffsetOf(ty, 3) shouldBe 8
  }

  "The offset of array elements" should "be as if shifting by the element size and aligned at each element" in {
    val ty = TypeArray(TypeInt(64), 100)
    elemOffsetOf(ty, 0L) shouldBe 0
    elemOffsetOf(ty, 50L) shouldBe 400
  }

  "In a hybrid, fields" should "be laid out in the fixed-then-var fasion" in {
    val ty = TypeHybrid(Seq(TypeInt(8), TypeInt(16), TypeInt(32), TypeInt(64)), TypeDouble())
    hybridSizeOf(ty, 10) shouldBe 96
    hybridAlignOf(ty, 10) shouldBe 8
    fieldOffsetOf(ty, 0) shouldBe 0
    fieldOffsetOf(ty, 1) shouldBe 2
    fieldOffsetOf(ty, 2) shouldBe 4
    fieldOffsetOf(ty, 3) shouldBe 8
    varPartOffsetOf(ty) shouldBe 16
  }

  "In a hybrid with no fixed parts, the variable part" should "have offset 0" in {
    val ty = TypeHybrid(Seq(), TypeFloat())
    hybridSizeOf(ty, 10) shouldBe 40
    hybridAlignOf(ty, 10) shouldBe 4
    varPartOffsetOf(ty) shouldBe 0
  }
}