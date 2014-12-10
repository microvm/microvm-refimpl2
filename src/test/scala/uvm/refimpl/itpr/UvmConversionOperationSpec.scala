package uvm.refimpl.itpr

import uvm.refimpl.itpr._
import org.scalatest._
import scala.BigInt
import scala.math.BigInt.int2bigInt

object UvmConversionOperationSpec {
  val F_1P64 = (BigInt(1) << 64).floatValue
  val F_1P63 = (BigInt(1) << 63).floatValue
  val F_1P62 = (BigInt(1) << 62).floatValue
  val F_1P61 = (BigInt(1) << 61).floatValue
  val F_1P32 = (BigInt(1) << 32).floatValue
  val F_1P31 = (BigInt(1) << 31).floatValue
  val F_1P30 = (BigInt(1) << 30).floatValue
  val F_1P16 = (BigInt(1) << 16).floatValue
  val F_1P15 = (BigInt(1) << 15).floatValue
  val D_1P64 = (BigInt(1) << 64).doubleValue
  val D_1P63 = (BigInt(1) << 63).doubleValue
  val D_1P62 = (BigInt(1) << 62).doubleValue
  val D_1P61 = (BigInt(1) << 61).doubleValue
  val D_1P32 = (BigInt(1) << 32).doubleValue
  val D_1P31 = (BigInt(1) << 31).doubleValue
  val D_1P30 = (BigInt(1) << 30).doubleValue
  val D_1P16 = (BigInt(1) << 16).doubleValue
  val D_1P15 = (BigInt(1) << 15).doubleValue
}

class UvmConversionOperationSpec extends FlatSpec with Matchers {
  import UvmConversionOperationSpec._

  behavior of "Float to unsigned integer"

  it should "return 0 if the input is NaN" in {
    OpHelper.floatToI(java.lang.Float.NaN, 42, false) shouldBe 0
  }

  it should "return 0 for 0.0 or negative numbers" in {
    OpHelper.floatToI(0.0f, 64, false) shouldBe 0
    OpHelper.floatToI(-0.0f, 64, false) shouldBe 0
    OpHelper.floatToI(-1.0f, 64, false) shouldBe 0
    OpHelper.floatToI(java.lang.Float.NEGATIVE_INFINITY, 64, false) shouldBe 0
  }

  it should "return the largest unsigned integer when the source is too big" in {
    OpHelper.floatToI(java.lang.Float.POSITIVE_INFINITY, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.floatToI(1e30f, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.floatToI(F_1P64, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.floatToI(F_1P63, 63, false) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.floatToI(F_1P32, 32, false) shouldBe ((BigInt(1) << 32) - 1)
  }

  it should "return the number rounded down" in {
    OpHelper.floatToI(0.1f, 64, false) shouldBe 0
    OpHelper.floatToI(3.14f, 32, false) shouldBe 3
    OpHelper.floatToI(31.4f, 32, false) shouldBe 31
    OpHelper.floatToI(F_1P63, 64, false) shouldBe (BigInt(1) << 63)
    OpHelper.floatToI(F_1P62, 63, false) shouldBe (BigInt(1) << 62)
    OpHelper.floatToI(F_1P31, 32, false) shouldBe (BigInt(1) << 31)
    OpHelper.floatToI(1.0f, 32, false) shouldBe 1

    OpHelper.floatToI(F_1P16, 16, false) shouldBe ((BigInt(1) << 16) - 1) // (1<<16) - 1 == 65535
  }

  behavior of "Float to signed integer"

  it should "return 0 if the input is NaN" in {
    OpHelper.floatToI(java.lang.Float.NaN, 64, true) shouldBe 0
  }

  it should "return the largest signed integer when the source is too big" in {
    OpHelper.floatToI(java.lang.Float.POSITIVE_INFINITY, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.floatToI(1e30f, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.floatToI(F_1P64, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.floatToI(F_1P63, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.floatToI(F_1P63, 63, true) shouldBe ((BigInt(1) << 62) - 1)
    OpHelper.floatToI(F_1P62, 63, true) shouldBe ((BigInt(1) << 62) - 1)
    OpHelper.floatToI(F_1P32, 32, true) shouldBe ((BigInt(1) << 31) - 1)
    OpHelper.floatToI(F_1P31, 32, true) shouldBe ((BigInt(1) << 31) - 1)
  }

  it should "return the smallest signed integer when the source is too small" in {
    OpHelper.floatToI(java.lang.Float.NEGATIVE_INFINITY, 64, true) shouldBe (BigInt(1) << 63)
    OpHelper.floatToI(-1e30f, 64, true) shouldBe (BigInt(1) << 63)
    OpHelper.floatToI(-F_1P64, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 63), 64)
    OpHelper.floatToI(-F_1P63, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 63), 64)
    OpHelper.floatToI(-F_1P63, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 63)
    OpHelper.floatToI(-F_1P62, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 63)
    OpHelper.floatToI(-F_1P32, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 31), 32)
    OpHelper.floatToI(-F_1P31, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 31), 32)
    OpHelper.floatToI(-F_1P15, 16, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 15), 16)
  }

  it should "return the number rounded towards zero" in {
    OpHelper.floatToI(0.1f, 64, true) shouldBe 0
    OpHelper.floatToI(-0.1f, 64, true) shouldBe 0
    OpHelper.floatToI(3.14f, 32, true) shouldBe 3
    OpHelper.floatToI(31.4f, 32, true) shouldBe 31
    OpHelper.floatToI(-3.14f, 32, true) shouldBe OpHelper.unprepare(-3, 32)
    OpHelper.floatToI(-31.4f, 32, true) shouldBe OpHelper.unprepare(-31, 32)
    OpHelper.floatToI(F_1P62, 64, true) shouldBe (BigInt(1) << 62)
    OpHelper.floatToI(F_1P61, 63, true) shouldBe (BigInt(1) << 61)
    OpHelper.floatToI(F_1P30, 32, true) shouldBe (BigInt(1) << 30)
    OpHelper.floatToI(1.0f, 32, true) shouldBe 1 // 1
    OpHelper.floatToI(-F_1P62, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 64)
    OpHelper.floatToI(-F_1P61, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 61), 63)
    OpHelper.floatToI(-F_1P30, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 30), 32)
    OpHelper.floatToI(-1.0f, 32, true) shouldBe OpHelper.unprepare(-1, 32) // -1

    OpHelper.floatToI(32767.0f, 16, true) shouldBe ((BigInt(1) << 15) - 1)
    OpHelper.floatToI(-32767.0f, 16, true) shouldBe OpHelper.unprepare(-((BigInt(1) << 15) - 1), 16)
  }

  ////////// DUBLE ////////////

  behavior of "Double to unsigned integer"

  it should "return 0 if the input is NaN" in {
    OpHelper.doubleToI(java.lang.Double.NaN, 42, false) shouldBe 0
  }

  it should "return 0 for 0.0 or negative numbers" in {
    OpHelper.doubleToI(0.0f, 64, false) shouldBe 0
    OpHelper.doubleToI(-0.0f, 64, false) shouldBe 0
    OpHelper.doubleToI(-1.0f, 64, false) shouldBe 0
    OpHelper.doubleToI(java.lang.Double.NEGATIVE_INFINITY, 64, false) shouldBe 0
  }

  it should "return the largest unsigned integer when the source is too big" in {
    OpHelper.doubleToI(java.lang.Double.POSITIVE_INFINITY, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.doubleToI(1e30f, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.doubleToI(D_1P64, 64, false) shouldBe ((BigInt(1) << 64) - 1)
    OpHelper.doubleToI(D_1P63, 63, false) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.doubleToI(D_1P32, 32, false) shouldBe ((BigInt(1) << 32) - 1)
  }

  it should "return the number rounded down" in {
    OpHelper.doubleToI(0.1f, 64, false) shouldBe 0
    OpHelper.doubleToI(3.14f, 32, false) shouldBe 3
    OpHelper.doubleToI(31.4f, 32, false) shouldBe 31
    OpHelper.doubleToI(D_1P63, 64, false) shouldBe (BigInt(1) << 63)
    OpHelper.doubleToI(D_1P62, 63, false) shouldBe (BigInt(1) << 62)
    OpHelper.doubleToI(D_1P31, 32, false) shouldBe (BigInt(1) << 31)
    OpHelper.doubleToI(1.0f, 32, false) shouldBe 1

    OpHelper.doubleToI(D_1P16, 16, false) shouldBe ((BigInt(1) << 16) - 1) // (1<<16) - 1 == 65535
  }

  behavior of "Double to signed integer"

  it should "return 0 if the input is NaN" in {
    OpHelper.doubleToI(java.lang.Double.NaN, 64, true) shouldBe 0
  }

  it should "return the largest signed integer when the source is too big" in {
    OpHelper.doubleToI(java.lang.Double.POSITIVE_INFINITY, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.doubleToI(1e30d, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.doubleToI(D_1P64, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.doubleToI(D_1P63, 64, true) shouldBe ((BigInt(1) << 63) - 1)
    OpHelper.doubleToI(D_1P63, 63, true) shouldBe ((BigInt(1) << 62) - 1)
    OpHelper.doubleToI(D_1P62, 63, true) shouldBe ((BigInt(1) << 62) - 1)
    OpHelper.doubleToI(D_1P32, 32, true) shouldBe ((BigInt(1) << 31) - 1)
    OpHelper.doubleToI(D_1P31, 32, true) shouldBe ((BigInt(1) << 31) - 1)
  }

  it should "return the smallest signed integer when the source is too small" in {
    OpHelper.doubleToI(java.lang.Double.NEGATIVE_INFINITY, 64, true) shouldBe (BigInt(1) << 63)
    OpHelper.doubleToI(-1e30d, 64, true) shouldBe (BigInt(1) << 63)
    OpHelper.doubleToI(-D_1P64, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 63), 64)
    OpHelper.doubleToI(-D_1P63, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 63), 64)
    OpHelper.doubleToI(-D_1P63, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 63)
    OpHelper.doubleToI(-D_1P62, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 63)
    OpHelper.doubleToI(-D_1P32, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 31), 32)
    OpHelper.doubleToI(-D_1P31, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 31), 32)
    OpHelper.doubleToI(-D_1P15, 16, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 15), 16)
  }

  it should "return the number rounded towards zero" in {
    OpHelper.doubleToI(0.1d, 64, true) shouldBe 0
    OpHelper.doubleToI(-0.1d, 64, true) shouldBe 0
    OpHelper.doubleToI(3.14d, 32, true) shouldBe 3
    OpHelper.doubleToI(31.4d, 32, true) shouldBe 31
    OpHelper.doubleToI(-3.14d, 32, true) shouldBe OpHelper.unprepare(-3, 32)
    OpHelper.doubleToI(-31.4d, 32, true) shouldBe OpHelper.unprepare(-31, 32)
    OpHelper.doubleToI(D_1P62, 64, true) shouldBe (BigInt(1) << 62)
    OpHelper.doubleToI(D_1P61, 63, true) shouldBe (BigInt(1) << 61)
    OpHelper.doubleToI(D_1P30, 32, true) shouldBe (BigInt(1) << 30)
    OpHelper.doubleToI(1.0d, 32, true) shouldBe 1 // 1
    OpHelper.doubleToI(-D_1P62, 64, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 62), 64)
    OpHelper.doubleToI(-D_1P61, 63, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 61), 63)
    OpHelper.doubleToI(-D_1P30, 32, true) shouldBe OpHelper.unprepare(-(BigInt(1) << 30), 32)
    OpHelper.doubleToI(-1.0d, 32, true) shouldBe OpHelper.unprepare(-1, 32) // -1

    OpHelper.doubleToI(2147483647.0d, 32, true) shouldBe ((BigInt(1) << 31) - 1)
    OpHelper.doubleToI(-2147483647.0d, 32, true) shouldBe OpHelper.unprepare(-((BigInt(1) << 31) - 1), 32)
    OpHelper.doubleToI(32767.0d, 16, true) shouldBe ((BigInt(1) << 15) - 1)
    OpHelper.doubleToI(-32767.0d, 16, true) shouldBe OpHelper.unprepare(-((BigInt(1) << 15) - 1), 16)
  }

}