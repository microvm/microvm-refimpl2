package uvm

import org.scalatest.Matchers
import org.scalatest.FlatSpec

class NameSpaceSpec extends FlatSpec with Matchers {
  behavior of "NestedNamespace"

  class Foo(val i: Int) extends IdentifiedSettable
  class Bar(i: Int, val j: Double) extends Foo(i)
  class Baz(i: Int, j: Double, val k: Long) extends Bar(i, j)

  import RichIdentifiedSettable._

  it should "remember things by ID and name" in {
    val ns = new NestedNamespace[Foo](None)
    val f = new Foo(42) := (1, "f")
    ns.add(f)

    val g = new Foo(99) := 2
    ns.add(g)

    ns.get(1) shouldBe Some(f)
    ns.get("f") shouldBe Some(f)
    ns.get(2) shouldBe Some(g)
    ns.get(3) shouldBe None
    ns.get("g") shouldBe None
  }

  it should "handle nested namespaces" in {
    val nsFoo = new NestedNamespace[Foo](None)
    val nsBar = nsFoo.makeSubSpace[Bar]()
    val nsBaz = nsBar.makeSubSpace[Baz]()

    val f = new Foo(1) := (1, "f")
    val g = new Bar(1, 2) := (2, "g")
    val h = new Baz(1, 2, 3) := (3, "h")

    nsFoo.add(f)
    nsBar.add(g)
    nsBaz.add(h)

    nsFoo(1) shouldBe f
    nsFoo(2) shouldBe g
    nsFoo(3) shouldBe h
    nsBar.get(1) shouldBe None
    nsBar(2) shouldBe g
    nsBar(3) shouldBe h
    nsBaz.get(1) shouldBe None
    nsBaz.get(2) shouldBe None
    nsBaz(3) shouldBe h

  }
}