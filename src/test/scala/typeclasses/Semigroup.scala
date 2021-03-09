package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Semigroup.Implicit._
import typeclasses.Monoid.Implicit._

class SemigroupSpec extends AnyFlatSpec with should.Matchers {
  "Semigroup" should "be implemented for Int" in {
    (1 combine 2) should be(3)
  }

  it should "be implemented for Option" in {
    (Option(1) combine Option(1)) should be(Some(2))
    (Option(1) combine Option.empty[Int]) should be(Some(1))
    (Option.empty[Int] combine Option(1)) should be(Some(1))
    (Option.empty[Int] combine Option.empty[Int]) should be(None)
  }

  it should "be implemented for Function" in {
    val fa = (x: Int) => x * x
    val fb = (x: Int) => x + x

    val fCombined = fa.combine(fb)

    fCombined(10) should be(120)
  }
}
