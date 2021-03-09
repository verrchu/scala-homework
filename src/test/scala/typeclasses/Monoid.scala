package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Semigroup.Implicit._
import typeclasses.Monoid.Implicit._

class MonoidSpec extends AnyFlatSpec with should.Matchers {
  "Monoid" should "be implemented for Int" in {
    Monoid[Int].empty should be(0)
  }

  it should "be implemented for Option" in {
    Monoid[Option[Int]].empty should be(Option.empty[Int])
  }

  it should "be implemented for Function" in {
    val fEmpty = Monoid[Int => Int].empty

    fEmpty(42) should be(Monoid[Int].empty)
  }
}
