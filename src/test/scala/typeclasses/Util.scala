package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Monoid.Implicit._
import typeclasses.Functor.Implicit._
import typeclasses.Semigroupal.Implicit._
import typeclasses.Applicative.Implicit._
import typeclasses.Foldable.Implicit._

import typeclasses.Util.Implicit._

class UtilSpec extends AnyFlatSpec with should.Matchers {
  "Util" should "provide combineAll" in {
    Util.combineAll(List(1, 2, 3)) should be(6)
    Util.combineAll(List(Option(1), Option(2))) should be(Some(3))
  }

  "Util" should "provide mapN" in {
    (Option("a"), Option(2)).mapN(_ * _) should be(Some("aa"))
  }

  "Util" should "provide traverse" in {
    List(List(1), List(2)).traverse(_.headOption) should be(Some(List(2, 1)))
    List(List(1), List[Int]()).traverse(_.headOption) should be(None)
  }
}
