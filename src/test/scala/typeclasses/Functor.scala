package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Functor.Implicit._
import typeclasses.Applicative.Implicit._

class FunctorSpec extends AnyFlatSpec with should.Matchers {
  "Functor" should "be implemented for Option" in {
    Option(1).fmap(_ + 10) should be(Option(11))
    Option.empty[Int].fmap(_ + 10) should be(Option.empty[Int])
  }

  it should "be implemented for Either" in {
    (Right(1): Either[Int, Int]).fmap(_ + 10) should be(Right(11))
    (Left(1): Either[Int, Int]).fmap(_ + 10) should be(Left(1))
  }

  it should "be implemented for Map" in {
    Map("a" -> 1, "b" -> 2).fmap(_ + 10) should be(Map("a" -> 11, "b" -> 12))
  }
}
