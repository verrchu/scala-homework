package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Semigroupal.Implicit._
import typeclasses.Applicative.Implicit._

class SemigroupalSpec extends AnyFlatSpec with should.Matchers {
  "Semigroupal" should "be implemented for Option" in {
    Option(1).product(Option("a")) should be(Option((1, "a")))
    Option(1).product(Option.empty[String]) should be(None)
    Option.empty[Int].product(Option("a")) should be(None)
    Option.empty[Int].product(Option.empty[String]) should be(None)
  }

  it should "be implemented for Either" in {
    (Right(1): Either[Int, Int]).product(
      Right("a"): Either[Int, String]
    ) should be(Right((1, "a")))

    (Left(1): Either[Int, Int]).product(
      Right("a"): Either[Int, String]
    ) should be(Left(1))

    (Right(1): Either[Int, Int]).product(
      Left(1): Either[Int, String]
    ) should be(Left(1))

    (Left(1): Either[Int, Int]).product(
      Left(2): Either[Int, String]
    ) should be(Left(1))
  }

  it should "be implemented for Map" in {
    Semigroupal[Map[String, *]].product(
      Map("a" -> 1, "b" -> 2),
      Map("b" -> List(2), "c" -> List(3))
    ) should be(
      Map("b" -> (2, List(2)))
    )

    Semigroupal[Map[String, *]].product(
      Map("a" -> 1, "b" -> 2),
      Map("c" -> List(3))
    ) should be(
      Map[String, (Int, String)]()
    )

    Semigroupal[Map[String, *]].product(
      Map[String, Int](),
      Map[String, String]()
    ) should be(
      Map[String, (Int, String)]()
    )
  }
}
