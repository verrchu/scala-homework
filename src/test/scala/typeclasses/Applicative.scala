package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Applicative.Implicit._

class ApplicativeSpec extends AnyFlatSpec with should.Matchers {
  "Applicative" should "be implemented for Option" in {
    Applicative[Option].pure(1) should be(Some(1))
  }

  it should "be implemented for Either" in {
    Applicative[Either[String, *]].pure(1) should be(Right(1))
  }
}
