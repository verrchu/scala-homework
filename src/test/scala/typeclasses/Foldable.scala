package typeclasses

import org.scalatest._
import flatspec._
import matchers._

import typeclasses.Foldable.Implicit._

class FoldableSpec extends AnyFlatSpec with should.Matchers {
  "Foldable" should "be implemented for List" in {
    Foldable[List].empty[Int] should be(List[Int]())
    List(1).append(2) should be(List(2, 1))
    Foldable[List].fold(List(1, 2, 3), 0)(_ + _) should be(6)
  }
}
