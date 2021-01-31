package basics

import org.scalatest._
import flatspec._
import matchers._

class MathSpec extends AnyFlatSpec with should.Matchers {
  "GCD" should "work" in {
    // Methematicians say it is right
    // https://math.stackexchange.com/questions/495119/what-is-gcd0-0
    Math.gcd(0, 0) should be(0)

    Math.gcd(0, 7) should be(7)
    Math.gcd(7, 0) should be(7)
    Math.gcd(3, 7) should be(1)
    Math.gcd(3, 9) should be(3)
    Math.gcd(33, 121) should be(11)
  }

  "LCM" should "work" in {
    Math.lcm(0, 0) should be(0)

    Math.lcm(0, 2) should be(0)
    Math.lcm(2, 0) should be(0)
    Math.lcm(1, 2) should be(2)
    Math.lcm(3, 7) should be(21)
    Math.lcm(4, 6) should be(12)
  }
}
