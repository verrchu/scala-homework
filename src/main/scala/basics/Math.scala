package basics

object Math {
  def lcm(a: Int, b: Int): Int = (a, b) match {
    case (0, 0) => 0
    case (a, b) => a * b / gcd(a, b)
  }

  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (0, b)            => b
    case (a, 0)            => a
    case (a, b) if (a < b) => gcd(a, b % a)
    case (a, b)            => gcd(a % b, b)
  }
}
