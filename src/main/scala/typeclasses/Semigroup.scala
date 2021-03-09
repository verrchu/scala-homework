package typeclasses

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def apply[A: Semigroup]: Semigroup[A] = implicitly

  object Implicit {
    implicit class SemigroupOps[A: Semigroup](x: A) {
      def combine(y: A): A = Semigroup[A].combine(x, y)
    }
  }
}
