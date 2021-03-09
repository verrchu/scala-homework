package typeclasses

import Semigroup.Implicit.SemigroupOps

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A: Monoid]: Monoid[A] = implicitly

  object Implicit {
    implicit def intMonoid: Monoid[Int] = new Monoid[Int] {
      override def empty: Int = 0

      override def combine(x: Int, y: Int): Int = x + y
    }

    implicit def optMonoid[A: Semigroup]: Monoid[Option[A]] =
      new Monoid[Option[A]] {
        override def empty: Option[A] = None

        override def combine(x: Option[A], y: Option[A]) = (x, y) match {
          case (Some(x), Some(y)) => Some(x combine y)
          case (x, y)             => x.orElse(y)
        }
      }

    implicit def funcMonoid[A, B: Monoid]: Monoid[A => B] =
      new Monoid[A => B] {
        override def empty: A => B = _ => Monoid[B].empty

        override def combine(x: A => B, y: A => B): A => B = { (a: A) =>
          x(a).combine(y(a))
        }
      }
  }
}
