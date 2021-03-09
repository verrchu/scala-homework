package typeclasses

import Semigroup.Implicit._
import Semigroupal.Implicit._
import Functor.Implicit._
import Applicative.Implicit._
import Foldable.Implicit._

object Util {
  def combineAll[A: Monoid](xs: List[A]): A =
    xs.foldLeft(Monoid[A].empty)(_ combine _)

  object Implicit {
    implicit class MapNOps[A, B, F[_]: Functor: Semigroupal](x: (F[A], F[B])) {
      def mapN[R](f: (A, B) => R): F[R] = x match {
        case (fa, fb) => Semigroupal[F].product(fa, fb).fmap(f.tupled)
      }
    }

    implicit class TraverseOps[A, F[_]: Foldable](xs: F[A]) {
      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = {
        xs.fold(Applicative[G].pure(Foldable[F].empty[B])) { (x, acc) =>
          (f(x), acc).mapN((x, acc) => acc.append(x))
        }
      }
    }
  }
}
