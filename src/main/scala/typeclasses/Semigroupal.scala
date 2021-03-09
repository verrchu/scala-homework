package typeclasses

trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object Semigroupal {
  def apply[F[_]: Semigroupal]: Semigroupal[F] = implicitly

  object Implicit {
    implicit class SemigroupalOps[A, F[_]: Semigroupal](fa: F[A]) {
      def product[B](fb: F[B]): F[(A, B)] = Semigroupal[F].product(fa, fb)
    }

    implicit def mapSemigroupal[K]: Semigroupal[Map[K, *]] =
      new Semigroupal[Map[K, *]] {
        override def product[A, B](
            fa: Map[K, A],
            fb: Map[K, B]
        ): Map[K, (A, B)] = {
          val faKeys = fa.keys.toSet
          val fbKeys = fb.keys.toSet

          val commonKeys = faKeys.intersect(fbKeys)

          commonKeys.map(key => (key, (fa.get(key).get, fb.get(key).get))).toMap
        }
      }
  }
}
