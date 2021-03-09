package typeclasses

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]: Functor]: Functor[F] = implicitly[Functor[F]]

  object Implicit {
    implicit class FunctorOps[F[_]: Functor, A](fa: F[A]) {
      def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
    }

    implicit def mapFunctor[K]: Functor[Map[K, *]] = new Functor[Map[K, *]] {
      override def fmap[A, B](fa: Map[K, A])(f: A => B): Map[K, B] =
        fa.mapValues(f).toMap
    }
  }
}
