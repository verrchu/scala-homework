package typeclasses

trait Applicative[F[_]] extends Semigroupal[F] with Functor[F] {
  def pure[A](x: A): F[A]
}

object Applicative {
  def apply[F[_]: Applicative]: Applicative[F] = implicitly

  object Implicit {
    implicit def optApplicative: Applicative[Option] = new Applicative[Option] {
      override def pure[A](x: A): Option[A] = Some(x)

      override def fmap[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
        case Some(x) => Some(f(x))
        case None    => None
      }

      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        (fa, fb) match {
          case (Some(a), Some(b)) => Some((a, b))
          case _                  => None
        }
    }

    implicit def eitherApplicative[L]: Applicative[Either[L, *]] =
      new Applicative[Either[L, *]] {
        override def pure[R](x: R): Either[L, R] = Right(x)

        override def fmap[RA, RB](
            fa: Either[L, RA]
        )(f: RA => RB): Either[L, RB] =
          fa match {
            case Left(x)  => Left(x)
            case Right(x) => Right(f(x))
          }

        override def product[RA, RB](
            fa: Either[L, RA],
            fb: Either[L, RB]
        ): Either[L, (RA, RB)] = (fa, fb) match {
          case (Right(a), Right(b)) => Right((a, b))
          case (Left(x), _)         => Left(x)
          case (_, Left(x))         => Left(x)
        }
      }
  }
}
