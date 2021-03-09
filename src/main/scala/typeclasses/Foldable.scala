package typeclasses

trait Foldable[F[_]] {
  def empty[A]: F[A]
  def append[A](xs: F[A], x: A): F[A]
  def fold[A, B](fa: F[A], acc: B)(f: (A, B) => B): B
}

object Foldable {
  def apply[F[_]: Foldable]: Foldable[F] = implicitly

  object Implicit {
    implicit class FoldableOps[F[_]: Foldable, A](fa: F[A]) {
      def fold[B](acc: B)(f: (A, B) => B): B = Foldable[F].fold(fa, acc)(f)
      def append(x: A): F[A] = Foldable[F].append(fa, x)
    }

    implicit def listFoldable: Foldable[List] = new Foldable[List] {
      override def empty[A]: List[A] = Nil

      override def append[A](xs: List[A], x: A): List[A] = x :: xs

      override def fold[A, B](fa: List[A], acc: B)(f: (A, B) => B): B =
        fa match {
          case Nil     => acc
          case x :: xs => fold(xs, f(x, acc))(f)
        }
    }
  }
}
