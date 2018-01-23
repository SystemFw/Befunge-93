package befunge

import cats._, implicits._

object primitives {
  trait Stack[F[_], A] {
    def pop: F[A]
    def push(a: A): F[Unit]

    def op(f: (A, A) => A)(implicit ev: Monad[F]): F[Unit] =
      for {
        a <- pop
        b <- pop
        _ <- push(f(a, b))
      } yield ()
  }

  trait Motion[F[_]] {
    def advance: F[Unit]
    def changeDirection(d: motion.Direction): F[Unit]
  }

  trait Console[F[_]] {
    def put(a: String): F[Unit]
    def readInt: F[Int]
    def readChar: F[Char]
  }

  trait Random[F[_], A] {
    def oneOf(n: List[A]): F[A]
  }
}
