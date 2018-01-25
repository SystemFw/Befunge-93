package befunge
package semantics


import data.space.{Direction, Point}

import cats._, implicits._
import cats.data.NonEmptyList

object primitives {
  trait Stack[F[_], A] {
    def pop: F[A]
    def push(a: A): F[Unit]

    def op(f: (A, A) => A)(implicit ev: Monad[F]): F[Unit] =
      for {
        b <- pop
        a <- pop
        _ <- push(f(a, b))
      } yield ()
  }

  trait Space[F[_], A] {
    def advance: F[Unit]
    def changeDirection(d: Direction): F[Unit]
    def getAt(p: Point): F[A]
    def writeAt(p: Point, v: A): F[Unit]
  }

  trait Console[F[_]] {
    def put(a: String): F[Unit]
    def readInt: F[Int]
    def readChar: F[Char]
  }

  trait Random[F[_], A] {
    def oneOf(n: NonEmptyList[A]): F[A]
  }
}
