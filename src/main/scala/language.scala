package befunge

import cats._, implicits._
import primitives.{Console, Motion, Stack}

object language {
  case class Befunge[F[_]](implicit S: Stack[F, Int],
                           M: Motion[F],
                           C: Console[F],
                           F: Monad[F]) {
    def number(n: Int): F[Unit] =
      S.push(n) *> M.advance

    def add: F[Unit] =
      for {
        a <- S.pop
        b <- S.pop
        _ <- S.push(a + b)
        _ <- M.advance
      } yield ()

    def not: F[Unit] =
      S.pop.flatMap { v =>
        if (v == 0) S.push(1) else S.push(0)
      } *> M.advance

    def gt: F[Unit] =
      for {
        a <- S.pop
        b <- S.pop
        _ <- if (b > a) S.push(1) else S.push(0)
        _ <- M.advance
      } yield ()

    def right: F[Unit] =
      M.changeDirection(motion.Right) *> M.advance

    def left: F[Unit] =
      M.changeDirection(motion.Left) *> M.advance

    def up: F[Unit] =
      M.changeDirection(motion.Up) *> M.advance

    def down: F[Unit] =
      M.changeDirection(motion.Down) *> M.advance

    def rightOnZero: F[Unit] =
      S.pop.flatMap { v =>
        if (v == 0) right else left
      } *> M.advance

    def downOnZero: F[Unit] =
      S.pop.flatMap { v =>
        if (v == 0) down else up
      } *> M.advance

    def discardFirst: F[Unit] =
      S.pop.void *> M.advance

    def printFirstAsInt: F[Unit] =
      S.pop.flatMap { v =>
        C.put(v.toString + " ")
      } *> M.advance

    def printFirstAsChar: F[Unit] =
      S.pop.flatMap { v =>
        C.put(v.toChar.toString + " ")
      } *> M.advance

    def skipNextCell: F[Unit] =
      M.advance *> M.advance

    def noOp: F[Unit] = F.unit *> M.advance
  }

  object Befunge {
    case class End()

    def fromInstr[F[_]](c: Char)(implicit BF: Befunge[F],
                                 F: MonadError[F, String],
                                 ev2: Stack[F, Int],
                                 ev3: Motion[F],
                                 ev4: Console[F]): F[Option[End]] =
      c match {
        case c if Character.isDigit(c) => BF.number(c.toInt).as(None)
        case '+' => BF.add.as(None)
        case '!' => BF.not.as(None)
        case '`' => BF.gt.as(None)
        case '>' => BF.right.as(None)
        case '<' => BF.left.as(None)
        case '^' => BF.up.as(None)
        case 'v' => BF.down.as(None)
        case '_' => BF.rightOnZero.as(None)
        case '|' => BF.downOnZero.as(None)
        case '$' => BF.discardFirst.as(None)
        case '.' => BF.printFirstAsInt.as(None)
        case ',' => BF.printFirstAsChar.as(None)
        case '#' => BF.skipNextCell.as(None)
        case '@' => End().some.pure[F]
        case ' ' => BF.noOp.as(None)
        case c => F.raiseError(s"invalid input! $c")
      }
  }
}
