package befunge

import cats._, implicits._

import motion.{Direction, Up, Down, Left, Right}
import primitives.{Console, Motion, Random, Stack}

object language {
  case class Befunge[F[_]](implicit S: Stack[F, Int],
                           M: Motion[F],
                           C: Console[F],
                           R: Random[F, Direction],
                           F: Monad[F]) {
    def number(n: Int): F[Unit] =
      S.push(n) *> M.advance

    def add: F[Unit] =
      S.op(_ + _) *> M.advance

    def subtract: F[Unit] =
      S.op(_ - _) *> M.advance

    def multiply: F[Unit] =
      S.op(_ * _) *> M.advance

    def divide: F[Unit] =
      S.op(_ / _) *> M.advance

    def modulo: F[Unit] =
      S.op(_ % _) *> M.advance

    def not: F[Unit] =
      S.pop
        .map(v => if (v != 0) 0 else 1)
        .flatMap(S.push) *> M.advance

    def gt: F[Unit] =
      S.op((x, y) => if (x > y) 1 else 0) *> M.advance

    def right: F[Unit] =
      M.changeDirection(Right) *> M.advance

    def left: F[Unit] =
      M.changeDirection(Left) *> M.advance

    def up: F[Unit] =
      M.changeDirection(Up) *> M.advance

    def down: F[Unit] =
      M.changeDirection(Down) *> M.advance

    def random: F[Unit] =
      R.oneOf(List(Up, Down, Left, Right))
        .flatMap(M.changeDirection) *> M.advance

    def horizontalIf: F[Unit] =
      S.pop.flatMap { v =>
        if (v != 0) left else right
      } *> M.advance

    def verticalIf: F[Unit] =
      S.pop.flatMap { v =>
        if (v != 0) up else down
      } *> M.advance

    // stringMode

    def dup: F[Unit] =
      S.pop.flatMap(v => S.push(v) *> S.push(v)) *> M.advance

    def swap: F[Unit] =
      for {
        a <- S.pop
        b <- S.pop
        _ <- S.push(a)
        _ <- S.push(b)
        _ <- M.advance
      } yield ()

    def discard: F[Unit] =
      S.pop.void *> M.advance

    def outputInt: F[Unit] =
      S.pop
        .map(_.toString + " ")
        .flatMap(C.put) *> M.advance

    def outputAscii: F[Unit] =
      S.pop
        .map(_.toChar.toString + " ")
        .flatMap(C.put) *> M.advance

    def bridge: F[Unit] =
      M.advance *> M.advance

    // get put

    def inputInt: F[Unit] =
      C.readInt.flatMap(S.push) *> M.advance

    def inputChar: F[Unit] =
      C.readChar.flatMap(x => S.push(x.toInt)) *> M.advance

    def noOp: F[Unit] = F.unit *> M.advance
  }

  object Befunge {
    case class End()

    def fromInstr[F[_]](c: Char)(implicit BF: Befunge[F],
                                 F: MonadError[F, Throwable],
                                 ev2: Stack[F, Int],
                                 ev3: Motion[F],
                                 ev4: Console[F]): F[Option[End]] =
      c match {
        case c if Character.isDigit(c) => BF.number(c.toInt).as(None)
        case '+' => BF.add.as(None)
        case '-' => BF.subtract.as(None)
        case '*' => BF.multiply.as(None)
        case '/' => BF.divide.as(None)
        case '%' => BF.modulo.as(None)
        case '!' => BF.not.as(None)
        case '`' => BF.gt.as(None)
        case '>' => BF.right.as(None)
        case '<' => BF.left.as(None)
        case '^' => BF.up.as(None)
        case 'v' => BF.down.as(None)
        case '?' => BF.random.as(None)
        case '_' => BF.horizontalIf.as(None)
        case '|' => BF.verticalIf.as(None)
        // case '"' => toggle string mode
        case ':' => BF.dup.as(None)
        case '\\' => BF.swap.as(None)
        case '$' => BF.discard.as(None)
        case '.' => BF.outputInt.as(None)
        case ',' => BF.outputAscii.as(None)
        case '#' => BF.bridge.as(None)
        // case 'g' => get
        // case 'p' => put
        case '&' => BF.inputInt.as(None)
        case '~' => BF.inputChar.as(None)
        case '@' => End().some.pure[F]
        case ' ' => BF.noOp.as(None)
        case c => F.raiseError(new Exception(s"invalid input! $c"))
      }
  }
}
