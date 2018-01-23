package befunge

import cats._, implicits._

import space.{Direction, Up, Down, Left, Right, Point}
import primitives.{Console, Space, Random, Stack}

object language {
  abstract class Befunge[F[_]](implicit ST: Stack[F, Int],
                               S: Space[F, Char],
                               C: Console[F],
                               R: Random[F, Direction],
                               F: Monad[F]) {
    def number(n: Int): F[Unit] =
      ST.push(n) *> S.advance

    def add: F[Unit] =
      ST.op(_ + _) *> S.advance

    def subtract: F[Unit] =
      ST.op(_ - _) *> S.advance

    def multiply: F[Unit] =
      ST.op(_ * _) *> S.advance

    def divide: F[Unit] =
      ST.op(_ / _) *> S.advance

    def modulo: F[Unit] =
      ST.op(_ % _) *> S.advance

    def not: F[Unit] =
      ST.pop
        .map(v => if (v != 0) 0 else 1)
        .flatMap(ST.push) *> S.advance

    def gt: F[Unit] =
      ST.op((x, y) => if (x > y) 1 else 0) *> S.advance

    def right: F[Unit] =
      S.changeDirection(Right) *> S.advance

    def left: F[Unit] =
      S.changeDirection(Left) *> S.advance

    def up: F[Unit] =
      S.changeDirection(Up) *> S.advance

    def down: F[Unit] =
      S.changeDirection(Down) *> S.advance

    def random: F[Unit] =
      R.oneOf(List(Up, Down, Left, Right))
        .flatMap(S.changeDirection) *> S.advance

    def horizontalIf: F[Unit] =
      ST.pop.flatMap { v =>
        if (v != 0) left else right
      } *> S.advance

    def verticalIf: F[Unit] =
      ST.pop.flatMap { v =>
        if (v != 0) up else down
      } *> S.advance

    def stringMode: F[Unit]

    def dup: F[Unit] =
      ST.pop.flatMap(v => ST.push(v) *> ST.push(v)) *> S.advance

    def swap: F[Unit] =
      for {
        b <- ST.pop
        a <- ST.pop
        _ <- ST.push(b)
        _ <- ST.push(a)
        _ <- S.advance
      } yield ()

    def discard: F[Unit] =
      ST.pop.void *> S.advance

    def outputInt: F[Unit] =
      ST.pop
        .map(_.toString + " ")
        .flatMap(C.put) *> S.advance

    def outputAscii: F[Unit] =
      ST.pop
        .map(_.toChar.toString + " ")
        .flatMap(C.put) *> S.advance

    def bridge: F[Unit] =
      S.advance *> S.advance

    def get: F[Unit] =
      for {
        y <- ST.pop
        x <- ST.pop
        v <- S.getAt(Point(x, y))
        _ <- ST.push(v.toInt)
        _ <- S.advance
      } yield ()

    def put: F[Unit] =
      for {
        y <- ST.pop
        x <- ST.pop
        v <- ST.pop
        _ <- S.writeAt(Point(x, y), v.toChar)
        _ <- S.advance
      } yield ()

    def inputInt: F[Unit] =
      C.readInt.flatMap(ST.push) *> S.advance

    def inputChar: F[Unit] =
      C.readChar.flatMap(x => ST.push(x.toInt)) *> S.advance

    def noOp: F[Unit] = F.unit *> S.advance
  }

  object Befunge {
    sealed trait Running
    case object Continue extends Running
    case object Stop extends Running

    def fromInstr[F[_]](c: Char)(implicit BF: Befunge[F],
                                 F: MonadError[F, Throwable],
                                 ev2: Stack[F, Int],
                                 ev3: Space[F, Char],
                                 ev4: Console[F]): F[Running] =
      c match {
        case c if Character.isDigit(c) =>
          // NOTE:  asDigit, not toInt, we don't want the ascii value
          BF.number(c.asDigit).as(Continue)
        case '+' => BF.add.as(Continue)
        case '-' => BF.subtract.as(Continue)
        case '*' => BF.multiply.as(Continue)
        case '/' => BF.divide.as(Continue)
        case '%' => BF.modulo.as(Continue)
        case '!' => BF.not.as(Continue)
        case '`' => BF.gt.as(Continue)
        case '>' => BF.right.as(Continue)
        case '<' => BF.left.as(Continue)
        case '^' => BF.up.as(Continue)
        case 'v' => BF.down.as(Continue)
        case '?' => BF.random.as(Continue)
        case '_' => BF.horizontalIf.as(Continue)
        case '|' => BF.verticalIf.as(Continue)
        case '"' => BF.stringMode.as(Continue)
        case ':' => BF.dup.as(Continue)
        case '\\' => BF.swap.as(Continue)
        case '$' => BF.discard.as(Continue)
        case '.' => BF.outputInt.as(Continue)
        case ',' => BF.outputAscii.as(Continue)
        case '#' => BF.bridge.as(Continue)
        case 'g' => BF.get.as(Continue)
        case 'p' => BF.put.as(Continue)
        case '&' => BF.inputInt.as(Continue)
        case '~' => BF.inputChar.as(Continue)
        case '@' => (Stop: Running).pure[F]
        case ' ' => BF.noOp.as(Continue)
        case c => F.raiseError(new Exception(s"invalid input! $c"))
      }

    def stringMode[F[_]](c: Char)(implicit BF: Befunge[F],
                                  F: MonadError[F, Throwable],
                                  ev2: Stack[F, Int],
                                  ev3: Space[F, Char],
                                  ev4: Console[F]): F[Running] =
      c match {
        case '"' => BF.stringMode.as(Continue)
        case c => BF.number(c.toInt).as(Continue)
      }
  }
}
