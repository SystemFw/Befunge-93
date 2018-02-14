package befunge
package semantics

import data.space.{Direction, Up, Down, Left, Right, Point}
import primitives.{Console, Space, Random, Stack}

import cats._, implicits._
import cats.data.NonEmptyList

import Language._

/* In general, you want to put the constraints on the methods, not on the
 * class.  In this case the parser needs to be able to parse the full
 * language, so it doesn't matter that much.
 */
abstract class Language[F[_]](implicit ST: Stack[F, Int],
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
      .map(v => (!v.toBoolean).toInt)
      .flatMap(ST.push) *> S.advance

  def gt: F[Unit] =
    ST.op((x, y) => (x > y).toInt) *> S.advance

  def right: F[Unit] =
    S.changeDirection(Right) *> S.advance

  def left: F[Unit] =
    S.changeDirection(Left) *> S.advance

  def up: F[Unit] =
    S.changeDirection(Up) *> S.advance

  def down: F[Unit] =
    S.changeDirection(Down) *> S.advance

  def random: F[Unit] =
    R.oneOf(NonEmptyList.of(Up, Down, Left, Right))
      .flatMap(S.changeDirection) *> S.advance

  def horizontalIf: F[Unit] =
    ST.pop.flatMap { v =>
      if (v.toBoolean) S.changeDirection(Left) else S.changeDirection(Right)
    } *> S.advance

  def verticalIf: F[Unit] =
    ST.pop.flatMap { v =>
      if (v.toBoolean) S.changeDirection(Up) else S.changeDirection(Down)
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
      .map(_.toAsciiChar.toString + " ")
      .flatMap(C.put) *> S.advance

  def bridge: F[Unit] =
    S.advance *> S.advance

  def get: F[Unit] =
    for {
      y <- ST.pop
      x <- ST.pop
      v <- S.getAt(Point(x, y))
      _ <- ST.push(v.toAsciiInt)
      _ <- S.advance
    } yield ()

  def put: F[Unit] =
    for {
      y <- ST.pop
      x <- ST.pop
      v <- ST.pop
      _ <- S.writeAt(Point(x, y), v.toAsciiChar)
      _ <- S.advance
    } yield ()

  def inputInt: F[Unit] =
    C.readInt.flatMap(ST.push) *> S.advance

  def inputChar: F[Unit] =
    C.readChar.flatMap(x => ST.push(x.toAsciiInt)) *> S.advance

  def noOp: F[Unit] = S.advance
}

object Language {
  sealed trait Running
  case object Continue extends Running
  case object Stop extends Running

  def fromInstr[F[_]](c: Char)(implicit BF: Language[F],
                               F: MonadError[F, Throwable]): F[Running] = {
    val tokens = Map(
      '+' -> BF.add,
      '-' -> BF.subtract,
      '*' -> BF.multiply,
      '/' -> BF.divide,
      '%' -> BF.modulo,
      '!' -> BF.not,
      '`' -> BF.gt,
      '>' -> BF.right,
      '<' -> BF.left,
      '^' -> BF.up,
      'v' -> BF.down,
      '?' -> BF.random,
      '_' -> BF.horizontalIf,
      '|' -> BF.verticalIf,
      '"' -> BF.stringMode,
      ':' -> BF.dup,
      '\\' -> BF.swap,
      '$' -> BF.discard,
      '.' -> BF.outputInt,
      ',' -> BF.outputAscii,
      '#' -> BF.bridge,
      'g' -> BF.get,
      'p' -> BF.put,
      '&' -> BF.inputInt,
      '~' -> BF.inputChar,
      ' ' -> BF.noOp
    )

    val invalidToken = F.raiseError(new Exception(s"invalid input! $c"))

    if (c == '@') (Stop: Running).pure[F]
    else
      c.toDigit
        .map(BF.number)
        .orElse(tokens.get(c))
        .getOrElse(invalidToken)
        .as(Continue)  
  }

  def stringMode[F[_]](c: Char)(implicit BF: Language[F],
                                ev: Functor[F]): F[Running] = {
    if (c == '"') BF.stringMode
    else BF.number(c.toAsciiInt)
  }.as(Continue)

  implicit class CharToAscii(c: Char) { def toAsciiInt: Int = c.toInt }
  implicit class CharToDigit(c: Char) {
    def toDigit: Option[Int] =
      (Character isDigit c).guard[Option].as(c.asDigit)
  }
  implicit class AsciiToChar(i: Int) { def toAsciiChar: Char = i.toChar }
  implicit class IntToBoolean(i: Int) { def toBoolean = i != 0 }
  implicit class BooleeanToInt(b: Boolean) { def toInt = if (b) 1 else 0 }
}
