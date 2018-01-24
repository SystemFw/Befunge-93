package befunge
package interpreter

import data.space.{Direction, Right, Torus, Point}
import data.Stack
import semantics.primitives.{Stack => StackLang, Space, Console, Random}
import semantics.Language

import cats._, data._, implicits._
import cats.effect.IO
import scala.io.StdIn
import scala.util.{Random => SRand}

case class Ctx(space: Torus[Char] = Torus.empty[Char],
               direction: Direction = Right,
               stack: Stack[Int] = Stack.empty[Int],
               stringModeOn: Boolean = false) {
  def onSpace(f: Torus[Char] => Torus[Char]) = copy(space = f(this.space))
  def onDirection(f: Direction => Direction) =
    copy(direction = f(this.direction))
  def onStringModeOn(f: Boolean => Boolean) =
    copy(stringModeOn = f(this.stringModeOn))
  def onStack(f: Stack[Int] => Stack[Int]) = copy(stack = f(this.stack))
}

object Ctx {
  type F[A] = StateT[IO, Ctx, A]

  implicit def showCtx: Show[Ctx] = new Show[Ctx] {
    def show(ctx: Ctx): String =
      s"""Current context:
           | Next instruction: ${ctx.space.get}
           | Current direction: ${ctx.direction}
           | String mode: ${if (ctx.stringModeOn) "On" else "Off"}
           | Stack: ${ctx.stack.state
        .map(i => i.toString -> i.toChar.toString)}
         """
  }

  /* NOTE: The spec prescribes returning 0 when popping from an empty stack */
  implicit def stackI: StackLang[F, Int] =
    new StackLang[F, Int] {
      def push(a: Int): F[Unit] =
        StateT.modify(_.onStack(_.push(a)))
      def pop: F[Int] = StateT { ctx =>
        val (v, newStack) = ctx.stack.pop
        IO.pure { ctx.onStack(_ => newStack) -> v.getOrElse(0) }
      }
    }

  implicit def spaceI: Space[F, Char] =
    new Space[F, Char] {
      def advance: F[Unit] =
        StateT.modify(ctx => ctx.onSpace(_.advance(ctx.direction)))
      def changeDirection(d: Direction): F[Unit] =
        StateT.modify(_.onDirection(_ => d))
      def getAt(p: Point): F[Char] =
        StateT.get[IO, Ctx].map(_.space.getAt(p).getOrElse(' '))
      def writeAt(p: Point, v: Char): F[Unit] =
        StateT.modify(_.onSpace(_.writeAt(p, _ => v)))
    }

  // TODO take the input stream as an argument?
  // have it in the context perhaps?
  implicit def consoleI: Console[F] =
    new Console[F] {
      def put(s: String): F[Unit] = StateT.liftF(IO(println(s)))
      def readChar: F[Char] = StateT.liftF(IO(StdIn.readChar))
      def readInt: F[Int] = StateT.liftF(IO(StdIn.readInt))
    }

  implicit def randomI: Random[F, Direction] =
    new Random[F, Direction] {
      def oneOf(n: List[Direction]): F[Direction] =
        StateT.liftF(IO(SRand.shuffle(n).head))
    }

  implicit def languageI: Language[F] =
    new Language[F]() {
      def stringMode: F[Unit] =
        StateT.modify[IO, Ctx](_.onStringModeOn(!_)) *> spaceI.advance
    }
}
