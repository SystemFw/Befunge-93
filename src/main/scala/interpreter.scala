package befunge

import cats._, data._, implicits._
import cats.effect.IO
import scala.io.StdIn
import scala.util.{Random => SRand}

import motion.{Direction, Right, Torus}
import stack.Stack
import primitives.{Stack => StackLang, Motion, Console, Random}
import language.Befunge

object interpreter {

  case class Ctx(space: Torus[Char] = Torus.empty[Char],
                 direction: Direction = Right,
                 stack: Stack[Int] = Stack.empty[Int]) {
    def onTorus(f: Torus[Char] => Torus[Char]) = copy(space = f(this.space))
    def onDirection(f: Direction => Direction) =
      copy(direction = f(this.direction))
    def onStack(f: Stack[Int] => Stack[Int]) = copy(stack = f(this.stack))
  }

  type F[A] = StateT[IO, Ctx, A]

  def stackForF: StackLang[F, Int] = new StackLang[F, Int] {
    def push(a: Int): F[Unit] =
      StateT.modify(_.onStack(_.push(a)))

    // NOTE: The spec prescribes returning 0 when popping from an empty stack
    def pop: F[Int] = StateT { ctx =>
      val (v, newStack) = ctx.stack.pop
      IO.pure { ctx.onStack(_ => newStack) -> v.getOrElse(0) }
    }
  }

  def motionForF: Motion[F] = new Motion[F] {
    def advance: F[Unit] = StateT.modify { ctx =>
      ctx.onTorus(_.advance(ctx.direction))
    }

    def changeDirection(d: Direction): F[Unit] =
      StateT.modify(_.onDirection(_ => d))
  }

  // TODO take the input stream as an argument?
  // have it in the context perhaps?
  def consoleForF: Console[F] = new Console[F] {
    def put(s: String): F[Unit] = StateT.liftF(IO(println(s)))
    def readChar: F[Char] = StateT.liftF(IO(StdIn.readChar))
    def readInt: F[Int] = StateT.liftF(IO(StdIn.readInt))
  }

  def randomForF: Random[F, Direction] = new Random[F, Direction] {
    def oneOf(n: List[Direction]): F[Direction] =
      StateT.liftF(IO(SRand.shuffle(n).head))
  }

  def befungeForF: Befunge[F] =
    Befunge[F]()(stackForF, motionForF, consoleForF, randomForF, implicitly)

  def runLoop: F[Unit] = {
    implicit def s = stackForF
    implicit def m = motionForF
    implicit def c = consoleForF
    implicit def bf = befungeForF

    for {
      instr <- StateT.get[IO, Ctx].map(_.space.get.getOrElse(' '))
      prog <- Befunge.fromInstr[F](instr)
      _ <- if (prog.isDefined) ().pure[F] else runLoop
    } yield ()
  }
}
