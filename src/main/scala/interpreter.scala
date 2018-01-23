package befunge

import cats._, data._, implicits._
import cats.effect.IO
import scala.io.StdIn
import scala.util.{Random => SRand}

import space.{Direction, Right, Torus, Point}
import stack.Stack
import primitives.{Stack => StackLang, Space, Console, Random}
import language.Befunge

object interpreter {

  case class Ctx(space: Torus[Char] = Torus.empty[Char],
    direction: Direction = Right,
    stack: Stack[Int] = Stack.empty[Int],
  stringModeOn: Boolean = false) {
    def onSpace(f: Torus[Char] => Torus[Char]) = copy(space = f(this.space))
    def onDirection(f: Direction => Direction) =
      copy(direction = f(this.direction))
    def onStringModeOn(f: Boolean => Boolean) = copy(stringModeOn = f(this.stringModeOn))
    def onStack(f: Stack[Int] => Stack[Int]) = copy(stack = f(this.stack))
  }

  type F[A] = StateT[IO, Ctx, A]

  /* NOTE: The spec prescribes returning 0 when popping from an empty stack */
  def stackForF: StackLang[F, Int] = new StackLang[F, Int] {
    def push(a: Int): F[Unit] =
      StateT.modify(_.onStack(_.push(a)))

    def pop: F[Int] = StateT { ctx =>
      val (v, newStack) = ctx.stack.pop
      IO.pure { ctx.onStack(_ => newStack) -> v.getOrElse(0) }
    }
  }

  def spaceForF: Space[F, Char] = new Space[F, Char] {
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
    new Befunge[F]()(stackForF, spaceForF, consoleForF, randomForF, implicitly) {
      def stringMode: F[Unit] =
        StateT.modify[IO, Ctx](_.onStringModeOn(!_)) *> spaceForF.advance
    }

  def runLoop: F[Unit] = {
    implicit def st = stackForF
    implicit def s = spaceForF
    implicit def c = consoleForF
    implicit def bf = befungeForF

    for {
      ctx <-  StateT.get[IO, Ctx]
      instr = ctx.space.get.getOrElse(' ')
      prog = if(ctx.stringModeOn) Befunge.stringMode[F](instr) else Befunge.fromInstr[F](instr)
      res <- prog
      _ <- if (res.isDefined) ().pure[F] else runLoop
    } yield ()
  }

  /* As per the spec,  programs always start at origin, hence we trim */
  def apply(input: String): IO[Unit] =
    runLoop.runA(Ctx(space = Torus.fromString(input.trim, identity)))
}

