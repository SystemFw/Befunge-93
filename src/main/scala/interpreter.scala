package befunge

import cats._, data._, implicits._

import motion.{Direction, Right, Torus}
import stack.Stack
import primitives.{Stack => StackLang, Motion, Console}
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

  // TODO use something with faster appends than string
  type F[A] = StateT[EitherT[Writer[String, ?], String, ?], Ctx, A]

  implicit def stackForF: StackLang[F, Int] = new StackLang[F, Int] {
    def push(a: Int): F[Unit] =
      StateT.modify(_.onStack(_.push(a)))

    def pop: F[Int] = StateT { ctx =>
      val (e, newStack) = ctx.stack.pop

      e match {
        case Some(v) => EitherT.pure(ctx.onStack(_ => newStack) -> v)
        case None => EitherT.leftT("Trying to pop from an empty stack ")
      }
    }
  }

  implicit def motionForF: Motion[F] = new Motion[F] {
    def advance: F[Unit] = StateT.modify { ctx =>
      ctx.onTorus(_.advance(ctx.direction))
    }

    def changeDirection(d: Direction): F[Unit] =
      StateT.modify(_.onDirection(_ => d))
  }

  implicit def consoleForF: Console[F] = new Console[F] {
    def put(s: String): F[Unit] =
      StateT.liftF(EitherT.right(Writer.tell(s)))
  }

  implicit def befungeForF: Befunge[F] = Befunge[F]

  def runLoop: F[Unit] =
    for {
      instr <- StateT
        .get[EitherT[Writer[String, ?], String, ?], Ctx]
        .map(_.space.get.getOrElse(' '))
      prog <- Befunge.fromInstr[F](instr)
      _ <- if (prog.isDefined) ().pure[F] else runLoop
    } yield ()
}
