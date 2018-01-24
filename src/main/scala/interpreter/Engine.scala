package befunge
package interpreter

import semantics.Language, Language.{Running, Continue, Stop}
import data.space.Torus

import cats._, data._, implicits._
import cats.effect.IO
import scala.io.StdIn

object Engine {
  // Note = Ctx.F[A] = StateT[IO, Ctx, A]

  /* As per the spec,  programs always start at origin, hence we trim */
  def apply(input: String, engine: Ctx.F[Unit] = runLoop): IO[Unit] =
    engine.runA(Ctx(space = Torus.fromString(input.trim, identity)))

  def runLoop: Ctx.F[Unit] = runStep {
    case Continue => runLoop
    case Stop => ().pure[Ctx.F]
  }

  def debugLoop: Ctx.F[Unit] = {
    def debug = StateT.get[IO, Ctx].flatMap { ctx =>
      val text = s"${ctx.show} \n Press any character to step"
      StateT liftF IO { println(text); StdIn.readChar }
    }

    runStep {
      case Continue => debug *> debugLoop
      case Stop => ().pure[Ctx.F]
    }
  }

  def runStep(k: Running => Ctx.F[Unit]): Ctx.F[Unit] =
    StateT.get[IO, Ctx].flatMap { ctx =>
      import Ctx._
      val instr = ctx.space.get.getOrElse(' ')
      val prog =
        if (ctx.stringModeOn) Language.stringMode[Ctx.F](instr)
        else Language.fromInstr[Ctx.F](instr)

      prog.flatMap(k)
    }
}
