package befunge

object yo {
  import space._, interpreter._
  import cats._, implicits._

  def m(s: String) = {
    val ctx = Ctx(space = Torus.fromString(s, identity))
    runLoop.run(ctx).unsafeRunSync
  }

  def input =
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """.trim.stripMargin('m')

  def output = m(input)

  val ctx = Ctx(space = Torus.fromString(input, identity))
  val s = Torus.fromString("abc\nefg", identity)

}
