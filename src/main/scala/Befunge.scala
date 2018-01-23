package befunge

object yo {
  import motion._, interpreter._
  import cats._, implicits._

  val p = consoleForF.put("hello").run(Ctx()).value.run._1 // "hello"
  val p2 = stackForF.pop.run(Ctx()).value.run //("",Left(Trying to pop from an empty stack ))

  def m(s: String) = {
    val ctx = Ctx(space = Torus.fromString(s, identity))
    runLoop.run(ctx).value.run
  }

  def input =
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """.trim.stripMargin('m')

  //def output = m(input)

  val ctx = Ctx(space = Torus.fromString(input, identity))
  def move(ctx: Ctx) =
    List
      .fill(1)(motionForF.advance)
      .sequence_
      .run(ctx)
      .value
      .run
      .map(_.map(_._1.space.get))

  def o = move(ctx.onTorus(s => s.copy(focus = Point(1, 2))))

  val s = Torus.fromString("abc\nefg", identity)

}
