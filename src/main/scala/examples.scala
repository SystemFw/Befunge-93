package befunge

object examples {
  def hwi =
        """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """

  def hwi2 =
 """
64+"!dlroW ,olleH">:#,_@
"""
  def ei = "@"

  def hw = interpreter {
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """
  }.unsafeRunSync

  def hw2 = interpreter {
 """
64+"!dlroW ,olleH">:#,_@
"""
  }.unsafeRunSync

  def empty = interpreter {
    """
@
"""
  }.unsafeRunSync

  def step(i: String) = interpreter(i, interpreter.debugLoop).unsafeRunSync
}
