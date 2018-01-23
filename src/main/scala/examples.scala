package befunge

object examples {
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
}
