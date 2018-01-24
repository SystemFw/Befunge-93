package object befunge {
  import interpreter.Engine
  import cats.effect.IO

  def run(s: String): IO[Unit] =
    Engine(s)

  def debug(s: String): IO[Unit] =
    Engine(s, Engine.debugLoop)

  // prints hello world
  def hw =
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """

  // also prints hello world, uglier, but shorter
  def hw2 =
    """
64+"!dlroW ,olleH">:#,_@
"""

  // reads one char from stdIn, prints it to stdOut
  def cat =
    """
~:1+!#@_,
"""

  // reads an integer, outputs its factorial
  def factorial =
    """
&>:1-:v v *_$.@ 
 ^    _$>\:^
"""

  // prints primes using the sieve of eratosthenes
  def sieve =
    """
2>:3g" "-!v\  g30          <
 |!`"O":+1_:.:03p>03g+:"O"`|
 @               ^  p3\" ":<
2 234567890123456789012345678901234567890123456789012345678901234567890123456789
"""

  // plays the less or more guessing game
  // it also prints it in case you want to quit early
  def lessOrMoreGame =
    """
vv  <      <                                                                   
    2                                                                          
    ^  v<                                                                      
 v1<?>3v4                                                                      
    ^   ^                                                                      
>  >?>  ?>5^                                                                   
    v   v                                                                      
 v9<?>7v6                                                                      
    v  v<                                                                      
    8                                                                          
    >  >   ^                                                                   
 vv  <      <                                                                  
     2                                                                         
     ^  v<                                                                     
  v1<?>3v4                                                                     
     ^   ^                                                                     
 >  >?>  ?>5^                                                                  
     v   v      v          ,*25         <<                                     
  v9<?>7v6                              ,,                                     
     v  v<                              ""                                     
     8                                  ><                                     
     >  >   ^                           ""v                                    
  >*:.>0"!rebmun tupnI">:#,_$25*,:&:99p`|^<       _0"!niw uoY">:#,_$25*,@      
      ^         <                       >:99g01-*+^
"""

}
