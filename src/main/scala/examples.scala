package befunge

object examples {
  // prints hello world
  def hw = interpreter {
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """
  }.unsafeRunSync

  // also prints hello world, uglier, but shorter
  def hw2 = interpreter {
 """
64+"!dlroW ,olleH">:#,_@
"""
  }.unsafeRunSync

  // reads one char from stdIn, prints it to stdOut
  def cat = interpreter {
      """
~:1+!#@_,
"""
  }.unsafeRunSync

  // reads an integer, outputs its factorial
  def factorial = interpreter {
    """
&>:1-:v v *_$.@ 
 ^    _$>\:^
"""
  }.unsafeRunSync

  // prints primes using the sieve of eratosthenes
  def sieve = interpreter {
    """
2>:3g" "-!v\  g30          <
 |!`"O":+1_:.:03p>03g+:"O"`|
 @               ^  p3\" ":<
2 234567890123456789012345678901234567890123456789012345678901234567890123456789
"""
  }.unsafeRunSync

  // plays the less or more guessing game
  def lessOrMoreGame = interpreter {
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
  }.unsafeRunSync

}
