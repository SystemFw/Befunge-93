# Befunge-93

A purely functional Befunge-93 interpreter, written in final tagless style.
Showcasing the beauty and power of compositional design.

## The interpreter

Given a Befunge-93 program, usually expressed as a multiline string

``` scala
val helloWorld = 
    """
>              v
v  ,,,,,"Hello"<
>48*,          v
v,,,,,,"World!"<
>25*,@
   """
```

you can run it to completion by calling `befunge.run(helloWorld)`, or
step through it in debug mode with `befunge.debug(helloWorld)`. Note
that both return a `cats.effect.IO]`, which describes their behaviour
in a purely functional manner. You can compose the `IO` computations
in a larger program, or, if you want to actually trigger their
execution because you are in `main` or in the `repl`, call
`.unsafeRunSync` on them.


The `befunge` object also contains some example programs for you to
try out.


## The language
Befunge-93 is a stack-based, reflective, bidimensional esoteric programming language.
The spec is [here](Befunge-93_spec.md)

A short table of the commands is as follows:

### Command Summary ###

    COMMAND         INITIAL STACK (bot->top)RESULT (STACK)
    -------         -------------           -----------------
    + (add)         <value1> <value2>       <value1 + value2>
    - (subtract)    <value1> <value2>       <value1 - value2>
    * (multiply)    <value1> <value2>       <value1 * value2>
    / (divide)      <value1> <value2>       <value1 / value2> (nb. integer)
    % (modulo)      <value1> <value2>       <value1 mod value2>
    ! (not)         <value>                 <0 if value non-zero, 1 otherwise>
    ` (greater)     <value1> <value2>       <1 if value1 > value2, 0 otherwise>
    > (right)                               PC -> right
    < (left)                                PC -> left
    ^ (up)                                  PC -> up
    v (down)                                PC -> down
    ? (random)                              PC -> right? left? up? down? ???
    _ (horizontal if) <boolean value>       PC->left if <value>, else PC->right
    | (vertical if)   <boolean value>       PC->up if <value>, else PC->down
    " (stringmode)                          Toggles 'stringmode'
    : (dup)         <value>                 <value> <value>
    \ (swap)        <value1> <value2>       <value2> <value1>
    $ (pop)         <value>                 pops <value> but does nothing
    . (pop)         <value>                 outputs <value> as integer
    , (pop)         <value>                 outputs <value> as ASCII
    # (bridge)                              'jumps' PC one farther; skips
                                            over next command
    g (get)         <x> <y>                 <value at (x,y)>
    p (put)         <value> <x> <y>         puts <value> at (x,y)
    & (input value)                         <value user entered>
    ~ (input character)                     <character user entered>
    @ (end)                                 ends program
