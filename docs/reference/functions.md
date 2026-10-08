---
title: Functions
weight: 30
---

# Functions

A function is defined with `def`. Its parameters are patterns, it may be written as several
clauses with guards, and a function whose body produces several values is a generator.

## Defining a function

`def name( parameters )` is followed by the body: either `= expression` on the same line, or an
indented block.

```funl
def square( x ) = x * x

def hanoi( n, source, target, auxiliary )
  if n > 0
    hanoi( n - 1, source, auxiliary, target )
    target += source.removeLast
    hanoi( n - 1, auxiliary, target, source )

write( square(7) )

a = buffer( [3, 2, 1] )
b = buffer()
c = buffer()
hanoi( 3, a, c, b )
write( a, b, c )
```

```output
49
Buffer(), Buffer(), Buffer(3, 2, 1)
```

## Lambdas

A lambda is `x -> x + 1` with one parameter and `(n, m) -> n * m` with several. A lambda closes
over the variables it mentions, and a lambda can produce another lambda:

```funl
offset = 10
add = x -> x + offset
mul = (n, m) -> n * m
write( add(5), mul(3, 4) )

curry = f -> a -> b -> f(a, b)
uncurry = f -> (a, b) -> f(a)(b)
write( curry(mul)(6)(7) )
write( uncurry(curry(mul))(2, 9) )
```

```output
15, 12
42
18
```

In braces, a lambda after a key is that key's value, so a map can hold functions; a set holding a
lambda whose parameter is a cons, `x:xs -> x`, writes it in parentheses:

```funl
ops = {double: (x) -> 2x, sum: (a, b) -> a + b}
firsts = {(x:xs -> x)}
write( ops.double(4), ops.sum(1, 2), firsts.length )
```

```output
8, 3, 1
```

## Operators as functions

An operator in parentheses is a function of two arguments: `(+)`. A **section** fixes one side of
it: `(+ 1)` supplies the right operand, `(10 -)` the left one. A section of a comparison is a
function that fails as the comparison does.

```funl
write( (+)(3, 4) )
write( (+ 1)(10) )
write( (2 *)(21) )
write( (10 -)(1) )
write( (< 5)(3) )
write( (< 5)(8) )
```

```output
7
11
42
9
5
```

## Builtins as values

A builtin that takes a fixed number of arguments, named without calling it, is a function value: it
can be passed, stored and called later. A generating builtin such as `odd` still generates, and
still fails, when called through the value.

```funl
def each( f, [] ) = []
def each( f, x : xs ) = f( x ) : each( f, xs )

write( each(abs, [-1, 2, -3]) )
total = sum
write( total(1..4) )
parity = odd
write( [parity(n) | n <- 1..6] )
```

```output
[1, 2, 3]
10
[1, 3, 5]
```

Called through the value with the wrong number of arguments, a builtin faults as any function value
does:

```funl
f = abs
write( f(1, 2) )
```

```error
'abs' takes 1 argument and was given 2
```

A builtin that takes any number of arguments, such as `max` or `write`, is only called:

```funl
write( max )
```

```error
`max` is a relation or a builtin, which is called rather than used as a value
```

## Clauses, tried in order

A function may be defined by several clauses. They are tried in order, and **the first whose
parameters match and whose guard succeeds is chosen**. A guard is written `| condition = body`, one
per line, and `otherwise` is the guard that always holds.

```funl
def sign( n )
  | n < 0 = 'negative'
  | n == 0 = 'zero'
  | otherwise = 'positive'

def pow( _, 0 ) = 1
def pow( x, n ) | n > 0 = x * pow( x, n - 1 )
def pow( _, _ ) = error( "pow: negative exponent" )

write( sign(-3), sign(0), sign(8) )
write( pow(2, 10), pow(3, 0) )
```

```output
negative, zero, positive
1024, 1
```

The last clause of `pow` catches every argument the others refuse, and reports it with `error`,
which stops the program:

```funl
def pow( _, 0 ) = 1
def pow( x, n ) | n > 0 = x * pow( x, n - 1 )
def pow( _, _ ) = error( "pow: negative exponent" )

write( pow(2, -1) )
```

```error
pow: negative exponent
```

The choice is **committed**: once a clause is chosen, a failure in its body makes the call fail,
and the clauses after it are not tried. A guard that fails, on the other hand, passes the clause
over.

```funl
def first( n ) | n > 0 = fail
def first( n ) = 'second clause'

write( first(5) )
write( first(-5) )
```

```output
second clause
```

When no clause matches, that is an error, not a failure, and the message names the function:

```funl
def only_zero( 0 ) = 'zero'

write( only_zero(0) )
write( only_zero(1) )
write( 'never printed' )
```

```error
argument match not found: no clause of 'only_zero' matches (1)
```

## Patterns

Every parameter is a pattern. A pattern may be a literal, a variable, `_` (which matches anything
and binds nothing), a tuple `(a, b)`, a list `[x, y]`, the empty list `[]`, a cons `x:xs` (a
non-empty list split into its head and the rest), a record `point(x, y)`, an alternation `1 | 2`,
or a named pattern `p@(a, b)`, which binds the whole argument as well as its parts.

```funl
def kind( 0 ) = 'zero'
def kind( 1 | 2 ) = 'small'
def kind( 'a' ) = 'letter a'
def kind( (a, b) ) = 'pair of ' + a + ' and ' + b
def kind( [] ) = 'empty list'
def kind( [x] ) = 'one-element list of ' + x
def kind( [x, y] ) = 'two-element list'
def kind( x:xs ) = 'list starting ' + x
def kind( _ ) = 'something else'

write( kind(0) )
write( kind(2) )
write( kind('a') )
write( kind((3, 4)) )
write( kind([]) )
write( kind([9]) )
write( kind([1, 2]) )
write( kind([5, 6, 7]) )
write( kind(3.5) )

data point( x, y )
def norm1( point(x, y) ) = abs(x) + abs(y)
write( norm1(point(3, -4)) )

def both( p@(a, b) ) = (p, a + b)
write( both((1, 2)) )
```

```output
zero
small
letter a
pair of 3 and 4
empty list
one-element list of 9
two-element list
list starting 5
something else
7
((1, 2), 3)
```

A record pattern names a constructor a `data` declares, in the file or imported, and gives each of
its fields. A pattern that could never match is refused, wherever it is written: a parameter, a
`val`, a `for` or a comprehension.

```funl
data pair = p(a, b)

def first( p(a) ) = a
```

```error
the constructor `p` takes 2 fields, and this pattern gives 1
```

```funl
val zork(x) = 1
```

```error
`zork` is not defined
```

A relation's head is the exception: it may write a term no `data` declares, which is a Prolog term
([Relations](relations.md#facts-and-rules)). So may a `catch`'s pattern, which takes apart whatever
was thrown ([Success and failure](success-and-failure.md#what-a-fault-is)).

```funl
def held( box(1, 2) )

free t
held( t )
write( t )
write( 1 + #a catch error(type_error(kind, _), _) -> kind )
```

```output
box(1, 2)
number
```

Matching is **one-way**: a parameter pattern inspects its argument and binds the pattern's own
variables, and never changes the argument.

```funl
def tail( _:xs ) = xs

l = [1, 2, 3]
write( tail(l) )
write( l )
```

```output
[2, 3]
[1, 2, 3]
```

## `where`

`where`, after a clause, introduces local definitions, values and functions alike. They may be
mutually recursive, and the guards and the body can see them.

```funl
def pow( _, 0 ) = 1
def pow( x, n ) | n > 0 = pow_( x, n - 1, x )
  where
    pow_( _, 0, v ) = v
    pow_( u, n, v )
      | even( n ) = pow_( u*u, n/2, v )
      | otherwise = pow_( u, n - 1, u*v )
    even( 0 ) = true
    even( n ) = odd( n - 1 )
    odd( 0 ) = fail
    odd( n ) = even( n - 1 )

def sumsq( a, b ) = s
  where
    s = sq( a ) + sq( b )
    sq( x ) = x * x

write( pow(2, 10), pow(5, 3) )
write( sumsq(3, 4) )
```

```output
1024, 125
25
```

## A block of definitions

`def` on a line of its own opens a block of clauses for several functions. The functions in one
block may call one another, which is how mutually recursive functions are written at the top level.

```funl
def
  foldl( f, z, [] )   = z
  foldl( f, z, x:xs ) = foldl( f, f(z, x), xs )

  sum( l ) = foldl( (+), 0, l )

write( sum([1, 2, 3, 4]) )
```

```output
10
```

## Generator functions

A function whose body produces several values is a **generator**: a body expression passes every
value it produces through to the caller, without bound.

```funl
def upto( n ) = 1 to n

every write( upto(3) )
```

```output
1
2
3
```

`yield e` produces `e`'s value to the caller and leaves the function suspended where it is. Asking
for another value resumes it after the `yield`, and the `yield` itself then produces `()`. A
`yield` that is the last thing the body does produces no more values once resumed, so this `g`
produces exactly `1` and `2`:

```funl
def g()
  yield 1
  yield 2

def mid()
  x = yield 'a'
  write( 'resumed with ' + x )
  yield 'b'

def evens( n )
  for i <- 1..n
    if 2 div i then yield i

every write( g() )
every write( mid() )
every write( evens(7) )
```

```output
1
2
a
resumed with ()
b
2
4
6
```

A loop that is the last thing a `yield`ing function does ends it with no value of its own when a
plain `break` leaves it, just as when the loop runs out: the function's values are the ones it
yielded. A `break (v)` there produces `v` as one more value, and in a function that does not
`yield`, a loop's `break` gives the function's result as anywhere else.

```funl
def below( limit )
  for i <- 1..
    if i * i > limit then break
    yield i * i

def then_root( limit )
  for i <- 1..
    if i * i > limit then break (i - 1)
    yield i * i

def has_negative( xs ) = for x <- xs do if x < 0 then break

every write( below(10) )
every write( then_root(10) )
write( has_negative([1, -2]) )
```

```output
1
4
9
1
4
9
3
()
```

`yield` of an expression that generates yields each of its values. This prints every permutation
of a list, in the order of Heap's algorithm:

```funl
def
  permute( [] ) = seq()
  permute( l ) = permute_( l.length, array(l) )
  permute_( n, a )
    if n == 1
      seq( a )
    else
      for i <- 0..<n - 1
        yield permute_( n - 1, a )

        if 2 div n
          swap( a(i), a(n - 1) )
        else
          swap( a(0), a(n - 1) )

      permute_( n - 1, a )

every write( permute([1, 2, 3]) )
```

```output
(1, 2, 3)
(2, 1, 3)
(3, 1, 2)
(1, 3, 2)
(2, 3, 1)
(3, 2, 1)
```

`return e` produces only `e`'s **first** value, while a body written `= e` passes every value
through:

```funl
def firstOf( xs )
  return !xs

def allOf( xs ) = !xs

every write( firstOf([7, 8, 9]) )
every write( allOf([7, 8, 9]) )
```

```output
7
7
8
9
```

Inside parentheses, in a call's arguments and at the head of `every`, `name = e` is an assignment
expression: it stores each value of `e` in `name` and produces it. Here `k` is bound once for each
value of the outer range:

```funl
every write( (k = 1 to 3) to k + 2 )
```

```output
1
2
3
2
3
4
3
4
5
```

## Partial function literals

An indented block of lambdas is an anonymous function of several clauses. Each lambda is a clause,
and may carry a guard; the clauses are tried in order with committed choice, exactly as a `def`'s
are, and no clause matching is an error.

```funl
val classify =
  0 -> 'zero'
  n | n < 0 -> 'negative'
  _ -> 'positive'

write( classify(0), classify(-4), classify(9) )
```

```output
zero, negative, positive
```

```funl
val only =
  0 -> 'zero'

write( only(0) )
write( only(1) )
```

```error
argument match not found
```
