---
title: Numbers
weight: 30
---

# Numbers

FunL has three kinds of number: **integers** of any size, **rationals** (exact fractions), and
**reals** (binary floating point). Arithmetic stays exact until a program asks for inexactness: an
integer that grows past 64 bits keeps growing, dividing two integers that do not divide evenly gives a
rational, and a real appears only where a real literal or an operation on a real brings one in.

## Writing numbers

An integer is written in decimal, or in hexadecimal, octal or binary with `0x`, `0o` or `0b`, and
`_` may separate digits. A real has a decimal point, an exponent, or both. An integer may have any
number of digits, in any of the four bases.

```funl
write( 1_000_000 )
write( 0x1F )
write( 0o17 )
write( 0b101 )
write( 123456789012345678901234567890 )
write( 0xFFFF_FFFF_FFFF_FFFF_FF )
write( 2.5 )
write( 2.5e3 )
write( 2e-1 )
```

```output
1000000
31
15
5
123456789012345678901234567890
4722366482869645213695
2.5
2500
0.2
```

Any other base from 2 to 36 is written Icon's way, `NrDDD`: the base in decimal, an `r` (or `R`) and
the digits, which past 9 are letters in either case. It is one number of any size, with `_` between
digits as in the other forms, so `16rFF` is 255 and not `16` times a name `rFF`. A base outside 2 to 36,
a digit the base does not have, or no digits at all is an error.

```funl
write( 16rFF, 16rff, 2r1010, 36rZZ, 8R777 )
write( 16r1_0000_0000_0000_0000 )
write( 36rZZZZZZZZZZZZZZZZZZZZ )
```

```output
255, 255, 10, 1295, 511
18446744073709551616
13367494538843734067838845976575
```

## Integers grow without bound

An integer result too large for 64 bits is carried as a big integer, and one that comes back into
range is an ordinary integer again.

```funl
write( 9223372036854775807 + 1 )
write( 2 ^ 100 )
write( 2 ^ 64 - 1 )
write( 123456789012345678901234567890 - 123456789012345678901234567889 )
```

```output
9223372036854775808
1267650600228229401496703205376
18446744073709551615
1
```

## `/` is exact division

Dividing two integers gives a rational when the division is not exact, and an integer when it is. A
rational is always in lowest terms, with its sign on the numerator.

```funl
write( 7 / 2 )
write( 6 / 3 )
write( 4 / -6 )
write( 1/3 + 1/6 )
write( 1/2 + 1/2 )
write( 7 / 2 * 2 )
```

```output
7/2
2
-2/3
1/2
1
7
```

## Reals

An operation with a real operand produces a real, and **a real result stays a real even when its
value is whole**: `2.5 * 2` is a real, though it prints as `5`. A real prints as the shortest decimal
that reads back as the same value, with no `.0` and no exponent, so a whole real looks like an
integer; `is` tells them apart.

```funl
write( 2.5 * 2 )
write( 1/2 + 0.5 )
write( 1.0 / 3 )
write( 0.1 + 0.2 )
write( 100.0 )
write( 1e10 )
write( 1e20 )
write( 1e-7 )
write( -0.0 )
write( "${2.5} and ${1.0}" )
write( 2.5 * 2 is real )
```

```output
5
1
0.3333333333333333
0.30000000000000004
100
10000000000
100000000000000000000
0.0000001
-0
2.5 and 1
5
```

## The kinds, and `is`

`is` tests which kind a number is. `integer`, `rational` and `real` are the three kinds, and `number`
is any of them. A rational is a fraction that is not an integer, so an exact result that comes out
whole is an `integer`, while a whole real is still a `real`.

```funl
write( 1/3 is rational )
write( 3 is rational )
write( 3/2 * 2 is integer )
write( 2.5 * 2 is real )
write( 10^30 is integer )
write( 0.5 is number )
```

```output
1/3
3
5
1000000000000000000000000000000
0.5
```

## `\` is integer division, and it floors

`a \ b` is the quotient **rounded down, toward negative infinity** — not toward zero. With a negative
operand the two differ: `-7 \ 2` is `-4`, because -3.5 rounds down to -4. `//` is the same floor
division, written another way.

```funl
write( 7 \ 2 )
write( -7 \ 2 )
write( 7 \ -2 )
write( -7 \ -2 )
write( 7 // 2 )
write( -7 // 2 )
```

```output
3
-4
-4
3
3
-4
```

It applies to every kind of number, and the quotient is of the operands' kind: an integer from exact
operands, a real from a real one.

```funl
write( (2/3) \ (1/4) )
write( -(1/2) \ 1 )
write( 7.5 \ 2 )
write( -7.5 \ 2 )
write( -(10^30) \ 7 )
```

```output
2
-1
3
-4
-142857142857142857142857142858
```

## `mod` and `%`

`mod` is the remainder that goes with `\`: it has the sign of the **divisor**, and
`a == b * (a \ b) + a mod b` always holds. `%` is the remainder of division rounded toward zero, and
has the sign of the **dividend**. The two agree when the operands have the same sign.

```funl
write( 7 mod 2 )
write( -7 mod 2 )
write( 7 mod -2 )
write( 7 % 2 )
write( -7 % 2 )
write( 7 % -2 )
write( -7.5 mod 2 )
write( -7.5 % 2 )
write( (2/3) % (1/4) )
write( 10^30 mod 7 )
```

```output
1
1
-1
1
-1
1
0.5
-1.5
1/6
1
```

```funl
a = -7
b = 2
write( b * (a \ b) + a mod b )
```

```output
-7
```

## Division by zero

`/`, `\`, `//`, `mod` and `%` by zero are faults, not failures: the program stops.

```funl
write( 7 \ 0 )
```

```error
7 \ 0 divides by zero
```

```funl
write( 5 mod 0 )
```

```error
5 mod 0 divides by zero
```

```funl
write( 1.0 / 0 )
```

```error
1 / 0 divides by zero
```

## `^` is power

`^` raises a number to a power. An integer or rational raised to an integer power is exact, a
negative power included.

```funl
write( 2 ^ 10 )
write( (-2) ^ 3 )
write( 2 ^ -2 )
write( (1/2) ^ 3 )
write( 1.5 ^ 2 )
write( 0 ^ 0 )
```

```output
1024
-8
1/4
1/8
2.25
1
```

A real power, or a fractional one, gives a real: `4 ^ (1/2)` is a real, which prints as `2`.

```funl
write( 2 ^ 0.5 )
write( 4 ^ (1/2) )
write( 2 ^ 2.0 )
write( (1/4) ^ 0.5 )
write( 2 ^ -1.0 )
```

```output
1.4142135623730951
2
4
0.5
0.5
```

A negative number has no real fractional power, so asking for one is a fault:

```funl
write( (-8) ^ (1/3) )
```

```error
-8 ^ 1/3 has no value
```

Zero to a negative power divides by zero, whether the power is an integer or not:

```funl
write( 0 ^ -1 )
```

```error
0 ^ -1 divides by zero
```

```funl
write( 0 ^ -0.5 )
```

```error
0 ^ -0.5 divides by zero
```

A real power too large for a real is a fault too:

```funl
write( 10 ^ 400.0 )
```

```error
10 ^ 400 is too large for a float
```

## `div` asks "divides"

`a div b` succeeds, producing `b`, when the integer `a` divides the integer `b`, and fails when it
does not. It is a test, not a division.

```funl
write( 3 div 9 )
write( 3 div 10 )
write( [n | n <- 1..12 if 3 div n] )
```

```output
9
[3, 6, 9, 12]
```

```funl
write( 1.5 div 3 )
```

```error
'div' asks whether one integer divides another, and was given the real 1.5
```

## Comparing numbers

The orderings `<`, `<=`, `>`, `>=` and the equalities `==`, `!=` compare numbers of different kinds
by value, exactly: `1 == 1.0` holds, and a rational is compared with a real's exact binary value. Like
every comparison, one that holds produces its right operand.

```funl
write( 1 == 1.0 )
write( 2 == 2.0 )
write( 1 + 1/2 == 3/2 )
write( 1 != 1.0 )
write( 0.5 < 1/2 )
write( 1/3 < 0.3333333333333333 )
write( 0.3333333333333333 < 1/3 )
```

```output
1
2
3/2
1/3
```

## `abs`, `min` and `max`

`abs` keeps its argument's kind. `min` and `max` take any number of arguments, of any kinds, and
produce the least or greatest as it was given.

```funl
write( abs(-7/2) )
write( abs(-3) )
write( abs(-2.5) )
write( min(3, 1/2, 0.75) )
write( max(3, 1/2, 4.5) )
```

```output
7/2
3
2.5
1/2
4.5
```

## Functions

The functions of a number are builtins, each answering the real it is the function of: `sqrt`, `exp`,
`log` (natural), `sin`, `cos`, `tan`, `asin`, `acos` and `atan`, which takes one number or two,
`atan(y, x)`, the angle of the point (x, y). `atan2` is the two-argument form alone. Angles are in
radians.

```funl
write( sqrt(16), sqrt(2), sqrt(1/4) )
write( exp(0), log(1) )
write( sin(0), cos(0), tan(0) )
write( asin(0), acos(1), atan(0) )
write( atan(1, 1), atan2(1, 1) )
```

```output
4, 1.4142135623730951, 0.5
1, 0
0, 1, 0
0, 0, 0
0.7853981633974483, 0.7853981633974483
```

`sinh`, `cosh`, `tanh`, `asinh`, `acosh` and `atanh` are the hyperbolic functions and their inverses;
`cot` and `acot` the cotangent and its inverse; `log2` the base-2 logarithm; `log` with two arguments,
`log(base, x)`, the logarithm of `x` to `base`; and `copysign(x, y)` is `x` with the sign of `y`
(a negative zero counts as negative).

```funl
write( sinh(0), cosh(0), tanh(0), asinh(0), acosh(1), atanh(0) )
write( sinh(1), atanh(0.5) )
write( log(2, 8), log2(8), log(1) )
write( cot(1), acot(1), acot(0) )
write( copysign(3, -0.0), copysign(-2.5, 1) )
```

```output
0, 1, 0, 0, 0, 0
1.1752011936438014, 0.5493061443340549
3, 3, 0
0.6420926159343308, 0.7853981633974483, 1.5707963267948966
-3, 2.5
```

`floor`, `ceiling`, `round` (halves away from zero), `truncate` and `integer` (the same as `truncate`)
answer integers, and exactly: a rational is rounded as a rational, an integer is its own. `sign`
keeps its argument's kind, and `float` makes a real. `integer` takes a string too, and so do `real`
and `numeric` ([below](#converting-a-string-to-a-number)).

```funl
write( floor(7/2), ceiling(7/2), round(7/2), truncate(-7/2), floor(-7/2) )
write( floor(2.5), round(-2.5), integer(2.7), floor(5) )
write( sign(-3), sign(2.5), sign(0), float(1/4), float(3) )
```

```output
3, 4, 4, -3, -4
2, -3, 2, 5
-1, 1, 0, 0.25, 3
```

`pi`, `e` (Euler's number), `phi` (the golden ratio) and `epsilon` (the gap between 1.0 and the next
real above it) are values, written without parentheses. Each is a builtin like the functions, and a
program's own `e` or `pi` — a `val`, a parameter — hides it where it is in scope, as any declaration
hides a builtin (see [Assignment and scope](assignment.md#a-builtins-name-is-free-to-use)).

```funl
write( pi, e, phi, epsilon )
write( 2 * pi )
val f = pi -> pi + 1
write( f(1) )
val e = 2
write( e )
```

```output
3.141592653589793, 2.718281828459045, 1.618033988749895, 0.0000000000000002220446049250313
6.283185307179586
2
2
```

`dtor(x)` turns degrees into radians and `rtod(x)` radians into degrees; each answers a real.

```funl
write( dtor(180), rtod(pi), dtor(90) )
write( sin( dtor(30) ) )
```

```output
3.141592653589793, 180, 1.5707963267948966
0.49999999999999994
```

```funl
write( dtor('a') )
```

```error
'dtor' wants a number and was given the string 'a'
```

A function given an argument it has no value for is a fault, as `(-8) ^ (1/3)` is.

```funl
write( sqrt(-1) )
```

```error
sqrt(-1) has no value
```

```funl
write( log(0) )
```

```error
log(0) has no value
```

`log(base, x)` has no value when `base` is 1, `base` or `x` is not positive; `acosh` wants at least 1;
`atanh` wants a number strictly between -1 and 1; `cot(0)` has no value.

```funl
write( log(1, 5) )
```

```error
log(1, 5) has no value
```

```funl
write( acosh(0.5) )
```

```error
acosh(0.5) has no value
```

```funl
write( atanh(1) )
```

```error
atanh(1) has no value
```

```funl
write( exp(1000) )
```

```error
exp(1000) is too large for a float
```

Something that is not a number is a fault too.

```funl
write( sqrt('a') )
```

```error
'sqrt' wants a number and was given the string 'a'
```

## `odd` and `even`

`odd(n)` and `even(n)` take an integer, of any size. Like a comparison, each produces `n` when it holds
and fails when it does not.

```funl
write( [x | x <- 1..8 if odd(x)] )
write( even(-4) )
write( odd(2 ^ 70 + 1) )
write( if even(7) then 'even' else 'odd' )
```

```output
[1, 3, 5, 7]
-4
1180591620717411303425
odd
```

Anything but an integer is a fault.

```funl
write( odd(0.5) )
```

```error
'odd' wants an integer and was given the real 0.5
```

```funl
write( even(1/2) )
```

```error
'even' wants an integer and was given the rational 1/2
```

## Bits

`iand`, `ior` and `ixor` combine two integers bit by bit, `icom` flips every bit, and `ishift(i, j)`
shifts `i` left by `j` places, or right by `-j` places when `j` is negative. The integers are read as if
two's complement went on for ever, so a negative one has an endless run of ones on its left, and
nothing overflows: a result past 64 bits is a big integer. A right shift rounds down, so `ishift(-8, -1)`
is `-4` and `ishift(-1, -100)` is `-1`.

```funl
write( iand(12, 10), ior(12, 10), ixor(12, 10), icom(5) )
write( ishift(1, 4), ishift(256, -4), ishift(-8, -1) )
write( ishift(1, 70), iand(-1, 2 ^ 70), icom(2 ^ 70) )
```

```output
8, 14, 6, -6
16, 16, -4
1180591620717411303424, 1180591620717411303424, -1180591620717411303425
```

Anything but an integer is a fault.

```funl
write( iand(1.5, 1) )
```

```error
'iand' wants an integer and was given the real 1.5
```

```funl
write( ishift(1, 0.5) )
```

```error
'ishift' wants an integer and was given the real 0.5
```

## Random numbers

The prefix `?` draws at random, and what it draws depends on what follows.

| | |
|---|---|
| `?n`, `n` a positive integer | an integer from 1 to `n`, a die roll: `n` is a count, not an index |
| `?0` | a real from 0 up to, but not including, 1 |
| `?s`, `s` a string | one character of it |
| `?c`, `c` a cset | one character of it, as a one-character string, from the same characters `!c` lists; a complement faults as `!c` does, and a cset cannot be assigned through `?` |
| `?c`, `c` a list, range, tuple, set, array or buffer | one element of it |
| `?m`, `m` a map | one value of it |

An empty string or collection has nothing to draw, so the draw fails. A negative count, or something
with no elements, is a fault. `random()` is the same real from 0 up to 1 as `?0`, for a program that
says what it means.

```funl
n = ?6
write( if n >= 1 and n <= 6 then 'a die' else 'not a die' )
write( ?"a", ?[7], ?{8}, ?array([9]) )
r = random()
write( if r >= 0 and r < 1 then 'below one' else 'out of range' )
write( if ?"" then 'drew' else 'nothing to draw' )
```

```output
a die
a, 7, 8, 9
below one
nothing to draw
```

```funl
write( ?(-1) )
```

```error
'?' wants a count of zero or more and was given -1
```

`?` is a prefix operator, so it takes nothing away from the infix `s ? e` that scans a string: `"abc" ? ?"a"`
scans `"abc"` with the draw `?"a"`.

On an array, a buffer or a mutable map, `?c` can also be assigned to, and then it picks a random element
of `c` to replace. The collection is evaluated once. `?c = x`, `?c += x` and `?c <- x` all work as
they do on a named element, and on an empty collection they fail.

```funl
a = array([0, 0, 0])
every 1 to 5 do ?a += 2
write( sum(a) )
b = buffer()
b += 1
?b = 9
write( b )
```

```output
10
Buffer(9)
```

**The generator** is sysl's PCG32, seeded from the operating system the first time a program draws, so
two runs differ. `random_seed(n)` restarts it from the seed `n` and `random_seed()` reads the seed it
started from, so a run is repeated by setting the same one. The sequences repeat within FunL; they are
not the ones Icon's own generator would give.

```funl
random_seed(7)
a = [random(), ?100, ?"abcdef"]
random_seed(7)
b = [random(), ?100, ?"abcdef"]
write( if a == b then 'repeated' else 'differs' )
write( random_seed() )
```

```output
repeated
7
```

## Converting a string to a number

Arithmetic never reads a number out of a string; `integer`, `real` and `numeric` do. Each takes a
number or a string. A string is read as a number literal is, so it converts exactly when the same
characters in a program would be a number, and the three then make of it what they make of a
number: `integer` truncates toward zero as `truncate` does, `real` makes a real, and `numeric` leaves the number the
kind it is.

```funl
write( integer('42') + 1, integer('1_000'), integer('0xFF') )
write( integer('2.7'), integer('-2.7'), real('3'), real(1/4) )
write( numeric('7'), numeric('0.5'), numeric(7/2) )
```

```output
43, 1000, 255
2, -2, 3, 0.25
7, 0.5, 7/2
```

Two things a literal does not have are taken from a string, since a string being converted is data
rather than program: a sign, and spaces around the number. A radix written `NrDDD` is a literal too (see
*Writing numbers*), so a program's `16rFF` and the string `'16rFF'` are the same number.

```funl
write( integer(' -17 '), integer('+5') )
write( integer('16rFF'), integer('36rZZ'), integer('2r1010'), integer('8r777') )
```

```output
-17, 5
255, 1295, 10, 511
```

A string that is not a number makes the conversion fail, so a conversion is also the test:

```funl
write( if integer('ten') then 'a number' else 'not a number' )
write( [numeric(s) | s <- ['1', 'two', '3.5', ' 4 ', '1.', '37r1', '7/2']] )
```

```output
not a number
[1, 3.5, 4]
```

A string spelling a number too large to hold is a fault, as the same literal is an error in a
program, and so is a value that is neither a number nor a string:

```funl
write( real('1e999') )
```

```error
'1e999' is too large for a float
```

```funl
write( integer([1]) )
```

```error
'integer' wants a number or a string and was given the list [1]
```

## Arithmetic on something that is not a number

An arithmetic operator given something other than a number is a fault. (`+` is the exception: on
strings, lists, sets and maps it has meanings of its own.)

```funl
write( 'a' * 2 )
```

```error
'*' wants numbers and was given the string 'a'
```
