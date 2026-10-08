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
`_` may separate digits. A real has a decimal point, an exponent, or both. A decimal integer may have
any number of digits; one written in another radix has to fit 64 bits.

```funl
write( 1_000_000 )
write( 0x1F )
write( 0o17 )
write( 0b101 )
write( 123456789012345678901234567890 )
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
2.5
2500.0
0.2
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
value is whole**: `2.5 * 2` is `5.0`, not `5`. A real prints as the shortest decimal that reads back
as the same value, always with a `.0` or an exponent.

```funl
write( 2.5 * 2 )
write( 1/2 + 0.5 )
write( 1.0 / 3 )
write( 0.1 + 0.2 )
write( 100.0 )
write( 1e10 )
write( 1e22 )
write( 1e-7 )
```

```output
5.0
1.0
0.3333333333333333
0.30000000000000004
100.0
10000000000.0
1.0e+22
1.0e-07
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
5.0
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
3.0
-4.0
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
1.0 / 0 divides by zero
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

A real power, or a fractional one, gives a real: `4 ^ (1/2)` is `2.0`, not `2`.

```funl
write( 2 ^ 0.5 )
write( 4 ^ (1/2) )
write( 2 ^ 2.0 )
write( (1/4) ^ 0.5 )
write( 2 ^ -1.0 )
```

```output
1.4142135623730951
2.0
4.0
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
10 ^ 400.0 is too large for a float
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
1.0
2.0
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

## Arithmetic on something that is not a number

An arithmetic operator given something other than a number is a fault. (`+` is the exception: on
strings, lists, sets and maps it has meanings of its own.)

```funl
write( 'a' * 2 )
```

```error
'*' wants numbers and was given the string 'a'
```
