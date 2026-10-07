---
title: Success and failure
weight: 10
---

# Success and failure

Evaluating a FunL expression either **produces a value** or **fails**. Failure is not an error: it
is the ordinary way for an expression to say "no". A comparison that does not hold fails, and `fail`
fails on purpose.

## A failing expression produces nothing

A statement whose expression fails does nothing more, and the next statement runs. Here the second
and third calls to `write` are never made, because their arguments fail:

```funl
write( 1 < 2 )
write( 1 > 2 )
write( fail )
write( 'done' )
```

```output
2
done
```

## A comparison produces its right operand

A comparison that holds produces its **right** operand, not a boolean. That is what lets
comparisons chain: `1 < x < 10` is `(1 < x) < 10`, and `1 < x` produces `x`.

```funl
x = 7
write( 1 < x < 10 )
write( 1 < x < 5 )
write( (1 < 2) + 10 )
```

```output
10
12
```

## Failure propagates outwards

An operator whose operand fails fails. A call whose argument fails is never made, and fails. A list
one of whose elements fails fails. The failure travels outwards until something catches it, and a
statement is the most common thing that does.

```funl
def half( n ) = n / 2

write( half(10 > 4) )
write( half(1 > 2) )
write( [1, 3 > 2] )
write( [1, 2 > 3] )
write( 1 + fail )
write( 'reached' )
```

```output
2
[1, 2]
reached
```

## A fault is not a failure

A fault is an error: it stops the program, and no statement catches it. Dividing by zero is one.

```funl
write( 1 / 0 )
write( 'never printed' )
```

```error
divides by zero
```

## `true` and `false`, and conditions

`true` and `false` are ordinary values, and `false` can be stored and written like any other. But
in a **condition**, `false` fails just as a failing expression does. The conditions are the test of
`if` and `elif`, the test of `while`, a guard, an operand of `and`, `or` and `not`, and a
comprehension's filter.

```funl
v = false
write( v )
if v then write( 'v holds' ) else write( 'v does not hold' )
write( v or 'other' )
write( not v )
write( [b | b <- [true, false, true] if b] )
```

```output
false
v does not hold
other
()
[true, true]
```

A guard is a condition too, so a clause whose guard is `false` is passed over:

```funl
def check( b ) | b = 'yes'
def check( _ ) = 'no'

write( check(true) )
write( check(false) )
```

```output
yes
no
```

## `if`

`if c then a else b` produces `a`'s value when `c` succeeds and `b`'s when it fails. **Without an
`else`, the `if` fails when its test fails.** The block form takes `elif` and `else` on lines of
their own.

```funl
x = 7
write( if x > 5 then 'big' else 'small' )
write( if x > 50 then 'big' )

if x < 0
  write( 'negative' )
elif x < 10
  write( 'small' )
else
  write( 'large' )
```

```output
big
small
```

## `not`, `and` and `or`

`not e` succeeds, producing `()`, exactly when `e` fails. `e1 and e2` succeeds with `e2`'s value
when both succeed. `e1 or e2` produces `e1`'s value when `e1` succeeds, and otherwise `e2`'s.

```funl
write( not (1 > 2) )
write( not (1 < 2) )
write( 1 < 2 and 'both' )
write( 1 > 2 and 'both' )
write( 1 < 2 or 'second' )
write( 1 > 2 or 'second' )
```

```output
()
both
2
second
```

## A loop fails when it runs out

A `while` loop fails when its test fails, and a `for` loop fails when its values are spent. A
`break` makes the loop succeed, with the value it carries, or with `()` when it carries none.

```funl
i = 0
write( while i < 3 do i = i + 1 )
write( i )
write( for d <- 1.. do if d * d >= 30 then break (d) )
write( repeat break )
```

```output
3
6
()
```

A `break` belongs to a loop, and one written anywhere else is refused before the program runs:

```funl
break
```

```error
`break` is written inside a loop
```
