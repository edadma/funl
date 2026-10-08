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
Only a `catch`, below, stops a fault.

```funl
write( 1 / 0 )
write( 'never printed' )
```

```error
divides by zero
```

## Catching faults

`e catch err -> r` produces `e`'s value, unless a fault is raised while `e` is evaluated. Then the
fault is bound to `err` and the expression produces `r` instead. `err.message` is the sentence the
fault would have printed:

```funl
write( 1 / 0 catch e -> e.message )
x = 10 / 0 catch _ -> 0
write( x )
```

```output
1 / 0 divides by zero
0
```

The recovery may be an indented block, whose last line is its value:

```funl
total = 10 / 0 catch e ->
  write( "recovering: ${e.message}" )
  0
write( total )
```

```output
recovering: 10 / 0 divides by zero
0
```

**`catch` binds more loosely than every operator except the lambda arrow**, so `a + b catch …`
guards the whole sum and `x -> 1 / x catch …` is a lambda whose body guards. The recovery reaches as
far right as a lambda's body does, so a `catch` inside a longer expression is put in parentheses.

### What a fault is

A fault is an **error term**, `error(Formal, Context)`, the same term Prolog's `catch/3` sees
([Prolog](prolog.md#errors-and-exceptions)). What follows `catch` is a **pattern**, matched against
that term, so a `catch` can take apart the kind of error it caught. `error(x)` raises
`error(user_error(x), _)`:

```funl
write( 1 + #a catch error(type_error(kind, culprit), _) -> [kind, culprit] )
write( error("no such user") catch error(user_error(m), _) -> m )
```

```output
[number, a]
no such user
```

**A fault the pattern does not match is raised again**, unchanged, for an enclosing `catch` or the
end of the program to see. Here `safe` catches arithmetic with no answer and nothing else:

```funl
def safe( n ) = 100 / n catch error(evaluation_error(_), _) -> 0

write( safe(4) )
write( safe(0) )
write( safe(#x) )
```

```error
'/' wants numbers and was given the atom x
```

A guard follows the pattern after `|`, as a lambda's does, and a fault whose guard fails is raised
again too:

```funl
def share( n ) = 100 / n catch e | n == 0 -> 'nothing to share'

write( share(5) )
write( share(0) )
```

```output
20
nothing to share
```

Catches nest, and the innermost one that takes the fault is the one that recovers. A fault raised in
the recovery is not caught by its own `catch`:

```funl
write( (1 / 0 catch error(type_error(_, _), _) -> 'inner') catch _ -> 'outer' )
write( (1 / 0 catch _ -> error( "tried again" )) catch e -> e.message )
```

```output
outer
tried again
```

### Failure is not a fault

An expression that fails is not caught: `e catch …` fails when `e` fails.

```funl
write( (1 > 2) catch _ -> 'caught' )
write( (1 > 2 catch _ -> 'caught') or 'failed' )
```

```output
failed
```

### Catching a generator

A `catch` guards its expression for as long as the expression can produce values, so a fault raised
when a generator is **resumed** for another value is caught too. Catching it ends the expression:
the values it had not yet produced are not tried.

```funl
def check( n ) = if n == 0 then error( "found a zero" ) else n

every write( check(2 | 0 | 3) catch e -> e.message )
```

```output
2
found a zero
```

What is done with a value after the guarded expression has produced it is not guarded. Here `10 / 5`
is written, and `10 / 0`, made from the guarded expression's second value, is a fault the `catch`
does not see:

```funl
every write( 10 / ((5 | 0) catch _ -> 1) )
```

```error
10 / 0 divides by zero
```

### Throwing a value

`throw(x)` raises `x` itself, rather than an error term around it, as Prolog's `throw/1` does. A
`catch` matches what was thrown, and a value with no message has no `message`:

```funl
data oops( n )

write( throw(oops(3)) catch oops(n) -> n + 1 )
write( (throw(oops(3)) catch b -> b.message) or 'no message' )
```

```output
4
no message
```

A ball a Prolog predicate throws reaches FunL the same way. [`prolog/oops.pl`](prolog/oops.pl)
holds `risky(X) :- throw(oops(X)).`:

```funl
import "prolog/oops.pl"

write( risky(7) catch oops(n) -> n )
```

```output
7
```

A ball nothing catches ends the program, naming the ball:

```funl
throw( #loose )
```

```error
uncaught exception: loose
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
