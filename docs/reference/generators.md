---
title: Generators
weight: 20
---

# Generators

An expression can produce a value and then, if asked, another. Such an expression is a
**generator**. What asks for the next value is failure: when something after a generator fails,
evaluation goes back into the generator for its next value and tries again. This is called
**backtracking**, and it is how FunL searches without a loop being written. Failure itself is
described in [Success and failure](success-and-failure.md).

`every` asks a generator for all of its values:

```funl
every write( 1 to 3 )
```

```output
1
2
3
```

## The generators

| expression | produces |
|---|---|
| `!c` | each element of a list, array, set, string or range `c` |
| `a \| b` | every value of `a`, then every value of `b` |
| `\|e` | the values of `e`, then `e`'s values again, for as long as each round produces one |
| `i to j`, `i until j`, optional `by k` | the numbers from `i` to `j`, inclusive (`to`) or exclusive (`until`) of `j`, in steps of `k` (1 when there is no `by`) |
| a call to a generating function | each value the function produces |

```funl
every write( ![10, 20] )
every write( !'ab' )
every write( 'x' | 'y' | 'z' )
every write( 1 until 3 )
every write( 1 to 10 by 4 )
every write( 10 to 1 by -4 )
```

```output
10
20
a
b
x
y
z
1
2
1
5
9
10
6
2
```

`!` of an empty collection produces nothing, so it fails:

```funl
every write( ![] )
write( 'nothing written' )
```

```output
nothing written
```

`|e` starts `e` over each time it runs out, and stops when a round produces no value at all. Here
each round produces one value until `n` reaches 3:

```funl
n = 0
every write( |(n < 3 and (n = n + 1)) )
```

```output
1
2
3
```

## `to` counts with any number

`to` and `until` count with integers, rationals and reals alike. The values are `i`, `i + k`,
`i + 2*k`, `i + 3*k` and so on, each worked out from the start by the same arithmetic as `+` and
`*`, so a rational step counts exactly, and integers and rationals mix as they do in `+`. A
negative step counts down. A real step makes every value a real, the first one included:

```funl
every write( 0 to 1 by 1/4 )
every write( 1 to 2 by 0.5 )
every write( 3/2 until 0 by -1/2 )
```

```output
0
1/4
1/2
3/4
1
1.0
1.5
2.0
3/2
1
1/2
```

Because each value is worked out from the start rather than from the value before, a real step's
rounding does not build up along the range: every value is rounded once, however far along it is,
and counting from 0 by `0.1` reaches `1.0`:

```funl
every write( 0 to 1 by 0.1 )
```

```output
0.0
0.1
0.2
0.30000000000000004
0.4
0.5
0.6000000000000001
0.7000000000000001
0.8
0.9
1.0
```

The range stops at the first value that is past `j`: greater than `j` for `to` and greater than or
equal for `until` when counting up, less than or less than or equal when counting down. The
comparison is the plain one, with no tolerance, so a value's rounding shows. `0 + 3*0.1` is
`0.30000000000000004`, which is past `0.3`; the rational step reaches `3/10` exactly:

```funl
every write( 0 to 0.3 by 0.1 )
every write( 0 to 3/10 by 1/10 )
```

```output
0.0
0.1
0.2
0
1/10
1/5
3/10
```

A step of zero, of any kind, is refused, and so is anything that is not a number:

```funl
every write( 1 to 2 by 0.0 )
```

```error
`to ... by 0.0` would produce 1.0 for ever: a step cannot be zero
```

```funl
every write( 1 to 'ten' )
```

```error
`to` counts with numbers and was given the string 'ten'
```

## A range is a value; `to` is a generator

`1..5` is a range: one value, which can be stored, written and iterated. `1 to 5` produces five
values one after another. `..<` excludes the end, `..+n` counts `n` values from the start, and
`a..` has no end.

```funl
r = 1..3
write( r )
write( 1..<5 )
write( 1..+3 )
every write( !r )
write( 1 to 5 )
for v <- 10.. do if v > 12 then break else write( v )
```

```output
1..3
1..<5
1..<4
1
2
3
1
10
11
12
```

`write( 1 to 5 )` writes only `1`, because a statement stops at its first result (below).

A range value counts with integers only; for other numbers, use `to`:

```funl
write( 0..1/2 )
```

```error
a range counts with integers, and its end is the rational 1/2
```

## Drawing from a generator with `<-`

The binding `x <- e` of a `for` or a comprehension takes **every** value `e` produces. A value that
is a collection — a list, set, map, array, buffer or range — is iterated, and any other value is
taken as it is. So `x <- [1, 2]` and `x <- 1..2` give the elements, and `x <- g()` gives each value
a generating function produces:

```funl
def evens(n)
  for i <- 1..n do yield 2*i

write( [x | x <- evens(3)] )
write( [x | x <- 1 to 3] )
write( [x | x <- ('a' | [1, 2] | 'b')] )
for x <- evens(2) do write( x )
```

```output
[2, 4, 6]
[1, 2, 3]
["a", 1, 2, "b"]
2
4
```

A string and a tuple are not collections here: each is taken whole. To draw a string's characters
or a tuple's elements, say so with `!`, which generates them:

```funl
write( [w | w <- ('to' | 'be')] )
write( [c | c <- 'to'] )
write( [c | c <- !'to'] )
write( [t | t <- (1, 2)] )
write( [x | x <- !(1, 2)] )
```

```output
["to", "be"]
["to"]
["t", "o"]
[(1, 2)]
[1, 2]
```

Because each collection is iterated, a generator of lists gives their elements. To keep each value
whole, put the generator in a list, `x <- [e]`: each of its values is then a list of one, whose
element is the value.

```funl
write( [l | l <- ([1, 2] | [3])] )
write( [l | l <- [[1, 2] | [3]]] )
```

```output
[1, 2, 3]
[[1, 2], [3]]
```

A generator that produces nothing gives nothing to draw:

```funl
def none()
  fail

write( [x | x <- none()] )
```

```output
[]
```

## Backtracking into a generator

When an operator's result fails, evaluation backtracks into its operands for their next values. A
comparison with a generator operand therefore finds the first value that makes it hold:

```funl
write( 3 < (1 to 5) )
write( ((x = 1 to 9) * x) > 20 and x )
```

```output
4
5
```

Inside parentheses, in a call's arguments and at the head of `every`, `name = e` stores each value
of `e` in `name` and produces it. So in the next program the outer `1 to 3` binds `k`, the inner
generator runs out, and `every` asks the outer generator for the next `k`:

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

`e1 & e2` is conjunction: `e2` is evaluated for each value of `e1`, and the conjunction produces
`e2`'s values. A failing `e2` sends evaluation back into `e1`:

```funl
every write( (k = 1 to 3) & k * 10 )
every write( (k = 1 to 6) % 2 == 0 & k )
```

```output
10
20
30
2
4
6
```

`e1 or e2` is a generator too: every value of `e1`, then every value of `e2`.

```funl
every write( (1 to 3) > 1 or 10 )
```

```output
1
1
10
```

A function whose body ends in a generator, or which uses `yield`, is a generator too; see
[Generator functions](functions.md#generator-functions).

## A search is an expression

Generators, comparisons and reversible assignment (`<-`, which is undone when evaluation
backtracks over it; see [Assignment and scope](assignment.md)) together make a search one
expression. This
places five queens on a 5×5 board so that none attacks another: the row is chosen by a generator,
each constraint is a comparison, and the placement is reversible, so a failing constraint backtracks
into the generator for the next row.

```funl
n = 5
row = array( n )
down = array( 2 * n - 1 )
up = array( 2 * n - 1 )
solution = array( n )
count = 0

def solve( c )
  every row(r = 0 until n) == down(r + c) == up(n - 1 + r - c) == undefined and
        row(r) <- down(r + c) <- up(n - 1 + r - c) <- r
    solution(c) = r

    if c == n - 1
      count = count + 1
      if count == 1 then write( seq(solution) )
    else
      solve( c + 1 )

solve( 0 )
write( count )
```

```output
(0, 2, 4, 1, 3)
10
```

## Bounded expressions

**A bounded expression stops at its first value**: once it has produced one, its generators are
discarded and nothing backtracks into it again.

| construct | what is bounded |
|---|---|
| a statement | the whole statement: it runs to its first result or to failure, then the next statement runs |
| `if c then a else b` | `c`; `a` and `b` are not |
| `while c do b` | `c` and `b`, each on every turn |
| `repeat b` | `b`, on every turn |
| `every e do b` | `b`, once for each value of `e`; `e` is driven through all its values |
| `for x <- c if f do b` | `b`; the head is a generator |
| `not e` | `e` |

A statement is bounded only once it has a result. Until then it backtracks like any expression, so
a statement that ends in failure tries every value before giving up:

```funl
x = 1 to 5
write( x )
write( 1 to 3 ) & fail
(k = 1 to 5) > 3 & write( k )
```

```output
1
1
2
3
4
```

The test of an `if` is bounded, but the branch taken is not:

```funl
every write( if (k = 1 to 3) > 1 then k )
every write( if 1 < 2 then 'a' | 'b' else 'c' )
```

```output
2
a
b
```

`not e` searches `e` for a value, and succeeds only when there is none:

```funl
write( not (1 to 3) > 5 )
write( not (1 to 3) > 2 )
```

```output
()
```

`every` drives its generator through every value, running the body once for each, and then
fails, since it exists for what its body does:

```funl
every k = 1 to 3 do write( k | 'never' )
write( every 1 to 3 do () )
write( 'after' )
```

```output
1
2
3
after
```

A `for` loop takes its values by pattern, with an optional filter after `if`:

```funl
for (a, b) <- [(1, 'one'), (2, 'two'), (3, 'three')] if a != 2 do write( b )
```

```output
one
three
```

## `break`, `continue` and labels

`continue` starts the loop's next turn, and `break` leaves it. A loop takes an optional label, and
`break label` and `continue label` reach out to the loop of that name. A `break` may carry a value,
which becomes the loop's value (see [A loop fails when it runs
out](success-and-failure.md#a-loop-fails-when-it-runs-out)).

```funl
n = 0
repeat
  n = n + 1
  if n == 3 then continue
  if n > 5 then break
  write( n )

outer: for i <- 1..3
  for j <- 1..3
    if j == 2 then continue outer
    if i == 3 then break outer
    write( (i, j) )

v = outer: for i <- 1..5
  for j <- 1..5
    if i * j == 12 then break outer (i, j)
write( v )
```

```output
1
2
4
5
(1, 1)
(2, 1)
(3, 4)
```

A label must name a loop around the `break`:

```funl
for i <- 1..3
  break nowhere
```

```error
no loop around this `break` is labelled `nowhere`
```

`break` and `continue` reach only the loops of the function they are written in, so one in a
function defined inside a loop is refused:

```funl
for i <- 1..3
  def f() = break
  f()
```

```error
`break` is written inside a loop
```
