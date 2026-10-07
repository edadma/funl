---
title: The language
weight: 10
---

# The language

This chapter is FunL as the old implementation and its test suite define it — the seventy-odd
programs in `FunLExamples` and `FunLTests` are the behavioural specification, and every example
below is taken from or modelled on one of them. It says what a reader needs in order to follow the
machine chapter, and it marks every place the rewrite changes something.

## Every expression succeeds or fails

**The central rule: evaluating an expression either produces a value or fails, and failure is not
an error.** It is the ordinary way to say "no". A comparison that does not hold fails; a lookup that
finds nothing fails; `fail` fails on purpose.

```
if 3 < 5
  write( 1 )        ;; runs: 3 < 5 succeeds
elif 4 < 5
  write( 2 )

write( 3 )
```

A comparison that succeeds produces its **right** operand, which is what lets comparisons chain:

```
1 < x < 10          ;; (1 < x) produces x, then x < 10
```

and it is why the n-queens program can write a three-way test as one expression:

```
row(r) == down(r + c) == up(n - 1 + r - c) == undefined
```

**Failure propagates outwards until something catches it.** An operator whose operand fails fails;
a call whose argument fails fails. What catches a failure is a *bounded* context, and the most
common one is the statement: a statement that fails simply moves on to the next.

```
write( 1 < 0 )      ;; prints nothing -- the comparison fails, so the call is never made
write( 'next' )     ;; prints next
```

### Conditions and `false`

**FunL has the values `true` and `false`, but a condition is decided by success and failure.** In
the old implementation `if false then 'a' else 'b'` answers `'a'`, because `false` is a value and a
value is success. The instruction that would have failed on `false` exists and its use in the
compiler is commented out.

> **Decided — does `false` fail in a condition?**
> **Decision: yes.** A condition — the test of `if`, `elif`, `while`, a guard, an operand of
> `and`, `or` and `not`, a comprehension filter — fails when its expression fails *or* produces
> `false`. Everywhere else `false` is an ordinary value. This keeps goal-directed evaluation intact
> (a comparison still fails rather than answering `false`) while making `if done then …` mean what
> every reader expects. **Rejected:** keep the old rule (a condition only fails on failure),
> which is Icon's, and costs a `== true` wherever a boolean is tested; or make every operation that
> could produce `false` fail instead, which removes booleans as values and breaks `write(x is int)`.

## Generators: an expression may produce more than one value

**An expression can produce a value, and later — if the context asks — another.** The context asks
by failing: when something after a generator fails, the machine backtracks into the generator for
its next value and tries again. This is Icon's goal-directed evaluation, and FunL has the same
generators:

| expression | produces |
|---|---|
| `!c` | each element of a list, array, set, string, range, or iterator `c` |
| `a \| b` | every value of `a`, then every value of `b` |
| `\|e` | the values of `e`, then `e`'s values again, for as long as each round produces one |
| `i to j`, `i until j`, optional `by k` | the numbers from `i`, inclusive or exclusive of `j` |
| a call to a generating function | each value the function produces (below) |

```
every write( (k = 1 to 3) to k + 2 )
```

prints `1 2 3 2 3 4 3 4 5`, one per line: the outer `1 to 3` binds `k`, the inner generator runs to
exhaustion, and `every` keeps asking until the outer generator is exhausted too.

**`..` is a range VALUE and `to` is a GENERATOR**, which is the one distinction in this table that is
easy to get wrong: `1..5` is a range object you can store and iterate, `1 to 5` produces five values
one after another. `..<` excludes its end, `..+n` counts `n` from the start, `a..` is an unbounded
lazy list.

**Goal-directed evaluation is what makes a search an expression.** In n-queens the row is chosen by
a generator, the three constraints are comparisons, and the placement is a reversible assignment —
a failing constraint backtracks into the generator for the next row without a loop being written:

```
def solve( c )
  every row(r = 0 until n) == down(r + c) == up(n - 1 + r - c) == undefined and
        row(r) <- down(r + c) <- up(n - 1 + r - c) <- r
    solution(c) = r

    if c == n - 1
      write( seq(solution) )
    else
      solve( c + 1 )
```

### Control structures, and which of them are bounded

**A bounded expression is one whose generators are discarded once it has produced its first
value.** A statement is bounded, and so are these positions:

| construct | behaviour |
|---|---|
| a statement | runs to its first result or to failure; either way control moves to the next statement |
| `if c then a else b` | `c` is bounded; `a` or `b` is not. Without `else`, the `if` fails when `c` fails |
| `while c do b` | `c` and `b` are each bounded on every turn; ends when `c` fails |
| `repeat b` | `b` bounded, forever, until `break` |
| `every e do b` | drives `e` through **all** its values, running `b` (bounded) for each; `every` itself then fails |
| `for x <- c if f do b` | iterates `c` by pattern, with an optional filter; the head is a generator, the body is bounded |
| `not e` | succeeds (with `()`) exactly when `e` fails; `e` is bounded |
| `e1 & e2`, `e1 and e2` | conjunction: `e2` is evaluated for each value of `e1` until it succeeds |
| `e1 or e2` | alternation at the logical level, the same machinery as `\|` |

Loops take an optional label (`outer: for …`) and `break label` / `continue label` reach out by
name; `break` may carry a value, `break (e)`.

**`every` failing at the end is Icon's rule and FunL keeps it**: `every` exists for its side effects,
and a statement that fails is harmless.

## Functions

```
def hanoi( n, source, target, auxiliary )
  if n > 0
    hanoi( n - 1, source, auxiliary, target )
    target += source.removeLast
    hanoi( n - 1, auxiliary, target, source )
```

A body is an indented block or `= expression`. Lambdas are `x -> x + a` and `(n, m) -> n * m`, and
they close over what they mention:

```
curry = f -> a -> b -> f(a, b)
uncurry = f -> (a, b) -> f(a)(b)
```

An operator in parentheses is a function, `(+)`, and a section fixes one side, `(+ 1)`, `(2 *)`.

### Clauses, patterns and guards: committed choice

**A function may be defined by several clauses, tried in order; the first whose parameters match and
whose guard succeeds is chosen, and the rest are discarded.** That is committed choice — once a
clause is chosen, failure inside its body does not try the next clause.

```
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
def pow( _, _ ) = error( "pow: negative exponent" )
```

The pieces in that example:

- **Parameters are patterns.** A literal (`0`), a variable, `_`, a tuple `(a, b)`, a list `[x, y]`,
  a cons `x:xs`, the empty list `[]`, a record `point(x, y)`, an alternation `0 | 1`, a named
  pattern `p@(a, b)`, and a typed pattern `x::int`.
- **Guards** are written `| condition = body`, one per line, with `otherwise` for the last.
- **`where`** introduces local definitions — values and functions, mutually recursive — visible to
  the guards and the body.
- **`def` may open a block of clauses for several functions**, which is how mutually recursive
  helpers are written at the top level:

  ```
  def
    foldl( f, z, [] )   = z
    foldl( f, z, x:xs ) = foldl( f, f(z, x), xs )

    sum( l ) = foldl( (+), 0, l )
  ```

- **No clause matching is an error**, not a failure: `argument match not found` names the function.

**Pattern matching here is ONE-WAY.** A parameter pattern inspects the argument and binds the
pattern's own variables; it never changes the argument. The [logic chapter](logic.md) is where
two-way matching comes in, and it comes in only for relations.

### Functions are generators when their body is

**A function whose result expression generates is itself a generator.** `yield e` produces `e`'s
value to the caller and leaves the function suspended where it is; asking for another value resumes
it after the `yield`:

```
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

every write( permute([1, 2, 3, 4]) )        ;; all 24 permutations, Heap's order
```

`permute` generates because its body `permute_(…)` does: **a body expression passes every value it
produces through to the caller.** The old implementation emits nothing to bound it.

> **Decided — is `return e` bounded?**
> The old compiler unmarks the enclosing statements and then evaluates `e` *unbounded*, so
> `return !xs` generates every element. **Decision: `return e` produces only `e`'s first
> value** (Icon's rule, and what a reader of `return` expects), while a body written `= e` or ending
> in an expression keeps passing every value through, and `yield` is how a block body generates
> explicitly. **Rejected:** keep `return` unbounded, so `return` means "leave the enclosing
> statements" and nothing more.

### Partial function literals

An indented block of lambdas is a partial function — the clause idea without a name:

```
val classify =
  0 -> 'zero'
  n | n < 0 -> 'negative'
  _ -> 'positive'
```

**Fixed in the rewrite:** the old compiler parses this and emits *no code at all* for it, so the
value on the stack is whatever was there before. The rewrite compiles it exactly as a multi-clause
anonymous function with committed choice.

## Data

| kind | literal or constructor |
|---|---|
| numbers | `123`, `1.5`, `2n` is `2 * n` (juxtaposition multiplies) |
| strings | `'abc'` or `"abc"`; `$name` and `${expr}` interpolate, `$$` is a dollar |
| regex | `` `a(b|c)*` `` |
| booleans, nothing | `true`, `false`, `null`, `undefined`, `()` |
| tuple | `(1, 2)` |
| list | `[1, 2, 3]`, `x:xs`, comprehensions `[x^2 \| x <- 1..10 if odd(x)]` |
| set, map | `{1, 2}`, `{a: 1, "b": 2}`, set comprehension `{x \ 2 \| x <- 1..5}` |
| mutable | `array(n)`, `buffer()`, `map(m)`, `set(s)` |
| records | `data point(x, y)`; `data shape = circle(r) \| square(s)` |

Elements are reached by call syntax and by field syntax: `r.a`, `r("b")`, `r(1)`, `m.a`, `m("a")`,
`[3, 4, 5](1)`. A field write on a mutable map adds the key.

**`undefined` is the value of a declared variable that was never assigned**, and two prefix
operators test it: `\x` succeeds if `x` is defined and `/x` if it is undefined. Both are
assignable, so `\a = 123` assigns only if `a` already has a value and `/b = 123` only if `b` has
none.

**Fixed in the rewrite:** records and functions report a class — the old value model has
`clas = null` on records and on function references, so `x is` against either answers nothing
useful. Every value has a type the program can test.

### Numbers

**The numeric tower is exact until a program asks for inexactness**: integers grow without bound,
dividing two integers that do not divide evenly gives a rational, and a real or a decimal appears
only when a literal or an operation introduces one.

```
write( 7 / 2 )       ;; 7/2
write( 7 \ 2 )       ;; 3     -- integer division
write( 2 ^ 100 )     ;; 1267650600228229401496703205376
write( 1 / 3 + 1/6 ) ;; 1/2
```

`div` asks "divides": `3 div n` succeeds when 3 divides `n`. `mod`, `%` and `//` are the remainder
and floor forms; the machine chapter has the [tower and its promotion rules](vm.md#numbers).

**An exact result is demoted to the smallest exact kind that holds it** — a rational whose
denominator is 1 is an integer — **and an inexact result is never demoted**: `2.5 * 2` is the real
`5.0`, not the integer `5`. The old arithmetic library carried a demotion of whole-valued doubles
to integers; the rewrite has no such path.

## Assignment, and assignment that undoes itself

`=` assigns, `+=` and the other compound forms update, and several targets take several values at
once, `a, b = b, a`. A name assigned at the top level without `val` or `var` is declared by the
assignment.

**`x <- e` is reversible assignment**: it assigns, and if the expression it is part of is later
backtracked into, the old value comes back. It is what lets the n-queens placement above undo
itself when a later column fails.

**Fixed in the rewrite:** `x in c` and `x not in c` parse and do nothing — the old machine's case
for them is empty, so the stack is left one value short. They are membership tests, and in keeping
with the rest of the language they succeed with `x` or fail.

## String scanning

**`s ? e` evaluates `e` with `s` as the subject and a position at its start**, and a family of
builtins read and move that position. Every movement is reversible: backtracking past a `tab` or a
`move` puts the position back.

| builtin | does |
|---|---|
| `tab(i)` | moves to position `i`, produces the text passed over |
| `move(n)` | moves `n` characters, produces the text passed over |
| `pos(i)` | succeeds if the position is `i` |
| `upto(c)` | generates each position at which a character of cset `c` occurs |
| `many(c)` | the position after the longest run of characters in `c` |
| `any(c)` | the position after one character in `c` |
| `match(s)` | the position after `s` if the subject continues with it |
| `find(s)` | generates each position at which `s` occurs |

```
text ?
  while write(tab(upto(' ')))
    tab(many(' '))

  write(tab(0))
```

Alternation backtracks the position, which is the whole point:

```
"asdf," ? tab(upto(',') + 1) & write(move(1)) | write(tab(upto('.')))
```

prints nothing for `"asdf,"` (there is nothing after the comma and no `.`), `z` for `"asdf,zxvc."`,
and `asdf` for `"asdf."`. `line ?= e` scans `line` and assigns the result back to it.

### Patterns inside scanning

**A regex is a matcher inside a scan**, and it backtracks with everything else. Patterns are written
as regex literals or built from combinators:

```
'123cc' ? write(rep(ccls(digits))) & write(rep(string('c')))      ;; 123, then cc

if 'aaa12cc' ? tab(many('a')) & write(repn(2, ccls(digits))) & write(rep1(string('c')))
  write('match')
```

`string`, `ccls`, `rep`, `rep1`, `repn`, `opt` and their reluctant forms are compile-time
constructors of the same patterns a regex literal produces; the [machine
chapter](vm.md#regex-compiled-into-the-same-machine) shows that both become ordinary instructions.

## Rough edges, and what the rewrite does about each

| old behaviour | in the rewrite |
|---|---|
| `if false` takes the `then` branch | `false` fails a condition (decided above) |
| `in` / `not in` do nothing and unbalance the stack | membership, succeeding with the left operand |
| a partial function literal compiles to nothing | a committed-choice anonymous function |
| records and functions have no class | every value has a type |
| system variables (`$name`) answer raw host values | they answer FunL values |
| `::` (a typed pattern) is used by the grammar but is not a token | a token |
| `return e` generates | bounded: the first value only (decided above) |
