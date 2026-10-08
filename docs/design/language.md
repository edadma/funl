---
title: The language
weight: 10
---

# The language

This chapter says what FunL is: its expressions, functions, data, scanning, regex and values. It
says what a reader needs in order to follow the machine chapter, and it marks every place a rule
was chosen to close a rough edge.

## Every expression succeeds or fails

**The central rule: evaluating an expression either produces a value or fails, and failure is not
an error.** It is the ordinary way to say "no". A comparison that does not hold fails; a lookup that
finds nothing fails; `fail` fails on purpose. The one lookup that does not is a mutable map made
with a default, `map(c, x)` (§ "Data"), which answers `x` for a key it does not hold.

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

**FunL has the values `true` and `false`, but a condition is decided by success and failure.** If
only failure failed, `if false then 'a' else 'b'` would answer `'a'`, because `false` is a value and
a value is success.

> **Decided — does `false` fail in a condition?**
> **Decision: yes.** A condition — the test of `if`, `elif`, `while`, a guard, an operand of
> `and`, `or` and `not`, a comprehension filter — fails when its expression fails *or* produces
> `false`. Everywhere else `false` is an ordinary value. This keeps goal-directed evaluation intact
> (a comparison still fails rather than answering `false`) while making `if done then …` mean what
> every reader expects. **Rejected:** a condition that only fails on failure,
> which is Icon's, and costs a `== true` wherever a boolean is tested; or make every operation that
> could produce `false` fail instead, which removes booleans as values: `val flag = false` could not store `false`, and `write(flag)` could not print it.

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

> **Decided (2026-10-08) — what does the binding `x <- e` of a `for` or comprehension draw?**
> **Decision: every value of `e`; each value that is a collection is iterated, and any other value
> is drawn as it is.** So `[x | x <- g()]` collects everything the generating function `g` produces,
> `x <- [1, 2]` and `x <- 1..2` still give the elements, and `x <- [[1, 2], [3]]` still gives the two
> lists. The rule is one step, per value, so a generator of collections is flattened:
> `x <- ([1, 2] | [3])` gives 1, 2, 3, and a generator of strings gives their characters. Keeping
> each value whole is `x <- [e]`, whose values are lists of one. An unbound variable is still
> refused. **Rejected:** choosing by whether `e` generates (it cannot be told from one value: a
> generator's last value leaves no choice point, and an ordinary call may leave one), and refusing a
> non-collection value (which was the old behaviour and made a generating function unusable in a
> comprehension). This makes an iterator value unnecessary for drawing from a generator in a
> `for` or comprehension: `x <- g()` does it.

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
name; `break` may carry a value, `break (e)`. **A loop fails when it runs out** -- its condition
fails, its generators are spent -- **and a `break` makes it succeed**, with the value or `()`, so
`val cr = for d <- 1.. do if d*d >= n then break (d)` is the first such `d`. `break` and `continue`
reach only the loops of the function they are written in.

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
  pattern `p@(a, b)`, and a typed pattern `x::integer`, which names its type as `is` does.
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
produces through to the caller.** Nothing bounds it.

A `yield` resumed carries on with `()` as its own value — except where it is the last thing the
body does, when the function has no more to produce and fails, as Icon's procedure does on falling
off its end. So `def g()` with the two lines `yield 1` and `yield 2` produces exactly `1` and `2`,
and no third value `()`.

Inside parentheses, in a call's arguments and at the head of `every`, `name = e` is an assignment
expression: it stores each value of `e` in `name` and produces it, so `every write( (k = 1 to 3) to
k + 2 )` binds `k` once per value of the outer range.

> **Decided — is `return e` bounded?**
> A `return` could unmark the enclosing statements and then evaluate `e` *unbounded*, so
> `return !xs` would generate every element. **Decision: `return e` produces only `e`'s first
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

**Closed by design:** it compiles exactly as a multi-clause anonymous function with committed
choice, and never to nothing.

## Data

| kind | literal or constructor |
|---|---|
| numbers | `123`, `1.5`, `2n` is `2 * n` (juxtaposition multiplies) |
| strings | `'abc'` or `"abc"`; `$name` and `${expr}` interpolate, `$$` is a dollar |
| regex | `` `a(b|c)*` `` |
| booleans, nothing | `true`, `false`, `undefined`, `()` |
| tuple | `(1, 2)` |
| list | `[1, 2, 3]`, `x:xs`, comprehensions `[x^2 \| x <- 1..10 if odd(x)]` |
| set, map | `{1, 2}`, `{a: 1, "b": 2}`, set comprehension `{x \ 2 \| x <- 1..5}` |
| mutable | `array(n)`, `buffer()`, `map(m)`, `map(m, default)`, `set(s)` |
| records | `data point(x, y)`; `data shape = circle(r) \| square(s)` |

> **Decided — a compound term needs a `data` declaration (user, 2026-10-08).** Outside a relation
> head, `point(1, 2)` builds a record only when `data point(x, y)` is declared; an undeclared
> functor is "not defined", as any other unknown name is.

Elements are reached by call syntax and by field syntax: `r.a`, `r("b")`, `r(1)`, `m.a`, `m("a")`,
`[3, 4, 5](1)`. A field write on a mutable map adds the key.

**A mutable map can have a default, as Icon's `table(x)` does**: `map(c, x)` is `map(c)` whose
missing keys read as `x` instead of failing. So counting needs no membership test:

```
val counts = map( {}, 0 )
every counts( !split(text) ) += 1    ;; each word of the string text; a new word reads 0, and the assignment adds it
write( counts("zzz") )               ;; 0, and "zzz" is still not a key
```

- **Reading a missing key gives the default and adds nothing**: `m(k)`, `m[k]` and `m.k` alike.
  `m.length` is still the size, since a collection's `length` is answered before the default.
- **Assigning adds the key**, and an update such as `+=` or `-=` is a read and an assignment, so it
  starts from the default and adds the key.
- **The default is not an entry**: `k in m`, `m.length`, `for (k, v) <- m` and `!m` see only the
  keys that were assigned.
- **The default is one value, never copied**, Icon's behaviour and Icon's trap: with
  `map({}, buffer())` every missing key reads the *same* buffer, and `m(k) += x` appends to that
  shared buffer in place without adding `k`. A collection per key is made by assigning one.
- **`map(m)` copies entries, not a default**, and a map made with `map()` or `map(c)` fails on a
  missing key exactly as an immutable map does.
- `map` with three or more arguments is an `existence_error` for `map/3`, and a first argument that
  is not a collection is a type error, as for `map(c)`.

> **Decided — do immutable map literals carry a default?**
> **Decision: no.** `{…}` has no spelling for one, as Icon has none for a table literal, and a lookup
> in an immutable map always fails on a missing key. A default belongs to a map that is being
> filled, which is a mutable one; `map({a: 1}, 0)` is the way to get both.

**`undefined` is the value of a declared variable that was never assigned**, and two prefix
operators test it: `\x` succeeds if `x` is defined and `/x` if it is undefined. Both are
assignable, so `\a = 123` assigns only if `a` already has a value and `/b = 123` only if `b` has
none.

> **Decided — may two `data` constructors share a name and a number of fields?**
> **Decision: no; it is a compile-time error at the second declaration**, naming the first one's
> position, wherever the two are written (a nested block, or a FunL file Prolog imports). A record is
> known by its functor and number of fields, so two such constructors could not be told apart in a
> term Prolog made. The same name with a different number of fields is a different functor and is allowed.

### Types, and `is`

**Every value has a type, and `x is t` tests it.** Like a comparison, it **succeeds producing `x`**
when `x`'s value is of type `t` and **fails** otherwise, so it is a condition, a guard and a filter
without a boolean in between:

```
if x is string then write( x ) else write( "not text" )
def kind( x ) | x is number = "number"
def kind( _ ) = "something else"
every write( (1 | 'a' | 2.5 | #b) is number )     ;; 1, then 2.5
[x | x <- items if x is record]
```

- **`t` is a name, never an expression**, resolved when the program is compiled. A name that is not a
  type is an error there: ``error: `integr` is not a type``, and `x is 4` is a syntax error.
- **`is` binds as a comparison does**: on the level of `==`, `<` and `in`, left-associative with
  them, tighter than `|`, `and` and `or`. So `a < b is integer` tests the comparison's value (`b`),
  and `x is integer and y is string` needs no parentheses.
- **The name is looked up in this order**: a `data` type the program declares, then a constructor
  the name means where it is written, then the built-in names below. A program's own `data list = …`
  therefore hides the built-in `list` from `is`, and nothing else.
- **A `data` type is tested by its name, and each constructor by its own**: with
  `data shape = circle(r) | square(s) | blank`, `circle(2) is shape` and `circle(2) is circle` both
  succeed, `square(1) is circle` fails, and `blank is shape` succeeds, a constructor with no fields
  being its atom. A record's class is its constructor, so `data point(x, y)` makes `point(1, 2) is
  point` succeed. A record is tested by its functor and number of fields, so a term of the same shape
  that unification or Prolog made is one too.
- **A bound logic variable is tested by its value**; only an unbound one is a `variable`.

> **Decided — what `is` produces (user, 2026-10-08).** `x is t` is a test that produces `x` or fails,
> like a comparison; it never produces `true` or `false`.

> **Decided — `var` is a keyword (user, 2026-10-08).** The type test for an unbound logic variable is
> `x is variable`, and `not (x is variable)` is its negation; there is no `var(x)` or `nonvar(x)`.

| name | the values of that type |
|---|---|
| `number` | every number: `integer`, `rational` and `real` |
| `integer` | an integer of any size, `3`, `10^30` |
| `rational` | an exact fraction that is not an integer, `1/3` |
| `real` | an inexact number, `1.5` |
| `string` | `'abc'` |
| `atom` | `#name`, and a constructor with no fields |
| `boolean` | `true`, `false` |
| `list` | `[]`, a list cell `x:xs`, and a range, which reads as a list wherever one is |
| `range` | `1..10`, `1..<n`, `1..` |
| `tuple` | `(1, 2)` |
| `unit` | `()` |
| `map` | `{a: 1}`, and a mutable map, `map()` |
| `set` | `{1, 2}`, and a mutable set, `set(s)` |
| `array` | `array(n)` |
| `buffer` | `buffer()` |
| `cset` | `cset('aeiou')`, `letters` |
| `function` | a function or a lambda, and a constructor with fields, which makes a record when called |
| `record` | a record, and any other compound term |
| `undefined` | `undefined` |
| `variable` | an unbound logic variable |

The names are the ones FunL's messages already use for those values (``'div' … was given the
rational 1/3``, `a function`), and the ones of the builtins that make the mutable kinds.

### Numbers

**The numeric tower is exact until a program asks for inexactness**: integers grow without bound,
dividing two integers that do not divide evenly gives a rational, and a real or a decimal appears
only when a literal or an operation introduces one.

> **Decided — a real literal needs a digit before the point (user, 2026-10-08).** `0.5` is a real;
> `.5` is a syntax error, because `.` is field access.

```
write( 7 / 2 )       ;; 7/2
write( 7 \ 2 )       ;; 3     -- integer division
write( -7 \ 2 )      ;; -4    -- it floors
write( 2 ^ 100 )     ;; 1267650600228229401496703205376
write( 1 / 3 + 1/6 ) ;; 1/2
```

**`\` floors: the quotient is rounded toward negative infinity, never truncated toward zero**, so
`-7 \ 2` is `-4` and `7 \ -2` is `-4`. `//` is the same floor division. `mod` is the remainder that
goes with it, with the sign of the divisor (`-7 mod 2` is `1`, so `a == b * (a \ b) + a mod b`), and
`%` is the remainder of truncating division, with the sign of the dividend (`-7 % 2` is `-1`).

`div` asks "divides": `3 div n` succeeds when 3 divides `n`. The machine chapter has the [tower and its promotion rules](vm.md#numbers).

**An exact result is demoted to the smallest exact kind that holds it** — a rational whose
denominator is 1 is an integer — **and an inexact result is never demoted**: `2.5 * 2` is the real
`5.0`, not the integer `5`. Whole-valued doubles are never demoted
to integers.

> **Decided — what is `^` with a power that is not an integer?** **A real.** An exact base to an
> integer power stays exact (`2 ^ -2` is `1/4`); a real or fractional power is C's `pow` on reals
> (`2 ^ 0.5` is `1.4142135623730951`, `4 ^ (1/2)` is `2.0`), a negative base to one has no value and
> faults as every non-finite real does, and zero to any negative power divides by zero.

## Assignment, and assignment that undoes itself

`=` assigns, `+=` and the other compound forms update, and several targets take several values at
once, `a, b = b, a`. A name assigned at the top level without `val` or `var` is declared by the
assignment.

**Every block has names of its own**: an indented block, a sequence `(s; s; e)`, and a `for`'s
bindings. `val`, `var`, `free` and a pattern declare in the innermost block, in scope from the end
of the declaration to the end of the block, shadowing the name outside it. `x = e` stores into the nearest `x` in scope -- this block's, an enclosing
block's, or an enclosing function's -- and declares `x` in the innermost block only when there is
none, which is how a loop assigns the counter declared above it. `x op= e`,
`x++` and `x--` change a variable that must already be in scope, and a name declared by `val` or
bound by `where` cannot be assigned at all. **A name read after its block has ended reads
`undefined`**; a name that
no block has declared before the read is refused.

**`x <- e` is reversible assignment**: it assigns, and if the expression it is part of is later
backtracked into, the previous value comes back. It is what lets the n-queens placement above undo
itself when a later column fails.

**`x in c` and `x not in c` are membership tests**, and in keeping with the rest of the language
they succeed with `x` or fail.

## String scanning

**`s ? e` evaluates `e` with `s` as the subject and a position at its start**, and a family of
builtins read and move that position. Every movement is reversible: backtracking past a `tab` or a
`move` puts the position back, and a bounded expression that completes keeps it. However control
leaves `s ? e` — its value, failure, `return`, `break`, `continue` or `yield` — the subject and
position around the scan are in force again.

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

### Ordinary string functions

Scanning takes a string apart; the everyday transformations are plain functions beside it:
`split(s)` (the words), `split(s, sep)` (at a string, or at the characters of a cset), `join(c, sep)`,
`trim`, `trim_start`, `trim_end`, `upper`, `lower` and `replace(s, old, new)`. The tests
`starts_with(s, p)`, `ends_with(s, p)` and `contains(s, p)` produce `s` or fail, as `odd` does, so
they serve as conditions and as filters. Where a string stands in another is scanning's `find(p, s)`,
a generator of positions, and is not duplicated. Case is Unicode's simple mapping, one character for
one.

## Source files, and literate FunL

A FunL source file is `.funl`. **A file named `.lfunl` is literate FunL**: a Markdown document whose
lines indented four columns are the program, under exactly sysl's `.lsysl` rules — consecutive
indented blocks are one block whatever prose sits between them, a fenced block (` ``` ` or `~~~`) is
an illustration and never runs, an indented block under a list item is prose, and a tab in the
indentation or a fence never closed is refused. The name decides and nothing else. The prose is
blanked rather than removed (`sh.sysl.parsing`'s `tangle_literate`), so every diagnostic names the
line and column of the document the reader has open. `funl program.lfunl` and a Prolog file's
`:- import("file.lfunl")` both read it.

## Rough edges, and what FunL does about each

| rough edge | what FunL does |
|---|---|
| `if false` takes the `then` branch | `false` fails a condition (decided above) |
| `in` / `not in` do nothing and unbalance the stack | membership, succeeding with the left operand |
| a partial function literal compiles to nothing | a committed-choice anonymous function |
| records and functions have no class | every value has a type, which `x is t` tests |
| system variables (`$name`) answer raw host values | they answer FunL values |
| `::` (a typed pattern) is used by the grammar but is not a token | a token |
| `return e` generates | bounded: the first value only (decided above) |
