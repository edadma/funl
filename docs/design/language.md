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

**A fault is not a failure**, and no bounded context stops one: it unwinds until a `catch` takes it,
and otherwise ends the program.

> **Decided (user, 2026-10-08; [modules](modules.md#questions-decided) question 4) — how does FunL catch
> a fault?** **Decision: slate's postfix form, `e catch p -> r`, with an optional guard
> `e catch p | g -> r`.** A fault raised while `e` is evaluated — or while `e` is resumed for another
> value — is the ISO error term Prolog's `catch/3` sees; it is matched against the pattern `p`, `g` is
> tried, and `r`'s values stand in for `e`'s. A fault `p` or `g` refuses is raised again, unchanged.
> Failure is not a fault and passes through: `e catch …` fails when `e` does. `catch` binds looser
> than every operator but the lambda arrow, and the recovery reaches as far right as a lambda's body.
> `err.message` reads an error term's message; `throw(x)` raises `x` itself, as `throw/1` does. It is
> compiled over the control-stack entry `catch/3` uses, so it is transparent to backtracking exactly
> as `catch/3` is. **Rejected:** a block `try`/`catch` (one clause is what an expression has room
> for, and a block is an expression already) and `finally` (slate has none).

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
> `x <- ([1, 2] | [3])` gives 1, 2, 3. Keeping each value whole is `x <- [e]`, whose values are
> lists of one. An unbound variable is still refused.
>
> **Decided (2026-10-08): a string and a tuple are not collections to `<-`; each is drawn whole.**
> So `x <- "abc"` gives `"abc"` once, a generator of strings gives the strings (`for line <-
> lines(p)` gives lines, not characters), and `x <- (1, 2)` gives the tuple. Drawing a string's
> characters or a tuple's elements is explicit: `x <- !s`, since `!` still generates them. **Rejected:**
> refusing them, since `<-` draws every other non-collection value as it is. **Rejected:** choosing by whether `e` generates (it cannot be told from one value: a
> generator's last value leaves no choice point, and an ordinary call may leave one), and refusing a
> non-collection value (which was the old behaviour and made a generating function unusable in a
> comprehension). This makes an iterator value unnecessary for drawing from a generator in a
> `for` or comprehension: `x <- g()` does it.
>
> **Decided (2026-10-08): a map is drawn whole by `<-`, as a string and a tuple are.** So
> `for row <- db.query(sql)` gives each row (a row is a map), and a generator of maps gives the
> maps. Iterating a map's entries is explicit: `x <- !m`, since `!` still generates them as pairs
> `(key, value)`, so `for (k, v) <- !m`. This holds for a mutable map too. **Rejected:** keeping
> maps iterated, which made every generator of records (rows, parsed JSON objects) need
> `x <- [g()]`.

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

### Taking drawn values apart with a pattern

> **Open question P3, revisited (2026-10-08) — what does a pattern on the left of `<-` see?** The
> question was raised when `<-` iterated every collection it drew, tuples included: `[a | (a, b) <-
> pairs()]` over a generator yielding `(1, "a")` drew `1` and `"a"`, matched neither, and answered
> `[]`. The three Decided blocks above have since made a tuple, a string and a map whole to `<-`,
> and `zip` has become a native that answers a list of tuples. This section re-asks P3 in that light.

**The problem, as first stated, is gone.** Run today (dev `63fa7ac`), with
`def pairs() = (1, "a") | (2, "b")` and `val m = {x: 1, y: 2}`:

| program | prints |
|---|---|
| `write( zip([1, 2, 3], ["a", "b"]) )` | `[(1, "a"), (2, "b")]` |
| `write( zip3([1, 2, 3], ["a", "b"], [true, false]) )` | `[(1, "a", true), (2, "b", false)]` |
| `write( [b \| (a, b) <- zip([1, 2, 3], ["a", "b"])] )` | `["a", "b"]` |
| `for (a, b) <- pairs() do write( b )` | `a` then `b` |
| `write( [a \| (a, b) <- pairs()] )` | `[1, 2]` |
| `write( [p \| p <- pairs()] )` | `[(1, "a"), (2, "b")]` — no longer flattened |
| `write( [a \| (a, b) <- (1, "a")] )` | `[1]` — one tuple, drawn whole |
| `for (k, v) <- !m do write( k, v )` | `x, 1` then `y, 2` (a mutable map alike) |
| `write( [p \| p <- !m] )` | `[("x", 1), ("y", 2)]` |
| `write( [0 \| () <- [(), (1, 2), ()]] )` | `[0, 0]` — `()` is the empty tuple |

So `(a, b) <- zip(xs, ys)` and `(k, v) <- !m` both work, because each drawn value is one tuple
and the pattern meets it whole. Nothing in the language needs to change for the case P3 named.

**What the rest of the pattern language does on the left of `<-` today:**

| program | prints | reading |
|---|---|---|
| `[a \| (a, b) <- [(1, "a"), 5, (2, "b", 3), (3, "c")]]` | `[1, 3]` | a value the pattern does not match is passed over |
| `[a \| (a, b) <- [[1, 2], [3, 4]]]` | `[]` | a tuple pattern does not match a list |
| `[y \| [x, y] <- [(1, 2)]]` | `[]` | nor a list pattern a tuple |
| `[x \| [x, y] <- [[1, 2], [3, 4]]]` | `[1, 3]` | a list pattern over a list of lists |
| `[x \| x:_ <- [[1, 2], [3, 4]]]` | `[1, 3]` | a cons pattern likewise |
| `[a + c \| ((a, b), c) <- [((1, 2), 3), ((4, 5), 6)]]` | `[4, 10]` | nested patterns |
| `[p \| p@(a, _) <- [(1, 2), (3, 4)]]` | `[(1, 2), (3, 4)]` | a named pattern |
| `[x \| (x, 0 \| 1) <- [(1, 0), (2, 5), (3, 1)]]` | `[1, 3]` | an alternation inside a tuple |
| `[x + y \| point(x, y) <- [point(1, 2), 7, point(3, 4)]]` | `[3, 7]` | a record pattern (`data point(x, y)`) |
| `[x + y \| point(x, y) <- pts()]`, `pts()` yielding records | `[3, 7]` | a record is not a collection, so drawn whole |
| `[r \| circle(r) <- [circle(1), square(2), circle(3)]]` | `[1, 3]` | selecting one constructor of a `data` type |
| `[n \| (n, _) <- ()]` | `[]` | `()` produces nothing |

**The one shape that still does not reach its values is a list or cons pattern over a generator of
lists**, because a drawn list is a collection and is iterated before the pattern sees it. With
`def lists() = [1, 2] | [3, 4]`:

| program | prints |
|---|---|
| `[x \| [x, y] <- lists()]` | `[]` — the pattern meets `1`, `2`, `3`, `4` |
| `[x \| x:_ <- lists()]` | `[]` |
| `[x \| x <- lists()]` | `[1, 2, 3, 4]` — the Decided flattening |
| `[x \| [x, y] <- [lists()]]` | `[1, 3]` — `[e]` keeps each list whole |
| `[x \| [x, y] <- ![[1, 2], [3, 4]]]` | `[]` — `!` generates the lists, `<-` then iterates each |

This is the Decided rule working as written ("a generator of collections is flattened; keeping each
value whole is `x <- [e]`"), not a defect, and no prelude function generates lists.

**How the other binding places treat the same patterns, today:**

| place | a value the pattern does not match | example, and what it prints |
|---|---|---|
| `x <- e` in a comprehension or `for` | passed over; the next value is drawn | `[a \| (a, b) <- [(1, 2), 5]]` → `[1]` |
| a function parameter | a fault, since clauses are committed choice | `def f( (a, b) ) = a` then `f([1, 2])` → `argument match not found: no clause of 'f' matches ([1, 2])` |
| `val (c, d) = e` | the statement fails and the names stay unassigned | `val (c, d) = [3, 4]` then `write( c + d )` → `'+' wants numbers and was given undefined` |
| `every` | takes no pattern: `(a, b) = e` is a statement, not an expression | `every (a, b) = !zip(...)` → `this cannot be matched against anything` (read as a pattern `every(a, b)`); `every x = !zip(...) do write( x(0) )` works |

The `<-` reading is the goal-directed one: a mismatch is a failure, and a failure inside a `for`
head or a comprehension backtracks into the generator for its next value, exactly as a failing
`if` filter does. A function's clauses fault because a call that no clause accepts has nowhere left
to go. The two rules are consistent with each other; they differ where the failure lands.

#### Options

**A — close P3: the current rule is the rule.** A pattern on the left of `<-` meets each drawn
value; a tuple, string, map or record arrives whole, a list, array, set or range is iterated first,
and a value the pattern does not match is passed over. A generator of lists is taken apart through
`[e]`. *Cost:* documentation only — this section becomes a Decided block, the `modules.md` P3 block
is replaced by a pointer here, and the comprehensions reference page shows the tables above as
runnable blocks. *Binds:* nothing that runs today changes.

**B — a pattern's shape decides whether a drawn collection is iterated.** A list or cons pattern
would meet a drawn list whole, so `[x | [x, y] <- lists()]` is `[1, 3]`. *Breaks:* the same rule
applies to the one value `[[1, 2], [3, 4]]`, which is a list whose shape is `[x, y]`, so
`[x | [x, y] <- [[1, 2], [3, 4]]]` would become `[[1, 2]]` instead of `[1, 3]`. One value cannot
say whether it came from a generator (the Decided block's own reason for rejecting that test), so
the only way out is "try whole, then iterate", which answers both — two different results from one
pattern over one value. *Cost:* the `<-` drawing code in the compiler and its tests; every list
comprehension in the prelude source (`concat`, `concatMap`, `unzip`) re-read under the new rule.
Rejected in the recommendation for that ambiguity.

**C — a value the pattern does not match is a fault, as a parameter's is.** `[a | (a, b) <- [(1,
2), 5]]` would fault on `5`. *Breaks:* selecting by constructor (`circle(r) <- shapes`) and by
literal (`(x, 0) <- pairs`), both of which read naturally and both of which Haskell's list
comprehensions also treat as a filter; and it makes a failure that FunL handles everywhere else by
backtracking into a fault in this one place. *Cost:* small (one branch in the drawing code), plus a
`|` filter in every program that relies on selection.

**Recommendation: A.** Making tuples, strings and maps whole has already answered P3: zipped lists,
map entries and generators of tuples all destructure, nested, with constructors and alternations.
What remains — a list pattern over a generator of lists — is the Decided flattening rule doing what
it says, and `[e]` is its documented escape; B cannot change it without making one pattern mean two
things over one value. The mismatch rule stays a filter because a mismatch is a failure and `<-`
is where failure means "try the next value".

#### Open questions

1. **Is P3 closed by the whole-drawing decisions, with the rule as stated in option A?**
   *Recommended:* yes.
2. **A drawn value the pattern does not match — passed over (today) or a fault?** *Recommended:*
   passed over, as a filter, matching a failing `if` in the same head.
3. **A list or cons pattern over a generator of lists — `[e]` (today) or shape-directed drawing?**
   *Recommended:* `[e]`; no prelude function generates lists, so it arises only in a program's own
   generator, which can as easily `yield` a tuple.
4. **`zip` beside an endless range.** Today `zip([1, 2], 0..)` faults with `'zip' was given the
   endless range 0.., which has no last element`, as `docs/library/prelude.md` shows. The natives
   pair as many places as the shortest input has, so they can stop at a finite input's end and need
   never reach an endless range's last element. *Recommended:* accept an endless range when at least
   one other input is finite, so `zip(0.., xs)` numbers a list (`[(0, "a"), (1, "b")]`); fault only
   when every input is endless. *Cost:* the zip family's natives in `prelude.sysl` (which convert each input through
   `seq.sysl`, where the refusal is raised) and their tests, and
   the prelude page's refusal example.
5. **`zip` given a string, tuple or map.** Today the natives draw their inputs as `<-` does, so
   `zip("ab", [1, 2, 3])` is `[("ab", 1)]`, `zip({a: 1}, [1])` is `[({"a": 1}, 1)]` and
   `zip((1, 2), [3, 4])` is `[((1, 2), 3)]`, while `reverse("abc")` faults with `'reverse' wants a
   list and reached the string 'abc'`; the source-carried ones follow `<-` too (`take(2, "abc")` is
   `["abc"]`, `map(c -> c + c, "ab")` is `["abab"]`). *Recommended:* the prelude's list functions
   refuse a string, tuple or map as `reverse` does, naming the explicit form in the message
   (`[c | c <- !s]`), since a one-element answer is never what such a call meant.
6. **Should `every` take a pattern, `every (a, b) = !zip(xs, ys) do …`?** Today it cannot: a pattern
   assignment is a statement, and `every (a, b) = …` reads as the term `every(a, b)`. *Recommended:*
   no — `for (a, b) <- e do …` is the destructuring form, and `every` keeps an expression.

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
fails, its generators are spent -- **and a `break` makes it succeed**, with the value or `()`
(except a bare `break` out of the loop that ends a `yield`ing function, decided below), so
`val cr = for d <- 1.. do if d*d >= n then break (d)` is the first such `d`. `break` and `continue`
reach only the loops of the function they are written in.

**`every` failing at the end is Icon's rule and FunL keeps it**: `every` exists for its side effects,
and a statement that fails is harmless.

### Blocks, and optional `end` markers

*Decided (user, 2026-10-09) and built: Option A, every question answered as recommended — see the
Decided block at the end of this section. The reference is
[functions](../reference/functions.md#end-markers).*

**The problem.** A FunL block ends where the indentation goes back, and that is all that marks it.
Short blocks need nothing more. A long one does: once a body runs past a screen, or holds blank
lines, or ends four levels deep, a reader at the dedent cannot see which construct just closed:

```
def solve( board, col )
  if col > 8
    write( board )
  else
    for row <- 1..8 do
      if safe( board, row, col )
        place( board, row, col )

        ;; ... twenty more lines ...

        solve( board, col + 1 )
write( 'done' )
```

sysl, the language FunL is written in, has the answer in its *flexible* syntax, which comes from Scala
3: indentation stays the structure, and **an optional `end` marker naming what it closes** may follow
any long block. The compiler checks the name, so it cannot drift from the thing it claims to close:

```
def solve( board, col )
  ...
end solve
```

**What FunL did before markers.** There was no `end`. A line `end hanoi` under a function was refused, and so
is `end if` under an `if` and `end while` under a `while`:

```
def hanoi( n )
  if n > 0
    write( n )
    hanoi( n - 1 )
end hanoi
```
```
error: expected the end of the line
 --> p2.funl:5:5
  |
5 | end hanoi
  |     ^^^^^ found `hanoi`
```

The `if` and `while` forms print the same refusal, ending `` found `if` `` and `` found `while` ``.
**`end` is an ordinary name**, and nothing in the repository's FunL uses it (no example, no test
program, no reference page; the Prolog tests' `end` atoms are Prolog's). As a variable,
`val (start, end) = (1, 5)` then `write( end - start )` prints `4`; as a function, `def end( x ) = x + 1`
then `write( end(1) )` prints `2`. A line holding only `end` is a read of that name, so under a
function it says `` `end` is not defined ``. The `.pl` front end has its own lexer and never sees any of this.

**FunL's layout is already stricter than sysl's**, which matters for where a marker may stand. A line
indented to a column between two open levels is refused —
`` this line lines up with no block that is open ``, *indented to a column between two levels* —
where sysl accepts such a line as a dedent (sysl's own reference lists ragged dedent as an open hole).

#### What Scala 3 and sysl do

Scala 3's rules (its reference, *Optional Braces* § *End Markers*; read there, not run, as no Scala
compiler is on this machine):

- **`end` and one specifier token make up the whole line.** The specifier is an identifier or one of
  `if while for match try new this val given extension`.
- **The specifier must match the statement just before the marker**: a definition's own name
  (`end solve`); the statement's keyword for `if`, `while`, `for`, `match` and `try`; `val` after a
  pattern `val`; `given`, `extension` or `new` after an anonymous one, which has no name to give.
- **The marker sits at the indentation of the statement it closes.**
- **`end` is a soft keyword**: everywhere else it is an ordinary identifier.
- **Optional, and for long regions only.** The reference suggests one where a block holds blank lines,
  runs past about fifteen lines, or ends four or more levels deep.

sysl (its reference pages `declarations.md` § *end markers* and `statements.md` § *if*, and the
parser's `accept_end_marker` and `accept_end_name`) is that design, cut down in four places. Every
claim below is a run of the prerelease alpha.5 compiler:

| | Scala 3 | sysl |
|---|---|---|
| a definition | `end name` | `end name`, checked: `'end g' does not match 'f'` |
| control | `if while for match try` | `end if`, `end while`, `end for`, `end loop`; **no `end match`** (a function ending `end match` is refused `indent expected` on the next line) |
| a block-bodied `val` | `end x`, `end val` | **none**: `end x` after `val x =` and its block is refused `newline expected` |
| a wrong keyword | refused as a mismatch | **refused, but poorly**: `end for` under a `while` says only `newline expected`, pointing at `for` |
| alignment | at the statement's indentation | **not checked**: `end f` two columns in from `f`'s line is accepted, by the ragged dedent |
| after a one-line form | — | **accepted**: `f(n: int) -> int = n + 1` then `end f` compiles, and so does `end if` under a one-line `if … then …` |
| a bare `end` | an identifier | an identifier: `undefined name 'end'` |
| required anywhere | no | **yes, once**: a struct with no fields must say `end Name` |

sysl also has a style rule on top: **a block of six lines or fewer, head to marker, carries no
marker** (`~/dev/sysl-lang/CLAUDE.md`).

#### Which FunL constructs would take a marker

FunL's constructs, mapped by Scala 3's rule — a named construct takes its name, an unnamed one its
keyword — with sysl's answer beside each:

| FunL construct | the marker | Scala 3 | sysl |
|---|---|---|---|
| a `def` clause whose body is indented — a block, guard lines, a `where` block, a `:-` rule body | `end f`, after the last line of the clause (its `where` included) | `end f` | `end f` |
| the same under `async def`, `export def` | `end f` | `end f` | `end f` |
| a bare `def` opening a block of clauses for several names | `end def` | the keyword, as for an anonymous `given` | none |
| `val x =`, `var x =` with an indented value | `end x`; `end val` after a pattern | `end x`, `end val` | none |
| `if` … `elif` … `else` | `end if`, **once**, after the whole chain | `end if` | `end if` |
| `while`, `for`, `repeat`, `every` (labelled or not) | `end while`, `end for`, `end repeat`, `end every` | `end while`, `end for` | `end while`, `end for`, `end loop` |
| a lambda's block, a partial function, a `?` scanning block, a `catch` arm, a `where` block on its own, a `( ; )` sequence, `data` | **none** | none: no specifier names them | none |

A lambda, a scan and a `catch` arm are expressions that usually sit inside a call's brackets, where
a line of its own for the marker would split the bracket; Scala 3 gives a lambda no specifier for the
same reason. `data` has no indented body. A `where` block belongs to its clause, so `end f` closes both.

#### Options

**A — Scala 3's rules, as sysl intends them, with FunL's two strictnesses.** Every row of the table
above. `end` stays a name: a line is a marker only when it is `end` and one word and nothing else, so
`end( 2 )`, `end - 1` and `end = 3` keep their meaning, and no program FunL accepts today changes
meaning, since every `end word` line is refused today. Checked, with a diagnostic for each way to be
wrong:

```
def f( x )
  x + 1
end g
```
```
error: `end g` does not match `f`, the definition it closes
```

```
while i < 2
  i += 1
end for
```
```
error: `end for` does not match the `while` it closes
```

and a marker in a block that has nothing for it to close — under a one-line form, or indented into
the body it meant to close — is refused where it stands:

```
def f( x ) = x + 1
end f
```
```
error: `end f` closes nothing here: `f`'s body is on the `def` line
 = note: an end marker follows an indented body, at the column of the line that opened it
```

Alignment needs no rule of its own: FunL's layout already refuses a column between two levels, so a
marker is either at the opener's column (it closes), deeper (it is a stray line of the body), or
shallower (a stray line of an enclosing block) — and a stray marker is the refusal above.

- **Cost:** the lexer is untouched (`end` stays a `Name`). The parser gains one recogniser — `end`, a
  name or keyword, then the end of the line — and a check of it against what the construct expected,
  called after a clause's body (`parse_def.sysl`'s `clause` and `definition`, for the bare `def`
  group), after the outermost `if` of a chain (`parse_expr.sysl`'s `if_expr`, which recurses for
  `elif` and so must take the marker only at the top), after each loop (`parse_loop.sysl`), and after
  a block-bodied `val`/`var` (`parse.sysl`'s `statement`); `statement` refuses a marker no construct
  took. About 150 lines and ~25 tests: each marker accepted, each mismatch, a stray marker deeper and
  shallower, a marker after a one-line form, `end` still a variable, a function and a call, a `:-` rule
  body and a `where` closed by `end f`. The functions reference page gains a section; nothing in the
  compiler or the VM sees a marker.
- **Binds:** nothing. Prolog is untouched (its own lexer), `.lfunl` is untouched (a marker is a program
  line like any other), atoms `#end` are untouched.

**B — sysl's rules verbatim.** `end f`, `end if`, `end while`, `end for`, and `end repeat` in the
place of `end loop`; no marker for a `val`, no `end def`, no `end every`; a marker accepted after a
one-line form; a wrong keyword left to the generic refusal. The cost is a little less (no `val` case,
no one-line check, one diagnostic fewer). What it buys is that a sysl programmer meets nothing new;
what it costs is sysl's three weak spots carried into a language that has no reason for them —
`expected the end of the line` for `end for` under a `while`, and a marker on a one-liner that
claims a block exists where none does.

**C — a bare `end`, with no specifier**, as Ruby and Lua write it. Rejected: it checks nothing, which
is the whole value of a marker; it takes `end` away as a name (a lone `end` line is a variable read
today); and neither Scala 3 nor sysl has it.

> **Decided (user, 2026-10-09) — Option A: Scala 3's end markers, optional and checked.** It is
> Scala 3's design, which is what sysl's flexible syntax is modelled on, and it differs from sysl
> only where sysl is weaker than its own model: the marker after a `val` block and the keyword for
> every loop, a named mismatch for a wrong keyword, and no marker after a one-line form. FunL needs
> no required marker — it has no fieldless struct, and `data` declares no body.
>
> - **E1.** Markers are optional and checked: `end` and one word make up the whole line, and the
>   word must name the construct that ended at that line's column.
> - **E2.** The constructs are the table above: a `def` clause by its name (`async def`, `export def`,
>   a rule body and a `where` included), a bare `def` group by `end def`, a block-bodied `val x =` by
>   `end x` or `end val` (`end val` only, after a pattern), a block-bodied `var x =` by `end x`, an `if`
>   chain by `end if`, and the loops by `end while`, `end for`, `end repeat`, `end every`. A lambda,
>   a partial function, a `?` scan, a `catch` arm, a lone `where`, a `( ; )` sequence and `data` take
>   none.
> - **E3.** A labelled loop is closed by its word, `end for`, never by its label.
> - **E4.** A marker after a one-line form is refused: `` `end f` closes nothing here: `f`'s body is on
>   the `def` line ``. So is a marker indented into the body it meant to close, and one with nothing
>   ending before it (`` `end f` closes nothing here ``). A marker naming the wrong construct names both:
>   `` `end g` does not match `f`, the definition it closes ``, `` `end for` does not match the `while`
>   it closes ``.
> - **E5.** One `end if` closes a whole `if`/`elif`/`else` chain.
> - **E6.** A line holding only `end` is still a read of the name `end`; where nothing is called that,
>   `` `end` is not defined `` carries the note *an end marker names what it closes: `end f` after
>   `def f`, `end if` after an `if`*.
> - **E7.** sysl's style rule binds FunL's examples and reference pages: **a marker only on a block of
>   seven lines or more, head to marker inclusive**, so short code reads as it did before markers.
>   Blocks whose subject is the marker itself (the reference section on markers) are exempt.
>
> **How it is built** (`parse_end.sysl`): the lexer is untouched and `end` stays a `Name`. Each
> construct that may take a marker records, as it finishes, the token it finished before and whether
> it ended with an indented block; every loop that reads a block's lines takes a marker line after
> each line and judges it against the constructs that ended exactly there. A block ends at its
> `Dedent`, so a marker at the opener's column meets the opener, one indented deeper meets nothing,
> and one at an enclosing block's column meets that block's construct — no alignment rule of its own.

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

> **Decided — does a loop ending a generator produce a trailing `()`?**
> A loop fails when it runs out and a `break` makes it succeed with `()`, so a `yield`ing function
> whose last statement is `for i <- 1.. do if i > 3 then break else yield i` would produce `1 2 3`
> and then `()`. **Decision: a loop that is the last thing a `yield`ing function clause does ends
> on a bare `break` exactly as it does on running out — it fails, and the function produces no
> `()` after its yielded values** (the same rule as a final `yield`). It holds for every loop
> (`for`, `while`, `repeat`, `every`) and for a labelled `break` reaching that loop. `break (v)`
> there still produces `v`; a loop whose value is used anywhere else, and a loop ending a function
> that does not `yield` (whose result is the loop's value), keep "`break` succeeds with `()`".
> **Rejected:** keep the trailing `()`, which every such generator's caller would have to skip.

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

### One name, several arities

*Open — the questions at the end of this section are the user's. Nothing here is built.*

**The problem.** A function wants a short form and a long one, and FunL has no default parameters;
a relation wants the arities Prolog would give it:

```
def range( n )       = range( 0, n )
def range( a, b )    = a..<b

def parent( #tom )               ;; parent/1: tom is a parent
def parent( #tom, #bob )         ;; parent/2: tom is bob's parent
```

The design already speaks of **one namespace keyed by name and arity** ([prolog](prolog.md#calling-funl-from-prolog)),
of `any(c)` and `any(p, xs)` being kept apart by it ([prelude](modules.md#the-prelude--the-first-module-written-in-funl)),
and of `sides` being "a relation of two arguments and a function of one at once"
([modules](modules.md#questions-decided)). What FunL *does* is narrower.

**What FunL does today.** Within one file, a name is one definition of one arity:

```
def f( x ) = x + 1
def f( x, y ) = x * y
```
```
error: `f` was defined with 1 argument, and this clause has 2
 --> a1.funl:2:5
  = note: every clause of a definition takes the same number of arguments
```

The same refusal meets two arities in a `where` block, and two relations (`def p(#a)` and
`def p(#a, #b)`). Name/arity *does* work in three places:

- **Builtin against prelude**, by a fallback in `called` (`scope.sysl`): a builtin that does not take
  the call's count hands the call to the prelude's function of that arity. `any(odd, [2, 3])` prints
  `true`. But a program's own definition hides every arity of the name — with `def any( x ) = x * 100`,
  `any(odd, [1, 2])` is refused: `` `any` takes 1 argument, and this call gives 2 ``.
- **Prolog predicates called from FunL.** With `p(a).` and `p(b, c).` in an imported `.pl` file,
  `every p(x) do write( x )` and `every p(x, y) do write( (x, y) )` print `a` and `(b, c)`.
  A FunL relation of the name hides the other arity, though: with `def q( #a )` in the file and
  `q(b, c).` imported, `q(x, y)` is refused, `` `q` takes 1 argument, and this call gives 2 `` — the
  shared name/arity namespace the Prolog chapter decided is not what FunL's own scope does.
- **Constructors**, which the [data decision](#data) lets share a name across field counts:
  `data t = pt(x) | pt(x, y)` builds both `pt(1)` and `pt(1, 2)`. **As a value it picks one
  silently**: `g = pt` then `g(3)` faults, `the constructor 'pt' takes 2 fields, and was given 1`.

Through a function value the count is checked when the call is made: `g = f` with one-argument `f`,
then `g(5, 6)`, faults `'f' takes 1 argument and was given 2`. A builtin of several counts is
already one value that answers each: `g = map`, then `g(abs, [-1, 2])` is `[1, 2]` and
`g({"a": 1})` makes a map. There are no default parameters: `def f( x, y = 1 )` is refused,
`this cannot be matched against anything`.

#### Option A — one name, one arity (today), made consistent

Keep the refusal. A FunL definition owns its name at every arity, in FunL's scope; the name/arity
namespace is a Prolog-side notion only. Fix the constructor value to refuse when the name has two
field counts, and rewrite the Prolog chapter's sentence and the `sides` example to say so.

```
def range( n )    = range2( 0, n )
def range2( a, b ) = a..<b
```

- **Cost:** small — the constructor-value refusal, a test each, and the doc sentences. ~50 lines.
- **Binds:** FunL relations can never mirror a Prolog program's `p/1` and `p/2`; a Prolog
  library ported to FunL renames. A program's `def any( x )` keeps hiding prelude `any/2`. FunL and
  its Prolog view disagree about what a name is, which every reader of the Prolog chapter will trip on.

#### Option B — name/arity for relations only

Relations are keyed by name and arity, as Prolog's are; functions stay one arity per name. Relations
are not values (`write( p )` is refused today, *called rather than used as a value*), so the
question of what `f` means as a value never arises.

- **Cost:** `scope.sysl` (`add_top_clause`, `by_name`) and `scope_walk.sysl` (local `def` groups)
  key relations by `(name, arity)`; `global`/`called` take the count. ~150 lines.
- **Binds:** `range(n)` / `range(a, b)` still needs two names. Two rules for one keyword — a `def`
  with `=` and a `def` without obey different namespaces, which is the kind of split a user learns
  by tripping on it.

#### Option C — name/arity for everything, and a name with several arities is one dispatching value

**A definition is keyed by name and arity, function or relation.** A call written `f(a, b)` is
resolved when compiled, by its count, to `f/2` — a static call, indexed, no cost. **Named without
a call, `f` is one function value holding every function arity of the name in scope**, and a call
through it picks the member by its count when it is made. That is what a builtin of several counts
already is (`map` above), so it adds no new behaviour, only a new way to reach it.

```
def f( x )    = x + 1
def f( x, y ) = x * y

f( 10 )                    ;; 11, f/1
f( 3, 4 )                  ;; 12, f/2
map( f, [1, 2, 3] )        ;; [2, 3, 4]: map calls with one argument
foldl( f, 1, [2, 3, 4] )   ;; 24: foldl calls with two
g = f
g( 1, 2, 3 )               ;; fault: 'f' takes 1 or 2 arguments and was given 3
f( 1, 2, 3 )               ;; refused: `f` takes 1 or 2 arguments, and this call gives 3
```

What follows for each neighbour:

- **Default parameters** — FunL has none, and with C it needs none: `def range( n ) = range( 0, n )`
  is the default. A later `y = 1` in a parameter list would be sugar for exactly that pair of
  arities, never a second mechanism.
- **Variables shadow every arity.** A `val`, `var`, parameter, pattern, `free` or assignment holds
  one value, so it hides the whole name where it is in scope — the decided
  [shadowing rule](#assignment-and-assignment-that-undoes-itself), unchanged.
- **Builtins and the prelude are shadowed per arity.** A program's `def any( x )` takes `any/1`;
  `any(p, xs)` still reaches the prelude. This is the rule `called` already applies between a
  builtin and the prelude, extended to the program's own definitions, and it is Prolog's: a
  program's `p/1` does not hide a library's `p/2`. The value `any` is then the program's `any/1`
  together with the prelude's `any/2`.
- **Nested `def` and `where`** — see question 3: the nearest block that defines the name owns it at
  every arity, or only at the arities it defines.
- **Modules.** Already decided: `export` exports a procedure (a name and arity), and **an import
  brings every exported arity of the name**. A module's value is a map from export name to value
  ([modules](modules.md#a-module-is-a-value)), so its entry for a multi-arity name is the dispatching
  value, and `geometry.area(...)` written as a call is still resolved statically against the
  module's export list.
- **Constructors** become values the same way: `g = pt` answers `g(3)` with `pt(3)` and `g(3, 4)`
  with `pt(3, 4)`, where today it faults.
- **Relations** are not values, so the value of a name is its *function* arities only; a name with
  no function arity keeps the "called rather than used as a value" refusal.
- **The Prolog view is the natural one.** FunL relation `p/n` is predicate `p/n`, function `f/n` is
  `f/(n+1)`. Two FunL definitions that would occupy one Prolog key are refused in the file, naming
  both: **function `f/1` and relation `f/2` are both `f/2` to Prolog** (question 5). Prolog calling a
  multi-arity FunL name needs nothing new; a dispatching value passed to Prolog is an opaque
  `<function>`, as any closure is.

- **Cost:** `scope.sysl` and `scope_walk.sysl` key definitions by `(name, arity)` and keep a name →
  arities index; `add_top_clause` and the local-group code drop the arity refusal and add the
  Prolog-key refusal; `global`/`called` resolve by count against the merged program, import,
  builtin and prelude layers; the compiler emits a **family value** for a bare multi-arity name (a
  small VM object holding one function value per arity, its call checking the count where
  `run.sysl` checks a native's); `module.sysl` puts the family in the export map. A few hundred lines
  and ~25 tests, every refusal among them; the reference page on functions gains a section.
- **Binds:** no valid program changes meaning — every program FunL accepts today defines each name
  at one arity. The refusal `` `f` was defined with 1 argument, and this clause has 2 `` goes away;
  its tests become tests of the two arities working. **A mistyped clause with a wrong parameter
  count is no longer caught at the definition** but at the first call of the arity the program meant,
  as in Prolog (`` `f` takes 1 or 2 arguments, and this call gives 3 ``). The `sides` example in
  the modules decision is exactly the clash of question 5.

> **Recommendation: Option C.** FunL is the modern Prolog, and Prolog's unit is `name/arity`; the
> design already claims that namespace in two chapters and the implementation already honours it for
> builtins, the prelude and Prolog predicates — C makes FunL's own definitions agree. It is also the
> pleasant answer to default parameters without adding them. The value question has a precedent the
> user decided: a builtin of several counts is one value that answers each count (P4), so a
> program's name of several arities is the same thing, and a module's map entry needs exactly one
> value per name.

**Open questions.**

1. **Adopt Option C — name/arity for functions and relations alike?** *Recommended: yes.*
2. **What is a bare multi-arity name as a value?** The dispatching family above, or refused (*`f` has
   arities 1 and 2; write a lambda*)? *Recommended: the family — it is what a variadic builtin's
   value already is, and a module map needs one value per name.*
3. **Does a nested `def` or `where` group of `g` hide every outer `g`, or only its own arities?**
   *Recommended: every arity — the nearest block that defines a name owns it, so a reader finds every
   clause of a call in one place, and a missing arity is a clear refusal. Only the program's top level
   merges per arity, and only with the builtins and the prelude, which no block of the program wrote.*
4. **May a file define `f/2` while importing `f` (at other arities)?** *Recommended: no, as decided
   ([modules](modules.md#names-and-the-builtin-rule): an imported name may not also be defined by
   the file); `as` renames on the way in.*
5. **Function `f/n` and relation `f/(n+1)` in one file — the same Prolog key — refused always, or
   only when Prolog imports the file?** *Recommended: always, at the second definition, naming both
   and saying which Prolog key they share, so a FunL file never becomes unimportable later; and the
   `sides` example in the modules decision is reworded to arities that do not clash.*
6. **Default parameters?** *Recommended: none; two arities are how FunL writes a default. If they
   are ever added, `def f( x, y = 1 )` is sugar for `f/1` and `f/2`.*
7. **Until C is decided, the constructor value `g = pt` over two field counts picks the last one
   silently.** *Recommended: under C it becomes the family; under A or B it is refused when compiled,
   naming both counts.*

## Data

| kind | literal or constructor |
|---|---|
| numbers | `123`, `1.5`, `2n` is `2 * n` (juxtaposition multiplies) |
| strings | `'abc'` or `"abc"`; `$name` and `${expr}` interpolate, `$$` is a dollar |
| regex | `` `a(b|c)*` `` |
| booleans, nothing | `true`, `false`, `undefined`, `()` |
| tuple | `(1, 2)`; `()` is the tuple of nothing |
| list | `[1, 2, 3]`, `x:xs`, comprehensions `[x^2 \| x <- 1..10 if odd(x)]` |
| set, map | `{1, 2}`, `{a: 1, "b": 2}`, set comprehension `{x \ 2 \| x <- 1..5}` |
| mutable | `array(n)`, `buffer()`, `map(m)`, `map(m, default)`, `set(s)` |
| records | `data point(x, y)`; `data shape = circle(r) \| square(s)` |

> **Decided — a compound term needs a `data` declaration (user, 2026-10-08).** Outside a relation
> head, `point(1, 2)` builds a record only when `data point(x, y)` is declared; an undeclared
> functor is "not defined", as any other unknown name is.

> **Decided — `()` is the empty tuple (user, 2026-10-08).** The unit value `()` and the tuple of
> nothing are one value. Tuple operations take it as a tuple of length 0: `().length` is `0`,
> `() is tuple` and `() is unit` both succeed, `!()` and `x <- ()` produce nothing, `x in ()` fails,
> a slice of it is `()`, and the pattern `()` matches it while `(a, b)` does not. It prints `()`.

> **Decided — the map reading of `{key: lambda}` wins (user, 2026-10-08).** `:` is cons, so
> `f: (x) -> e` alone is a lambda whose parameter is the cons `f:(x)`. In braces that lambda is
> taken apart at its parameter's top `:` into a map entry: the key `f` and the lambda `(x) -> e`
> (and `{i: x -> e}` likewise). A set that holds a lambda whose parameter is a cons writes it in
> parentheses, `{(x:xs -> x)}`, or parenthesizes the parameter, `{(x:xs) -> x}`.

Elements are reached by call syntax and by field syntax: `r.a`, `r("b")`, `r(1)`, `m.a`, `m("a")`,
`[3, 4, 5](1)`. A field write on a mutable map adds the key.

**A mutable map can have a default, as Icon's `table(x)` does**: `map(c, x)` is `map(c)` whose
missing keys read as `x` instead of failing. So counting needs no membership test:

```
val counts = map( {}, 0 )
every counts( !split(text) ) += 1    ;; each word of the string text; a new word reads 0, and the assignment adds it
write( counts("zzz") )               ;; 0, and "zzz" is still not a key
```

- **Reading a missing key gives the default and adds nothing**: `m(k)` and `m.k` alike.
  `m.length` is still the size, since a collection's `length` is answered before the default.
- **Assigning adds the key**, and an update such as `+=` or `-=` is a read and an assignment, so it
  starts from the default and adds the key.
- **The default is not an entry**: `k in m`, `m.length` and `!m` (so `for (k, v) <- !m`) see only the
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

> **Decided (2026-10-07) — is there slicing?** **Decision: `c(r)` with a range `r` is the slice of
> a string, list, range, tuple, array or buffer**: the elements at the 0-based indexes `r` names, in
> its order (every range form, `by` steps included, backwards too), as a value of `c`'s kind; an
> array's or buffer's slice is a copy, and a string is sliced by characters through its cursor. **A
> slice naming any index that is not there fails**, as a single index does, rather than clamping; an
> open-ended `a..` runs to the end and may start at the length. A map is not sliced: the range is a
> key like any other.

> **Decided (user, 2026-10-08) — how many ways are there to index?** **Decision: one.** An element
> is `c(i)`, counting from 0, and `c(r)` is a slice; a map's value is `m(k)` or `m.k`. There is no
> `c[i]`: brackets after a value are refused at compile time, with a message naming `c(i)`, counting
> from 0. A negative index names no element, so `c(-1)` fails as an index past the end does.
> Scanning's positions are a different thing and stay Icon's (see [String scanning](#string-scanning)).

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
  that unification or Prolog made is one too. Where the file sees constructors of one name with
  different numbers of fields (`data point(x)` and `data point(x, y)`), `is point` tests for a record
  of any of them.
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
| `tuple` | `(1, 2)`, and `()`, the tuple of nothing |
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

> **Decided — a program's names shadow the builtins (2026-10-07).** The builtins are names
> outside every block of the program. A `val`, `var`, `free`, assignment, pattern, parameter or local
> `def` of a builtin's name hides the builtin wherever the declaration is in scope, and a top-level
> `def` of one is the program's definition in place of the builtin. Otherwise every builtin added
> later would take a name away from programs already written, and ordinary names (`sum`, `log`,
> `round`, `sign`) would be unusable as variables. What stays refused is a top-level variable of a
> name the program itself defines at the top level — a function, a relation, or an imported Prolog
> predicate — since those are hoisted and a variable could not hide them lexically. The Prolog side
> is unaffected: a Prolog builtin predicate cannot be redefined (`permission_error`).

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

> **Decided (user, 2026-10-08) — scanning positions stay Icon's.** A position falls *between*
> characters: `1` is before the first, `0` is the end, and a negative position counts back from the
> end. This is what `tab`, `move`, `pos`, `upto`, `many`, `any`, `match` and `find` read and
> produce, and it is not an element index: `s(0)` is the first character, `s ? tab(2)` the text
> before position 2, which is the same character.

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
