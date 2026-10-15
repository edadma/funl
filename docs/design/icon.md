---
title: Icon parity
weight: 58
---

# Icon parity

**FunL is meant to do everything Icon 9.5 does** — Griswold's Icon, not Unicon. This chapter goes
through Icon's language and its standard repertoire of functions and keywords, and for each thing
FunL lacks says what Icon does, how FunL should spell it, and how the [machine](vm.md) runs it.

**Parity is of capability, not of spelling.** An Icon program should have a FunL translation that
is about as short and runs the same way; it need not have the same characters. Where Icon's name or
operator is free in FunL and reads well, FunL takes it. Where FunL already says the same thing
another way, the thing is **covered** and the section cites the FunL form and asks nothing. Where
parity would reopen something a chapter has already decided — indexing, `\`, records, faults,
`case`, string comparison, the word `until` — the decision stands, and the section puts the
trade-off as an open question with a recommendation. Pleasantness breaks ties, and where FunL
already follows slate or JavaScript, their spelling is the precedent.

The questions are numbered I1 to I21 and collected at the end and in the [README](README.md#open-questions).

## Already covered

These Icon constructs have a FunL spelling now, and this chapter asks nothing about them.

| Icon | FunL | where |
|---|---|---|
| goal-directed evaluation, failure, generators, `every`, `\|`, `\|e`, `&`, `not` | the same | [language](language.md#every-expression-succeeds-or-fails) |
| `i to j by k` | the same; `i until j` excludes `j` | [language](language.md#generators-an-expression-may-produce-more-than-one-value) |
| `suspend e` | `yield e`, which yields every value of `e` | [language](language.md#functions-are-generators-when-their-body-is) |
| `suspend e1 do e2` | `every x = e1 do (yield x; e2)` | |
| `return e`, bounded | the same, decided | [language](language.md#functions-are-generators-when-their-body-is) |
| `fail` (in a procedure) | `fail` | |
| `next` | `continue` | [language](language.md#control-structures-and-which-of-them-are-bounded) |
| `break e` | `break (e)` | |
| `while`, `repeat`, `if` | the same | |
| `procedure … end`, `local`, `global`, `link` | `def`, block-scoped names, top-level names, `import` | [modules](modules.md) |
| `record` | `data` | [language](language.md#data) |
| `x := e`, `x <- e` reversible | `x = e`, `x <- e` | [language](language.md#assignment-and-assignment-that-undoes-itself) |
| `x :=: y` | `x, y = y, x`, for every assignable place: `a(i), a(j) = a(j), a(i)` | |
| `\x`, `/x` | the same, assignable | [language](language.md#data) |
| `*x` | `*x`, and `x.length` | |
| `s1 \|\| s2`, `L1 \|\|\| L2` | `+` | |
| `(e1, e2, e3)` mutual evaluation; `i(e1, …, en)` selection | a tuple `(e1, e2, e3)` (each must succeed); `(e1, …, en)(i - 1)` | |
| `table(x)`, `set(L)`, `list` | `map({}, x)`, `set(c)`, `array(n)`, `buffer()` | [language](language.md#data) |
| `!T` over a table, `key(T)` | `for (k, v) <- !m`; `(k, _) <- !m` | |
| `member(S, x)`, `insert(S, x)`, `insert(T, k, v)` | `x in S`, `S += x`, `T(k) = v` | |
| `seq(i, j)` | `!(i..)`, `i to … by j` over a range | |
| `S1 ++ S2`, `S1 -- S2` over sets | `+`, `-` | |
| `upto`, `many`, `any`, `match`, `find`, `tab`, `move`, `pos`, `s ? e`, `s ?:= e` | the same; `s ?= e` | [language](language.md#string-scanning) |
| `string(x)` of a number, implicitly | interpolation `"${x}"`; `str(x)` below | |
| `getenv`, `system`, `exit`, `remove`, `rename`, `delay` | `funl:process`'s `env`, `run`, `exit`; `funl:fs`'s `remove`, `rename`; `funl:time`'s `sleep` | [modules](modules.md#the-first-batch) |
| `&clock`, `&date`, `&dateline` as data | `funl:time`'s `now`, `format`, `fields` | |
| `loadfunc`, `callout` | native modules | [modules](modules.md#built-in-modules-funlname) |
| a call with too few arguments gets `&null`, extra ones are dropped | not adopted: a call is resolved by its count, decided; two arities or a variadic `def` say it | [language](language.md#one-name-several-arities) |
| `procedure p(a, b[])`, `p ! L`, `args(p)` | `def p(a, ..b)`, `p(..L)` anywhere among the arguments, `arity(p)` generating each count (I5, decided) | [functions](../reference/functions.md#variadic-functions) |
| `&fail`, `&null` as values | `fail`, `undefined` | |

## Control

### `case`

> **Decided (user, 2026-10-15) — I2, a `case` expression: postfix `e match`** over a block of
> clauses, built: the subject made once and bounded, committed choice, an unbounded body, no clause
> taking the value failing the `match`, and `(e)` in a clause's pattern comparing with `e`'s first
> value. Described in [Functions § `match`](../reference/functions.md#match).

### Limitation

> **Decided (user, 2026-10-15) — I3, limitation: the infix word `e limit n`**; `\` stays integer
> division. Built with Icon's semantics (`n` first, a generating `n` re-limits `e`, 0 fails without
> evaluating `e`; a non-integer limit is a type error, a negative one a domain error) on
> `LimitSet`/`LimitCount` and two hidden slots, the count and the place of `e`'s mark (`limit.sysl`).
> The mark's place is recorded because `|x` and a suspended generator leave marks of their own open
> above it. `limit` binds between a cons and a comparison (tighter than `|`, which this section's
> draft put above it — FunL's `|` is looser than `&` and comparison, so no level satisfied the whole
> draft), and is a word only in an operator's place. The language is described in
> [Generators](../reference/generators.md#limit-takes-the-first-few-values).

## Procedures

### `static` and `initial`

> **Decided (user, 2026-10-15) — I4, whose is a static: each closure value's.** A nested `def` or a
> lambda made twice counts twice; a named function is one value per frame it is defined in, so its
> calls and its uses as a value share the statics. Built: [Functions](../reference/functions.md#static-and-initial).

## Co-expressions

> **Decided (user, 2026-10-15) — I7: Icon's spelling**, `create e`, `@c`, `v @ c`, `^c`, `*c`,
> `!c`; **I8: `create` copies the innermost frame and shares the frames outside it.** Built
> (`coexpr.sysl`) as this section drafted it: a co-expression is a `Machine` set aside in a
> `CoExprObj` -- its chunk ending in `CoYield`, two copies of the creating frame, its activator, its
> count -- switched by `Activate` and `CoYield` with buffer handles, never a stack copy; a fault
> nothing inside catches ends it and is raised again at the `@`; `&current`, `&source` and `&main`
> are keywords, and `&features` generates `"co-expressions"`. See
> [co-expressions](../reference/coexpressions.md). Where the draft was silent or did not fit:
>
> - `1..` is a range value in FunL, not a generator, so the draft's `create 1..` is `create !(1..)`.
> - Prefix `*x`, listed above as covered, did not exist: it is built here, the length of anything
>   that is not a co-expression. `(*c)` and `(^c)` stay operator sections; `@^c` needs none.
> - `create` reads its operand as loosely as `not` does, so `create !xs | !ys` is one co-expression.
> - `x <- c`, as a binding, generates by activation as `!c` does.
> - Activating the running co-expression produces the transmitted value at once.
> - A result or a failure goes to the activator, or -- that one having ended, or being suspended at
>   a result of its own, which symmetric activation can bring about -- to `&main`, which is always
>   waiting at an `@` while anything else runs.
> - `return`, `yield`, and `break` or `continue` leaving `create` are refused when compiled, as is
>   `await` inside `create` until item 18.
> - At the top level the innermost frame is the top level's, so a co-expression made there copies
>   the top-level names; a top-level function it calls still reads the originals.

**With `async` tasks** (milestone 11 item 18). A co-expression is not a task: the loop never
schedules it, and it runs only when activated. It **runs as part of the task that activates it**
(built: the task's promise, number and `&main` go with the running machine across every switch). An
`await` inside a co-expression parks the running machine — the co-expression's — and the task waits;
its activator chain stays suspended in its objects, and when the promise settles, the task resumes
inside the co-expression. A co-expression belongs to the task that created it: activating it from
another task is the fault `permission_error(activate, coexpression, C)`, as binding another task's
variable is refused. Each task has its own `&main`, the machine it started on (built).

**What the collector sees.** A `CoExprObj`'s tracer walks its parked machine as the running one is
walked, both frame copies, and its activator; the running chain is reached from the VM's `&current`
and `&main` (built). A co-expression nothing refers to is collected, its finalizer releasing its
machine's buffers — a generator abandoned half way costs nothing more; item 18 proves it under the
stress heap.

> **Open question I9 — logic variables inside a co-expression.** A binding a co-expression makes
> is trailed on its own trail, so only its own backtracking undoes it; the activator's backtracking
> does not reach into a co-expression. (a) **a co-expression may bind any variable its task may**,
> with that consequence documented (a binding made inside `c` of a variable from outside survives
> the activator's backtracking, as an ordinary assignment would); (b) a co-expression may bind only
> variables it made, as a task may bind only its own. **Recommendation: (a).** It is the same
> visible rule as an assignment in a co-expression, and refusing it would make a co-expression
> useless for walking a relation's solutions, the obvious use.

## Scanning


## Strings and csets

### String comparison

**Icon:** `<<`, `<<=`, `==`, `~==`, `>>=`, `>>` compare strings lexically, and `<`, `=`, `~=` …
compare numbers, because Icon converts operands: `"10" < "9"` compares 10 with 9.

**FunL:** `<`, `==` and the rest compare strings lexically and numbers numerically, and nothing is
converted. Icon's two families exist only because of the conversion, so FunL's one family is
**covered**.

> **Open question I13 — Icon's string comparison operators.** (a) none: FunL's polymorphic
> comparison covers them; (b) add `<<` and the rest as string-only comparisons that refuse a
> non-string. **Recommendation: (a).** Without conversion, the second family would compare exactly
> what the first does.

> **Open question I14 — Icon's implicit conversion.** Icon converts freely: `"3" + 4` is 7, `3 ||
> 4` is `"34"`, `upto(3)` takes the cset `'3'`. FunL refuses an operand of the wrong kind with a
> fault. (a) keep refusing; give each conversion a function (`integer`, `real`, `numeric`, `str`,
> `cset`); (b) convert as Icon does. **Recommendation: (a).** A conversion that guesses is how a
> typo becomes a wrong answer instead of a message; the functions make every Icon conversion one
> call away. A string where a cset is wanted is already accepted, which is the one conversion Icon
> programs lean on everywhere.

## Structures

| Icon | FunL | |
|---|---|---|
| `S1 ** S2` | `**` | new |
| `serial(x)` | not adopted: `image` does not print serial numbers | |

## Numbers

### Bits, radix and conversion

| Icon | FunL | |
|---|---|---|
| `integer(x)`, `real(x)`, `numeric(x)` | the same; a string is read as a literal is, Icon's `16rFF` included; fail when it is not one | new |
| `16rFF` literal | the same (I16) | done |

> **Decided (user, 2026-10-15) — I16, radix literals.** `NrDDD` is a literal, `N` from 2 to 36 and
> the digits in either case, any size, `_` between digits; it takes `N r name` away from juxtaposition.
> `integer("16rFF")` reads the same form, by the same reader.

## Input and output

**Icon:** `open(s, mode)` opens a file (`"r"`, `"w"`, `"a"`, `"b"` read and write, `"c"` create,
`"t"`/`"u"` translated or not, `"p"` a pipe to or from a shell command) and **fails** when it cannot.
`read(f)` is the next line without its newline, failing at the end; `reads(f, n)` the next `n`
characters; `!f` generates the lines. `write(x…)` and `writes(x…)` write their arguments — to the
file an argument names, else `&output` — with or without a newline. `seek(f, i)`, `where(f)`,
`flush(f)`, `close(f)` (a pipe's exit status). `stop(x…)` writes to `&errout` and ends the program
with status 1. `chdir(s)`. `&input`, `&output`, `&errout` are the standard files. `getch`,
`getche` and `kbhit` read the keyboard unbuffered.

**FunL today:** whole files and the lines of a file, in `funl:fs`; `write` to standard output only,
with a newline; no standard input, no standard error, no open file.

**Proposed:**

- **`open(path, mode)` in `funl:fs`** answers a file `Handle` or fails, as `funl:fs` fails "for no
  such thing" ([decided](modules.md#questions-decided)); a mode string as Icon's, `"p"` included,
  run over `funl:process`. A file is a `Handle` with the methods `f.read()`, `f.reads(n)`,
  `f.write(x…)`, `f.writes(x…)`, `f.seek(i)`, `f.where()`, `f.flush()`, `f.close()`, and `!f`
  generates its lines. Positions in `seek` and `where` are Icon's, 1 before the first byte.
- `getch()`, `getche()` and `kbhit()` in a new `funl:term`, which puts the terminal in raw mode
  around each call.
- `&input`, `&output` and `&errout` are the three standard streams as handles.

> **Open question I17 — writing to a file.** Icon's `write(f, x)` writes to `f` because its first
> argument is a file. FunL's `write` prints whatever it is given, a handle included. (a) **a
> handle's method, `f.write(x)`**, as every handle in FunL is used, and `write` stays "print these";
> (b) Icon's rule: a file among `write`'s arguments redirects the rest. **Recommendation: (a).** It
> is how `funl:sqlite`'s handles already read, and (b) changes what `write(h)` prints for every
> handle.

## Keywords

> **Decided (user, 2026-10-15) — I18: `&name` as Icon has it**, one token when `&` touches a letter;
> `a &b` is refused with a note naming `a & b`. Built: the token and its refusal, `&subject` and
> `&pos` (`&pos <- i` reversible), the csets, `&null`, `&fail`, `&pi`, `&e`, `&phi`, `&random`, the
> clocks, `&version`, `&host`, `&progname`, `&features` (`"co-expressions"` among
> them), `&file`, `&line` and `&level` — see [keywords](../reference/keywords.md). Built since:
> `&trace` (calls, returns, suspensions, resumptions and failures, counted down per line), `&dump`,
> `display()`, `collect()`, `&collections` and `&allocated`, over a name table the compiler keeps per
> chunk (`trace.sysl`). **Decided (user, 2026-10-15): the line is Icon's, the call site's** — a
> line table per chunk (run-length, one mark where the line changes, kept beside the chunks and read
> only by a trace line or a fault) places a call at the call, a return or suspension at what
> returned, a failure at what failed; and an uncaught fault names its file and line
> (`f at prog.funl:3: …`, where it named an instruction index). A tail call still takes its caller's
> place, so the caller's return is not shown — a deliberate difference. `&line` stays fixed when
> parsed. **`&storage` and `&regions` are adopted** over the one region FunL allocates in, the
> collected heap: one number each, the bytes in use and the heap's size, where Icon's generate one
> per region. What remains:

| keyword | FunL |
|---|---|
| `&input`, `&output`, `&errout` | adopted ([I/O](#input-and-output)): the three standard streams as the file handles item 9 makes |
| `&error`, `&errornumber`, `&errortext`, `&errorvalue` | not adopted ([I19](#errors)) |
| `&col`, `&row`, `&window`, `&x`, `&y`, `&interval`, `&control`, `&meta`, `&shift` and the mouse-event keywords | adopted in [Graphics](graphics.md#events) |

**Machine (as built, `keywords.sysl`):** a keyword is fixed when parsed (`&null`, `&fail`, `&file`,
`&line`), a builtin constant folded when compiled, or a call of a builtin registered under the
keyword's own spelling (`&pos`), which no FunL name can reach. A store is a builtin `&name=` or
`&name<-` that checks the value's kind. The remaining keywords join the same table.

## Errors

**Icon:** a run-time error ends the program, unless `&error` is non-zero, in which case it
decrements `&error`, records the error in `&errornumber`, `&errortext` and `&errorvalue`, and turns
the error into **failure**. `runerr(i, x)` raises one; `errorclear()` resets the record.

**FunL:** a fault is not a failure; `e catch p -> r` takes one, with the error term in `p`, and a
recovery that fails turns it into failure — `e catch _ -> fail` is Icon's `&error` for one
expression. `throw(x)` and `error(msg)` raise. This is **covered**, by a [decided](language.md#every-expression-succeeds-or-fails)
design that scopes the conversion to an expression instead of a global counter.

> **Decided (user, 2026-10-15) — I19: nothing more.** `catch` covers `&error`, `throw` covers
> `runerr`, and the error keywords are not adopted: a global switch that turns faults into failures
> anywhere — in a library, in Prolog — would break every guarantee the decided design makes about
> faults.

## The preprocessor

> **Decided (user, 2026-10-15) — I20: Icon's whole preprocessor**, not only conditional
> compilation (the narrower recommendation was declined): `$define`, `$undef`, `$ifdef`, `$ifndef`,
> `$else`, `$endif`, `$include` (positions carried through, include cycles refused), `$line` and
> `$error`, with the platform and feature names predefined, over FunL's token stream and in a
> literate file's tangled program. Built: `preproc.sysl`; specified in
> [Source files](../reference/source-files.md#the-preprocessor).

## Graphics

**Icon 9.5 ships graphics facilities**: windows, drawing, fonts, colours, images and events, with
some thirty keywords (`&window`, `&x`, `&y`, the mouse events) and about seventy functions
(`WOpen`, `DrawLine`, `Event`, …). They are an optional part of an Icon build, described in their
own book, rather than part of the language.

> **Decided (user, 2026-10-15) — I21, the graphics facilities: in scope**, as the native module
> `funl:graphics`, with a design of its own: [Graphics](graphics.md), whose open questions are
> G1–G12, built as milestone 12.

## The questions, collected

| | question | recommendation |
|---|---|---|
| I2 | a `case` expression | decided: postfix `e match`, [Functions](../reference/functions.md#match) |
| I3 | limitation, `\` being integer division | the infix word `e limit n` |
| I4 | whose is a static | each closure value's |
| I5 | a variadic `def` beside name/arity | **decided** (user, 2026-10-15): the family member for every count no fixed arity takes; built |
| I6 | reflection by name | `proc` over top-level names and builtins; no `variable`, no string invocation |
| I7 | co-expression spelling | Icon's `create`, `@`, `^`, `*`, `!` |
| I8 | does `create` copy the locals | copy the innermost frame, share the rest |
| I9 | logic variables inside a co-expression | bind as the task may; trailed on the co-expression's own trail |
| I10 | Icon's positions in indexing | keep 0-based `c(i)`; add assignment to a string section |
| I11 | `map(s1, s2, s3)` | `translate(s, from, to)` |
| I12 | `reverse` of a string | `reverse(s)` reverses it |
| I13 | Icon's string comparison operators | none; polymorphic comparison covers them |
| I14 | implicit conversion | none; conversion functions |
| I15 | mutable records | keep immutable; add `with(r, {f: x})` |
| I16 | radix literals | the `NrDDD` literal, decided (user, 2026-10-15) |
| I17 | writing to a file | handle methods, `f.write(x)` |
| I18 | how keywords are spelled | `&name`, one token |
| I19 | `&error` and `runerr` | nothing more; `catch` and `throw` cover them |
| I20 | a preprocessor | decided: Icon's whole preprocessor |
| I21 | the graphics facilities | decided: in scope, [Graphics](graphics.md) |
