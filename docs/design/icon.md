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

### `until`

**Icon:** `until e1 do e2` evaluates `e1`, bounded; if it fails, evaluates `e2`, bounded, and goes
round again; when `e1` succeeds the loop ends and, as every Icon loop that runs out does, fails.
`until e1` with no `do` is legal and spins on `e1` alone. `break` leaves it with a value.

**FunL today:** `until` is the infix word of the exclusive generator, `1 until 3`, the generator
twin of `..<`. It is not a Decided block, but it is the word.

**Proposed:** take both, by position. **`until` at the start of an expression is the loop; `until`
after an operand is the range**, as `-` is negation before an operand and subtraction after one.
The parser already tells a prefix use of a token from an infix one, so nothing is ambiguous:

```
until done(state) do state = step(state)
every write(1 until 4)
```

The loop is `while not e1 do e2` compiled directly: the condition's mark jumps into the body, its
success leaves the loop. It takes a label, `end until`, and `break (v)` as `while` does, and
fails when it runs out.

> **Open question I1 — the loop `until` beside the range `until`.** (a) both, by position;
> (b) the loop takes the word and the range is spelled `..<` or `to j - 1` only; (c) no `until`
> loop, `while not` covers it. **Recommendation: (a).** It costs nothing in the grammar and breaks
> no program; `while not` is the covered fallback but reads worse for the common "until converged"
> loop.

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

**Icon:** `static x` declares a variable that belongs to the procedure rather than to a call: it
starts `&null` when the program starts and keeps its value from one call to the next, recursion
included. `initial e` is a statement at the head of the body that is evaluated on the first call
only, before the rest of the body; it is how a static gets its first value. Neither is undone by
backtracking.

**Proposed:**

```
def next_id()
  static count = 0
  count += 1
  count

def lookup(k)
  static cache
  initial cache = map()
  cache(k) = cache(k) | compute(k)
```

- **`static x = e`** declares `x` and evaluates `e` once, on the first call, before the statement
  runs — Icon's `static x` and `initial x := e` in one line. `static x` alone starts `undefined`.
- **`initial` followed by an expression or an indented block** runs it on the first call only.
  `initial` blocks and `static` initializers run in the order they are written. The flag that says
  "done" is set *before* they run, so a recursive call from inside one does not run it again (and a
  failing or faulting initializer is not retried, as Icon's is not).
- A static is visible in the clause that declares it, from its declaration to the end of the body,
  like any block-scoped name, and is assigned with `=`, never undone by backtracking.

**Machine:** a closure whose chunk declares statics gets a statics object when it is made, holding
one cell per static and the "initialized" flag. `LoadStatic(i)` and `StoreStatic(i)` reach it
through the frame (the frame gains a pointer to its closure), and `Initial(skip)` tests and sets the
flag. The statics object is reached from the closure, so the collector sees it through its closure
like any captured frame.

> **Open question I4 — whose is a static: the definition's, or each closure's?** Icon has no
> nested procedures, so it never had to say. A nested `def` or a lambda is made afresh each time its
> enclosing body runs. (a) **each closure value has its own statics**, so a nested `def` made twice
> counts twice; (b) **one set of statics per definition in the source**, shared by every closure
> made from it, as C's are. For a top-level `def` the two agree. **Recommendation: (a).** A function
> value is a closure, and its static is part of that value, so the memo table a nested helper keeps
> is that helper's; (b) would make two closures that capture different frames share mutable state,
> which nothing else in FunL does.

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

### Sections and indexing

**Icon:** `s[i]`, `s[i:j]`, `s[i+:n]` and `s[i-:n]` take characters by position, 1 before the
first, 0 the end, negative counting back, and every one of them is assignable on a string variable.

**FunL:** an element is `c(i)` counting from 0 and a slice is `c(r)` for a range — decided, one way
to index. Every section is **covered** by a range: `s[i+:n]` is `s(i ..+ n)`, `s[i-:n]` is
`s(i - n ..+ n)`, a tail is `s(i..)`. Position arithmetic belongs to scanning, where positions stay
Icon's. What is not covered is **assigning to a section of a string variable**, `s[2:4] := "xy"`,
which builds a new string and stores it.

> **Open question I10 — Icon's positions in indexing.** (a) keep the decision: 0-based `c(i)`,
> ranges for sections, no negative indexes, and add `s(r) = t` on a string variable as "store a new
> string with that section replaced"; (b) reopen it for Icon's positions. **Recommendation: (a).**
> Every section has a range spelling, and the 0-based rule is shared with lists, arrays, tuples and
> Prolog's view of them. The assignable section is the one capability missing, and it fits the
> existing `c(i) = e` on a mutable collection.

## Strings and csets

### `map` and `reverse`

> **Open question I11 — `map(s1, s2, s3)`.** Icon's character mapping replaces each character of
> `s1` that occurs in `s2` with the one at the same place in `s3`, the last occurrence in `s2`
> winning; `s2` and `s3` must be the same length. FunL's `map` already has three meanings (the
> prelude's `map(f, xs)`, `map(c)`, `map(c, default)`), and `map` with three arguments is decided to
> be an `existence_error`. (a) a new name, **`translate(s, from, to)`**; (b) `map/3` over strings,
> reopening the decision. **Recommendation: (a).** A fourth meaning for `map` is one too many, and
> `translate` is what the operation is called elsewhere.

> **Open question I12 — `reverse` of a string.** P3.5 decided that the prelude's list functions
> refuse a string "as `reverse` refuses it". (a) **`reverse(s)` is the string reversed**, by
> characters, and P3.5's refusal stays for every other list function; (b) a separate
> `reverse_string`. **Recommendation: (a).** P3.5 refused answering a string *as one element*;
> a reversed string is a different, well-defined answer of the same kind, and `reverse` is the
> name anyone reaches for.

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

### Records

**Icon:** `r.f := x` changes a record in place.

**FunL:** a record is a compound term — the value Prolog sees and unifies — and immutable. A field
cannot be assigned.

> **Open question I15 — mutable records.** (a) keep records immutable, and add **`with(r, {f: x})`**,
> a copy of `r` with the named fields replaced; a record whose fields change in place is a mutable
> map, or a record holding an array; (b) mutable records, breaking the identity of a record with a
> Prolog term. **Recommendation: (a).** A term that changes under a unification that has already
> succeeded is the failure the logic chapter's design exists to rule out.

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

**Icon:** before compiling, lines beginning with `$` are directives: `$define name text`,
`$undef`, `$ifdef`, `$ifndef`, `$else`, `$endif`, `$include "file"`, `$line n "file"`, `$error
text`. Predefined names describe the platform and the features built in (`_UNIX`,
`_CO_EXPRESSIONS`, `_LARGE_INTEGERS`, …). Substitution is by token, not inside strings.

**FunL today:** `val` names a constant, `import` brings another file's names, and nothing selects
code by platform or feature when compiling. A line beginning with `$` is free (`$name`
interpolates only inside a string).

**Proposed:** Icon's directives, run over the token stream after the lexer and before the parser,
so substitution is by token and never reaches into a string or a comment: `$define` (a sequence of
tokens), `$undef`, `$ifdef`/`$ifndef`/`$else`/`$endif`, `$include` (textual, unlike `import`),
`$line` and `$error`. Predefined: `_FUNL`, `_MACOS`, `_LINUX`, `_UNIX`, `_CO_EXPRESSIONS`,
`_LARGE_INTEGERS`, and `_HTTP`, `_ASYNC`, `_PROFILE` for the features built in. Diagnostics name the
file and line a token came from, through `$include` and `$line`. In a literate file the directives
are in the tangled program, as everything else is.

> **Open question I20 — a preprocessor.** (a) Icon's directives, token-level, as above; (b) only
> conditional compilation on platform and features, `$ifdef` and its partners, with `val` and
> `import` covering `$define` and `$include`; (c) none. **Recommendation: (b).** `val` and
> `import` are better than `$define` and `$include` in every way but token pasting, and a textual
> include is the thing modules replaced. Selecting code by platform or feature is the one capability
> nothing covers. If parity is meant to the letter, (a) costs little more on top of (b).

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
| I1 | the loop `until` beside the range `until` | both, by position |
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
| I20 | a preprocessor | conditional compilation only |
| I21 | the graphics facilities | decided: in scope, [Graphics](graphics.md) |
