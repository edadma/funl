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
| a call with too few arguments gets `&null`, extra ones are dropped | not adopted: a call is resolved by its count, decided; two arities or a variadic `def` (below) say it | [language](language.md#one-name-several-arities) |
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

**Icon:** `case e of { e1 : r1; e2 : r2; default : rd }` evaluates `e` once, bounded. The
selectors are tried in order; each is evaluated and **may generate**, and each of its values is
compared with `e`'s by `===`. The first that matches chooses its result expression, whose values
are the `case`'s — unbounded, so a `case` may generate. With no match the `default` runs, and with
no `default` the `case` fails.

**FunL today:** clause dispatch and the partial function literal cover selection by pattern: a
value is classified by applying an indented block of lambdas to it. There is no expression form
that takes a subject first, and a partial function given a value no clause matches **faults**
(`argument match not found`), where Icon's `case` fails.

**Proposed:** a postfix `match`, as sysl and Scala write it, whose block is exactly a partial
function literal:

```
kind = n match
  0 -> 'zero'
  m | m < 0 -> 'negative'
  _ -> 'positive'
```

- The subject is evaluated once, bounded; clauses are tried in order with committed choice, as a
  partial function's are; the chosen clause's body is unbounded, so a `match` may generate.
- **No clause matching fails**, Icon's rule and goal-directed evaluation's, unlike a partial
  function, whose callers are functions and for whom a missing case is a programming error.
- **A parenthesized expression in a pattern compares against its value**, `(limit) -> …`, as a
  map pattern's `(e)` key is already the value of `e` ([M2](language.md#map-patterns)). This is
  how an Icon `case` on a variable's value is written. An alternation of such patterns, `(a) | (b)`,
  is Icon's generating selector.

It compiles as the partial function does, inline: a `Mark` per clause, the subject in a slot, and
`Fail` where the partial function has `NoMatch`.

> **Open question I2 — a `case` expression.** (a) postfix `e match` with a block of clauses, no
> match failing, `(e)` in a pattern meaning "equal to `e`'s value"; (b) Icon's `case e of` with the
> same block; (c) nothing new, the partial function applied, `(… block …)(e)`, faulting on no
> match. **Recommendation: (a).** It is the partial function FunL already has, put after its
> subject, and the spelling of sysl, which FunL is written in. (b) adds two words for the same
> thing. (c) is what FunL says now and reads backwards.

### Limitation

**Icon:** `e1 \ e2` evaluates `e2` first. Its value must be a non-negative integer (a negative one
is a run-time error). `e1` is then evaluated and produces at most that many results; when it has
produced them, it is not resumed again. **`e2` is an ordinary expression and may generate**: if the
context backtracks past `e1`, it resumes `e2`, and each new limit starts `e1` afresh — so
`(1 to 3) \ (1 | 2)` produces `1`, then `1, 2`. A limit of 0 fails at once without evaluating `e1`.
Every time the limitation itself is evaluated afresh (a loop's next turn) `e2` is evaluated again.

**FunL today:** `\` is integer division, which floors — a Decided block. The capability is
**covered by the prelude**: `take(n, () -> e)` draws at most `n` values from the thunk and generates
them, so `every write(take(3, () -> !xs))` is `every write(!xs \ 3)`.

**Proposed:** an infix word, **`e limit n`**, because the thunk form is a mouthful for something as
everyday as "the first three":

```
every write( !words limit 3 )
every write( (1 | 2 | 3) limit 2 )
```

`limit` binds looser than `|`, `to` and `by`, and tighter than comparison, `&`, `and` and
assignment, so the limited thing rarely needs parentheses (Icon's `\` binds tighter than `^`, so
Icon programs parenthesize almost every use).

**Machine:** the count must survive backtracking into `e`, which restores `e`'s operand cells, so
it cannot be an operand cell. It is a **compiler-allocated slot**, since an ordinary store to a slot
is not undone by backtracking ([frames](vm.md#frames-slots-and-closures)):

```
<n>                 generator; its choice points stay, so backtracking re-limits
LimitSet(t)         slot t = n; n not an integer faults (type_error), negative faults
                    (domain_error); t == 0 fails
MarkThrough         e's region begins
<e>
LimitCount(t)       t -= 1; if t == 0, cut control back to the region's mark, dropping e's
                    choice points, so the next failure reaches n's generator
```

> **Open question I3 — limitation.** (a) the infix word `limit`; (b) take back `\` for
> limitation and move integer division to `//` alone (which already exists), reopening the Decided
> block; (c) nothing new, `take(n, () -> e)` only. **Recommendation: (a).** (b) is Icon's
> spelling but breaks every program and page that divides with `\`, and the decision that `\`
> floors would have to move with it; (c) is covered but costs a lambda at every use.

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

### Variadic procedures and list invocation

**Icon:** `procedure p(a, b[])` collects the arguments after `a` into a list `b`, empty when there
are none. `p ! L` calls `p` with the elements of `L` as its arguments. `args(p)` is the number of
parameters, negative for a variadic procedure.

**FunL today:** a call is resolved by its count, and a builtin named as a value passes on however
many arguments it is given — but a program can neither declare a variadic function nor call one
with a list of arguments.

**Proposed: `..rest`, the spelling FunL already has for the rest of a map pattern** (JavaScript's
`...rest` without the third dot):

```
def log(level, ..parts) = write(level + ': ' + join(parts, ' '))

log('info', 'started', 'ok')
args = ['warn', 'disk', 'low']
log(..args)
```

- **`..rest` as a function's last parameter** binds an immutable list of the remaining
  arguments, as `..rest` in a map pattern binds an immutable map. It is a parameter, not a pattern
  inside a list, so it appears only last and only once.
- **`..e` as a call argument** spreads a list, tuple, array, buffer or range into the arguments,
  anywhere in the argument list and any number of times. The call's count is then known only when it
  runs, so it is resolved at run time through the family value, as a bare multi-arity name's call
  is ([one name, several arities](language.md#one-name-several-arities)).
- **`arity(f)`** generates the arities a function value takes, in increasing order; a variadic one's
  is its fixed count negated, as Icon's `args` reports it (`arity(log)` is `-1`).

> **Open question I5 — a variadic `def` beside name/arity.** Option C made `f/1` and `f/2` two
> functions. (a) **`def f(a, ..rest)` is the member of `f`'s family for every count of one or more
> that no fixed arity of `f` takes**: a call of two arguments goes to `f/2` if there is one, else to
> the variadic; two variadic clauses with different fixed counts in one family are refused; a
> relation cannot be variadic. (b) a variadic function is alone: no other arity of its name may be
> defined. **Recommendation: (a).** It keeps "the nearer, more specific definition wins", which
> Option C already uses, and lets a fixed arity be the fast case of a variadic one.

### Calling by name

**Icon:** `proc(s)` is the procedure, builtin or operator named by the string `s` (`proc("+", 2)`);
**string invocation** `"write"(x)` and `"+"(1, 2)` calls it directly; `function()` generates the
names of the builtins; `name(v)` and `variable(s)` go between a variable and its name; `invocable`
tells the linker what string invocation may reach.

**FunL today:** a function or builtin named without a call is a value, and `import * as m` gives a
module as a map of its names, so `m(name)` is calling by name *within a module*. A string followed
by `(…)` is indexing it — `"write"(1)` is the character `r`, by the [one way to
index](language.md#data) — so string invocation is closed to FunL.

**Proposed:** `proc(name)` and `proc(name, n)` — the function value the program's top level, then
the builtins and the prelude, give that name, optionally that arity; an operator's spelling, `"+"`,
gives its section `(+)`; it fails for a name nothing defines. `builtins()` generates the builtins'
names. `invocable` is not needed: nothing is linked away. **`name(v)` and `variable(s)` are not
adopted**: a local is a numbered slot, resolved when compiled, and has no name at run time.

> **Open question I6 — reflection by name.** (a) `proc` over top-level names and builtins only, no
> `variable`/`name`, no string invocation; (b) also `variable(s)` for top-level variables, keeping
> a name table for the top-level frame. **Recommendation: (a).** Top-level names are already in a
> table the compiler builds; locals by name would need a name per slot at run time for a feature
> almost no Icon program uses. String invocation would reopen indexing and buys nothing over
> `proc(s)(…)`.

## Co-expressions

**Icon:** `create e` makes a co-expression: `e`, with a copy of the current procedure's locals,
ready to run on a control stack of its own. Nothing runs yet.

- **`@C` activates it**: `e` runs until it produces a result, which becomes the value of `@C`, and
  `C` is suspended. The next `@C` resumes `e` for its next result. When `e` runs out, `@C` fails,
  and every later `@C` fails too. `@C` itself produces at most one value: it is not resumed by
  backtracking in the activator.
- **`x @ C` transmits `x`**: `x` becomes the value of the `@` inside `C` at which `C` last gave up
  control. On the first activation there is no such `@`, and `x` is discarded; likewise when `C` is
  resumed because its expression produced a result (it is resumed by backtracking, and the value has
  nowhere to go). A co-expression can activate any other, `&main` included, so co-expressions are
  symmetric coroutines: `@&source` gives control back to the activator and waits for its next value.
- **`^C` refreshes**: a new co-expression with `C`'s expression and the locals as they were when
  `C` was *created*, never yet activated. `C` itself is unchanged.
- **`*C`** is the number of results `C` has produced. `!C` generates the results of activating `C`
  until it fails; `|@C` does the same.
- **`&current`** is the running co-expression, **`&source`** the one that activated it, and
  **`&main`** the one the program started in.
- A local the co-expression assigns is its own copy; a global it assigns is everyone's.

**FunL today:** generators are resumed only by backtracking inside their own bounded expression,
so two generators cannot be stepped in lockstep, and a generator cannot be handed to some other part
of the program to be pulled from later. `async` tasks have a control stack of their own, but the
event loop decides when they run. The [modules chapter](modules.md#what-the-machine-needs-a-task-is-a-machine-set-aside)
names co-expressions as the nearest relative of a task, and the machinery is the same.

**Proposed: Icon's spelling.** `create` is a new word; `@`, `^` and `*` as prefix operators and
`@` infix are free in expressions:

```
val words = create !split(text)
val nums = create 1..

while w = @words
  write(@nums, ' ', w)

def consumer()
  repeat
    val line = @&source
    write('got ' + line)

val c = create consumer()
@c
'first' @ c
'second' @ c
```

- `@` already marks a named pattern, `p@(a, b)`, which is a name touching `@` inside a pattern.
  In an expression, `v @ c` is transmission; in a parameter or a `for` binding it is still the
  named pattern. The compiler decides when it knows which side of `->` or `<-` the text is on, as
  it already does for a lambda's parameters.
- `^c` is refresh and `*c` the count of results, matching `*x` as length (a co-expression's
  "length" is how far it has gone). `!c` generates by activation.
- **A fault inside a co-expression** ends it (later activations fail) and is raised again at the
  `@` that activated it, where a `catch` can take it — the rule `await` already follows for a
  task's fault. Icon stops the program; FunL lets the activator decide.
- `x is coexpression` tests for one; it prints as `<co-expression 3 (2 results)>`.

**Machine.** A co-expression is a **`Machine` set aside**, exactly as a task is: an operand stack,
control stack with its saved cells, trail, frame chain, scan state and regex captures, moved out
whole by `park` and back by `unpark`.

```
struct CoExprObj
    machine: Machine        // its state while it is not running
    start: *Chunk           // e, compiled as a lambda body is
    frame: *FrameObj        // the copy of the creating frame it runs in
    pristine: *FrameObj     // a second copy, for ^C
    activator: *CoExprObj   // &source
    results: usize          // *C
    task: u32               // the task that created it
    state: Fresh | Suspended | Done
```

- **`Create`** compiles `e` into its own chunk, as a lambda body is, ending in `CoYield`. At run
  time it copies the current frame — its slots, shallowly, keeping its static link, so locals are
  copied and anything further out is shared, which is Icon's rule — and keeps a second copy for
  refresh. Copying one frame is the whole cost of `create`.
- **`Activate`** parks the running machine into `&current`'s object, unparks the target's, and
  records `&current` as its activator. A fresh target starts at its chunk with the transmitted value
  dropped; one suspended at an `@` of its own gets the transmitted value written over that `@`'s
  operand slot — the slot an `await`'s promise occupies when a task resumes. A target that is
  `Done` makes `@` fail without switching.
- **`CoYield`** is the end of `e`: like `yield`, it pushes a `Choice` back into `e` first, then
  counts the result, parks the co-expression and unparks its activator with the result where the
  activator's `@` left its value. The next activation of a co-expression suspended here **fails into
  its own control stack**, which resumes `e` for its next value; the transmitted value is dropped,
  as Icon drops it. When `e`'s failure reaches the bottom of its control stack, the co-expression is
  `Done` and the activator's `@` fails.
- **`@` leaves nothing resumable in the activator**: it is one value or failure, a bounded call.
- **Refresh** makes a new object from `start` and a fresh copy of `pristine`.
- Switching is a swap of buffer handles, never a copy of a stack, so a co-expression costs one
  frame copy to make and nothing per value beyond a `yield`'s.

**With `async` tasks.** A co-expression is not a task: the loop never schedules it, and it runs
only when activated. It **runs as part of the task that activates it**. An `await` inside a
co-expression parks the running machine — the co-expression's — and the task waits; its activator
chain stays suspended in its objects, and when the promise settles, the task resumes inside the
co-expression. A co-expression belongs to the task that created it: activating it from another
task is the fault `permission_error(activate, coexpression, C)`, as binding another task's variable
is refused. Each task has its own `&main`, the machine it started on.

**What the collector sees.** A `CoExprObj` is a collected object whose tracer walks its parked
machine as the running one is walked, both frame copies, and its activator. The running chain is
reached from the VM's `&current` and the activator links; a co-expression parked inside a waiting
task is reached through the task's parked machine. A co-expression nothing refers to is collected,
its finalizer releasing its machine's buffers — a generator abandoned half way costs nothing more.

> **Open question I7 — co-expression spelling.** (a) Icon's `create`, `@c`, `v @ c`, `^c`, `*c`,
> `!c`; (b) JavaScript's generator objects: `create e` and methods `c.next()`, `c.next(v)`,
> `c.refresh()`, `c.count`. **Recommendation: (a).** The operators are free, short, and are what
> an Icon reader looks for; `@` keeps its named-pattern reading wherever a pattern is parsed. (b)
> fits FunL's handles but makes the symmetric `@&source` hand-off read as a method on a keyword.

> **Open question I8 — does `create` copy the locals?** Icon copies the creating procedure's
> locals, so a co-expression and its creator do not see each other's later assignments. A FunL
> lambda shares its frame. (a) **copy the innermost frame, share the frames outside it** (Icon's
> rule, and what `^c` needs to have something to go back to); (b) share, as a lambda does, and
> refresh restarts the expression only. **Recommendation: (a).** Sharing would make
> `create !xs` in a loop see the loop's later `xs`, and refresh would have nothing to restore.

> **Open question I9 — logic variables inside a co-expression.** A binding a co-expression makes
> is trailed on its own trail, so only its own backtracking undoes it; the activator's backtracking
> does not reach into a co-expression. (a) **a co-expression may bind any variable its task may**,
> with that consequence documented (a binding made inside `c` of a variable from outside survives
> the activator's backtracking, as an ordinary assignment would); (b) a co-expression may bind only
> variables it made, as a task may bind only its own. **Recommendation: (a).** It is the same
> visible rule as an assignment in a co-expression, and refusing it would make a co-expression
> useless for walking a relation's solutions, the obvious use.

## Scanning

### `&subject` and `&pos`

**Icon:** the scan's subject and position are the keywords `&subject` and `&pos`. Both can be
read anywhere, and assigned: `&subject := s` starts scanning `s` at position 1, and `&pos := i`
moves (failing when `i` is out of range). Outside any scan they are `""` and `1`. Assigning `&pos`
is an assignment, not undone by backtracking.

**Proposed:** the keywords as Icon has them (their spelling is [I18](#keywords)). Outside every
scan the machine's subject is `""` and its position 1, so they read as Icon's do. **`&pos = i` is
an ordinary assignment and `&pos <- i` the reversible one**, which Icon cannot say: the scan
position's existing trail entry is what `<-` pushes. An assignment outside a scan sets the outer
scan environment, which a scan saves on entry and puts back on leaving.

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

### Csets

The cset algebra is built (`+`, `-`, `**`, `complement`, `ascii`, `cset_all`; see
[scanning](../reference/scanning.md#character-sets)). What remains:

- **`lower` and `upper` as cset names beside `letters`.** Both names are already the case-mapping
  functions `lower(s)` and `upper(s)`, so neither can also be a named cset without a decision: a
  different spelling (`lowercase`/`uppercase`, `lower_letters`), or Icon's `&lcase`/`&ucase` only,
  under [I18](#keywords). Icon's keywords for the other named csets wait on I18 as well.

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

### Random numbers

**Icon:** `?i` is a random integer from 1 to `i` (`?0` a real in [0, 1)); `?s` a random character
of `s`; `?L`, `?R` a random element, assignable; `?S`, `?T` a random member or value. `&random` is
the seed, assignable, so a run can be repeated.

**Proposed:** **prefix `?`** as Icon has it — prefix `?` begins nothing in FunL, and infix `?`
stays scanning. `?n` counts from 1, Icon's dice-roll, since `n` is a count and not an index; `?c`
on a collection is a random element and is assignable on an array, buffer or mutable map. `random()`
is a real in [0, 1). `&random` reads and sets the seed; the generator is a documented one (sysl's),
not Icon's linear congruential one, so seeded runs repeat within FunL but not across to Icon.

### Bits, radix and conversion

| Icon | FunL | |
|---|---|---|
| `iand`, `ior`, `ixor`, `icom`, `ishift(i, j)` | the same names, over the whole integer tower as if two's complement were infinitely long; `ishift` left for positive `j`, right (flooring) for negative | new; Prolog has `/\`, `\/`, `xor`, `<<`, `>>` |
| `dtor`, `rtod` | the same | new |
| `&pi`, `&e`, `&phi` | `pi`, `e`, `phi` and the keywords | new |
| `16rFF` literal | see I16 | |

> **Open question I16 — radix literals.** Icon writes `16rFF` and `36rZZ`. FunL decided that a
> number literal touching a name multiplies, so `2r1` today is `2 * r1`. (a) **no `NrDDD` literal**:
> `0x`, `0o`, `0b` unlimited in size, and `integer("16rFF")` for the others; (b) `NrDDD` as a
> literal for `N` from 2 to 36, taking it away from juxtaposition. **Recommendation: (a).** `2r`
> meaning "twice r" is exactly the case juxtaposition was decided for; other bases are rare enough
> for a conversion.

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

**Icon's keywords are names that begin with `&`.** FunL needs a spelling for the ones it adopts:

| keyword | FunL |
|---|---|
| `&subject`, `&pos` | adopted, assignable ([scanning](#subject-and-pos)) |
| `&current`, `&source`, `&main` | adopted ([co-expressions](#co-expressions)) |
| `&lcase`, `&ucase`, `&letters`, `&digits`, `&ascii`, `&cset` | adopted |
| `&null`, `&fail` | adopted: `undefined` and `fail` |
| `&pi`, `&e`, `&phi` | adopted |
| `&input`, `&output`, `&errout` | adopted ([I/O](#input-and-output)) |
| `&random` | adopted, assignable |
| `&time` (CPU milliseconds), `&clock`, `&date`, `&dateline` | adopted, read from the host as `funl:time` reads it |
| `&version`, `&host`, `&progname` | adopted |
| `&features` | adopted: generates `"co-expressions"`, `"large integers"`, the platform, and each FunL feature built in (`http`, `async`, `profile`) |
| `&file`, `&line` | adopted: the source file and line where written, fixed when compiled |
| `&level` | adopted: the depth of calls, counted along the frame chain |
| `&trace` | adopted, assignable: while it is not zero, calls, returns, `yield`s, failures and resumptions are written to standard error, one line each, and it counts down per line as Icon's does; the check is a test on the call and return instructions only |
| `&collections`, `&allocated`, `&storage`, `&regions` | `&collections` and `&allocated` from sysl's collector; the region keywords are not adopted (sysl's heap has no regions); `collect()` asks for a collection |
| `&dump` | adopted: a non-zero value prints `display()`'s report when the program ends; `display()` prints the running function's named locals and the top-level names, from a name table the compiler keeps per chunk |
| `&error`, `&errornumber`, `&errortext`, `&errorvalue` | see I19 |
| `&col`, `&row`, `&window`, `&x`, `&y`, `&interval`, `&control`, `&meta`, `&shift` and the mouse-event keywords | graphics; see I21 |

**Machine:** `&name` is one token. Each keyword is either a constant folded when compiled, an
instruction reading a machine field (`&subject`, `&pos`, `&current`, `&level`), or a call of a
native. Assignable ones compile to a store instruction that checks the value's kind.

> **Open question I18 — how keywords are spelled.** (a) **`&name` as Icon has it, lexed as one
> token when `&` touches a letter**; conjunction keeps `&` with space after it, and `a &b`, which
> today is a conjunction, is refused with a note naming `a & b`; (b) a module of plain names,
> `import { subject, pos } from funl:icon`; (c) plain builtin names, `subject`, `pos`.
> **Recommendation: (a).** Keywords are a namespace a reader recognises at a glance, a program's
> names cannot shadow them, and the cost is one refusal for a spacing nobody writes. (c) collides
> with `pos(i)`, which is a different thing from `&pos`.

## Errors

**Icon:** a run-time error ends the program, unless `&error` is non-zero, in which case it
decrements `&error`, records the error in `&errornumber`, `&errortext` and `&errorvalue`, and turns
the error into **failure**. `runerr(i, x)` raises one; `errorclear()` resets the record.

**FunL:** a fault is not a failure; `e catch p -> r` takes one, with the error term in `p`, and a
recovery that fails turns it into failure — `e catch _ -> fail` is Icon's `&error` for one
expression. `throw(x)` and `error(msg)` raise. This is **covered**, by a [decided](language.md#every-expression-succeeds-or-fails)
design that scopes the conversion to an expression instead of a global counter.

> **Open question I19 — `&error` and `runerr`.** (a) nothing more: `catch` covers `&error`,
> `throw` covers `runerr`, and the error keywords are not adopted; (b) add `&error` as a global
> counter that makes every fault a failure, beside `catch`. **Recommendation: (a).** A global
> switch that turns faults into failures anywhere — in a library, in Prolog — breaks every
> guarantee the decided design makes about faults, and `catch` already says the same thing where
> it is meant.

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

> **Open question I21 — the graphics facilities.** (a) out of parity's scope: a later
> `funl:graphics` native module over a sysl binding, designed on its own terms when wanted;
> (b) in scope, with Icon's function set and keywords. **Recommendation: (a).** They are a
> library, not the language, and a window needs a design of its own (which thread owns it, how
> events reach the loop that runs `async` tasks) rather than a transcription of 1990s X11 calls.

## The questions, collected

| | question | recommendation |
|---|---|---|
| I1 | the loop `until` beside the range `until` | both, by position |
| I2 | a `case` expression | postfix `e match` with a clause block; no match fails; `(e)` pattern compares |
| I3 | limitation, `\` being integer division | the infix word `e limit n` |
| I4 | whose is a static | each closure value's |
| I5 | a variadic `def` beside name/arity | the family member for every count no fixed arity takes |
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
| I16 | radix literals | none; `0x`/`0o`/`0b` unlimited, `integer("16rFF")` |
| I17 | writing to a file | handle methods, `f.write(x)` |
| I18 | how keywords are spelled | `&name`, one token |
| I19 | `&error` and `runerr` | nothing more; `catch` and `throw` cover them |
| I20 | a preprocessor | conditional compilation only |
| I21 | the graphics facilities | out of scope; a later native module |
