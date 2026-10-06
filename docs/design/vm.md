---
title: The machine
weight: 20
---

# The machine

**FunL runs on one backtracking virtual machine, and everything that can be resumed is resumed the
same way:** a generator asked for its next value, the second arm of an alternation, a function
suspended at `yield`, a scan position moved by `tab`, a regex trying its next alternative, and — in
the [next chapter](logic.md) — a relation trying its next clause. All of them push an entry on one
control stack, and failure pops entries until one of them says where to go next.

The old machine already worked this way. What this chapter adds is a precise statement of the
state each entry saves, the two places the old design was unsound (the trail and the operand
stack), and a value model fit for a garbage-collected heap in sysl.

## The machine's state

```
struct Machine
    code: *Chunk            // the instructions being run
    ip: usize               // the next instruction
    stack: Buf[Value]       // the operand stack
    frame: *FrameObj        // the running function's frame (heap allocated)
    control: Buf[Entry]     // marks, choice points, restore points, catch frames
    saved: Buf[Value]       // operand cells saved by control entries, in step with `control`
    trail: Buf[Undo]        // what failure has to put back
    mark: usize             // index in `control` of the innermost mark
    stamp: u64              // bumped by every entry pushed; dates logic variables
    subject: Value          // the string being scanned
    pos: usize              // the scan position in it
    flags: u32              // regex flags in force (case-insensitive, dot-all, ...)
    captures: Buf[Span]     // regex capture groups, one per numbered group
```

**There is no host recursion anywhere in the instruction loop.** A FunL call does not become a
sysl call, a resumption does not unwind a sysl stack, and a `yield` does not capture one. That is
the precondition for everything below: the whole of a suspended computation is data — entries,
saved cells, frames — and data can be restored, collected and inspected.

## The control stack

```
struct Entry
    kind: EntryKind         // Mark(on_fail) | MarkThrough | Choice(alt) | Restore | Catch(handler)
    ip: usize               // where to resume, for Mark / Choice / Catch
    code: *Chunk
    frame: *FrameObj
    mark: usize             // the enclosing mark, put back on failure
    floor: usize            // operand stack height this entry restores to
    saved_at: usize         // where its saved cells start in `saved`
    trail: usize            // trail height when it was pushed
    stamp: u64              // machine stamp when it was pushed
    subject: Value          // scan state when it was pushed
    pos: usize
    flags: u32
```

Five kinds, each a few lines of behaviour:

| kind | pushed by | on failure |
|---|---|---|
| `Mark(on_fail)` | the start of a bounded expression with somewhere to go if it fails | restore, then jump to `on_fail` |
| `MarkThrough` | a bounded expression whose failure is its enclosing context's business | restore, keep failing |
| `Choice(alt)` | anything that can produce another result: alternation, a generator, a relation's next clause, a regex alternative | restore, then resume at `alt` |
| `Restore` | a call that returns while leaving entries behind (below) | restore, keep failing |
| `Catch(handler)` | a `catch` | keep failing; it only matters to `Throw` |

**Failure is one loop:**

```
fail()
    loop
        val e = control.pop()
        undo_trail_to(e.trail)
        stack.truncate(e.floor)
        stack.append(saved[e.saved_at..])
        saved.truncate(e.saved_at)
        frame = e.frame; code = e.code; mark = e.mark
        subject = e.subject; pos = e.pos; flags = e.flags
        e.kind match
            Mark(_) | Choice(_) -> ip = e.ip; return
            _ -> ()                                   // MarkThrough, Restore, Catch: keep going
```

**Every entry saves the scan position, the subject and the regex flags**, which is three words and
makes every movement of the scan position reversible without the movement doing anything. The old
machine instead had `tab` and `move` each push an extra entry whose only job was to run a closure
putting the position back (`pushChoice(vm => { vm.seq = …; vm.scanpos = … })`), and distinguished
"pattern" choice points, which saved the position, from ordinary ones, which did not. In the
rewrite there is one kind of choice point and no closures on the control stack.

## Marks: how a bounded expression is built

**A mark is the boundary of a bounded expression.** `Mark` pushes one and records it as the
innermost; `Unmark` ends the bounded expression by discarding *every* entry above and including that
mark — so whatever generators ran inside it can never be resumed — and drops the expression's value.
`UnmarkKeep` does the same and keeps the value.

```
Unmark
    val m = control[mark]
    commit_trail(m.trail)       // see "The trail"
    saved.truncate(m.saved_at)
    control.truncate(mark)
    stack.truncate(m.floor)
    mark = m.mark
```

With `Mark`, `Unmark`, `Choice` and `Fail`, every control structure of the [language
chapter](language.md) is a short pattern of instructions:

```
statement s                    Mark(next); s; Unmark; next:

if c then a else b             Mark(else); c; FailIfFalse; Unmark; a; Branch(end); else: b; end:
if c then a                    MarkThrough; c; FailIfFalse; Unmark; a

a | b                          Choice(second); a; Branch(end); second: b; end:

a & b                          a; Pop; b

not e                          Mark(ok); e; Unmark; Fail; ok: Push(())

every e do b                   MarkThrough; e; Pop; MarkThrough; b; Unmark; Fail
every e do b else x            Mark(other); e; Pop; MarkThrough; b; Unmark; Fail; other: x

while c do b                   top: MarkThrough; c; FailIfFalse; Unmark; Mark(top); b; Unmark; Branch(top)

|e                             top: MarkThrough; e; ChangeMark(top)
```

`FailIfFalse` is the instruction the [conditions open question](language.md#conditions-and-false)
turns on: with the recommendation taken it is emitted after every condition, and without it it is
not emitted at all. `ChangeMark` re-aims the current mark's resumption at the top of `|e`, so that
when `e` runs out of values in one round the next failure starts it again.

**`every` is the clearest picture of the whole machine**: its body ends in `Fail`, the failure
backtracks into the most recent generator in `e`, that generator produces its next value and the
body runs again, until every generator in `e` is exhausted and the failure lands on `every`'s own
mark — which passes it on, so `every` fails, or jumps to its `else`. `while` ends the same way: when
its condition fails the loop fails, which the enclosing statement absorbs.

### Leaving several marks at once

**`break`, `continue` and `return` have to discard every mark between them and their target.** The
old compiler counted the marks it had emitted (`markNesting`) and emitted that many `Unmark`s, which
is correct exactly as long as every construct that pushes a mark remembers to count it — a new
construct that forgets turns into a corrupted control stack with no diagnostic.

The rewrite records instead of counting. A loop's head stores the index of its mark in a frame slot
with `SaveMark(slot)`, and `break` is `UnmarkTo(slot)`, which truncates to that recorded entry and
puts back the mark register it recorded. `return` truncates to the frame's own entry height, which
the frame records when it is entered. Nothing has to agree with anything else about a count.

## The operand stack, and why backtracking has to copy part of it

**The old machine's operand stack was an immutable Scala list**, so a choice point could save the
whole stack by keeping a pointer to it, and restoring it cost nothing. A sysl `Buf` is mutable: once
a choice point has been pushed, the instructions after it pop cells *below* the height it was pushed
at and push others in their place. In `a + !xs`, the generator's choice point is pushed with `a`
beneath it; `+` then pops `a`. Restoring "the height" on backtracking would leave the sum where `a`
belongs.

**The rewrite copies.** A `Choice` entry saves the operand cells between the innermost mark's floor
and the top into `saved`, and failure writes them back. The cells between a mark and the top are
the operands of one bounded expression — two or three values in an ordinary program — so the copy is
small, and `saved` is one buffer used as a stack, so it allocates nothing once warm. This is how
Icon's own implementation works: suspension duplicates the current expression frame.

**Why the innermost mark's floor is far enough down.** Between pushing a choice point and resuming
it, the only cells that can change are those above the floor of the bounded expression the choice
point sits in — that expression's `Unmark` discards the choice point before anything outside it
runs. Every cell below the floor is therefore exactly as it was.

**One case breaks that, and it is a call.** A function whose body generates returns to its caller
with choice points still on the stack, and the caller then pops its own operands — cells below the
callee's marks — with those choice points still resumable. So **a return that leaves entries behind
pushes a `Restore` entry** saving the caller's operand cells from the caller's innermost mark to the
top. Backtracking meets the `Restore` first, puts the caller's cells back, keeps failing into the
callee's choice point, which puts the callee's cells back above them, and the callee resumes as if
it had never returned. A call that returns leaving nothing behind — almost every call — pushes
nothing.

> **Open question — how is the operand stack restored?**
> **Recommendation: copy the bounded expression's cells into the entry, and push a `Restore` on a
> non-deterministic return**, as above. The cost is a copy of a few cells per choice point and
> nothing per ordinary call. **Alternative: a persistent operand stack** — cons cells on the
> collected heap, as the old machine had — where a choice point saves one pointer and nothing is
> ever copied. It is simpler to get right and costs one heap allocation per push, on every
> expression whether it backtracks or not, which is the common case paying for the rare one.

## Frames, slots and closures

**A frame is a heap object, not a region of the operand stack.**

```
struct FrameObj
    chunk: *Chunk
    up: *FrameObj           // the frame this function was defined in (static link)
    caller: *FrameObj       // the frame to return to (dynamic link)
    return_ip: usize
    caller_mark: usize      // the caller's innermost mark, put back on return
    entry: usize            // control height when this frame was entered
    cut: usize              // control height a clause-level cut returns to
    slots: [Value]          // parameters and locals, numbered at compile time
```

Three things force the heap. A **closure** captures the frame it was made in and may outlive it. A
**suspended function** — a generator after `yield` — is resumed with its frame exactly as it was. And
**a choice point** names the frame its alternative runs in, which may be any frame that is still
reachable. A frame on a stack region would have to be copied out in all three cases; a frame on the
heap is simply kept alive by whatever refers to it.

**Locals are numbered slots, resolved at compile time.** `LoadSlot(depth, index)` walks `depth`
static links up and reads one slot; a closure is a chunk plus the frame it was made in. This is the
idea slate uses — names resolved once, by the compiler — without slate's complication of deciding
which names can live in operand-stack cells, because here every frame is already where a closure
can reach it.

**An ordinary assignment to a slot is not undone by backtracking**, which is Icon's rule and FunL's.
A generator resumed after a later assignment sees the later value. `x <- e` is the one assignment
that is undone, through the trail.

**What a heap frame costs is an allocation per call.** That is the price of the three properties
above and it is accepted. If a measurement later shows it matters, the remedy is a compile-time
proof that a given function can neither be captured, nor suspended, nor leave a choice point — such
a frame can come from a free list and go back to it on return.

### Calls, returns and `yield`

```
Call(argc)          callee and arguments on the stack; a closure gets a new frame with
                    caller_mark = mark, entry = cut = control.len; a native runs in place
Return              value = pop; if control.len > frame.entry, push a Restore for the caller;
                    mark = frame.caller_mark; ip = frame.return_ip; frame = frame.caller; push value
ReturnCommit        as Return, but first truncate control to frame.entry: a deterministic exit
TailCall(argc)      when control.len == frame.entry (nothing resumable was created in this frame),
                    reuse the frame; otherwise an ordinary Call
```

**`yield e` is `Choice(after); e; Return`.** The choice point is pushed *before* `e`, so if `e`
itself generates, its choice points are newer and are resumed first: `yield permute_(n - 1, a)`
yields every value of the inner call before the outer one continues past its `yield`. When they
are exhausted, failure reaches the `yield`'s choice point and the function carries on from `after`.

**A body expression's value is returned with `Return`, so its generators survive the return.** That
is the whole mechanism by which a function whose body generates is a generator.
[`return e`](language.md#functions-are-generators-when-their-body-is) is `ReturnCommit` under the
recommendation, and `Return` under the alternative.

**Committed-choice clause selection** is a mark per clause:

```
clause 1:   Mark(clause2); match parameters; guard; FailIfFalse; UnmarkKeep; body; Return
clause 2:   Mark(clause3); ...
last:       Mark(nomatch); ...
nomatch:    Error("argument match not found")
```

Failure while matching or in the guard reaches the clause's mark and moves to the next clause;
`UnmarkKeep` commits, so failure in the body is the body's own. The [logic chapter](logic.md) gives
relations the other choice: `Choice` per clause and no commit.

## Generators without mutable iterators

**A generator's state is an immutable cursor on the operand stack**, never a mutable object. `!c`
compiles to `GenStart; GenNext`:

```
GenStart        replace c with its first cursor: a list is its own cursor, an array becomes
                (array, 0), a range (current, last, step), a string (string, 0), a lazy list itself
GenNext         read the cursor; if exhausted, pop it and Fail; otherwise compute the element and
                the next cursor, overwrite the cursor with the next one, push Choice(this GenNext),
                then overwrite the cursor cell with the element
```

The choice point's saved cells hold the *advanced* cursor, so resuming at `GenNext` produces the next
element. `i to j by k` is the same with `(current, last, step)` and no collection at all.

**The old machine kept a mutable Scala iterator in the saved stack** and advanced it on each
resumption. That works only while every choice point is restored at most once, which is true today
and would silently stop being true the first time anything copied a choice point. An immutable
cursor makes the question not arise.

## Cut, `Atomic`, and why the barrier is never global

**Cutting means discarding the entries above a recorded height without failing.** It is how a regex
atomic group and lookaround commit, how Prolog's `!` commits to a clause, and how `->` commits to its
condition's first solution.

```
Barrier(slot)       slot = control.len
CutTo(slot)         control.truncate(slot), saved likewise; the trail is NOT unwound
CutClause           control.truncate(frame.cut)
```

**The height lives in a frame slot or in the frame itself, never in a machine register.** The 2019
Prolog engine kept its cut and mark points in global registers, so a cut inside a called predicate
leaked into the caller and a nested `->` overwrote the outer one's point. A height saved where the
construct that needs it can find it — a slot for `Barrier`, `frame.cut` for a clause — cannot be
overwritten by anybody else. The old FunL machine already did the right thing for regex, pushing
the height as a `Cut(size)` value onto the operand stack.

## The trail

**The trail records what failure has to put back that the control entries do not already save.**

```
enum Undo
    Bind(var: *VarObj)                          // a logic variable was bound
    Assign(place: Place, old: Value)            // x <- e overwrote a slot or an element
    Capture(group: usize, old: Span)            // a regex capture group was set
```

`undo_trail_to(h)` pops entries above `h` and reverses each: a `Bind` makes its variable unbound
again, an `Assign` writes the old value back, a `Capture` restores the old span.

### A separate trail, because a commit is not an undo

**In the old machine the trail was the control stack.** `x <- e` pushed an entry whose only content
was a closure restoring the old value, and failure ran it on the way past. That made the undo
records indistinguishable from choice points, so **every commit threw undo records away**: `CutInst`
and `UnmarkInst` both truncate the control stack, closures and all. For Icon's reversible assignment
that happens to be right at an `Unmark` — the assignment becomes permanent when its bounded
expression completes. It is wrong for a cut, and it is fatal for logic variables: a variable bound
after an older choice point and then cut past must still be unbound when that older choice point is
resumed. That is why the WAM keeps a trail separate from its choice points, and the rewrite does the
same.

**A commit therefore filters the trail instead of truncating it:**

| entry above the commit | kept? |
|---|---|
| `Bind(v)` | **kept** if `v` is older than the entry now on top of the control stack (`v.stamp < top.stamp`); otherwise dropped, nothing left being able to see the binding undone |
| `Assign` | **dropped at `Unmark`** — the bounded expression is over, so the assignment is permanent; **kept at a cut**, which does not end a bounded expression |
| `Capture` | kept while any entry remains |

**When the control stack is empty the trail is emptied**, nothing being left to undo anything for.
In practice that is the end of every top-level statement, so the trail never grows past one
statement's worth.

**Conditional trailing is what keeps it short.** Binding a variable created after the newest entry
needs no record — no surviving entry predates it — so `bind` compares stamps and trails only
variables older than the newest entry. The WAM makes the same test by comparing addresses; a
collected heap has no address order, so the variable carries the machine stamp from its creation
instead.

## String scanning

**The scan state is two registers, `subject` and `pos`, saved by every control entry.**

```
ScanBegin       push subject and pos as two operand cells; subject = pop'd string; pos = 1
ScanEnd         result = pop; pos = pop; subject = pop; push result
```

Nothing else is needed for reversibility. If the scanned expression later resumes — `every write(text
? upto(vowel))` — the generator's choice point inside it was pushed while `subject` was `text`, so
failure puts `text` and the right position back. The old machine needed two closures per scan to get
the same effect.

The builtins are natives reading and writing the two registers. `tab(i)` and `move(n)` set `pos` and
answer the substring; `upto(c)` and `find(s)` are generators, with a cursor exactly as `!c` has.

## Regex, compiled into the same machine

**A regex compiles to ordinary instructions, so it backtracks with the program around it.** That is
the reason it stays inside the VM rather than being handed to a library: an engine like PCRE2 or
libregexp returns one match and forgets how it got there, so

```
s ? (`(a|ab)(c|bcd)` & tab(0) & ok())
```

could never ask the pattern for its *next* way of matching when `ok()` fails. On this machine the
pattern's alternatives are choice points like any other, and `ok()` failing resumes them.

### The instructions

| forward | reverse | does |
|---|---|---|
| `Char(c)` | `CharRev(c)` | the character at (before) `pos` is `c`; step over it |
| `Class(set)` | `ClassRev(set)` | the same for a character class |
| `AnyChar` | `AnyCharRev` | any character, honouring dot-all |
| `Lit(s)` | `LitRev(s)` | a literal string, compared forwards (backwards from `pos`) |
| `Backref(n)` | `BackrefRev(n)` | the text of group `n` |
| `GroupStart(n)` | `GroupEnd(n)` | record where group `n` begins (in reverse, where it ends) |
| `GroupEnd(n)` | `GroupStart(n)` | record where group `n` ends; trails the old span |
| `AtStart`, `AtEnd`, `WordBoundary` | the same | anchors, unaffected by direction |
| `SavePos`, `RestorePos` | | push `pos` as an operand, put it back |
| `RepInit(lo, hi)`, `RepNext(body, exit)` | | a counted loop; the count is an operand cell |
| `SetFlags(on, off)` | | change `flags` for what follows |

`Choice`, `Barrier` and `CutTo` are the program's own, unchanged: **a regex alternative is just a
choice point**, and since every entry saves `pos`, there is no separate pattern choice point.

### Forward and reverse, and lookbehind with no restriction

**Every pattern is compiled in both directions.** In reverse mode a concatenation is compiled last
part first, each character test steps back over the character before `pos`, and a group records its
end before its start. Lookaround is then four short sequences:

```
(?=p)       Barrier(t); SavePos; p (forward); RestorePos; CutTo(t)
(?!p)       Barrier(t); Choice(ok); p (forward); CutTo(t); Fail; ok:
(?<=p)      Barrier(t); SavePos; p (reverse); RestorePos; CutTo(t)
(?<!p)      Barrier(t); Choice(ok); p (reverse); CutTo(t); Fail; ok:
(?>p)       Barrier(t); p; CutTo(t)
```

**A lookbehind may therefore be any pattern at all** — unbounded repetition, alternatives of
different lengths, nested lookaround, backreferences — because it is not matched by trying start
points behind `pos`; it is matched backwards *from* `pos`. PCRE2 refuses an unbounded lookbehind
outright. This is the old machine's design (`Compiler.compile(pat, mode)`, with
`LookbehindPattern` compiling its body with `!mode`) and the rewrite keeps it unchanged.

**Semantics are leftmost-first backtracking, Perl's and JavaScript's, not POSIX's leftmost-longest.**
A machine whose alternatives are tried in order cannot be leftmost-longest without exploring every
match, and goal-directed evaluation wants the order anyway. [The implementation
chapter](implementation.md#regex-the-att-data-and-a-second-oracle) says what that means for the AT&T
conformance data, which is written for POSIX.

### The combinators are the same patterns

`string(s)`, `ccls(c)`, `rep(p)`, `rep1(p)`, `repn(n, p)`, `opt(p)` and the reluctant forms build
pattern trees at compile time — the old implementation made them macros over the pattern AST, and
the rewrite does the same — so `rep(ccls(digits))` and `` `[0-9]*` `` compile to the same
instructions.

## The value model

```
enum Value
    Undefined
    Unit
    Null
    Int(i64)                        // the fast path of the integer tower
    Big(*BigObj)                    // sysl.math.bigint.BigInt, boxed
    Rat(*RatObj)                    // numerator and denominator, both BigInt
    Real(f64)
    Dec(*DecObj)                    // sysl.math.decimal.Decimal, boxed
    Str(*StrObj)
    Atom(u32)                       // interned; true and false are atoms
    Compound(*CompoundObj)          // a functor atom and arguments: records, Prolog terms
    Tuple(*TupleObj)
    Nil
    Cons(*ConsObj)                  // immutable lists, shared with Prolog
    Array(*ArrayObj)  Buffer(*BufferObj)  Set(*SetObj)  Map(*MapObj)  MutMap(*MutMapObj)
    CSet(*CSetObj)                  // a character set
    Range(*RangeObj)  Lazy(*LazyObj)
    Fn(*ClosureObj)  Native(u32)
    Pattern(*PatternObj)            // a compiled regex, both directions
    Var(*VarObj)                    // a logic variable: unbound, or bound to a Value
```

**A `Value` holds no reference-counted sysl member**, which is slate's rule and for its reason: a
value is copied constantly, and an ARC increment on every copy is a cost on every instruction. A
`BigInt` or a `string` lives inside a collected object that is its only owner, and the object's
finalizer releases it.

**`true` and `false` are the atoms `true` and `false`.** FunL tests them as booleans (`x is boolean`
answers whether a value is one of the two), and Prolog sees them as the atoms it already has, so the
two languages exchange booleans without a conversion.

**A `data` record is a compound term.** `data point(x, y)` makes `point` a constructor whose values
are compounds with functor `point/2`, and a nullary constructor is simply its atom. This is what
lets a FunL record unify with a Prolog term and print the same way in both.

### Strings

> **Open question — how are strings stored?**
> **Recommendation: UTF-8 bytes plus a character count and a forward cursor**, slate's arrangement:
> the count is written when the string is made and never goes stale (strings are immutable), a
> one-byte-per-character string indexes by arithmetic, and a character position is found by walking
> forward from the last one asked for. Scanning moves forward almost always, so the cursor makes a
> scan linear. Regex reverse mode steps back over UTF-8, which is unambiguous because a FunL string
> is validated when it is made. **Alternative: one 32-bit code point per character**, which makes
> every index O(1) and every string four times the size.

`pos` is held as a character position at the language surface and translated to a byte offset
through the cursor; the regex instructions work on byte offsets internally and never see a
character index at all.

### Numbers

**The tower is `Int` → `Big` → `Rat` → `Real` → `Dec`**: two integers, an exact fraction, a binary
float, and a decimal float at a set precision. An operation on two numbers is done in the higher of
their two kinds, with three rules on top:

- **An integer operation that overflows `i64` is redone in `Big`.** The check is the processor's
  overflow flag, which sysl's checked arithmetic exposes, so the common case costs one branch.
- **An exact result is demoted to the smallest exact kind that holds it**: a `Big` that fits `i64`
  becomes `Int`, a `Rat` with denominator 1 becomes an integer.
- **An inexact result is never demoted.** `Real` and `Dec` stay what they are.

Integer `/` produces a `Rat` when the division is not exact; `\` is integer division. Comparison
across kinds is exact — `1/3 < 0.3333333333333333` compares the rational with the double's exact
binary value — and **equality across kinds is numeric** for FunL's `==` (`1 == 1.0` succeeds) while
**unification is not** (`1 ~ 1.0` fails), for the reason the [logic chapter](logic.md) gives.

`sysl.math.bigint` supplies `BigInt` and `sysl.math.decimal` supplies `Decimal`; the rational type
is written here over `BigInt`, with every result normalised by the gcd. Printing follows slate's
`decimal.sysl` approach — exact digit generation rather than `printf` — so a real prints as the
shortest decimal that reads back as the same double.

## What the collector has to see

**Every structure in this chapter holds values the collector must treat as live**, and the most
dangerous ones are the ones that look like bookkeeping: a choice point keeps a frame alive, a frame
keeps its closure's environment alive, the `saved` buffer keeps operand cells alive that are nowhere
on the operand stack any more, and the trail keeps old values alive that no variable holds. All of
them are roots. The [implementation chapter](implementation.md#the-collector-and-its-roots) lists
them and says when a collection may run.
