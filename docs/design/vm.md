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

This chapter states precisely what state each entry saves, how the trail and the operand stack are
kept sound under backtracking, and a value model fit for a garbage-collected heap in sysl.

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
    scans: Buf[ScanSaved]   // the subject and position each open scan replaced, innermost last
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
    flags: u32              // regex flags when it was pushed
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
        flags = e.flags
        e.kind match
            Mark(_) | Choice(_) -> ip = e.ip; return
            _ -> ()                                   // MarkThrough, Restore, Catch: keep going
```

**An entry does not save the scan state; a change of it is trailed** (String scanning, below), so
failure that passes back over a movement of the position undoes it through `undo_trail_to`, with no
scan fields on the entry. Saving `subject` and `pos` in every entry would restore them at *every*
failure, which is wrong for a bounded expression that has completed: in `'abc' ? every 1 to 3 do
write(move(1))` the generator's choice point predates every `move`, so resuming it would put the
position back to 1 and write `a` three times, where Icon writes `a`, `b`, `c`. There are no
closures on the control stack, and one kind of choice point.

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

`FailIfFalse` is how [`false` fails a condition](language.md#conditions-and-false): it is emitted
after every condition and nowhere else. `ChangeMark` re-aims the current mark's resumption at the top of `|e`, so that
when `e` runs out of values in one round the next failure starts it again. It also makes the
enclosing mark the innermost again — what follows `|e` belongs to the enclosing expression — and
pushes a `Restore` for the reason a generating call's return does: the code after `|e` pops cells
below the round's mark while `e`'s choice points are still resumable (`a + |e`).

**`every` is the clearest picture of the whole machine**: its body ends in `Fail`, the failure
backtracks into the most recent generator in `e`, that generator produces its next value and the
body runs again, until every generator in `e` is exhausted and the failure lands on `every`'s own
mark — which passes it on, so `every` fails, or jumps to its `else`. `while` ends the same way: when
its condition fails the loop fails, which the enclosing statement absorbs.

### Leaving several marks at once

**`break`, `continue` and `return` have to discard every mark between them and their target.** Counting
the marks emitted and emitting that many `Unmark`s is correct exactly as long as every construct
that pushes a mark remembers to count it — a new construct that forgets turns into a corrupted
control stack with no diagnostic.

So the compiler records instead of counting. A loop's head stores the index of its mark in a frame slot
with `SaveMark(slot)`, and `break` is `UnmarkTo(slot)`, which truncates to that recorded entry and
puts back the mark register it recorded. `return` truncates to the frame's own entry height, which
the frame records when it is entered. Nothing has to agree with anything else about a count.

## The operand stack, and why backtracking has to copy part of it

**An immutable operand stack would let a choice point save the whole stack by keeping a pointer to
it, and restoring it would cost nothing.** A sysl `Buf` is mutable: once
a choice point has been pushed, the instructions after it pop cells *below* the height it was pushed
at and push others in their place. In `a + !xs`, the generator's choice point is pushed with `a`
beneath it; `+` then pops `a`. Restoring "the height" on backtracking would leave the sum where `a`
belongs.

**So it copies.** A `Choice` entry saves the operand cells between the innermost mark's floor
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

> **Decided — how is the operand stack restored?**
> **Decision: copy the bounded expression's cells into the entry, and push a `Restore` on a
> non-deterministic return**, as above. The cost is a copy of a few cells per choice point and
> nothing per ordinary call. **Rejected: a persistent operand stack** — cons cells on the
> collected heap — where a choice point saves one pointer and nothing is
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
are exhausted, failure reaches the `yield`'s choice point and the function carries on from `after`
with `()` as the `yield`'s value — or, where the `yield` ends the body, with `Fail`: the function
has nothing more to produce. `Return` truncates the operand stack to the frame's base after pushing
its `Restore`, so a `yield` inside an expression (`(yield 5, 6)`) leaves the caller none of that
expression's operands.

**A body expression's value is returned with `Return`, so its generators survive the return.** That
is the whole mechanism by which a function whose body generates is a generator.
[`return e`](language.md#functions-are-generators-when-their-body-is) is `ReturnCommit`, which
keeps only the first value.

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
element. `i to j by k` is the same with `(current, last, step)` and no collection at all. The
cursor of `!c` is two cells, what is left of the collection and a position in it, for every kind of
collection alike: a list's position stays 0 while its cell moves down the list. A generator whose
advanced cursor is already exhausted pushes no choice point, so its last element leaves nothing
behind.

**A cursor is immutable.** Keeping a mutable iterator in the saved stack and advancing it on each
resumption works only while every choice point is restored at most once, which would silently stop
being true the first time anything copied a choice point. An immutable cursor makes the question not
arise.

## Cut, `Atomic`, and why the barrier is never global

**Cutting means discarding the entries above a recorded height without failing.** It is how a regex
atomic group and lookaround commit, how Prolog's `!` commits to a clause, and how `->` commits to its
condition's first solution.

```
SaveHeight(slot)    slot = control.len
CutSlot(slot)       control.truncate(slot), saved likewise; the trail is NOT unwound
CutClause           control.truncate(frame.cut)
```

A regex's atomic group and lookaround keep the height in an operand cell instead (`Barrier`, `CutTo`),
which is just as private to the construct: nothing else can reach a cell a regex pushed.

**The height lives in a frame slot or in the frame itself, never in a machine register.** Cut and mark
points kept in global registers would let a cut inside a called predicate leak into the caller and a
nested `->` overwrite the outer one's point. A height saved where the construct that needs it can
find it — a slot for `SaveHeight`, `frame.cut` for a clause, an operand cell for a regex — cannot be
overwritten by anybody else.

## The trail

**The trail records what failure has to put back that the control entries do not already save.**

```
enum Undo
    Bind(var: *VarObj)                          // a logic variable was bound
    Assign(place: Place, old: Value)            // x <- e overwrote a slot or an element
    Scan(subject: Value, pos: usize)            // the scan state changed, and was this
    ScanOpened                                  // a scan began, pushing onto `scans`
    ScanClosed(subject: Value, pos: usize)      // a scan ended or was left, popping this off `scans`
    Capture(group: usize, old: Span)            // a regex capture group was set
```

`undo_trail_to(h)` pops entries above `h` and reverses each: a `Bind` makes its variable unbound
again, an `Assign` writes the old value back, a `Scan` puts the subject and position back,
`ScanOpened` pops `scans` and `ScanClosed` pushes its state back on, and a `Capture` restores the
old span.

### A separate trail, because a commit is not an undo

**Were the trail the control stack, `x <- e` would push an entry whose only content was a closure
restoring the old value, and failure would run it on the way past.** That makes the undo records
indistinguishable from choice points, so **every commit throws undo records away**: a cut and an
`Unmark` both truncate the control stack, closures and all. For Icon's reversible assignment
that happens to be right at an `Unmark` — the assignment becomes permanent when its bounded
expression completes. It is wrong for a cut, and it is fatal for logic variables: a variable bound
after an older choice point and then cut past must still be unbound when that older choice point is
resumed. That is why the WAM keeps a trail separate from its choice points, and so does this machine.

**A commit therefore filters the trail instead of truncating it:**

| entry above the commit | kept? |
|---|---|
| `Bind(v)` | **kept** if `v` is older than the entry now on top of the control stack (`v.stamp < top.stamp`); otherwise dropped, nothing left being able to see the binding undone |
| `Assign` | **dropped at `Unmark`** — the bounded expression is over, so the assignment is permanent; **kept at a cut**, which does not end a bounded expression |
| `Scan`, `ScanOpened`, `ScanClosed` | as `Assign`: a change of the scan state is a reversible assignment |
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

**The scan state is two registers, `subject` and `pos`, and every change of it is a reversible
assignment, trailed** (`set_scan`, an `Undo.Scan` of the state it replaces). As Icon defines `tab`
and `move`, failure that passes back over a change undoes it, and a bounded expression that
completes makes it permanent. Control entries carry no scan fields, because restoring the scan
state at every failure is wrong twice over: `'abc' ? every 1 to 3 do write(move(1))` must write
`a`, `b`, `c`, not `a` three times, and in

```
'ab cd' ?
  while tab(upto(' '))
    move(1)

  write(tab(0))
```

the loop ends by its condition failing, and the `move` its body made must survive that failure for
`tab(0)` to write `cd`.

```
ScanBegin       subject' = pop'd string; push (subject, pos) on scans; subject = subject'; pos = 1
ScanEnd         (subject, pos) = pop scans                 -- the scan's value stays on the stack
ScanLeave(n)    pop n off scans; (subject, pos) = the last popped, the outermost
```

Each push and pop of `scans` is trailed (`ScanOpened`, `ScanClosed`) as well as the change of
`subject` and `pos`. If the scanned expression later resumes — `every write(text ? upto(vowel))` —
failure back into it undoes the `ScanEnd`: the saved state goes back on `scans` and the subject and
position become the scan's again, where its own `ScanEnd` will find them.
### Leaving a scan by a jump

**`return`, `break`, `continue` and `yield` restore the scan state the outermost scan they leave
replaced**, as Icon restores the outer `&subject` and `&pos` whichever way control leaves `s ? e`.
Without it the inner subject would stay in force after the jump: `ScanEnd` never runs, and the
commit the jump makes (`UnmarkTo`, `ReturnCommit`) ends a bounded expression and so drops the `Scan`
record that failure would have used to put the outer state back.

The compiler knows how many scans a jump leaves, because a scan cannot cross a function: it counts
the scans of the function's body open around the code (`Ctx.scans`), and each loop records the count
at its own level. A `return` or `yield` leaves all of them, and a `break` or `continue` the ones
opened since its loop; where that is not zero, `ScanLeave(n)` precedes the jump.

- **`break` and `continue`**: `ScanLeave(n); UnmarkTo(slot)`. The restore is trailed and the
  `UnmarkTo` commit makes it permanent, keeping what the loop moved in its own scan; a `break`'s
  value is computed afterwards, outside the scans, as in Icon.
- **`return e`**: `e; ScanLeave(n); ReturnCommit`. `e` is evaluated inside the scans — `s ? return
  tab(0)` returns the rest of `s` — and the caller continues in its own scan.
- **`yield e`**: `Choice(after); e; ScanLeave(n); Return`. Resuming the generator fails back past
  the `ScanLeave`, which puts the generator's scans back on `scans` and its subject and position in
  the registers.

A generator in an enclosing scan, resumed after the jump, sees its own subject again either way: its
choice point predates the jump, and failing back to it undoes everything the jump trailed.

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
choice point**, and since every movement of `pos` is trailed, there is no separate pattern choice
point.

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
points behind `pos`; it is matched backwards *from* `pos`. The compiler takes a direction with each
pattern, and a lookbehind compiles its body in the opposite one.

> **Decided — lookbehind is JavaScript's.** A lookbehind is matched right to left from the
> position, as ES2018 specifies, so inside it alternatives, repetition, group captures,
> backreferences and atomic groups resolve as a right-to-left match does: the alternative tried
> first is the one taken, a repeated group keeps its leftmost turn, and a backreference sees a group
> written to its right.

> **Decided — a backreference to an unset group matches the empty string**, as in JavaScript: one
> reached before its group, or to a group skipped, not taken, or backtracked out of (the capture
> trail unsets it again), matches empty rather than failing.

**Semantics are leftmost-first backtracking, Perl's and JavaScript's, not POSIX's leftmost-longest.**
A machine whose alternatives are tried in order cannot be leftmost-longest without exploring every
match, and goal-directed evaluation wants the order anyway. [The implementation
chapter](implementation.md#regex-the-att-data-and-tests-worked-by-hand) says what that means for the AT&T
conformance data, which is written for POSIX.

### The combinators are the same patterns

`string(s)`, `ccls(c)`, `rep(p)`, `rep1(p)`, `repn(n, p)`, `opt(p)` and the reluctant forms build
pattern trees at compile time, as macros over the pattern AST, so `rep(ccls(digits))` and `` `[0-9]*` `` compile to the same
instructions.

## The value model

```
enum Value
    Undefined
    Unit
    Null
    Int(i64)                        // the fast path of the integer tower
    Big(*NumObj)                    // a dal Number holding a BigInt, boxed
    Rat(*NumObj)                    // a dal Number holding a Rational, boxed
    Real(f64)
    Dec(*DecObj)                    // sysl.math.decimal.Decimal, boxed -- not built: dal has no Dec yet
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

> **Decided — how are strings stored?**
> **Decision: UTF-8 bytes plus a character count and a forward cursor**, slate's arrangement:
> the count is written when the string is made and never goes stale (strings are immutable), a
> one-byte-per-character string indexes by arithmetic, and a character position is found by walking
> forward from the last one asked for. Scanning moves forward almost always, so the cursor makes a
> scan linear. Regex reverse mode steps back over UTF-8, which is unambiguous because a FunL string
> is validated when it is made. **Rejected: one 32-bit code point per character**, which makes
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

Integer `/` produces a `Rat` when the division is not exact; `\` is integer division, which floors (the quotient rounded toward negative infinity, so `-7 \ 2`
is `-4`). Comparison
across kinds is exact — `1/3 < 0.3333333333333333` compares the rational with the double's exact
binary value — and **equality across kinds is numeric** for FunL's `==` (`1 == 1.0` succeeds) while
**unification is not** (`1 ~ 1.0` fails), for the reason the [logic chapter](logic.md) gives.

**The tower is not FunL's: it is the `sysl-lang/dal` package**, sysl's counterpart of the Scala DAL —
a tagged number with the promotion, overflow and demotion rules above, exact comparison across
kinds, and printing by exact digit generation rather than `printf`, so a real prints as the shortest
decimal that reads back as the same double. It is built on the standard library's
`sysl.math.bigint`, `sysl.math.decimal` and `sysl.math.rational`. FunL's arithmetic and Prolog's
`is/2` both call it; what FunL adds is only the mapping between `dal`'s number and the `Value` enum.

**The mapping keeps the two word-sized kinds inline and boxes the two that own storage.** `Int` and
`Real` are `Value` variants of their own, so the instruction that adds two integers matches on the
value it already has and never builds a `dal` number at all: `+`, `-`, `*` and the orderings on two
`Int`s are a checked operation in the machine, and only a result that overflows, or an operand of
another kind, goes through `dal`. A `Big` or a `Rat` is a collected `NumObj` holding `dal`'s
`Number` -- the object is the `BigInt`'s only owner, as the value model asks -- under a tag of its
own, so a test of kind (`integer/1`, an integer-only operation's check) never reads the object. One
`Num(*NumObj)` variant for both was the alternative; it would make every such test a load.

**Two policies over one tower** (`number.sysl`): FunL's operators run under `dal.funl()`, where
`7 / 2` is `7/2`, and Prolog's `is/2` under `dal.iso_prolog(prefer_rationals)`, where `7 / 2` is
`3.5` -- or `7r2` with SWI-Prolog's `prefer_rationals` flag on -- and `//`, `mod`, `rem` and `div`
refuse a float. A refusal from `dal` becomes the ISO error term its README maps it to:
`evaluation_error(zero_divisor)`, `(float_overflow)`, `(undefined)`, `type_error(integer, X)`, and
`resource_error(memory)` for a power or a shift past 2^26 bits.

**Both readers take a float literal and an integer past 64 bits** -- `2.5`, `1e10`, `2e-1`,
`123456789012345678901234567890` -- carried as text until it becomes a constant; an integer written
in a radix still has to fit 64 bits. FunL prints a real as `dal` writes it, the shortest decimal that
reads back as the same double, always with a `.0` or an exponent (`5.0`, `1.0e+22`); Prolog writes a
rational as `7r2`.

## What the collector has to see

**Every structure in this chapter holds values the collector must treat as live**, and the most
dangerous ones are the ones that look like bookkeeping: a choice point keeps a frame alive, a frame
keeps its closure's environment alive, the `saved` buffer keeps operand cells alive that are nowhere
on the operand stack any more, and the trail keeps old values alive that no variable holds. All of
them are roots. The [implementation chapter](implementation.md#the-collector-and-its-roots) lists
them and says when a collection may run.
