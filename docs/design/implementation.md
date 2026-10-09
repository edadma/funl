---
title: The sysl implementation
weight: 50
---

# The sysl implementation

**FunL is a sysl program shaped like slate**: one package, one module, files under a thousand lines
cut at seams that few names cross, `sh.sysl.parsing` for the scanner tier, `sh.sysl.gc` for the
heap, and the language documented in `docs/` rather than in implementation notes. What it does
**not** take from slate is slate's interpreter core: slate's machine runs coroutines and its
generators are suspended machines, where FunL's whole execution model is the control stack of the
[machine chapter](vm.md). This chapter says which is which, file by file.

## The repository

```
README.md               what FunL is, how to install it, a taste
package.hocon           the package: name, version, the sysl floor, dependencies
<module path>/          the sources (below)
docs/design/            these pages
docs/reference/         the language as built, every program on it run by the suite
docs/library/           the builtins and the Prolog library, likewise run
tests/                  only published third-party suites, kept as published (below)
tests/regex/            the AT&T data files and the exclusion list
examples/               FunL's own programs, in `.funl` and `.pl`, written to be read
```

**A test's program is a string literal in the unit test**, with its expected output beside it.
`tests/` holds only third-party data, and `examples/` is untested by design: no test reads or runs it.

**Branches are `dev` and `stable`**, as slate's are: work lands on `dev` by fast-forward after the
full suite is green on the commit being merged, and a release is `stable` fast-forwarded to `dev`,
a tag, and a GitHub release. **Implementation notes go in `CLAUDE.md`**, which is never committed;
**the language goes in `docs/`**; the history goes in the git log.

**The module is a directory and its name comes from a domain**, a sysl rule (slate's
`dev.slatelang.slate` exists for the same reason).

**The repository is a workspace: the machine is a library at the root, and each executable is a
member over it.** A package whose root carries an entry file brings that file into every program
depending on it, and a program has one `main`, so neither driver can live in the library:

| member | builds | what it is |
|---|---|---|
| the root, package `funl-vm` | — | the module `io.github.edadma.funl`: the machine, both front ends, every test |
| `funl/` | `sysl build -p funl` → `funl/funl` | `main.sysl`: FunL programs, and `.pl` files through FunL's driver |
| `prolog/` | `sysl build -p prolog` → `prolog/prolog` | `main.sysl`: the standalone Prolog top level, over linenoise ([prolog.md](prolog.md)) |

The library is not called `funl` because the member that builds the `funl` executable is, and two
members may not share a name. `sysl test .` at the root tests all three.

**The module path is `io.github.edadma.funl`**, the repository's address `github.com/edadma/funl`
reversed, as every other edadma project spells its package. This document writes paths without the
prefix. A hyphen anywhere in the path is what such a name must avoid — `import dev.funl-lang.x`
stops at the hyphen.

## The files

**Seams are chosen by counting what crosses them**, which is slate's rule and its reason: `private`
in sysl is private to the file, so a cut that makes a dozen generic helpers module-wide is a bad cut
however even the halves look. The first layout, to be revisited by counting once the code exists:

| subject | files |
|---|---|
| the drivers | `funl/main.sysl` and `prolog/main.sysl`, members of the workspace; `toplevel.sysl` (the standalone Prolog's ISO top level) |
| the scanner tier | `lex.sysl` (FunL tokens over `sh.sysl.parsing`'s cursor, interpolation, regex literals), `tok.sysl` (the token kinds and their binding powers) |
| FunL parsing | `parse.sysl` (the `Parser`, statements, declarations), `parse_expr.sysl` (the `pratt` callbacks), `parse_def.sysl` (clauses, guards, `where`, facts and rules), `parse_loop.sysl` (the loops, their labels, `for` heads), `pattern.sysl` (parameter and `for` patterns), `regex_parse.sysl` (regex literal syntax to a pattern tree), `regex_tree.sysl` (the pattern tree and its character classes) |
| the tree | `ast.sysl` |
| resolving | `scope.sysl` (names to slots, relation variables, singleton warnings, mixed-kind refusal), `scope_walk.sysl` (reading one scope's code, hoisting), `scope_block.sysl` (blocks, loop targets, assignment targets) |
| compiling | `emit.sysl` (the chunk being built, labels, constants), `compile_expr.sysl` (`if`, alternation, statements, assignment), `settled.sysl` (which statements run without a mark), `compile_loop.sysl` (`every`, `while`, `repeat`, `for`, `break`, `continue`), `compile_def.sysl` (committed-choice clauses, relation clauses, heads), `compile_regex.sysl` (both directions) |
| the machine | `vm_state.sysl` (`struct Vm`, `current()`), `op.sysl` (the instruction set), `run.sysl` (the dispatch loop), `control.sysl` (entries, `fail`, marks, cut, `Restore`), `trail.sysl`, `frame.sysl`, `task.sysl` (parked machines, `start_async`, `await` and the ready queue), `gen.sysl` (cursors: `!`, `to`/`until`, and `|e`'s `ChangeMark`), `regex_run.sysl` (the regex instructions) |
| logic | `unify.sysl` (unification, dereferencing, standard order), `copy.sysl` (`copy_term`, `findall`'s copies), `index.sysl` (first-argument indexing, the instantiation check on a function's patterns) |
| values | `value.sysl`, `obj.sysl` (heap objects and constructors), `atom.sysl` (the intern table), `collector.sysl` (kinds, roots, the schedule), `seq.sysl` (lists, ranges and sets as values: `+`, `in`, indexing, `.length`, the list patterns, what a comprehension collects) |
| numbers | `number.sysl` (mapping `dal`'s number to and from `Value`, the FunL and ISO policies, `dal`'s refusals as ISO error terms), `arith.sysl` (FunL's operators over `dal`, with `Int` arithmetic inline), `render.sysl` (printing values) |
| builtins | `native_fn.sysl` (the registry), `natives.sysl` (which scope gets which), `scan.sysl` (`tab`, `move`, `upto` …), `collections.sysl`, `io.sysl` |
| Prolog | `pl_read.sysl` (tokens and the operator table), `pl_compile.sysl` (clauses and control constructs), `pl_db.sysl` (assert, retract, generations), `pl_arith.sysl` (`is` and comparison), `pl_builtins.sysl`, `pl_lib.sysl` (the library clauses, as Prolog source carried in a `raw"""` block), `pl_errors.sysl` (ISO error terms) |
| tests | `tests_kit.sysl` (run a program, capture what it printed) plus a `tests_*.sysl` per subject |

**The builtin registry is open from the start.** slate learned that a closed `enum` of builtins with
one central dispatch `match` makes `builtin.sysl` uncuttable — every implementation has to be
visible to the match. Here a builtin is `register(name, go)` written where it is implemented, and
dispatch is an index into a table.

**The machine's state is a `Vm` struct from the first commit**, reached through `current()`, and the
census test that slate wrote after the fact — every module-level `var` either `current()` itself, a
builtin id, or marked process-wide with a reason — is written on day one, while the count is zero.

## Dependencies

| | what for |
|---|---|
| `sh.sysl.parsing` | spans, the cursor, literal reading, interpolation, diagnostics and `Report`, `layout`, `pratt` |
| `sh.sysl.gc` | the heap: `Kind`s, `alloc`, `collect`, finalizers |
| `sysl-lang/dal` 0.1.1 | the numeric tower: `Int` → `Big` → `Rat` → `Real` (`Dec` is not in 0.1.x), promotion, overflow, demotion, exact comparison and printing, over the standard library's `bigint` and `rational` |

**No C library is bound**, and none is needed for the language: regex is the machine's own, numbers
are the standard library's, and there is no event loop. The binary needs nothing installed. A line
editor for the REPL is the first candidate for a binding, and it is not part of any milestone here.

**`sh.sysl.parsing` may be changed freely**, and the rule of the org applies: anything written for
FunL that another grammar could use — the runtime operator table for the Prolog reader is the
obvious candidate — goes into `parsing`, is released there, and is consumed here.

## The front end

**A hand-written recursive-descent parser with `pratt` for expressions**, which is slate's
arrangement and the `parsing` package's whole argument: what matters about a parser is what it says
about invalid input, and that is the part a generator or a combinator library cannot tell you
about.

- **The `TokenStream` `pratt` runs on is the parser**, so a callback can complain about what it read
  and can reach the scope table.
- **A parse error is a node**, so one pass reports every mistake in a file through a `Report`.
- **Indentation is `layout`'s**, Python-style off-side lines, with brackets suspending it. The tokens
  nominated to open a block when they end a line are `->`, `=`, `then`, `else`, `do`, `?` and `:-`,
  so a lambda or a scan written as an argument can still have an indented body. **A line ending in an
  infix operator continues onto the next**, which is indented deeper than the line the statement
  began on and opens and closes nothing (`layout`'s `continues`), so n-queens' condition runs over
  two lines with no bracket; `..`, `++` and `--` end a line as postfix operators and continue nothing.
- **The binding-power ladder**, lowest first: assignment forms (`=`, `<-`, `?=`,
  `+=` …) · scanning `?` · `or` · `and` · `not` · comparison, `in`, `is` · alternation `|` ·
  conjunction `&` · `fail` / `break` / `return` / `yield` · cons `:` (right) · ranges `..` and `to` ·
  `+ -` · `* / \ % // mod div` · `^` (right) · prefix `-` · `++ --` · prefix `! \ /` · application,
  field and index.
- **`::`, `#`, `~` and `:-` are tokens.** `::` is the typed pattern.

> **Decided — juxtaposition as multiplication.**
> Multiplying any two adjacent operands would make `2n` be `2 * n` — and `f (x)` too, wherever a
> space separates a name from a bracket. **Decision: juxtaposition multiplies only
> a number literal immediately followed by a name or `(`**, with no space — `2n`, `3(x + 1)`.
> **Rejected:** the general rule, and with it the parse in which a stray space changes a call into
> a multiplication.

**The resolver runs before the compiler** and does everything that needs to know what a name is:
numbering slots as `(depth, index)`, applying the [relation variable rule](logic.md#variables-in-a-relation),
warning about singletons, refusing a definition that mixes clause kinds, and allocating the hidden
slots `SaveMark` and `Barrier` write into.

## The collector and its roots

**Every heap value is a `gc` object with a `Kind`, and the root function marks everything the
machine can still reach.** `gc` never scans the sysl stack — sysl has no stack maps — so the
root function is the complete statement of what is live:

| root | why it is live |
|---|---|
| the operand stack, to its top | the expression being evaluated |
| `saved`, all of it | operand cells a choice point will write back |
| every control entry: `frame`, `subject` | a resumable alternative runs in that frame, on that subject |
| every trail entry: the variable, the old value, the assigned place's object | failure will write them back |
| the running frame, and through it the static and dynamic chains | the code running now and everything it returns to |
| globals | top-level names |
| every chunk's constant pool | strings, compiled patterns and literals the code pushes |
| the clause database | asserted clauses and their compiled chunks |
| the shadow stack | values a native is holding in sysl locals |
| every parked machine, each row above walked for it, and its promise | a task waiting at an `await` -- the awaited promise on its stack -- or for a task it started to give way |
| the running machine's promise, and every fault-settled promise nothing has awaited | what a task will settle, and the faults reported when the run ends |

**The dangerous rows are the second, third and fourth**, because they look like bookkeeping rather
than data. A choice point that has been pushed and not yet discarded keeps alive the frame of a
function that returned long ago, the cells of an expression that has moved on, and every value its
trail entries would restore. They are the reason a long `every` keeps its generator's state and
nothing else: the statement's `Unmark` is what lets all of it go.

**A `Value` in a sysl local is not a root** — slate's hardest-won rule, and it applies here with
more force, because a native here may run a FunL callback (`findall`, `sort` with a comparator, a
FunL function called from a Prolog goal) that collects. A native keeps what it needs on the operand
stack or holds it on the shadow stack across any call that can collect, and the tests run the
suite against a deliberately tiny heap so that collections land inside such calls.

**Collection happens at the dispatch loop's top, never inside `alloc`**, when allocation since the
last collection crosses a threshold. The threshold is raised after every collection to a multiple of
what survived, and it counts payload held outside the heap (string bytes, `BigInt` limbs) as well as
cells — slate's two lessons about scheduling, both of which cost it a performance bug to learn.

**What is not on the heap**: the syntax tree and the compiled code are ordinary reference-counted
sysl data, immutable and acyclic, and a heap object refers to a chunk by index. A counted `&T` may
not live in `gc` memory at all, which the `gc` package's README explains and slate's closures
already obey.

## What is reused, and what is written fresh

| reused | from where |
|---|---|
| the scanner tier, diagnostics, `layout`, `pratt` | `sh.sysl.parsing` |
| the collector | `sh.sysl.gc` |
| `BigInt`, `Decimal`, checked arithmetic | the sysl standard library |
| slate's *approach*: the `Vm` struct and its census, the open builtin registry, slot-numbered locals, exact number printing, a `Value` with no counted member, heap scheduling on live size and payload, docs that the suite runs, `dev`/`stable` | slate's design and `CLAUDE.md` |
| the AT&T regex data | Hackage's `regex-posix-unittest-1.1`, fetched from upstream (`tests/regex/SOURCE.txt`) |

| written fresh | why not reused |
|---|---|
| the instruction loop, control stack, trail | slate's machine is coroutine-shaped |
| the control stack's design: marks, clause marks, `yield` as choice-then-return, regex compiled forward and reverse | the [machine chapter](vm.md) |
| the operand-stack copying and the `Restore` entry | the operand stack is a mutable `Buf`, which a choice point cannot save by pointer |
| unification, relations, the Prolog front end | nothing to reuse |
| the regex engine | it is part of the machine; an external engine cannot be resumed |

## How it is tested

**`sysl test .`, from the project root, runs everything**, under `caffeinate -dimsu` on this machine.
The sysl tests drive the machine in the same process — `tests_kit.sysl` runs a program and answers
what it printed — so a test is a sentence in shouting case and an assertion about output.

### Programs are literals in the tests

**Every test's program is a string literal in the unit test**, with its expected output beside it,
and a test that needs a real file (consult, `:- import`) writes the literal to a scratch file first.
`tests/` holds only published third-party suites, kept as published; `examples/` holds FunL's own
programs for people to read, and no test reads or runs them. An example may show what a unit test
checks, and the test then holds its own copy as a literal.

### Regex: the AT&T data, and tests worked by hand

**The regex data is Haskell's `regex-posix-unittest` package** — the AT&T `testregex` files
`basic3`, `class`, `forced-assoc`, `left-assoc`, `nullsub3`, `osx-bsd-critical`, `repetition2`,
`right-assoc` and `totest`. The data files are vendored under `tests/regex/` with their provenance,
and one sysl test reads them directly rather than generating a test per line.

**A harness that collected *every* way the pattern could match and passed if *any* of them had the
expected spans would be weaker than it looks**: it accepts an engine that finds the expected
match only on its fourth backtrack, and it cannot tell leftmost-first from leftmost-longest at all —
which matters, because the AT&T data is written for POSIX and this engine is not POSIX.

> **Decided — how strict is the regex oracle?**
> **Decision: three tiers, all exact.** *Match or no match* must agree with the data on every
> line. *The overall span of the first match* must agree on every line where POSIX leftmost-longest
> and leftmost-first coincide, which is most of them. *Every capture span* must agree on every line
> not in `tests/regex/exclusions.txt`, a file listing each line where the two semantics genuinely
> differ, each with the reason, which the test reads — so the list cannot grow without a sentence
> being written. **What the AT&T data does not reach** — lookahead, lookbehind (matched right to left
> from the position, so of any length) and backreferences — is checked by unit tests in
> `tests_regex_lookaround.sysl`: a pattern and a subject as string literals, with the spans of the
> first match beside them, each worked out by hand under leftmost-first backtracking.
> **Rejected:** an any-match rule, which passes the data and proves little; and comparing against
> another regex engine, since this engine's semantics are its own and are stated by its tests.

### Logic and Prolog

- **The family tree runs in both syntaxes and prints one expected output**, duplicates included, as
  the [logic chapter](logic.md#the-family-tree-natively) gives it.
- **Every failure mode in the [Prolog chapter](prolog.md#failure-modes-of-a-prolog-engine-and-what-rules-each-one-out)
  is a named regression test**, written before the feature that guards it.
- **The INRIA ISO conformance suite** runs once the Prolog reader and core builtins exist, with
  every expected outcome either met or listed with a reason.
- **Interop has tests in both directions**: a Prolog clause calling a generating FunL function, a
  FunL `every` over a Prolog predicate, a `findall` over a FunL generator, a cut inside a predicate
  called from FunL that must not escape it.

### The machine itself

- **Every instruction sequence in the machine chapter is a test**, asserting the output of a program
  that exercises it — including the cases that motivated the design: a generating function returning
  into a caller with operands below its marks (the `Restore` entry), a cut past a binding that an
  older choice point must undo (the separate trail), an `every` over an `every` over a `yield`.
- **A collector stress run**: the whole suite again against a heap small enough that a collection
  happens every few hundred allocations, so that a value held only in a sysl local, or a root
  missing from the list above, fails a test instead of a user's program.
- **Error paths are tested with the features**: every instantiation error, type error and existence
  error the chapters name has a test asserting the sentence it prints.

## Measuring the machine

**Speed work is chosen from counts of what the machine executes, and the counter is a package
feature, `profile`, that is not in `default`** — slate's arrangement. `profile.sysl` defines
`CountingInstructions` as a constant the feature sets; the instruction loop in `run.sysl` bumps a
count per `Op` kind only behind `CountingInstructions && profile_on`, so in any other build the
optimiser deletes the branch and the loop is the one it would be with no counter in the source.
Measured at `-O2`, best of five on five loop and recursion programs, the build without the feature
ran within noise of the build before the counter existed (every program 0–4% faster, never slower).

```
sysl build -O2 -p funl --features profile
FUNL_PROFILE=1 funl/funl program.funl
```

**`FUNL_PROFILE` set to anything but `0` or the empty string** has `funl` (and `prolog`, built the
same way) write a table to standard error when the program ends, whatever its status: the total,
then each instruction kind executed, heaviest first, with its count and its share to a tenth of a
percent. A build without the feature answers the variable with two lines saying so, rather than an
empty table that would read as a program that ran nothing.

**The gate has a third shape**, beside `sysl test .` and `sysl test . --no-default-features`:
`sysl test . --features profile`, the shipped `default` plus the counter. `tests_profile.sysl`'s
exact-count tests exist only in a counting build, and the test that a build without the feature
counts nothing exists only outside one. Plain `sysl test .` turns on every feature the manifest
declares, `profile` included, so it is `--no-default-features` that tests the uncounted build.
