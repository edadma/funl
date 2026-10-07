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
tests/funl/             FunL programs with the output each must print
tests/prolog/           Prolog programs, the INRIA suite, the 2019 regressions
tests/regex/            the AT&T data files and the exclusion list
examples/               family_tree.fl, family_tree.pl, sicp/, and the old examples
```

**Branches are `dev` and `stable`**, as slate's are: work lands on `dev` by fast-forward after the
full suite is green on the commit being merged, and a release is `stable` fast-forwarded to `dev`,
a tag, and a GitHub release. **Implementation notes go in `CLAUDE.md`**, which is never committed;
**the language goes in `docs/`**; the history goes in the git log.

**The module is a directory and its name comes from a domain**, a sysl rule — and the executable is
named `funl` after the package, so nothing at the root may be called `funl` (slate's
`dev.slatelang.slate` exists for exactly this reason).

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
| the driver | `main.sysl` (arguments, the top level, a status), `repl.sysl` |
| the scanner tier | `lex.sysl` (FunL tokens over `sh.sysl.parsing`'s cursor, interpolation, regex literals), `tok.sysl` (the token kinds and their binding powers) |
| FunL parsing | `parse.sysl` (the `Parser`, statements, declarations), `parse_expr.sysl` (the `pratt` callbacks), `parse_def.sysl` (clauses, guards, `where`, facts and rules), `pattern.sysl` (parameter and `for` patterns), `regex_parse.sysl` (regex literal syntax to a pattern tree) |
| the tree | `ast.sysl` |
| resolving | `scope.sysl` (names to slots, relation variables, singleton warnings, mixed-kind refusal) |
| compiling | `emit.sysl` (the chunk being built, labels, constants), `compile_expr.sysl`, `compile_control.sysl` (marks, loops, `every`, `if`, alternation), `compile_def.sysl` (committed-choice clauses, relation clauses, heads), `compile_regex.sysl` (both directions) |
| the machine | `vm_state.sysl` (`struct Vm`, `current()`), `op.sysl` (the instruction set), `run.sysl` (the dispatch loop), `control.sysl` (entries, `fail`, marks, cut, `Restore`), `trail.sysl`, `frame.sysl`, `gen.sysl` (cursors: `!`, `to`/`until`, and `|e`'s `ChangeMark`) |
| logic | `unify.sysl` (unification, dereferencing, standard order), `copy.sysl` (`copy_term`, `findall`'s copies) |
| values | `value.sysl`, `obj.sysl` (heap objects and constructors), `atom.sysl` (the intern table), `collector.sysl` (kinds, roots, the schedule) |
| numbers | `number.sysl` (mapping `dal`'s number to and from `Value`), `arith.sysl` (FunL's operators over `dal`), `render.sysl` (printing values) |
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
| `sysl-lang/dal` | the numeric tower: `Int` → `Big` → `Rat` → `Real` → `Dec`, promotion, overflow, demotion, exact comparison and printing, over the standard library's `bigint`, `rational` and `decimal` |

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
about. The old FunL parser was Scala parser combinators with packrat memoisation; none of it is
carried over except the grammar it encodes.

- **The `TokenStream` `pratt` runs on is the parser**, so a callback can complain about what it read
  and can reach the scope table.
- **A parse error is a node**, so one pass reports every mistake in a file through a `Report`.
- **Indentation is `layout`'s**, Python-style off-side lines, with brackets suspending it. The tokens
  nominated to open a block when they end a line are `->`, `=`, `then`, `else`, `do`, `?` and `:-`,
  so a lambda or a scan written as an argument can still have an indented body.
- **The binding-power ladder is the old grammar's**, lowest first: assignment forms (`=`, `<-`, `?=`,
  `+=` …) · scanning `?` · `or` · `and` · `not` · comparison, `in`, `is` · alternation `|` ·
  conjunction `&` · `fail` / `break` / `return` / `yield` · cons `:` (right) · ranges `..` and `to` ·
  `+ -` · `* / \ % // mod div` · `^` (right) · prefix `-` · `++ --` · prefix `! \ /` · application,
  field and index.
- **`::`, `#`, `~` and `:-` are tokens.** `::` was used by the old grammar and never lexed.

> **Decided — juxtaposition as multiplication.**
> The old grammar multiplied any two adjacent operands, so `2n` is `2 * n` — and so is `f (x)`
> wherever a space separates a name from a bracket. **Decision: juxtaposition multiplies only
> a number literal immediately followed by a name or `(`**, with no space — `2n`, `3(x + 1)`, which
> is every use in the old tests. **Rejected:** keep the general rule, and with it the parse in
> which a stray space changes a call into a multiplication.

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

**The dangerous rows are the second, third and fourth**, because they look like bookkeeping rather
than data. A choice point that has been pushed and not yet discarded keeps alive the frame of a
function that returned long ago, the cells of an expression that has moved on, and every value its
trail entries would restore. They are the reason a long `every` keeps its generator's state and
nothing else: the statement's `Unmark` is what lets all of it go.

**A `Value` in a sysl local is not a root** — slate's hardest-won rule, and it applies here with
more force, because a native here may run a FunL callback (`findall`, `sort` with a comparator, a
FunL function called from a Prolog goal) that collects. A native keeps what it needs on the operand
stack or holds it on the shadow stack across any call that can collect, and the tests run the
corpus against a deliberately tiny heap so that collections land inside such calls.

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
| the old machine's *design*: the control stack, marks, clause marks, `yield` as choice-then-return, regex compiled forward and reverse | the 2021 Scala implementation |
| the old test programs and the AT&T regex data | `funl-2021`'s tests |

| written fresh | why not reused |
|---|---|
| the instruction loop, control stack, trail | slate's machine is coroutine-shaped; the old machine is Scala |
| the operand-stack copying and the `Restore` entry | the old machine had an immutable stack and never needed them |
| unification, relations, the Prolog front end | nothing to reuse: the 2019 engine is a different machine with the defects the Prolog chapter lists |
| the regex engine | it is part of the machine; an external engine cannot be resumed |

## How it is tested

**`sysl test .`, from the project root, runs everything**, under `caffeinate -dimsu` on this machine.
The sysl tests drive the machine in the same process — `tests_kit.sysl` runs a program and answers
what it printed — so a test is a sentence in shouting case and an assertion about output.

### The old tests are the first corpus

**`FunLTests` (≈35 tests), `FunLExamples` (≈40) and `FunLPredefTests` are ported as programs** under
`tests/funl/`, one file per old test with the name it had, and its expected output beside it. They
are the behavioural specification, so a port that changes an expected output is a decision about
the language and says so in the commit — the three places the rough-edge table in the [language
chapter](language.md#rough-edges-and-what-the-rewrite-does-about-each) changes behaviour are the
only ones expected. **The SICP examples** under the old repo's `examples/sicp-code` are the second
corpus, ported the same way once the core passes.

### Regex: the AT&T data, and a second oracle

**The old regex tests were generated from Haskell's `regex-posix-unittest` data** — the AT&T
`testregex` files `basic3`, `class`, `forced-assoc`, `left-assoc`, `nullsub3`, `osx-bsd-critical`,
`repetition2`, `right-assoc` and `totest`. The data files are vendored under `tests/regex/` with their
provenance, and one sysl test reads them directly rather than generating a test per line.

**The old harness was weaker than it looks**: it collected *every* way the pattern could match and
passed if *any* of them had the expected spans. That accepts an engine that finds the expected
match only on its fourth backtrack, and it cannot tell leftmost-first from leftmost-longest at all —
which matters, because the AT&T data is written for POSIX and this engine is not POSIX.

> **Decided — how strict is the regex oracle?**
> **Decision: three tiers, all exact.** *Match or no match* must agree with the data on every
> line. *The overall span of the first match* must agree on every line where POSIX leftmost-longest
> and leftmost-first coincide, which is most of them. *Every capture span* must agree on every line
> not in `tests/regex/exclusions.txt`, a file listing each line where the two semantics genuinely
> differ, each with the reason, which the test reads — so the list cannot grow without a sentence
> being written. **And a second oracle for the semantics this engine actually has**: the same
> patterns run through PCRE2 (`sysl-lang/pcre2`), as a test-only dependency, compared span for span,
> because a single oracle is an opinion and this one is about a different semantics.
> **Rejected:** keep the old any-match rule, which passes today's data and proves little.

### Logic and Prolog

- **The family tree runs in both syntaxes and prints one expected output**, duplicates included, as
  the [logic chapter](logic.md#the-family-tree-natively) gives it.
- **Every 2019 defect is a named regression test**, written before the feature that fixes it.
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
- **A collector stress run**: the whole corpus again against a heap small enough that a collection
  happens every few hundred allocations, so that a value held only in a sysl local, or a root
  missing from the list above, fails a test instead of a user's program.
- **Error paths are tested with the features**: every instantiation error, type error and existence
  error the chapters name has a test asserting the sentence it prints.
