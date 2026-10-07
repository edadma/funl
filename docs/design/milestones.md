---
title: Milestones
weight: 60
---

# Milestones

**The order is chosen so that the riskiest idea is proven first.** The new thing in this design is
not a functional language on a backtracking machine — the old implementation already was one — it
is unification and a trail on that machine, and the claim that a relation and a generator are the
same thing on the control stack. So the first milestone builds only as much of FunL as a logic
program needs, and runs one. Everything after it widens a machine whose core has already been
proven under backtracking, binding and collection at once.

**Every milestone ends the same way**: the full suite green from the project root, the new behaviour
covered by tests that would have failed before it, and the commit fast-forwarded onto `dev`. Each
names the decisions it rests on, so that revisiting one says which milestones it reaches.

## 1. The smallest machine that runs the family tree

**Rests on:** the unification operator, the rule marker, the atom literal, `free`, the
operand-stack strategy.

- The repository: `package.hocon`, the module, `main.sysl`, `tests_kit.sysl`, the `Vm` struct and its
  census test, the open builtin registry.
- Values: `Int`, `Str`, `Atom`, `Compound`, `Tuple`, `Nil`, `Cons`, `Unit`, `Var` — enough for facts —
  each a `gc` object kind with its tracer, and the root function covering every row of the [roots
  table](implementation.md#the-collector-and-its-roots) that exists yet.
- The front end for: `def` facts and `:-` rules, `def` blocks, rule bodies as blocks of conjuncts,
  `free`, `every … do`, `if … then … else`, `&`, `|`, `not`, `!=`, `~`, `#atom`, calls, `write`.
- The resolver, including the relation variable rule and singleton warnings.
- The machine: `Mark`, `MarkThrough`, `Choice`, `Restore`, `Unmark`, `UnmarkKeep`, `Fail`, operand
  copying, heap frames, `Call`, `Return`, `TailCall`; the trail with `Bind`, conditional trailing by
  stamp, and commit filtering; unification; `GetVar`, `GetValue`, `GetConst`, `GetCompound`.

**Done when:** `examples/family_tree.fl` prints exactly the output the [logic
chapter](logic.md#the-family-tree-natively) gives, duplicates included; `X ~ X` terminates; a
binding made after a choice point is undone when that choice point resumes, through an `Unmark` in
between; a relation returning into a caller with operands below its marks resumes correctly; and the
same tests pass against a heap small enough to collect inside every query.

## 2. FunL's core

**Rests on:** `false` in conditions, whether `return` is bounded, juxtaposition.

- The numeric tower: `Big`, `Rat`, `Real`, `Dec`, promotion and demotion, exact printing.
- Functions: committed-choice clauses, guards, `where`, lambdas, closures, sections, partial function
  literals, tail calls.
- Generators: `!`, `to`/`until`/`by`, `|e`, `yield`, a body that generates; immutable cursors.
- Control: `while`, `repeat`, `for` with patterns and filters, labelled `break`/`continue` with
  `SaveMark`/`UnmarkTo`, `return`.
- Data: lists, sets, maps, mutable maps, arrays, buffers, ranges, lazy lists, comprehensions,
  records and `data`; `in` and `not in`; `\x` and `/x`; `x <- e` with `Assign` trail entries.

**Done when:** every `FunLTests`, `FunLExamples` and `FunLPredefTests` program except the five
scanning and regex ones passes as a ported corpus file, and each rough edge the language chapter
lists as fixed has a test of the fixed behaviour.

## 3. String scanning

- `subject` and `pos` saved by every entry; `ScanBegin`, `ScanEnd`, `?=`.
- `tab`, `move`, `pos`, `upto`, `many`, `any`, `match`, `find`, and csets.
- The string representation the [strings question](vm.md#strings) settles.

**Done when:** `string scan 1`, `string scan 2` and `data backtracking 1` pass, and a generator
inside a scan resumed after the scan has ended sees the right subject and position.

## 4. Regex in the machine

**Answer needed first:** the strictness of the regex oracle.

- The regex literal parser and the pattern combinators, both producing one pattern tree.
- Compilation in both directions; the character, class, literal, group and backreference
  instructions; `Capture` trail entries; lookahead, lookbehind, atomic groups, flags.

**Done when:** `regex 1` and `regex 2` pass; the AT&T data passes under the agreed tiers with its
exclusion list; the PCRE2 comparison agrees on every pattern PCRE2 accepts; and lookbehinds PCRE2
refuses — unbounded, alternatives of different lengths, nested — have tests of their own.

## 5. The two directions of the call

- A function called from a rule body; an unbound variable reaching a function pattern as an
  instantiation error naming the function.
- A function seen as `f/(n+1)`, generators as multiple solutions.
- `findall`, `bagof`, `setof`, with the collected list held in an operand cell.
- First-argument indexing.

**Done when:** each direction has a test; `findall` over a FunL generator and over a relation both
collect correctly under the stress heap; and a recursive relation walking a 100,000-element list
finishes with a control stack whose height does not grow — the indexing has made it deterministic.

## 6. Prolog

**Answer needed first:** the term mapping for FunL-only kinds.

- The reader with a run-time operator table, in `sh.sysl.parsing` if it generalises.
- Clause compilation and the control constructs: `,`, `;`, `->`, `*->`, `\+`, `!` through
  `frame.cut`, `call/N` with its own barrier, `catch`/`throw` with `Catch` entries.
- ISO error terms for every runtime error, FunL's included.
- `is` and arithmetic comparison; the standard order of terms; the database with generations;
  the builtin set, the library predicates written in Prolog.
- `consult` and `import` of `.pl` files; the shared name/arity namespace and its collision refusal.

**Done when:** `examples/family_tree.pl` prints the same output as the FunL version; every 2019
defect has a named regression test and it passes; a Prolog clause and a FunL relation call each
other in both directions.

## 7. Conformance

- The INRIA ISO suite, with every expected outcome met or listed with a reason.
- The classic benchmark programs, for their answers.
- The SICP examples as a FunL corpus.
- Grammar rules (`-->`) and `phrase`.

**Done when:** the suite runs the INRIA file as a test and the reasons file is short enough to read.

## 8. From design to reference

- Each design page rewritten as `docs/reference/` and `docs/library/` pages whose programs the suite
  runs, the way slate's are, with a census of how many blocks each page carries.
- The README: what FunL is, how to install it, the family tree as the taste.
- The first release: `stable`, a tag, a GitHub release, and a Homebrew formula.

**Blocked on one thing outside the code:** the GitHub organisation, which the repository's remote,
the module path and the tap all wait on.
