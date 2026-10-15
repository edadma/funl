---
title: Milestones
weight: 60
---

# Milestones

**The order is chosen so that the riskiest idea is proven first.** The new thing in this design is
not a functional language on a backtracking machine — that is well understood — it
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

**Done when:** a unit test holding the family tree as a string literal (the `.funl` program in
`examples/family_tree.funl` shows the same thing) prints exactly the output the [logic
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

**Done when:** every construct above has unit tests whose programs are string literals with their
expected output beside them, and each rough edge the language chapter lists as closed has a test of
the closed behaviour.

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
exclusion list; and lookahead, backreferences and lookbehinds — including unbounded ones,
alternatives of different lengths, nested — have tests of their own, with spans worked out by hand.

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

**Done when:** the family tree as a Prolog string literal in a unit test prints the same output as
the FunL one (`examples/family_tree.pl` shows it); every failure mode the Prolog chapter lists has a
named regression test and it passes; a Prolog clause and a FunL relation call each
other in both directions.

## 7. Conformance

- The INRIA ISO suite, with every expected outcome met or listed with a reason.
- The classic benchmark programs, for their answers.
- FunL's own example programs in `examples/`, written to be read; no test runs them.
- Grammar rules (`-->`) and `phrase`.

**Done when:** the suite runs the INRIA file as a test and the reasons file is short enough to read.

## 8. From design to reference

- Each design page rewritten as `docs/reference/` and `docs/library/` pages whose programs the suite
  runs, the way slate's are, with a census of how many blocks each page carries.
- The README: what FunL is, how to install it, the family tree as the taste.
- The first release: `stable`, a tag, a GitHub release, and a Homebrew formula.

**Blocked on one thing outside the code:** the GitHub organisation, which the repository's remote,
the module path and the tap all wait on.

## 9. Modules and native modules

**Rests on:** [modules and native modules as slate does](modules.md) (user, 2026-10-08); **answers
needed first:** none; the [eleven questions](modules.md#questions-decided) are decided, and the
[prelude's](modules.md#the-prelude--the-first-module-written-in-funl) are decided but P3, the
generator of tuples.

- Module files: `export`, the three import forms, `import * as`, the module as an immutable map,
  exports as a snapshot, once per machine; the refusals (a circle, a name not exported, a name
  nothing binds), each drawn against its own file; imported names under the builtin shadowing rule.
- Relations across modules and Prolog's view: `export` of a procedure, qualified relation calls
  resolved statically, `:- import` seeing exports, `json:parse(T, V)` from Prolog.
- The built-in module table, the prefixed natives, source modules carried as `raw"""` blocks; the
  `Handle` and `Bytes` kinds with their tracers and finalizers; native generators over a handle.
- `funl:fs`, `funl:json`, `funl:process`, `funl:time`, `funl:sqlite` in the core; `funl:http` behind
  the `http` feature, with `left_out_module`; a FunL `catch` before `funl:http`.
- [Packages](packages.md): the manifest, the cache, `funl.sum`, `funl add`, `funl fetch`,
  `funl vendor`, `funl deps`; its open questions answered first.
- [The prelude](modules.md#the-prelude--the-first-module-written-in-funl), the first module written
  in FunL: the Haskell-style list library as a source module carried in the binary and auto-imported
  (shadowable by a program's own names), the natives for speed (`reverse`, `sort`, …), and the
  `take`/`map`/`filter` rule of a collection in and a list out, a thunk in and a generator out.

**Done when:** every refusal and every module has unit tests whose programs are string literals (a
test writes its module files to a scratch directory first); `sysl test .` and `sysl test .
--no-default-features` are both green; a row generator abandoned by a bounded context releases its
statement under the stress heap; and `otool -L` of a `funl` built with `--no-default-features`
names nothing beyond the system's own libraries and SQLite.

## 10. `async` and `await`

**Rests on:** [`async` and `await` as slate does](modules.md#async-and-await) (user, 2026-10-08);
milestone 9, whose native modules supply the I/O; **answers needed first:** none; decided questions 9–11 of [modules](modules.md#questions-decided).

- Tasks: a parked `Machine` per suspended call, `start_async` from both call paths, `Await` parking
  with the promise left on the operand stack, every parked machine a collector root.
- The loop on kairos: the two queues in their order, timers keeping the program alive, the top level
  as an async context; `funl:async` with `sleep`, the promise makers and the promise-shaped I/O.
- Faults across `await`: a FunL `catch` around an `await` catches a fault raised after it resumes;
  unawaited failures reported.

**Done when:** slate's interleaving example, rewritten in FunL as a string literal, prints the same
order; a failure after an `await` does not redo it, and a generator that awaits between its `yield`s
is backtracked into correctly; and the same tests pass against a heap small enough to collect while
tasks are parked.

## 11. Icon parity

**Rests on:** [Icon parity](icon.md) (user, 2026-10-14: everything Icon 9.5 has); milestone 10,
whose parked machines co-expressions reuse; I1–I21, all decided (user, 2026-10-15) and collected in
[The language § Icon parity](language.md#icon-parity). **Every item is done except item 9, files**,
which waits on sysl alpha.14 (its work is parked on the branch `m11-files`), and the three keywords
of item 10 that wait on it.

Each item is one agent's deliverable: built, tested with string-literal programs (an error path
included), its `docs/reference/` or `docs/library/` page written with programs the suite runs, its
section of `icon.md` removed, and fast-forwarded onto `dev`. In dependency order:

1. **String functions**: `repl`, `left`/`center`/`right` with a pad, `char`, `ord`, `entab`,
   `detab`, the cset argument of `trim`/`trim_start`/`trim_end`, `image`, `type`, `str`. Done.
2. **Conversions and literals**: `integer`, `real`, `numeric` from strings (`16rFF` included);
   `0x`/`0o`/`0b` literals past 64 bits; the `NrDDD` literal (I16), a base outside 2 to 36 refused
   as a radix. Done.
3. **Set and cset algebra**: `+`, `-` on csets, `**` on sets and csets, `complement`, the
   complement flag, `lower`/`upper`/`ascii`/`cset_all`. Done.
4. **Buffers as deques, and deletion**: `push`, `pop`, `get`, `pull`, `put`; `delete`, `m -= k`,
   `s -= x`; `array(n, x)`; `copy`; `same`; `sort(m)` and the prelude's `sortOn`. Done.
5. **Random numbers and bits**: prefix `?` (assignable on mutable collections), `random()`, the
   seed; `iand`, `ior`, `ixor`, `icom`, `ishift`; `dtor`, `rtod`, `pi`, `e`, `phi`. Done.
6. **Assignment forms**: the `op=` rule (`//=`, `**=`) and several-target reversible assignment
   `a, b <- b, a`. Done.
7. **Scanning additions**: prefix `=s`, `bal`. Done.
8. **Standard streams**: `read()`, `reads(n)`, `writes`, `ewrite`, `ewrites`, `stop`; `chdir` in
   `funl:fs`. Done.
9. **Files**: `open` and the file handle's methods in `funl:fs`, `!f`, pipes over `funl:process`;
   `funl:term` with `getch`, `getche`, `kbhit` ([icon](icon.md#input-and-output); I17 decided:
   handle methods). **Open**: waits on sysl alpha.14; parked on the branch `m11-files`.
10. **Keywords**: the `&name` token and refusal of `a &b`; `&subject` and `&pos` assignable (`&pos
    <- i` reversible), the csets, `&null`, `&fail`, the constants, `&random`, the clocks,
    `&version`, `&host`, `&progname`, `&features`, `&file`, `&line`, `&level`. Done, except
    **`&input`/`&output`/`&errout`, which wait on item 9**.
11. **`until` loop** (I1: both, by position). Done.
12. **Limitation**: `e limit n` with `LimitSet`/`LimitCount`, re-limiting when `n` generates (I3).
    Done.
13. **`match`**: the postfix block, failing on no match, and `(e)` value patterns (I2). Done.
14. **`static` and `initial`**: the statics object on the closure, `LoadStatic`/`StoreStatic`/`Initial`
    (I4). Done.
15. **Variadic functions and spread**: `..rest` parameters, `f(..xs)` resolved at run time, `arity`
    (I5). Done.
16. **Calling by name**: `proc`, `builtins()` (I6). Done.
17. **Co-expressions, the machine**: `create` (frame copies), `Activate`, `CoYield`, transmission,
    `^`, `*`, `!`, faults raised at the `@`, `&current`/`&source`/`&main` (I7, I8). Done.
18. **Co-expressions with tasks and logic**: `await` inside a co-expression, the cross-task
    activation fault, bindings on a co-expression's own trail, and collection of abandoned
    co-expressions under the stress heap (I9). Done.
19. **Tracing and introspection**: `&trace`, `display()`, `&dump`, `collect()`, `&collections`,
    `&allocated`, `&storage`, `&regions`, the per-chunk name table, and the per-chunk line table
    that places a trace line at the call site and a fault at its line. Done.
20. **Answers that reopen a decision**: `translate` (I11), `reverse` of a string (I12), `with`
    (I15), assignment to a string section (I10). Done.
21. **The preprocessor**: Icon's whole preprocessor (I20), over the token stream, with positions
    through `$include`. Done.

**Done when:** an Icon program of each kind in the chapter — a co-expression pipeline, a scanner
with `bal` and `&pos`, a `static` memo, a variadic logger, a file filter reading standard input —
has a FunL translation as a string literal in a unit test that prints what Icon prints; every
`icon.md` section is gone, and the page with it. **The programs are done** (`tests_icon_programs.sysl`,
with a sixth over `limit`, `until`, `match`, `proc`, `&trace`, `translate` and the preprocessor);
`icon.md` holds only item 9's section and the three keywords that wait on it.

## 12. Graphics

**Rests on:** [Graphics](graphics.md) (user, 2026-10-15: I21 in scope); milestone 9's handles and
generators; milestone 10's loop; milestone 11's `&name` keywords (item 10) and stream methods (item
9); **answers needed first:** G1–G12, and two org findings built and released before the items that
name them: **F1** (plutovg's font-face cache bound) and **F2** (a kairos driver that waits on a
second source).

Each item is one agent's deliverable, built, tested on hidden canvases with string-literal programs
(an error path included), its part of `docs/library/graphics.md` written with programs the suite
runs, its section of `graphics.md` removed, and fast-forwarded onto `dev`. In dependency order:

1. **The canvas and the context**: `funl:graphics` in the module table, the `window` handle kind
   (canvas plus context), `open` with `canvas: #hidden` over plutovg, `set`/`get`/`attributes` with
   their failures and faults, colour specifications and `color_value`, `pixel`, `image_bytes`,
   `clone`, `couple`, `uncouple`, `close` closing every handle on the canvas. Waits on G1, G2, G3,
   G5, G6, G12.
2. **Drawing**: `draw_point`, `draw_line`, `draw_segment`, the rectangles, polygons, circles and
   arcs drawn and filled, `draw_curve`, `erase_area`, `copy_area` (overlapping included), `clip`,
   `dx`/`dy`, negative extents, `linestyle`, `fillstyle` with Icon's patterns. Waits on item 1.
3. **`drawop: #reverse`**: the coverage mask and the exclusive-or; drawing twice restores. Waits on
   item 2 and G7.
4. **Images and palettes**: `read_image`, `write_image` (PNG, the extension refusal), the `image`
   attribute, `draw_image` with Icon's image strings, the palette functions in FunL source,
   `new_color` failing. Waits on item 1 and G8.
5. **Fonts and text**: the `"fixed"` face carried in the binary, family lookup through the font-face
   cache, `font` and its failure, the metric attributes, `text_width`, `draw_string`; the text cursor
   with `goto_rc`, `goto_xy`, `write`, `writes` and scrolling. Waits on item 2, G4 and F1.
6. **Events on a hidden canvas**: the queue, `enqueue`, `event()`, `pending`, `active`, every event
   code, the keywords `&x`, `&y`, `&row`, `&col`, `&interval`, `&control`, `&shift`, `&meta`,
   `&released`, `&window` and the event-code keywords, per machine; `read`/`reads` with `echo`.
   Waits on item 5, milestone 11 item 10, G9 and G10.
7. **Windows on screen**: the `graphics` feature over `sdl3` (`--no-default-features` refusing a
   visible canvas), the texture and showing on wait, `flush`, `sync`, `delay`, SDL events routed by
   window id, `#close`, resize keeping pixels, several windows, the canvas attributes (`label`,
   `pos`, `size`, `pointer`, `resize`, `canvas`), `raise`/`lower`, waiting at program end. First
   proves the dummy video driver opens from a `Background` shell; tested only on it. Waits on item 6.
8. **Events and the loop**: `next_event()` and its event map, the driver over SDL and libuv, `delay`
   as a timer inside a task, the one queue shared by both forms. Waits on item 7, G11 and F2.

**Done when:** Icon programs of each kind in the book — a rubber-band rectangle under `#reverse`, a
click-and-drag sketcher, a text window that echoes what is typed, an image read, annotated and
written — have FunL translations as string literals in unit tests that run on hidden canvases and
check the pixels and events Icon's would give; an example under `examples/` opens a real window from
a console shell; and `graphics.md` is gone, replaced by the library page.
