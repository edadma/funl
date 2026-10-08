---
title: Design
weight: 60
---

# Design

**These pages are the design of FunL written in sysl, before any of it exists.** Nothing here
describes code that runs today. They are written first so that every decision is argued once, in
one place, rather than settled by whoever happens to be writing the code that day.

**Their programs are not run.** A design page shows syntax nothing compiles yet, so its blocks
carry no language tag and no harness reads them. When a piece is built, the page that describes
it is rewritten as a reference page under `docs/reference/`, whose programs the suite does run,
and the design page goes.

## What FunL is, in one paragraph

FunL is a small, dynamically typed, indentation-structured language in the Icon tradition:
**every expression either produces a value or fails**, an expression may produce several values
one after another when the context asks for more, and control flow is driven by success and
failure rather than by `true` and `false`. Its implementation is a single backtracking virtual
machine — one stack of choice points drives generators, alternation, `every`, string scanning,
reversible assignment and regular expressions alike. **Add logic variables, a trail and unification
to that machine, and the same machine runs Prolog.** FunL relations, FunL functions, regular expressions and Prolog predicates then call one
another freely, because they are all just code on one machine.

## The chapters

Read them in order; each leans on the one before it.

| | |
|---|---|
| [The language](language.md) | goal-directed evaluation, generators, functions and clauses, patterns, scanning, regex, values, and the rough edges each rule closes |
| [The machine](vm.md) | the backtracking VM: machine state, the control stack of marks and choice points, the trail, generators, cut, scanning and regex in forward and reverse, the value model and the numeric tower |
| [Logic programming in FunL](logic.md) | logic variables, unification, facts and rules, clause selection, and how functions and relations call each other |
| [Prolog on the same machine](prolog.md) | standard Prolog syntax compiled to the same instructions, the builtin set it aims for, and the failure modes of a Prolog engine its design rules out |
| [The sysl implementation](implementation.md) | the repository and module layout, the parser, the collector and its roots, what is reused and what is written fresh, and how it is tested |
| [Modules and native modules](modules.md) | modules and `import` as slate has them, `funl:` modules over sysl packages, features, values that cross, the first batch, and `async`/`await` |
| [Milestones](milestones.md) | the order the work is done in, and what proves each step |

## Conventions on these pages

- **"Decided"** marks a choice between real alternatives. Each one names the decision and what was
  rejected, so that revisiting it changes one section rather than the whole page.
- **"Closed by design"** marks a place where a naive implementation would do something wrong or
  nothing at all, and says what FunL does instead.
- Instruction names are written `LikeThis`.
- A position in a string is a **character position** in the Icon sense: position 1 is before the
  first character, and position 0 means the end.

## The decisions, collected

Each is argued where it arises; this is the list.

| question | decision | where |
|---|---|---|
| Does `false` fail in a condition? | yes: a condition fails on failure *or* on `false` | [language](language.md#conditions-and-false) |
| Is `return e` bounded? | yes: `return` takes the first result, a body expression passes every result through | [language](language.md#functions-are-generators-when-their-body-is) |
| Does a loop ending a generator produce a trailing `()`? | no: a bare `break` out of the loop that ends a `yield`ing function fails, as running out does; `break (v)` still gives `v` (user, 2026-10-08) | [language](language.md#functions-are-generators-when-their-body-is) |
| How is the operand stack restored on backtracking? | copy the bounded expression's slice into the choice point | [vm](vm.md#the-operand-stack-and-why-backtracking-has-to-copy-part-of-it) |
| How are strings stored and indexed? | UTF-8 plus a character count and a forward cursor, as slate does | [vm](vm.md#strings) |
| What is the unification operator? | `~` | [logic](logic.md#unification-has-its-own-operator) |
| How is a rule marked? | `:-` after the head; a fact is a `def` with no body | [logic](logic.md#facts-and-rules) |
| How is an atom written? | `#name`; nullary `data` constructors are atoms too | [logic](logic.md#atoms) |
| How does FunL code declare a logic variable? | `free x`, Curry's word | [logic](logic.md#logic-variables-in-ordinary-code) |
| Is there a cut in FunL relations? | no `!` (it is the generator operator); `once` and `->` cover it | [logic](logic.md#cut-in-a-relation) |
| What do tuples, maps and closures look like as Prolog terms? | tuples are `tuple(...)` compounds; the rest are opaque and unify by identity | [prolog](prolog.md#the-term-mapping) |
| What is the module path? | `io.github.edadma.funl`, the repository `github.com/edadma/funl` reversed | [implementation](implementation.md#the-repository) |
| Does juxtaposition multiply? | only a number literal touching a name or `(`: `2n`, `3(x + 1)` | [implementation](implementation.md#the-front-end) |
| How strict is the regex conformance oracle? | exact on match and span, exact on captures outside a named exclusion list | [implementation](implementation.md#regex-the-att-data-and-tests-worked-by-hand) |
| What does `\` do on negative operands? | it floors (user, 2026-10-08) | [language](language.md#numbers) |
| How does lookbehind match? | as JavaScript's does, right to left (user, 2026-10-08) | [vm](vm.md#forward-and-reverse-and-lookbehind-with-no-restriction) |
| Is there a regex oracle? | no (user, 2026-10-08) | [implementation](implementation.md#regex-the-att-data-and-tests-worked-by-hand) |
| What does `x is t` produce? | `x` or failure, like a comparison (user, 2026-10-08) | [language](language.md#types-and-is) |
| How is an unbound variable tested? | `x is variable`; `var` is a keyword, no `var(x)`/`nonvar(x)` (user, 2026-10-08) | [logic](logic.md#logic-variables-in-ordinary-code) |
| What does a relation body see of top-level names? | only `val`s, functions, relations, constructors; not `var`, `free` or assignment names (user, 2026-10-08) | [logic](logic.md#variables-in-a-relation) |
| Can a real literal start with the point? | no: `.5` is a syntax error, `.` is field access (user, 2026-10-08) | [language](language.md#numbers) |
| Can ordinary code build a compound term without `data`? | no: outside a relation head an undeclared functor is "not defined" (user, 2026-10-08) | [language](language.md#data) |
| How do modules and native modules work? | as slate does, translated to FunL (user, 2026-10-08) | [modules](modules.md) |
| How do `async` and `await` work? | as slate does, translated to FunL (user, 2026-10-08) | [modules](modules.md#async-and-await) |
| What is a handle? | an opaque `Handle` kind with a per-kind method table, explicit `close()`, a finalizer as backstop (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| How does a sysl iterator cross? | as a native generator over a per-call state handle; each call starts afresh (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| Failure or fault for a native's error? | failure for "no such thing", "no more", "not well-formed"; an ISO error fault for the rest (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| How does FunL catch a fault? | slate's postfix `e catch err -> recovery`, before `funl:http` (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| How are relations exported and imported? | `export` on any clause exports the procedure; an import brings every arity; qualified calls resolved statically (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| What does Prolog see of a module? | a FunL file's exports; built-in modules module-qualified, `json:parse(T, V)` (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| What becomes of `import "file.pl"`? | kept as the one form for a Prolog file (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| Is there a bytes kind? | yes, `Bytes`, in milestone 9 (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| Is `await` resumed by backtracking? | no: bounded, one value, never redone; a generator body may `await` (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| Blocking or promise-shaped I/O? | both: blocking in each module, promise forms in `funl:async` (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| Which loop, and may two tasks share a logic variable? | kairos (`uv` behind an `async` feature); binding another task's variable is a fault (user, 2026-10-08) | [modules](modules.md#questions-decided) |
| What is the prelude? | the first module written in FunL: a Haskell-style list library, auto-imported like Haskell's Prelude, shadowable by a program's own names (user, 2026-10-08) | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| Builtins as values? | a builtin of a fixed arity named without a call is a function value, and `import * as` a module of FunL's own names the map of them; one of any arity is still only called (user, 2026-10-08) | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |

## Open questions

Each has a recommendation in its chapter; the user decides them before the milestone that needs them.

| question | recommendation | where |
|---|---|---|
| Prelude `map` against the builtin `map`? | one `map/2`; a function as first argument maps, anything else is the constructor | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| Which prelude names are core natives? | `reverse`, `sort`, `sortBy`, `concat`, `replicate`, `elem`, `last`, `init`, `drop`, the `zip` family; the rest FunL source | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| A generator of tuples cannot be destructured by `<-` | a tuple pattern matches a generated tuple whole; until then `zip` answers lists | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
