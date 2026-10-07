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
reversible assignment and regular expressions alike. **The rewrite finishes the idea that machine
was always halfway to:** add logic variables, a trail and unification, and the same machine runs
Prolog. FunL relations, FunL functions, regular expressions and Prolog predicates then call one
another freely, because they are all just code on one machine.

## The chapters

Read them in order; each leans on the one before it.

| | |
|---|---|
| [The language](language.md) | FunL as the old implementation and its tests define it: goal-directed evaluation, generators, functions and clauses, patterns, scanning, regex, values — with the rough edges this rewrite fixes |
| [The machine](vm.md) | the backtracking VM: machine state, the control stack of marks and choice points, the trail, generators, cut, scanning and regex in forward and reverse, the value model and the numeric tower |
| [Logic programming in FunL](logic.md) | logic variables, unification, facts and rules, clause selection, and how functions and relations call each other |
| [Prolog on the same machine](prolog.md) | standard Prolog syntax compiled to the same instructions, the builtin set it aims for, and the defects of the 2019 engine it must not repeat |
| [The sysl implementation](implementation.md) | the repository and module layout, the parser, the collector and its roots, what is reused and what is written fresh, and how it is tested |
| [Milestones](milestones.md) | the order the work is done in, and what proves each step |

## Conventions on these pages

- **"Decided"** marks a choice between real alternatives. Each one names the decision and what was
  rejected, so that revisiting it changes one section rather than the whole page.
- **"Fixed in the rewrite"** marks a place where the old implementation did something wrong or
  nothing at all, and says what the rewrite does instead.
- Instruction names are written `LikeThis` and are the rewrite's, not the old machine's, unless a
  sentence says otherwise.
- A position in a string is a **character position** in the Icon sense: position 1 is before the
  first character, and position 0 means the end.

## The decisions, collected

Each is argued where it arises; this is the list.

| question | decision | where |
|---|---|---|
| Does `false` fail in a condition? | yes: a condition fails on failure *or* on `false` | [language](language.md#conditions-and-false) |
| Is `return e` bounded? | yes: `return` takes the first result, a body expression passes every result through | [language](language.md#functions-are-generators-when-their-body-is) |
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
| How strict is the regex conformance oracle? | exact on match and span, exact on captures outside a named exclusion list | [implementation](implementation.md#regex-the-att-data-and-a-second-oracle) |
