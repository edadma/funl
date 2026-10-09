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
| [Packages](packages.md) | the manifest, specifiers and what they resolve to, exact versions and `funl.sum`, the cache and `vendor/`, offline use, `funl add`, and what a package can and cannot bring |
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
| Are `()` and the empty tuple one value? | yes: `()` is a tuple of length 0, and `x is unit` still names it (user, 2026-10-08) | [language](language.md#data) |
| Is `{f: (x) -> e}` a map or a set? | a map: the key `f` and the lambda `(x) -> e`; a set holds such a lambda in parentheses (user, 2026-10-08) | [language](language.md#data) |
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
| Prelude `map` against the builtin `map`? | one `map/2`: a function as first argument maps, anything else is the constructor (user, 2026-10-08) | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| Which prelude names are core natives? | `reverse`, `sort`, `concat`, `replicate`, `elem`, `last`, `init`, `drop`, `zip`, `zip3`, and `sortBy`, `zipWith`, `zipWith3` calling their function back as bounded calls (user, 2026-10-08) | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| Builtins as values? | a builtin named without a call is a function value (a variadic one passes on however many arguments it is given), and `import * as` a module of FunL's own names the map of them (user, 2026-10-08) | [modules](modules.md#the-prelude--the-first-module-written-in-funl) |
| A trailing comma in a map, set or list literal? | yes, language-wide, so a manifest stays ordinary FunL; a tuple, an argument list or a parameter list does not take one (user, 2026-10-09) | [packages](packages.md#open-questions) |

## Open questions

Each has a recommendation in its chapter; the user decides them before the milestone that needs them.

| question | recommendation | where |
|---|---|---|
| How does a package fetch: `git` or HTTPS tarballs? | `git clone --depth 1 --branch v<version>` and `git ls-remote --tags`, as the sysl compiler does; works in every build | [packages](packages.md#open-questions) |
| A package's other module: `tabular/pivot` or `tabular.pivot`? | `tabular/pivot`, as slate does | [packages](packages.md#open-questions) |
| Constructors unique across packages too? | keep the rule; the refusal names both packages; revisit on a real collision | [packages](packages.md#open-questions) |
| How does Prolog name a package? | an atom is a package (`:- import(tabular/pivot).`), a string a file | [packages](packages.md#open-questions) |
| May a package carry Prolog files? | yes, imported whole as `import "file.pl"` is | [packages](packages.md#open-questions) |
| A `funl` floor key in the manifest? | yes, optional, because the grammar is still moving | [packages](packages.md#open-questions) |
| Which package commands? | `add`, `fetch`, `vendor`, `deps` now; `bundle`, `brew`, `scripts` when shipping is wanted | [packages](packages.md#open-questions) |
| P3.1 Is P3 (a generator of tuples cannot be destructured by `<-`) closed by drawing tuples whole? | yes: `(a, b) <- zip(xs, ys)`, `(k, v) <- !m` and generated tuples already destructure; the rule is option A | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| P3.2 A drawn value the pattern does not match: passed over or a fault? | passed over, as a failing `if` filter is | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| P3.3 A list or cons pattern over a generator of lists | keep `[e]` to keep each list whole; no shape-directed drawing | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| P3.4 `zip` beside an endless range | accept it when another input is finite, so `zip(0.., xs)` numbers a list | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| P3.5 `zip` (and the prelude's list functions) given a string, tuple or map | refuse it as `reverse` does, not answer a one-element pairing | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| P3.6 Should `every` take a pattern? | no: `for (a, b) <- e` is the destructuring form | [language](language.md#taking-drawn-values-apart-with-a-pattern) |
| One name, several arities: adopt name/arity for functions and relations? | yes (Option C): `f(x)` and `f(x, y)` are `f/1` and `f/2`, a call resolved by its count when compiled | [language](language.md#one-name-several-arities) |
| A bare multi-arity name as a value | one family value that picks the arity by the count of each call, as a variadic builtin's value does | [language](language.md#one-name-several-arities) |
| Does a nested `def`/`where` of `g` hide every outer `g`? | yes, every arity; only the top level merges per arity, with the builtins and the prelude | [language](language.md#one-name-several-arities) |
| May a file define `f/2` while importing `f`? | no, as decided for imports; `as` renames | [language](language.md#one-name-several-arities) |
| Function `f/n` and relation `f/(n+1)` share a Prolog key | refused always, at the second definition; the `sides` example is reworded | [language](language.md#one-name-several-arities) |
| Default parameters? | none; two arities are the default. If ever added, sugar for the arities | [language](language.md#one-name-several-arities) |
| A constructor value over two field counts picks the last silently | the family under C; refused when compiled otherwise | [language](language.md#one-name-several-arities) |
| E1 Optional, checked `end` markers, Scala 3's rules (Option A)? | yes | [language](language.md#blocks-and-optional-end-markers) |
| E2 Which constructs take `end`? | `def` clauses (`end f`), a bare `def` group (`end def`), block `val`/`var` (`end x`, `end val`), `if` chains, `while`/`for`/`repeat`/`every`; no lambda, scan, `catch`, `where`, sequence, `data` | [language](language.md#blocks-and-optional-end-markers) |
| E3 A labelled loop: `end for` or `end outer`? | `end for` only | [language](language.md#blocks-and-optional-end-markers) |
| E4 `end f` after a one-line `def f( x ) = …` | refused (sysl accepts it) | [language](language.md#blocks-and-optional-end-markers) |
| E5 One `end if` for a whole `if`/`elif`/`else` chain? | yes | [language](language.md#blocks-and-optional-end-markers) |
| E6 A line holding only `end` | stays a read of the name; its refusal gains a note about markers | [language](language.md#blocks-and-optional-end-markers) |
| E7 sysl's seven-line rule for markers in examples and reference pages? | yes | [language](language.md#blocks-and-optional-end-markers) |
