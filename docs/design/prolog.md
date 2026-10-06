---
title: Prolog on the same machine
weight: 40
---

# Prolog on the same machine

**Standard Prolog source runs on the FunL machine, compiled by a second front end to the same
instructions FunL relations compile to.** A predicate defined in a `.pl` file and a relation defined
in a `.fl` file are the same kind of thing on the control stack, so each calls the other with no
bridge, no conversion of terms and no second engine. The [logic chapter](logic.md) is the
mechanism; this chapter is the second syntax, the builtin set it aims for, and the defects of the
2019 engine it is designed not to repeat.

**The 2019 engine is not ported.** It was a separate machine with its own instruction set, and most
of its defects (listed at the end) come from that machine's design rather than from slips in its
code, so the right move is to rebuild Prolog on a machine whose design already rules them out.

## Loading Prolog

```
consult("family.pl")                    ;; from FunL: load and compile the file's clauses

import "family.pl"                      ;; the same, at compile time, like any import
```

and from the command line, `funl family.pl` reads a file by its extension and starts the
interactive top level, or runs `:- initialization(main).` if the file has one. **A Prolog file's
directives run at load time** (`:- dynamic`, `:- op`, `:- initialization`), exactly as in any Prolog.

## The reader

**The reader is an operator-precedence parser over an operator table that changes at run time**,
because `op/3` lets a program add operators while it is being read. `sh.sysl.parsing`'s `pratt`
asks a callback for each token's binding power, and the callback here consults the table — so a
directive `:- op(700, xfx, ===>).` takes effect for the clauses after it with no change to the loop.
ISO's priorities (0–1200) and types (`xfx`, `xfy`, `yfx`, `fy`, `fx`, `xf`, `yf`) map onto the
left/right powers the loop already takes; a term written in canonical form, `+(1, 2)`, needs no
table at all.

**Tokens follow ISO**: a variable starts with a capital or `_`; an atom is a lower-case name, a run
of symbol characters, a quoted `'atom'`, or one of `[]`, `{}`, `!`, `;`, `,`; numbers include
`0'c`, `0x1F`, `0o17`, `0b101` and floats; `%` and `/* */` are comments.

**A double-quoted string is a string**, not a list of codes — the `double_quotes` flag defaults to
`string`, as in SWI-Prolog 7 and later. That is the setting under which a Prolog string and a FunL
string are the same value. A program that wants ISO's `codes` sets the flag.

## How a clause compiles

**A clause compiles exactly as a FunL rule does**: `Choice` between clauses, the head by
[two-way unification](logic.md#head-unification-is-two-way), the body as calls. The control
constructs map onto the machine's own instructions:

| Prolog | instructions |
|---|---|
| `A, B` | `A; Pop; B` |
| `A ; B` | `Choice(b); A; Branch(end); b: B; end:` |
| `C -> T ; E` | `Barrier(s); Choice(e); C; CutTo(s); T; Branch(end); e: E; end:` |
| `C *-> T ; E` | as `->`, but instead of `CutTo(s)` the `Choice(e)` entry is disarmed — turned into a `MarkThrough` — so `C`'s other solutions survive and `E` can no longer run |
| `\+ G` | `Mark(ok); G; Unmark; Fail; ok:` |
| `!` | `CutClause` — back to `frame.cut` |
| `call(G)`, `call(G, A…)` | a new frame for `G`, so a cut inside `G` is local to it |
| `catch(G, C, R)` | a `Catch` entry around `G` (below) |
| `true`, `fail`, `false` | nothing, `Fail`, `Fail` |

**A cut is transparent through `,`, `;` and `->` and opaque through `call/N`**, which is ISO and
falls out of where the barrier is kept: `CutClause` reads the frame, and `,`, `;` and `->` make no
frame, while `call/N` does.

### Dynamic predicates

**`assertz`, `asserta` and `retract` change a predicate's clause list, and a call sees the list as
it was when the call began** — the logical update view, which ISO requires. Each clause carries the
generation at which it was added and the generation at which it was retracted; a call records the
generation when it starts and skips clauses outside it. A clause added by `assert` is compiled when
it is added, so a dynamic predicate runs the same instructions a static one does.

### Meta-calls

`call(G)` with `G` an atom or a simple compound is a lookup of `name/arity` and a `Call`. A goal
containing control constructs, `call((A ; B))`, is compiled into a small chunk on the spot and run in
a new frame. Nothing is cached until a measurement says it should be.

## The term mapping

**Most kinds are shared outright**: a Prolog atom is a FunL atom, an integer is an integer from the
same tower (unbounded on both sides), a float is a real, a string is a string, a list is a list, a
compound term is a compound — and a FunL `data` record *is* a compound, so `point(1, 2)` built in
FunL unifies with `point(X, Y)` in Prolog. A rational from FunL is a number to Prolog arithmetic as
well, as SWI-Prolog's rationals are. `true` and `false` are atoms on both sides.

> **Open question — the term mapping for FunL-only kinds.**
> **Recommendation:** a **tuple** `(a, b)` is the compound `tuple(a, b)`, so a FunL function
> answering a pair can be destructured by a Prolog head; **arrays, buffers, maps, sets, closures and
> cursors are opaque** — they unify only with themselves and print as `<map>`, `<function>` and so
> on — because their contents can change and a term that changes under a binding is not a term;
> **`()`, `null` and `undefined`** are three distinct constants that unify only with themselves.
> **Alternatives:** tuples as `','(a, b)` (the conjunction functor, which some systems use and every
> reader trips on); or converting maps to association lists at the boundary, which copies on every
> call and silently breaks identity.

## Calling FunL from Prolog

**A FunL function `f` of `n` parameters is the predicate `f/(n+1)`**, its last argument unified with
the result, each generated value one solution:

```
:- import("geometry.fl").

area_of(Shape, A) :- area(Shape, A).          % area/1 in FunL is area/2 here
evens(L) :- findall(X, (between(1, 10, X), even(X, true)), L).
```

**Prolog and FunL share one namespace keyed by name and arity**, and a function of `n` parameters
occupies the key `name/(n+1)`. A Prolog predicate and a FunL function that would occupy the same key
are refused when the second is loaded, naming both — the alternative is one silently shadowing the
other, and which one depends on load order.

A FunL function's arguments are dereferenced before the call and must be bound where its patterns
look inside them; an unbound one is an `instantiation_error` naming the function, raised as an
ordinary Prolog exception.

**From FunL, a Prolog predicate is a relation** and is called like one — `every parent(x, #mia) do
…` — with nothing to declare.

## Errors and exceptions

**`catch/3` pushes a `Catch` entry; `throw/1` copies the ball, then unwinds the control stack to the
nearest `Catch` whose catcher unifies with it**, undoing the trail on the way exactly as failure
would, and runs the recovery goal in the catch's frame. An uncaught ball reaching the top level is
printed with the goal that raised it.

**Every runtime error is an ISO error term**: `error(type_error(evaluable, foo/0), context(is/2,
_))`, `error(instantiation_error, …)`, `error(existence_error(procedure, foo/2), …)`. FunL's own
runtime errors are raised as the same terms, so a Prolog `catch` around a call into FunL catches a
FunL type error. Whether FunL grows a `catch` of its own is a separate question this design leaves
for the user; the machinery it would use is the `Catch` entry already here.

## Arithmetic and order

**`is/2` evaluates with Prolog's operator meanings over FunL's tower.** The difference that matters
is `/`: in Prolog `7 / 2` is `3.5` (ISO; with the `prefer_rationals` flag, `7r2`), where FunL's
`7 / 2` is the rational `7/2`. Each syntax compiles its own `/`; the numbers are the same numbers.
`//`, `mod`, `rem`, `div`, `abs`, `sign`, `min`, `max`, `gcd`, `msb`, `**`, `^`, the bit operators,
`sqrt`, the trigonometric and exponential functions, `float`, `integer`, `truncate`, `round`,
`ceiling`, `floor` and `random` are the set.

**An unbound variable inside an arithmetic expression is an `instantiation_error`**, and an atom
that is not an evaluable is a `type_error(evaluable, Name/Arity)`.

**Standard order of terms** — what `@<`, `compare/3`, `sort/2` and `msort/2` use — is: variables
(by age), then numbers (by value; an integer and a float of equal value put the float first), then
atoms (by text), then strings (by text), then compounds (by arity, then name, then arguments left to
right). FunL-only kinds come after compounds, in a fixed order by kind and then by identity, so a
sort of mixed terms is total and repeatable.

## The builtin set

**ISO core first, then the SWI-Prolog library predicates programs actually reach for.** Grouped as
the standard groups them:

| group | predicates |
|---|---|
| control | `call/1-8`, `not/1`, `\+/1`, `once/1`, `ignore/1`, `forall/2`, `catch/3`, `throw/1`, `halt/0-1`, `between/3`, `repeat/0` |
| unification and comparison | `=/2`, `\=/2`, `unify_with_occurs_check/2`, `==/2`, `\==/2`, `@</2` and the other three, `compare/3` |
| type tests | `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `float/1`, `atomic/1`, `compound/1`, `callable/1`, `is_list/1`, `string/1`, `ground/1` |
| terms | `functor/3`, `arg/3`, `=../2`, `copy_term/2`, `term_variables/2` |
| arithmetic | `is/2`, `=:=/2`, `=\=/2`, `</2`, `>/2`, `=</2`, `>=/2`, `succ/2`, `plus/3` |
| database | `assert/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`, `abolish/1`, `clause/2`, `dynamic/1` |
| all solutions | `findall/3`, `findall/4`, `bagof/3`, `setof/3` (with `^`), `aggregate_all/3` (`count`, `sum`, `max`, `bag`, `set`) |
| atoms and strings | `atom_codes/2`, `atom_chars/2`, `char_code/2`, `atom_length/2`, `atom_concat/3`, `sub_atom/5`, `atom_number/2`, `number_codes/2`, `atom_string/2`, `string_concat/3`, `string_chars/2`, `string_codes/2`, `split_string/4`, `sub_string/5`, `upcase_atom/2`, `term_to_atom/2` |
| lists (library) | `append/3`, `member/2`, `memberchk/2`, `length/2`, `nth0/3`, `nth1/3`, `reverse/2`, `last/2`, `msort/2`, `sort/2`, `sort/4`, `predsort/3`, `sum_list/2`, `max_list/2`, `min_list/2`, `list_to_set/2`, `exclude/3`, `include/3`, `maplist/2-5`, `foldl/4-6`, `nb_getval/2`, `b_getval/2` |
| input and output | `write/1`, `writeln/1`, `print/1`, `write_canonical/1`, `writeq/1`, `nl/0`, `tab/1`, `format/1-2`, `read_term/2`, `read/1` |
| flags and operators | `op/3`, `current_op/3`, `set_prolog_flag/2`, `current_prolog_flag/2` |
| grammar rules | `-->` translation at load time, `phrase/2-3` |

**The library predicates are written in Prolog** wherever that is natural (`append`, `member`,
`reverse`, the `maplist` family), loaded from a source file carried in the binary, so they are
ordinary clauses a reader can look at. A predicate that needs the machine — `findall`, `copy_term`,
`assert`, arithmetic, the I/O — is native.

## The 2019 engine's defects, and what rules each one out

Every row is a regression test with the defect's name, written before the feature it guards.

| 2019 defect | why it happened | what in this design prevents it |
|---|---|---|
| a cut leaks into the caller | cut and mark points in global registers | the barrier lives in the frame (`frame.cut`) or a slot; nothing else can write it |
| a nested `->` corrupts the outer one | the same global register, overwritten by the inner | each `->` saves its barrier in its own slot |
| `call/1` behaves as `once/1` | the meta-call was bounded | `call/1` is a frame with its own cut barrier and no mark; its choice points survive the return |
| `==` fails on compound terms | identity compared instead of structure | `==` walks both terms; two variables are `==` only if they are the same variable |
| `X == X` fails | the variable compared to its dereferenced self | `==` dereferences both sides first, and a variable is identical to itself |
| `X = X` loops | binding a variable to itself made a cycle | unification tests identity before anything else |
| no standard order of terms | not implemented | `compare/3` implements the order above, and `sort/2` and `@<` use it |
| `1 = 1.0` succeeds | numbers compared by value | unification compares kinds first |
| an unbound variable in arithmetic overflows the stack | evaluation recursed on an unbound variable | evaluation dereferences and raises `instantiation_error` |
| `5 is 2+3` is a compile error | the compiler expected a variable on the left of `is` | `is` unifies its left side with the result; a number there is an ordinary comparison |
| `findall` is a stub | not implemented | `findall` is native, with the copy taken in an operand cell |
| `asserta` does nothing | not implemented | `asserta`/`assertz` share one clause store, with generations |
| no `catch`, `retract`, `assertz`, `bagof`, `setof` | not implemented | all in the builtin set above |

## How conformance is checked

**The INRIA ISO conformance suite (`inriasuite`) is the oracle for the builtins**: it is a file of
goals with their expected outcomes — solutions, failure, or a specific error term — written against
the standard, by people who did not write this implementation. It runs as a suite here once the
reader and the core builtins exist, and every expected outcome it records is either met or listed,
with the reason, in a file the test reads.

**The classic benchmark programs** — naive reverse, queens, the dev branch's `classic.prolog` — are
run for their answers rather than their speed, and the family tree runs in both syntaxes with one
expected output.
