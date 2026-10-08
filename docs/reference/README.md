---
title: Reference
weight: 20
---

# The FunL reference

What FunL is, page by page. Every program on these pages is run by the test suite, and what it
prints is compared with the `output` block under it; a program followed by an `error` block is
required to be refused with that message, and one followed by a `warning` block to draw that warning. A page here says what the language does, so a page and
the language cannot disagree for long.

| page | what it covers |
|---|---|
| [Success and failure](success-and-failure.md) | expressions that succeed or fail, conditions and `false`, `if`, `not`, `and`, `or`, loops that run out, catching faults |
| [Generators](generators.md) | expressions with many values, backtracking, `!`, `\|`, `to`, ranges, `&`, searches, bounded expressions, `every`, `for`, `break`/`continue` and labels |
| [Assignment and scope](assignment.md) | `=`, compound assignment, `++`/`--`, multiple assignment, `val` and `var`, block scope, reversible assignment `<-`, `in` and `not in` |
| [Data](data.md) | tuples, lists, maps, sets, comprehensions, slices, arrays, buffers, maps with a default, byte strings, handles, records, `undefined`, types and `is` |
| [Functions](functions.md) | `def`, lambdas, operator sections, builtins as values, clauses and guards, patterns, `where`, generator functions, `yield` and `return`, partial function literals |
| [Numbers](numbers.md) | integers, rationals and reals; `/`, `\` (floor), `//`, `mod`, `%`, `^`, `div`; comparison across kinds; `abs`, `min`, `max`; `sqrt`, `exp`, `log`, trigonometry, roundings, `pi` |
| [Relations](relations.md) | logic variables, unification `~`, atoms, facts and rules, two-way head unification, calling between functions and relations, negation, `findall`/`bagof`/`setof` |
| [String scanning](scanning.md) | `s ? e`, positions, `tab`, `move`, `pos`, `upto`, `many`, `any`, `match`, `find`, backtracking the position, `?=`, patterns and combinators inside a scan |
| [String functions](strings.md) | slices, `split`, `join`, `trim`, `trim_start`, `trim_end`, `upper`, `lower`, `replace`, the tests `starts_with`, `ends_with` and `contains`, `find` outside a scan, and `bytes` and `decode` |
| [Source files](source-files.md) | `.funl` files, literate FunL (`.lfunl`), running `funl`, importing a literate file into Prolog |
| [Modules](modules.md) | a file is a module, `export`, `import { ... } from`, `import * as`, qualified names, imports resolved first and run once, exports as a snapshot, circles of imports, what Prolog sees of a module |
| [Regular expressions](regex.md) | regex literals, classes, repetition, groups and backreferences, anchors, flags, lookahead, lookbehind, atomic groups, the pattern combinators, how a pattern matches and backtracks |
| [Prolog](prolog.md) | loading Prolog from FunL, the `prolog` executable, the reader and reading terms, control, dynamic predicates, the terms shared with FunL, calling FunL from Prolog, errors, arithmetic and rationals, standard order, cyclic terms, grammar rules, `format`, the builtin set |

## How a page is checked

A fenced block tagged `funl` (or `prolog`, or `lfunl`) is a program, and what follows it says what
kind:

- followed by an `output` block, it is run and must print exactly that;
- followed by an `error` block, it must be refused — at compile time or by a fault — with a message
  containing that text;
- followed by a `warning` block, it must compile with a warning containing that text; with an
  `output` block as well, it must also print exactly that;
- followed by none of them, it is a fragment, shown for its shape and not run.

The claim for a program is the first `output` or `error` block, and the first `warning` block, after
it and before the next program, so a page may put other blocks between a program and what it prints.
A `warning` block and an `output` block may come in either order; the pages put the warning first,
as the compiler says it first.

A program with an `output` block and no `warning` block must compile without a warning, so a page
never shows a program whose warning a reader running it would see and the page leaves out. A
`warning` block does not go with an `error` block: a refused program's warnings are part of what it
says, and the `error` block quotes them.

A block tagged `lfunl` is a whole literate document, compiled exactly as `funl` compiles a `.lfunl`
file. The word after `lfunl` is the document's file name (`document.lfunl` if there is none), and an
`error` block quotes positions as lines and columns of the document itself. A literate document has
fences of its own, so its outer fence is longer than any inside it — four backticks around inner
fences of three — and the block ends only at a fence at least as long as the one that opened it,
with nothing after it.
