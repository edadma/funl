---
title: Reference
weight: 20
---

# The FunL reference

What FunL is, page by page. Every program on these pages is run by the test suite, and what it
prints is compared with the `output` block under it; a program followed by an `error` block is
required to be refused with that message. A page here says what the language does, so a page and
the language cannot disagree for long.

| page | what it covers |
|---|---|
| [Success and failure](success-and-failure.md) | expressions that succeed or fail, conditions and `false`, `if`, `not`, `and`, `or`, loops that run out |
| [Functions](functions.md) | `def`, lambdas, operator sections, clauses and guards, patterns, `where`, generator functions, `yield` and `return`, partial function literals |

## How a page is checked

A fenced block tagged `funl` (or `prolog`) is a program, and what follows it says what kind:

- followed by an `output` block, it is run and must print exactly that;
- followed by an `error` block, it must be refused — at compile time or by a fault — with a message
  containing that text;
- followed by neither, it is a fragment, shown for its shape and not run.

The claim for a program is the first `output` or `error` block after it and before the next program,
so a page may put other blocks between a program and what it prints.
