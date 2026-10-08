---
title: Library
weight: 30
---

# The FunL library

The modules FunL brings with it. Each is imported by a name beginning `funl:`, and none of its names
is a builtin until a file imports it, so a program can use the same words for its own. Every program
on these pages is run by the test suite, exactly as the [reference](../reference/README.md)'s are.

The prelude is the one exception: every program sees it without an import.

| page | what it covers |
|---|---|
| [the prelude](prelude.md) | the list library every program sees: pairs, the ends of a list, folds, `map` and the other transformers, slicing, zipping, `iterate` and `forever` drawn through a thunk, `any`, `all`, `elem` and `lookup`, how a program's own names shadow it, `funl:prelude` by name and from Prolog |
| [`funl:fs`](fs.md) | reading and writing whole files and bytes, the lines of a file and the names in a directory as generators, `exists`, `stat`, making, removing and renaming, paths, what fails and what faults, the module from Prolog |
| [`funl:process`](process.md) | the program's arguments and environment, `exit`, running another program for its output and how it ended, its output as a generator of lines, the options, what fails and what faults, the module from Prolog |
| [`funl:json`](json.md) | `parse` and `stringify`: JSON text read into maps, lists, strings, numbers, booleans and `undefined`, values written compactly or with an indent, what fails and what faults, the module from Prolog |
| [`funl:sqlite`](sqlite.md) | opening a database in a file or in memory, `exec`, `run` with parameters, `query` as a generator of rows, the values that cross, what faults |
| [`funl:time`](time.md) | the wall and monotonic clocks, `sleep`, making, formatting and parsing ISO 8601 timestamps, the calendar reading of a time, the host's offset |
| [`funl:http`](http.md) | `fetch`: a request with a method, headers, a body and a timeout, the response as status, headers, text and bytes, what faults, the `http` feature, the module from Prolog |
