---
title: Library
weight: 30
---

# The FunL library

The modules FunL brings with it. Each is imported by a name beginning `funl:`, and none of its names
is a builtin until a file imports it, so a program can use the same words for its own. Every program
on these pages is run by the test suite, exactly as the [reference](../reference/README.md)'s are.

| page | what it covers |
|---|---|
| [`funl:fs`](fs.md) | reading and writing whole files and bytes, the lines of a file and the names in a directory as generators, `exists`, `stat`, making, removing and renaming, paths, what fails and what faults, the module from Prolog |
| [`funl:sqlite`](sqlite.md) | opening a database in a file or in memory, `exec`, `run` with parameters, `query` as a generator of rows, the values that cross, what faults |
