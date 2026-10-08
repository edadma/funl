# FunL

FunL is a small, dynamically typed language in the Icon tradition, written in
[sysl](https://github.com/sysl-lang/sysl). Every expression either produces a value or fails, and
an expression may produce several values one after another when its context asks for more, so
control flow is driven by success and failure rather than by `true` and `false`. Generators,
alternation, string scanning and regular expressions all run on one backtracking virtual machine.

Give that machine logic variables, a trail and unification and it runs Prolog as well. FunL
relations, FunL functions, regular expressions and ISO Prolog predicates call one another freely,
because they are all code on the same machine.

Two executables are built from this repository:

- `funl` runs FunL programs: `funl file.funl`, or a literate program, `funl file.lfunl`. Given a
  Prolog file, `funl file.pl` loads it (it may import FunL code) and starts the Prolog top level.
- `prolog` is a standalone standard-Prolog REPL.

FunL source files end in `.funl`; literate FunL, Markdown whose indented blocks are the program,
ends in `.lfunl`.

## Install

With Homebrew:

```
brew install edadma/tap/funl
```

Or build from source. You need `sysl` 0.1.0-alpha.2 or later:

```
brew install sysl-lang/tap/sysl
git clone https://github.com/edadma/funl
cd funl
sysl build -p funl
sysl build -p prolog
```

This produces `funl/funl` and `prolog/prolog`. The test suite is `sysl test .`.

## A taste

A family tree: facts, rules over them, and queries from ordinary code. This is the heart of
[`examples/family_tree.funl`](examples/family_tree.funl).

```
def
  female(#anne)
  female(#rosie)
  male(#randy)
  male(#don)
  male(#liam)
  male(#logan)
  male(#aiden)

  parent(#don, #randy)
  parent(#don, #anne)
  parent(#rosie, #randy)
  parent(#rosie, #anne)
  parent(#liam, #don)
  parent(#logan, #rosie)
  parent(#logan, #aiden)

def
  ancestor(x, y) :- parent(x, y) | parent(x, p) & ancestor(p, y)
  father(x, y) :- male(x) & parent(x, y)
  siblings(x, y) :- parent(p, x) & parent(p, y) & x != y
  uncle(u, n) :- male(u) & siblings(u, p) & parent(p, n)

free who
every father(who, _) do write(who)
every uncle(who, #randy) do write(who)
if father(who, #logan) then write(who)
```

`every` backtracks through every answer a relation can give; `if` takes the first. Running this
prints one name per line:

```
don
don
liam
logan
logan
aiden
```

The complete example, with more relations and queries, is
[`examples/family_tree.funl`](examples/family_tree.funl) (run it with `./funl/funl`). The same
family tree in standard Prolog is [`examples/family_tree.pl`](examples/family_tree.pl).

## More

- [`examples/`](examples/) has short programs for people to read: generators, string scanning,
  regex, data and patterns, the numeric tower, relations, puzzles, and FunL and Prolog sharing a
  program.
- [`docs/reference/`](docs/reference/README.md) is the manual. Every program in it is run by the
  test suite. Its pages cover success and failure, generators, assignment and scope, data,
  functions, numbers, relations, string scanning, string functions, source files, regular
  expressions, and Prolog.
- [`docs/design/`](docs/design/README.md) is the design: the language, the machine, logic
  programming, and Prolog on the same machine.
