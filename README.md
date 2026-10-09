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
  It also manages a project's packages: `funl add github.com/owner/repo` adds one to the project's
  `package.funl` and fetches it with `git`, `funl fetch` fetches what the project uses, `funl vendor`
  copies it into the project, and `funl deps` lists it. A program file named like one of those
  words is run by its path, `funl ./add`.
- `prolog` is a standalone standard-Prolog REPL.

FunL source files end in `.funl`; literate FunL, Markdown whose indented blocks are the program,
ends in `.lfunl`.

## Install

With Homebrew:

```
brew install edadma/tap/funl
```

Or build from source. You need `sysl` 0.1.0-alpha.5 or later:

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

## What FunL brings with it

- [`docs/library/`](docs/library/README.md) documents the modules, each imported by a name beginning
  `funl:`: `funl:fs`, `funl:process`, `funl:json`, `funl:time`, `funl:sqlite`, `funl:async` and
  `funl:http` (`fetch`, which needs libcurl on the machine). Prolog reaches them too.
- `funl:async` also has promise-shaped `fetch`, `read_file`, `write_file` and `run`, driven by libuv
  (Homebrew's `libuv` on macOS).
- `funl:process` has `env()`, which lists every environment variable, and an `inherit_env` option on
  `run`. A bare program name is found on the child's `PATH`.
- Input that is not valid UTF-8 is an error rather than a crash: a file name from `funl:fs`
  `read_dir` faults with `system_error`, an imported or consulted source is reported as unreadable,
  and Prolog's standard input refuses it. `ß` upper-cases to itself.
- The prelude is a list library every program sees: folds, `map` and the other transformers,
  slicing, zipping, `any`, `all`, `elem` and `lookup`. A program's own names shadow it.
- `async def` and `await` run tasks as parked machines on one loop, with promises from `funl:async`;
  failure and backtracking carry across an `await`.
- A builtin named without calling it is a function value, variadic ones included, and a generating
  builtin still generates when called through the value.
- A binding `x <- e` draws a string, a tuple or a map whole; `!` draws their parts.
- Packages: `import { rows } from tabular` reaches a package named in the project's `package.funl`,
  read from the project's `vendor/` or the cache and never fetched while a program runs; versions
  are exact and `funl.sum` records what each one hashed to (`docs/reference/packages.md`).
- One name can be defined at several arities (`f/1` and `f/2` are two functions), a block may close
  with an optional, checked `end` marker, and list, set and map literals take a trailing comma.
- `zip` and its family take an endless range beside a finite input, and the list functions refuse a
  string, tuple or map rather than guess.
- Prolog reads and writes `1.0Inf` and `1.5NaN`, has SWI's `float_overflow`, `float_zero_div` and
  `float_undefined` flags, and reads integers in any radix.

## More

- [`examples/`](examples/) has short programs for people to read: generators, string scanning,
  regex, data and patterns, the numeric tower, relations, puzzles, and FunL and Prolog sharing a
  program.
- [`docs/reference/`](docs/reference/README.md) is the manual. Every program in it is run by the
  test suite. Its pages cover success and failure, generators, assignment and scope, data,
  functions, numbers, relations, string scanning, string functions, source files, regular
  expressions, modules, packages, and Prolog.
- [`docs/design/`](docs/design/README.md) is the design: the language, the machine, logic
  programming, and Prolog on the same machine.
