---
title: Packages
weight: 116
---

# Packages

A **package** is somebody else's FunL, used by name: `import { rows } from tabular`. A quoted path
is a file, `funl:json` is one of FunL's own modules, and a bare word is a package.

The programs on this page belong to projects kept beside it, under [`packages/`](packages/). The
first is [`packages/report/`](packages/report/), whose packages are already in its `vendor/`
directory.

## The manifest

A project, and a package, is a directory holding a **`package.funl`**: one FunL map, read as data
and never run. The project [`packages/report/package.funl`](packages/report/package.funl) is:

```
;; The project the programs on the packages page belong to.
{
  name: "report",
  version: "0.1.0",
  dependencies: {
    tabular: { git: "github.com/example/tabular", version: "0.2.0" }
  }
}
```

and the package it depends on, `tabular` 0.2.0, has its own:

```
{
  name: "tabular",
  version: "0.2.0",
  main: "tabular.funl",
  modules: { pivot: "pivot.funl", rules: "rules.pl" },
  dependencies: {
    csv_kit: { git: "github.com/example/csv-kit", version: "0.2.0" }
  }
}
```

The keys are:

| key | what it is |
|---|---|
| `name`, `version` | required: the package's name and its exact version |
| `main` | the file `import ... from tabular` reaches |
| `modules` | the package's other modules, each a name and a file: `tabular/pivot` reaches `pivot.funl` |
| `dependencies` | the packages this one uses, each a name, a `git` repository and an exact `version` |
| `devDependencies` | packages only the project's own files use, such as a test library; a package's are never read by anything that depends on it |
| `funl` | the oldest FunL the package needs, `funl: "0.0.4"` |
| `description`, `homepage`, `license` | strings, for people |

A key of `dependencies` is the name an import writes, so it is a name: a letter or `_`, then letters,
digits and `_` (`csv_kit` for the repository `csv-kit`). A module's file is a `.funl`, `.lfunl` or
`.pl` file. A name is in `dependencies` or in `devDependencies`, never both.

## Importing a package

A package is imported by the name the manifest gives it, and one of its other modules with a `/`:

```funl packages/report/main.funl
import { rows } from tabular
import { pivot } from tabular/pivot

write( rows('north south east') )
write( pivot(21) )
```

```output
["north", "south", "east"]
42
```

Every form of `import` takes a package as it takes a file: `import * as t from tabular` names the
whole module, and a Prolog file in a package is imported whole, every predicate it defines with it:

```funl packages/report/main.funl
import * as t from tabular
import tabular/rules

free c
write( t.rows('a b') )
every colour(c) do write( c )
```

```output
["a", "b"]
red
green
```

A package's module is a module like any other: known by its real path, its top level run once, and
shared by every file that imports it.

## Which manifest a name is looked up in

**The manifest that governs a file is the nearest `package.funl` above it.** A file of the program
is governed by the project's; a file inside a package by the package's own. So `tabular.funl`, which
writes `import { fields } from csv_kit`, reaches *its* dependency — and the program, whose manifest
does not name `csv_kit`, cannot:

```funl packages/report/main.funl
import { fields } from csv_kit
```

```error
unknown module `csv_kit`
```

The note under that error names the packages the governing manifest does name. A program's
project is found by walking up from the program file's real path, so it works wherever it is run
from, and through a symbolic link. A program under no project is an ordinary program; it just has no
packages.

## What is refused

Each of these is refused against the import line, before anything runs. A module the package's
`modules` does not list — even a file that is there — cannot be imported, so a package's helpers
stay its own:

```funl packages/report/main.funl
import { secret } from tabular/helper
```

```error
`tabular` has no module `helper`
```

The note says which modules it does have: `tabular/pivot` and `tabular/rules`. A dot does not
name a module:

```funl packages/report/main.funl
import { pivot } from tabular.pivot
```

```error
a package's other module is written with a slash: `tabular/pivot`
```

A Prolog file has no exports to choose from:

```funl packages/report/main.funl
import { colour } from tabular/rules
```

```error
`tabular/rules` is a Prolog file, which has no exports to choose from
```

A package with no `main` is imported only through its modules, and a name no governing manifest
writes is an unknown module.

## Prolog

In a Prolog file, **an atom names a package and a string names a file**. `:- import(tabular).` reaches
the package's `main`, and `:- import(tabular/pivot).` — the term `/(tabular, pivot)` — one of its
modules:

```prolog packages/report/main.pl
:- import(tabular/pivot).
:- import(tabular/rules).
:- initialization(main).

main :- pivot(5, X), write(X), nl, forall(colour(C), (write(C), nl)).
```

```output
10
red
green
```

## Where packages are found

A package version lives in a directory of its own, `<host>/<owner>/<repo>/@v<version>/`. That
directory is looked for in two places:

- the project's **`vendor/`** directory, beside its `package.funl` —
  `packages/report/vendor/github.com/example/tabular/@v0.2.0/` for the programs above;
- the **cache**, `$HOME/.funl/pkg`, or the directory `FUNL_CACHE` names.

`vendor/` is read first. **Running a program never touches the network**: a package in neither place
is refused, naming the command that fetches it. The project
[`packages/unfetched/`](packages/unfetched/package.funl) depends on a version nothing has fetched:

```funl packages/unfetched/main.funl
import { rows } from tabular
```

```error
run `funl fetch`
```

The whole message names the version and the directory it would be in:
`tabular 0.9.0 is not in ~/.funl/pkg/github.com/example/tabular/@v0.9.0`, with the cache's real path,
and then the command.

## The package commands

Four commands put packages on the machine and say what a project uses. Each works on the project
the current directory is in — the nearest `package.funl` at or above it — and they are the only part
of FunL that reaches the network. They fetch with `git`, so a package can live on any host the
machine's `git` can clone from, with the user's own credentials; a machine without `git` can still
run a project whose packages are in its cache or its `vendor/`.

| command | what it does |
|---|---|
| `funl add <host>/<owner>/<repo>[@<version>] [--dev]` | adds the package to `package.funl` (to `devDependencies` with `--dev`), fetches it and everything it reaches, and records them in `funl.sum` |
| `funl fetch` | fetches every package the project reaches that is not on the machine, and checks and records `funl.sum` |
| `funl vendor` | copies every package the project uses from the cache into its `vendor/` |
| `funl deps` | prints the packages the project uses |

A word that is one of these is the command, so a program in a file called `add` is run by a path to
it: `funl ./add`.

### `funl add`

```
$ funl add github.com/example/tabular
fetching github.com/example/tabular v0.2.0
fetching github.com/example/csv-kit v0.2.0
added tabular 0.2.0
```

With no version, it takes the repository's newest tag that is `v` and a version — `v0.2.0`, never
`latest` or `v1.0-rc1` — comparing the numbers, so `v0.10.0` is newer than `v0.9.0`. `@0.2.0` (or
`@v0.2.0`) names one. The name it writes is the repository's, with anything an import cannot write
bare turned to `_`: `github.com/example/csv-kit` is added as `csv_kit`.

It changes as little of `package.funl` as it can. The new entry goes after the last one of its
section, with the same indent and the same trailing comma, or on the same line where the section is
written on one line; a section the file does not have yet becomes its last entry; comments stay
where they are. A project whose manifest is

```
{
  name: "report",
  version: "0.1.0", ;; first release
}
```

has, after `funl add github.com/example/tabular`,

```
{
  name: "report",
  version: "0.1.0", ;; first release
  dependencies: {
    tabular: { git: "github.com/example/tabular", version: "0.2.0" }
  },
}
```

A package already there at another version is moved to the one asked for: only its `version`
changes. The manifest is written last, after everything was fetched, so a version that does not
exist never reaches it. `funl add` is refused, and writes nothing, where:

- the repository has no version tag, or not the one asked for;
- the name is already in the manifest for another repository;
- the name is in the other section — a package is a dependency or a dev dependency, not both;
- what was named is a URL (`https://...`) rather than `<host>/<owner>/<repo>`.

### `funl fetch`

```
$ funl fetch
fetching github.com/example/tabular v0.2.0
fetching github.com/example/csv-kit v0.2.0
```

A package is fetched as its tag, `v<version>`, into its directory in the cache, and the packages its
own manifest names after it. Only the version the graph uses is fetched: where a package fetched
later asks for a newer version of one the project also names, the older version is not needed, and
its tag need not exist. A version the graph does use and whose tag is not there is refused with what
`git` said. A package version already in `vendor/` or the cache is not fetched
again, so on a machine that has everything `funl fetch` says nothing. Each package's files are then
held to `funl.sum`, and the versions it does not record yet are recorded. A tag that was moved after
it was first fetched is refused before anything reaches the cache.

### `funl vendor`

```
$ funl vendor
vendored tabular 0.2.0
vendored csv_kit 0.2.0
```

copies each package the project uses from the cache into `vendor/`, laid out as the cache is, so the
project then runs with no cache at all — on a machine with no network, or from a copy of the
project's directory. A package not in the cache is refused, naming `funl fetch`. One already in
`vendor/` is left as it is.

### `funl deps`

```
$ funl deps
report 0.1.0
  csv_kit 0.2.0 github.com/example/csv-kit
  tabular 0.2.0 github.com/example/tabular
    csv_kit 0.2.0 github.com/example/csv-kit
  check 0.1.0 github.com/example/check -- dev -- not fetched
warning: `csv_kit` is wanted at 0.1.0 and at 0.2.0 -- 0.2.0, the newer, is used, and what asked for 0.1.0 has never been run against it
```

prints the project, then each package under the one that asked for it, at the version used (see
[Versions](#versions)); a dev dependency is marked `-- dev` and a package not on the machine
`-- not fetched`. Where two manifests ask for different versions of one package, a `warning:` line
says which was used.

## `funl.sum`

`funl.sum`, beside a project's `package.funl`, records what each package version's files hashed to
when it was fetched, one line each:

```
github.com/example/tiny v0.1.0 sha256:ac491cb0d1866dbb6beab6e0638d04c7d08c8e93a67313573eb4f02cd9b0d624
```

The hash is over the package's files: each file's path and contents, in path order, with `.git`
left out, so a vendored copy hashes as the cached one does. A package whose files hash to something
else now is refused — the version's tag was moved after it was fetched, or the copy was edited. The
project [`packages/moved/`](packages/moved/funl.sum) records a hash its `tiny` no longer matches:

```funl packages/moved/main.funl
import { hello } from tiny
write( hello() )
```

```error
github.com/example/tiny v0.1.0 does not match funl.sum
```

A package version `funl.sum` does not record is not refused.

## Versions

A version is exact: `version: "0.2.0"` means the tag `v0.2.0`, so the manifests decide the whole
graph of packages and there is nothing to solve. When two manifests want different versions of one
package, **the newer is used, and the import that asked for the older says so**. In
[`packages/conflict/`](packages/conflict/package.funl) the project asks for `csv_kit` 0.1.0 and its
package `tiny` for 0.2.0:

```funl packages/conflict/main.funl
import { version } from csv_kit
import { hello } from tiny

write( version() )
write( hello() )
```

```warning
`conflict` asks for `csv_kit` 0.1.0, and 0.2.0, the newer, is used -- `conflict` has never been run against it
```

```output
0.2.0
tiny, over csv_kit 0.2.0
```

A manifest's `funl` key is the oldest FunL its package needs. A package needing a newer FunL is
refused rather than failing with parse errors inside it:

```funl packages/floor/main.funl
import { hello } from tiny
```

```error
`tiny` 0.1.0 needs FunL 99.0.0 or newer
```

The floor in a project's own manifest holds for its own programs too. The project
[`packages/future/`](packages/future/package.funl) asks for FunL 99.0.0, so nothing in it runs:

```funl packages/future/main.funl
write( "hello" )
```

```error
`future` 0.1.0 needs FunL 99.0.0 or newer
```

## A manifest is data

A manifest holds maps, lists, strings, numbers, `true`, `false` and `()`, and nothing that runs.
Anything else is refused by what it is, and so is a key that is not a manifest key — every mistake
in the file, each with the manifest's own line. The project
[`packages/broken/`](packages/broken/package.funl) wrote a name where a string goes, and slate's
`scripts`, which a FunL manifest does not have:

```funl packages/broken/main.funl
import { hello } from tiny
```

```error
a manifest is data, and the name `broken` is not
```

The same refusal goes on to say `` `scripts` is not a manifest key ``.

## Constructors across packages

**A constructor is unique across the whole program, packages included**, so a term Prolog makes,
known only by its name and number of fields, names one constructor. Two packages declaring
`node(l, v, r)` cannot be used together, and the refusal names both. In
[`packages/garden/`](packages/garden/package.funl), `trees` declares `tree = leaf | node(l, v, r)`
and `bushes` declares `bush = twig | node(l, v, r)`:

```funl packages/garden/main.funl
import { node } from trees
import { twig } from bushes
```

```error
so package `trees` 0.1.0 and package `bushes` 0.3.0 cannot be used together
```

## What a package cannot bring

A package is FunL source — `.funl` and `.lfunl` modules and Prolog files — and the data files its
own code reads. It cannot add a `funl:` module: those are compiled into the `funl` executable. It
can use every `funl:` module the running `funl` has.
