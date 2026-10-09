---
title: Packages
weight: 56
---

# Packages

**FunL's packages are slate's packages, as the [modules chapter](modules.md#packages) already
decided** (user, 2026-10-08): a bare word names a package; the project is found by walking up from
the entry file; a package's entry file is its manifest's `main` and its other modules are listed in
a `modules` map, never searched for; the cache is `$HOME/.funl/pkg`, overridable by `FUNL_CACHE`;
the lock file records the hash of the extracted tree; `funl add github.com/owner/repo[@version]`
edits the manifest surgically and writes it last; `funl vendor` copies the resolved graph into
`vendor/`; the manifest is `package.funl`, read as data; and a package cannot carry a native.

This chapter is the rest of it: the whole of the shape written out in FunL's terms, what FunL does
today, the places FunL cannot copy slate as it stands, the options for building it, and the
questions only the user can answer. It is a chapter of its own rather than a section of
`modules.md` because that chapter is already nine hundred lines and this one is read on its own by
whoever builds milestone 9's last bullet.

slate's own pages are the source for every "as slate does" below: its `docs/reference/packages.md`,
and `packages.sysl`, `fetch.sysl`, `sum.sysl`, `vendor.sysl` and `add.sysl` in its tree.

## The problem

A program wants somebody else's FunL. Today it can only copy the file in:

```
;; report.funl
import { rows, total } from tabular          ;; the package's main
import { pivot } from tabular/pivot          ;; one of its other modules

for r <- rows( "sales.csv" ) do write( r )
write( total(rows("sales.csv"), "amount") )
```

What has to exist for that to run: a manifest saying which `tabular` (a repository and a version), a
command that fetches it, a place it is kept, a rule for what `tabular` and `tabular/pivot` resolve
to, a record that the code compiled today is the code compiled yesterday, and a way to run all of it
on a machine with no network.

## What FunL did before packages

This section is the starting point the design was written against; [the decided block](#decided)
says what is built, and the [reference page](../reference/packages.md) what FunL does now.

**The module system is built; packages are not.** A quoted path is a file, read relative to the
importing file, and a module is known by its real path:

```
;; lib/geo.funl
export def double( x ) = 2 * x

;; a.funl
import { double } from "lib/geo.funl"
write( double(21) )
```
```
42
```

**A bare word is already parsed as a package specifier** (`PackageSpec` in `ast.sysl`, made by
`import_line` in `parse.sysl`) and refused by `imports_of` in `module.sysl`, which is the seam this
chapter's resolver goes into:

```
import { parse_csv } from tabular
```
```
error: unknown module `tabular`
 --> b.funl:1:27
  |
1 | import { parse_csv } from tabular
  |                           ^^^^^^^
```

A dotted bare word is read as one package name, and a bare file name gets a note:

```
import { area } from geometry.funl
```
```
error: unknown module `geometry.funl`
 --> f.funl:1:22
  |
1 | import { area } from geometry.funl
  |                      ^^^^^^^^^^^^^
  |
  = note: a file is imported by a quoted path: `"geometry.funl"`
```

slate's spelling of a package's other module, `tabular/pivot`, does not parse, and quoted it is a
file path:

```
import { parse_csv } from tabular/rows
```
```
error: expected the end of the line
 --> c.funl:1:34
  |
1 | import { parse_csv } from tabular/rows
  |                                  ^ found `/`
```
```
import { parse_csv } from "tabular/rows"
```
```
error: `tabular/rows` is neither a FunL module nor a Prolog file
 --> d.funl:1:27
  |
1 | import { parse_csv } from "tabular/rows"
  |                           ^^^^^^^^^^^^^^
  |
  = note: a FunL module is a `.funl` or `.lfunl` file, and a Prolog file a `.pl` file
```

**Prolog's `:- import` reads an atom as a file path**, exactly as it reads a string:

```
:- import(tabular).
```
```
error: `tabular` cannot be read: no such file or directory
 --> p.pl:1:11
  |
1 | :- import(tabular).
  |           ^^^^^^^
```

**The `funl` executable has no subcommands**: its one form is `funl <program> [argument ...]`, so
`funl add github.com/x/y` tries to run a file called `add`:

```
funl: cannot read add: no such file or directory
```

**A manifest written as a FunL map literal already parses** — this file runs, doing nothing:

```
;; package.funl
{
  name: "tabular",
  version: "0.1.0",
  main: "tabular.funl",
  modules: { rows: "rows.funl" },
  dependencies: {
    csv_kit: { git: "github.com/example/csv-kit", version: "0.2.0" }
  }
}
```

and so does one with a trailing comma, which slate's manifests write and `slate install` preserves:

```
write( {
  name: "tabular",
  version: "0.1.0",
  modules: { rows: "rows.funl" },
} )
```
```
{"name": "tabular", "version": "0.1.0", "modules": {"rows": "rows.funl"}}
```

**Constructors are unique across the whole program**, which two packages make bite:

```
;; lib/t1.funl
export data tree = leaf | node(l, v, r)
;; lib/t2.funl
export data bush = leaf | node(l, v, r)

;; g.funl
import { node } from "lib/t1.funl"
import * as b from "lib/t2.funl"
```
```
error: `lib/t2.funl` could not be loaded
 --> g.funl:2:20
  |
2 | import * as b from "lib/t2.funl"
  |                    ^^^^^^^^^^^^^
  |
  = note: error: the constructor `leaf` of 0 fields is already declared at lib/t1.funl:1:20; two constructors with one name and one number of fields cannot be told apart
 --> lib/t2.funl:1:20
  |
1 | export data bush = leaf | node(l, v, r)
  |                    ^^^^

error: the constructor `node` of 3 fields is already declared at lib/t1.funl:1:27; two constructors with one name and one number of fields cannot be told apart
 --> lib/t2.funl:1:27
  |
1 | export data bush = leaf | node(l, v, r)
  |                           ^^^^
```

## The shape, as slate does

Everything in this section is slate's rule translated, and is what every option below builds.

### The manifest

A project or a package is a directory holding a **`package.funl`**, which is one FunL map literal:

```
;; package.funl
{
  name: "tabular",
  version: "0.2.0",
  main: "tabular.funl",                       ;; what `import ... from tabular` reaches
  modules: { pivot: "pivot.funl" },           ;; what `tabular/pivot` reaches
  dependencies: {
    csv_kit: { git: "github.com/example/csv-kit", version: "0.2.0" }
  },
  devDependencies: {                          ;; this package's own tests; no consumer resolves them
    check: { git: "github.com/example/check", version: "0.1.0" }
  }
}
```

- **The keys are slate's**: `name`, `version`, `main`, `description`, `homepage`, `license`,
  `modules`, `dependencies`, `devDependencies`, and FunL's own `funl` floor ([decided](#decided)
  7), and nothing else — an unknown key is named. `name` and `version` are required; a dependency
  takes `git` and `version`, both required. A name may be in one section or the other, never both.
  (slate's `scripts` is left out until a program wants shipping; [decided](#decided) 8.)
- **It is parsed by FunL's own parser and never run**: a restriction pass over the parsed
  expression accepts a map, a list, a string, a number, `true`, `false` and `()`, and refuses
  anything else by what it is — "the name `b`", "a call", "a `+` between two things" — carrying on
  so three mistakes report three, and refusing a repeated key. A resolver deciding what to fetch is
  reading a file that arrived with the thing it is deciding about; a manifest that ran would make
  that a code-execution step. The diagnostics are the program diagnostics, caret and all, because
  the manifest is FunL source.
- **A bare key is a string**, as it already is in a FunL map (`{name: "t"}("name")` is `"t"`), so
  `name:` and `"name":` are the same key. Comments are `;;`.

### Specifiers and what they resolve to

```
import { area } from "geometry.funl"      ;; a quoted path: a file, relative to this file
import { rows } from tabular               ;; a package: its manifest's `main`
import { pivot } from tabular/pivot        ;; a package's listed module: `modules.pivot`
import { parse } from funl:json            ;; one of FunL's own
```

- **A package's import name is its key in the `dependencies` map of the manifest that governs the
  importing file**, and its other modules are exactly the keys of its own `modules` map. A key
  `funl add` writes is the repository's name with anything not legal in a name turned to `_`
  (`csv-kit` becomes `csv_kit`), as slate's is, because the import writes it bare.
- **The manifest that governs a file is the nearest one above it.** A file of the program is
  governed by the project's; a file inside a package is governed by that package's own, so a
  package's `import ... from csv_kit` reaches *its* dependency and never the program's. A package's
  own relative imports (`import "util.funl"`) are ordinary file imports.
- **The resolved file is then a module like any other**: keyed by its real path, loaded once per
  machine, its top level run once, its exports a snapshot (all decided in
  [modules](modules.md#a-file-is-a-module)). A package at one version has one directory, so two
  importers of `tabular` share one module, and that is what makes `x is tabular.row` mean the same
  thing in both.
- **Refused before anything runs, each against the import line**: a bare word no governing manifest
  names (with the names it does); a module the package's `modules` map does not list (with the ones
  it does, never a search, so a private helper in a package cannot be imported by accident); a
  package with no `main` imported bare; and a package not on the machine — ``tabular 0.2.0 is not in
  ~/.funl/pkg/github.com/example/tabular/@v0.2.0 -- run `funl fetch` ``, slate's sentence.

### Versions: exact, and no lockfile

**A version is exact** — `version: "0.2.0"` means that tag, `v0.2.0` — so the whole graph is
determined by the manifests and there is nothing to solve. The one case exactness does not settle,
two packages wanting different versions of one, **takes the newer and says so**, naming which
package asked for the older one and that it has never been run against the newer. (This is also
what the sysl compiler's minimal version selection answers, the highest version anybody asked for.)

**`funl.sum` is not a lockfile**, for slate's reason: the manifests already pin every version, so a
lockfile would restate them. What is still open is whether a version is the same *bytes* — a tag can
move — so `funl.sum`, beside `package.funl`, records one line per package version:

```
github.com/example/tabular v0.2.0 sha256:9bc4ef1e645bb3b6ab9d0e59fb2c3bc966a816ee4145f9560aba0e05586688a6
```

The hash is over the **extracted tree**, every file's path and contents length-framed, sorted by
path, `.git` excluded — so it survives a change of transport and a vendored copy hashes the same as
the cached one. It records the **whole graph**, packages reached through other packages included, and
it is **append-only**: an entry for a version nothing uses any more is kept, so a tag moved after you
left it is still caught. A mismatch refuses the build, naming the package, both hashes, and that the
tag was moved or the cache tampered with.

### Where things are, and offline use

- **The project is found by walking up from the entry file's real path**, so a program works wherever
  it is run from and a script symlinked into `~/bin` still finds its project. **A file under no
  project is not an error**: a program that imports no package needs no manifest.
- **The cache is `$HOME/.funl/pkg/<host>/<owner>/<repo>/@v<version>/`**, `FUNL_CACHE` replacing
  `$HOME/.funl/pkg`. The version is in the path, so two projects on different versions share the
  cache.
- **`vendor/` beside the manifest is the cache's layout inside the project**, `@v` and all, and is
  read in preference to the cache; nothing after the lookup knows which it was, and `funl.sum`
  verifies both alike.
- **Running a program never touches the network.** `funl prog.funl` reads manifests and the cache
  (or `vendor/`) and nothing else; only `funl add` and `funl fetch` fetch. So a project whose cache
  is warm, or which is vendored, runs on a machine with no network, and a missing package is a
  refusal naming the command that fetches it rather than a download in the middle of a run.

### The commands

| command | what it does |
|---|---|
| `funl add github.com/owner/repo[@version] [--dev]` | writes the dependency into `package.funl` (into `devDependencies` with `--dev`), fetches it and everything it reaches, appends to `funl.sum`; with no version, the newest `v`-prefixed tag |
| `funl fetch` | fetches everything the manifest reaches that the cache lacks, and checks `funl.sum` |
| `funl vendor` | copies the resolved graph into `vendor/` |
| `funl deps` | prints the resolved graph, a dev dependency marked `-- dev`, one not on the machine `-- not fetched` |

**`funl add` is slate's surgical rewrite**: it changes the smallest run of bytes that has to change —
a new entry takes its siblings' indent and their trailing comma, a `;;` comment is never moved — reads
the result back through the manifest reader before writing, and **writes the manifest last**, after
the fetch succeeded, so a version that does not exist never reaches the file. A name already there at
another version is moved; a name already there from another repository is refused.

**A subcommand word is told from a program by being one of these words**, as slate does: `funl add`
is the command, and a program file called `add` is run as `funl ./add`.

### Prolog's view

A FunL file's exports are what Prolog's `:- import` sees (decided in
[modules](modules.md#questions-decided)), and a package's modules are FunL files, so Prolog reaches
them the same way, naming a package with an atom ([decided](#decided) 4). **The standalone `prolog` executable has no
FunL front end** and so no packages, as it has no `funl:` modules.

### What a package can and cannot bring

**A package is FunL source and nothing else** — `.funl` and `.lfunl` modules, and ([decided](#decided) 5)
Prolog files — plus whatever data files its own code reads relative to itself.

**It cannot bring a native.** A native is sysl code compiled into the `funl` binary, registered in
`natives.sysl` and listed in the module table (`modules.md` §
[who can add one](modules.md#who-can-add-one)), and a package arrives after the binary was built. So
a package **can use** every `funl:` module the running binary has — and where it imports one this
build left out (`funl:http` under `--no-default-features`), the refusal is the existing
`left_out_module` sentence, drawn against the package's own import line, which names the feature to
build with. It **cannot add** one: a package that needs a C library or a sysl package FunL does not
wrap is a pull request to this repository (or first an org binding, then the native module over
it). That is slate's line exactly, and slate's `pg` package is the case that shows its cost.

## Where FunL cannot copy slate as it stands

1. **The transport.** slate fetches GitHub's tarball over its own HTTPS client on OpenSSL, gunzips
   with miniz and reads the tar itself; all three are in slate's core. FunL's HTTPS is libcurl,
   **behind the optional `http` feature**, and it carries neither a gunzip nor a tar reader. The sysl
   compiler itself, which is also a sysl program with a package manager, does not fetch tarballs at
   all: it runs `git clone --quiet --depth 1 --branch v<version>` through `sysl.process` and asks
   `git ls-remote --tags` for the newest version. Options A and B below differ exactly here.
2. **Constructors are unique program-wide**, so two packages each declaring `node/3` cannot be used
   together (the probe above). slate's classes are per-module values and never collide. Kept, the
   refusal naming both packages ([decided](#decided) 3).
3. **The manifest's syntax is FunL's map literal**, which took no trailing comma until it was
   decided (open question 6) that every list, set and map literal does.
4. **Prolog has one flat namespace and its own `:- import`**, which slate has no counterpart for.
   An atom names a package ([decided](#decided) 4).
5. **FunL's grammar is not final** (releases stay 0.0.x until it is), so a package written against a
   newer FunL fails here with parse errors that blame the package rather than the version. slate's
   manifest has no floor key; FunL's has `funl` ([decided](#decided) 7).

## Options

### A. slate's package manager whole: HTTPS tarballs

Port slate's `packages.sysl`, `fetch.sysl`, `sum.sysl`, `vendor.sysl`, `add.sysl` and the manifest
reader, fetching `https://github.com/<owner>/<repo>/archive/refs/tags/v<version>.tar.gz` over
`sysl-lang/curl` and unpacking it with `sysl-lang/miniz` plus a gzip-framing and tar reader written
here; the newest version from GitHub's tags API.

```
$ funl add github.com/example/tabular
fetching github.com/example/tabular v0.2.0
added tabular 0.2.0
```

- **Costs**: about 2,300 lines — the six files (~1,750 in slate, `manifest.sysl` the largest at
  400), a tar reader (slate's `tar.sysl`, ~260) and gzip framing (~150), the CLI dispatch in
  `funl/main.sysl`, the `PackageSpec` arm in `module.sysl`, the `/` in `import_line`, Prolog's
  `:- import` arm — and ~800 lines of tests over a fixture cache and an in-memory tarball. A new
  dependency, `miniz`, in the core.
- **Binds**: fetching exists only in a build with `http`, so `funl --no-default-features` can run
  packages but not fetch them (refused with the feature named); only GitHub; GitHub's API rate limit
  on a version-less `add`, which has to be named as itself.

### B. slate's shape, the sysl compiler's transport: `git` — recommended

Everything in [the shape](#the-shape-as-slate-does) exactly as written, but a fetch is
`git clone --quiet --depth 1 --branch v<version> <url> <dir>.partial` through `sysl.process` (which
`funl:process` already wraps), `.git` removed, the tree hashed, then renamed into place; a
version-less `funl add` asks `git ls-remote --tags <url>`.

```
$ funl add github.com/example/tabular
fetching github.com/example/tabular v0.2.0
added tabular 0.2.0
```

- **Costs**: about 1,900 lines — option A less the gzip and tar readers (`fetch.sysl` becomes the
  sysl compiler's ~220 lines of clone, verify and rename), no new dependency — and
  the same ~800 lines of tests, the fetch tests cloning a fixture repository made with `git init` in
  a scratch directory rather than serving a tarball.
- **Binds**: `git` on the machine that fetches (a refusal names it: ``cannot fetch github.com/example/
  tabular v0.2.0: git is not on this machine``). In exchange it fetches in **every** build, the
  `--no-default-features` one included; any git host works (GitLab, Codeberg, a private server,
  with the user's own git credentials); no API rate limit; and it is the convention every org
  package is already fetched by.
- **Differs from slate for a stated reason**, which is the user's rule for departing from it: FunL
  does not have slate's HTTPS-and-gunzip core to stand on, and the one other sysl program with a
  package manager shows the cheaper road. Nothing a FunL user writes or reads differs — manifest,
  specifiers, cache layout, `funl.sum`, `vendor/` and commands are slate's.

### C. Packages that carry natives

A package may also hold sysl source declaring natives, and `funl build` compiles a project-specific
`funl` with every such package's natives registered, much as a sysl workspace member is a program
over a library.

- **Costs**: everything in A or B, plus a manifest key for sysl sources and their own `dependencies`, a
  generated sysl program per project (`main` plus an `install_` line per package), a sysl compiler
  on every user's machine at `funl build` time, a cache of built binaries keyed over every package,
  and a story for the `prolog` executable. Several thousand lines, and a second distribution model.
- **Binds**: a program is no longer run by *the* `funl`; it is run by its project's. It contradicts
  the decided "a package cannot carry a native, as in slate", so it would need that decision
  reopened in slate's terms first.

## Decided

**Decided (user, 2026-10-09, questions 1–5, 7 and 8 "as recommended"):**

1. **The transport is `git` (option B).** A fetch is `git clone --quiet --depth 1 --branch
   v<version> <url> <dir>.partial` through `sysl.process`, `.git` removed, the tree hashed, then
   renamed into place; a version-less `funl add` asks `git ls-remote --tags <url>`. It fetches in every
   build, the `--no-default-features` one included. Rejected: HTTPS tarballs (option A), which would
   cost a new core dependency and a feature gate for a transport FunL has no other use for; and
   packages carrying natives (option C), which contradicts the decided "a package cannot carry a
   native".
2. **A package's other module is `tabular/pivot`**, as slate writes it. A `.` already means field
   selection on an imported module map, and a `/` cannot be mistaken for it. A dotted bare word stays
   refused, its note naming the slash form. Rejected: `tabular.pivot`.
3. **Constructors stay unique across the whole program, packages included**, because that is what
   lets a term Prolog made, known only by functor and arity, name one constructor. The refusal names
   both packages and their versions. Revisit it only when a real pair of packages collides; the
   alternative (constructors scoped to their module, a Prolog-made term resolved by the importer's
   scope) is a change to the term mapping in [prolog](prolog.md#the-term-mapping), not to packages.
4. **In Prolog an atom is a package and a string is a file**, mirroring FunL's bare word against
   quoted path: `:- import(tabular).` and `:- import(tabular/pivot).` (the compound
   `/(tabular, pivot)`, standard Prolog's own module notation) reach the package, and
   `:- import("family.pl").` stays a file. Rejected: an atom read as a file path, which is what it
   meant before and which no test or page relied on.
5. **A package may carry Prolog files**: `main` and `modules` may name a `.pl` file, imported whole
   as `import "file.pl"` is (`import tabular/rules` with no braces, every predicate entering the
   shared namespace). `.lfunl` is allowed everywhere `.funl` is.
7. **A manifest may carry `funl: "0.0.5"`**, optional, the oldest FunL the package needs. It is
   checked when the package is resolved and refused naming the package and both versions, because
   FunL's grammar is still moving and a package written for a newer one would otherwise fail with
   parse errors that point into the package. It is a floor, moved only when the package needs a
   newer FunL. Rejected: no floor key, as slate's manifest has none.
8. **The commands are `add`, `fetch`, `vendor` and `deps`**; `bundle`, `brew` and manifest
   `scripts` come when a FunL program first wants shipping, each as slate has it. There is no
   `install` alias, `add` being the word the modules chapter already chose.

**Built** (stage 1): the manifest reader, finding the project by walking up from the entry file,
`funl.sum` and the tree hash, the cache and `vendor/`, and resolving `import ... from tabular` and
`tabular/pivot` (and Prolog's atoms) against what is already on the machine. **To build** (stage 2):
the four commands and the `git` transport, which fill the cache and `vendor/` laid out as stage 1
reads them, and write `funl.sum`. A run never touches the network.

Write it so the language-neutral half — tree hash, sum file, cache layout, vendor copy — reads as
slate's and sysl's do line for line; if a third language wants it, that half is the part to lift
into an org package, with three implementations in view rather than one.

## Open questions

6. **May a map or list literal end with a trailing comma?** `funl add` writes entries in the file's
   own style, and slate's manifests end every entry with one. **Decided (user, 2026-10-09): yes, in
   every map, set and list literal, language-wide** — one line or several — rather than a
   manifest-only dialect, so a manifest stays ordinary FunL. A comma that ends nothing is still
   refused: `[,]`, `{,}`, `[1,,2]`, `{a: 1,,}`. A tuple, an argument list and a parameter list do not
   take one: neither this chapter nor the language chapter says they should, and `(x,)` is refused
   today, as it stays.
