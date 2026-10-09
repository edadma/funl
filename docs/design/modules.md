---
title: Modules and native modules
weight: 55
---

# Modules and native modules

**FunL takes slate's module system and slate's native modules whole, translated into FunL's
syntax.** slate (`~/dev/slate-language/slate`) has already argued every question a module system
raises — what a module is, how a name crosses, when an import is resolved, how a built-in module
wraps a sysl package, which libraries go behind a feature — and has shipped the answers. FunL is a
second language on the same compiler, the same `gc`, the same `parsing` and the same org packages, so
nothing is gained by arguing them again. This chapter says what the translation is, and lists only
the places where FunL is a different kind of language and cannot copy slate as it stands.

> **Decided (user, 2026-10-08): modules and native modules as slate does.** Every rule below marked
> "as slate does" is slate's own, from its `docs/reference/modules.md`, `docs/reference/packages.md`,
> `docs/library/` and the module and feature sections of its `CLAUDE.md`. Revisiting one means
> revisiting it in slate's terms first. The eleven places FunL differs from slate were decided the
> same day (the last section); the prelude's four open questions are the only parts not settled.

## A file is a module

**As slate does: a file is a module, and what another file can see is what it writes `export` in
front of.** A `.funl` file and a `.lfunl` file are both modules; the literate one is tangled first,
exactly as when it is run.

```
;; geometry.funl

export data shape = circle(r) | square(s)

export def area( circle(r) ) = 3 * r * r
export def area( square(s) ) = s * s

export def sides( #square, 4 )
export def sides( #triangle, 3 )

def helper( x ) = x                    ;; no other file can reach this
```

`export` goes in front of a `def`, a `val`, a `var` or a `data`. A `data` crosses as both halves at
once, as slate's does: the constructors as values, and the declaration the resolver uses for
patterns and `x is shape`.

> **Decided (2026-10-07) — a `data` type's name is its file's.** A constructor was already a name of
> the block declaring it; the type's name now is too, rather than one name for the whole machine.
> **Decision:** `x is t` finds `t` among the file's own `data` types, then those it imported — by
> name (`import { shape }`, `as` renaming it) or qualified, `x is geometry.shape` (and
> `x is geometry.circle`) through `import * as geometry` — and a type the file neither declared nor
> imported is "not a type". Two modules may each declare a type `shape`; a file may not declare a
> type an import named. A record prints with its constructor's bare name, never qualified.
> **Constructors stay unique across the program**: the [language chapter's
> rule](language.md#data) that two constructors of one name and one number of fields are refused
> wherever written still holds between modules, so a term Prolog made, known only by functor and
> arity, always names one constructor and needs no scope to be read.

A file takes what it needs by name, or takes the whole module under one name:

```
import { area, circle, sides as edges } from "geometry.funl"
import * as geometry from "geometry.funl"

write( area(circle(2)), geometry.area(geometry.square(3)) )
free n
if edges(#square, n) then write( n )
```

**The import and export lines are the new syntax this chapter adds**; everything else in its
sketches parses under the current grammar (each was run through `funl/funl`, which refused only the
names nothing defines yet).

## The three kinds of specifier

**As slate does: a quoted path is a file, a bare word is a package, and `funl:name` is one of FunL's
own.** They are different syntax rather than three readings of one string.

```
import { area } from "geometry.funl"     ;; a quoted path  -- a file
import { parse_csv } from tabular        ;; a bare word    -- a package
import { parse, stringify } from funl:json
```

- **A path is relative to the file the import is written in**, which is already how `import
  "family.pl"` reads its path.
- **The extension decides**: `.funl` and `.lfunl` are FunL modules, `.pl` is Prolog (see the open
  question on Prolog files), and anything else is refused, slate's asset imports being a browser
  concern FunL does not have.
- **A bare `geometry.funl` is refused rather than guessed at**: an unquoted specifier is a package.

## Imports are resolved before anything runs

**As slate does: the machine never sees an import.** FunL already works this way for Prolog files —
`compile_funl` loads every `import "file.pl"` before the resolver runs, so that the resolver knows
the predicates by name. A FunL module joins the same pass: its file is parsed, resolved and compiled
first, its top level runs once before the importing file's, and the importer's resolver sees its
export list.

**A module is instantiated once per machine** and runs its top level once however many files import
it, as slate's does and as a Prolog file imported twice already is.

## A module is a value

**As slate does: a module is a value of a kind the language already has, so there is nothing new for
the collector to trace.** slate's module is an object; **FunL's is an immutable map from export name
to value**, so `geometry.area(...)` is the field selection FunL already has on a map whose keys are
strings (`m.name` reads `m("name")` today). It follows, as in slate, that **a module's exports are a
snapshot** taken when its file finishes: an `export var` the module changes afterwards is not seen
changing from outside.

## Names, and the builtin rule

**An imported name is a top-level name of the importing file**, declared by the import. Two rules
follow from decisions already made:

- **It shadows a builtin of the same name** — the [shadowing rule](language.md#assignment-and-assignment-that-undoes-itself) treats an
  import like a top-level `def`. A program that imports `parse` from `funl:json` has `parse` mean
  that wherever the builtin would have answered.
- **It may not also be defined by the file**, nor made a top-level variable: as slate refuses a
  definition taking a name its own block declared, and as FunL already refuses ``parent/2` cannot be
  defined twice` after `import "family.pl"`. `as` renames on the way in.

## What is refused, and when

All before the program runs, as slate does:

- **A circle of imports**, with the chain named.
- **Asking for a name a file does not export**, naming what it does export.
- **A name nothing has bound.**
- **Every complaint is drawn against the file it is about**, so an error inside `geometry.funl`
  reports `geometry.funl`'s line, not the importer's.

## Packages

**As slate does**: a bare word names a package; the project is found by walking up from the entry
file, and a file under no project is not an error; a package's entry file is its manifest's `main`,
required for anything imported; a package exposes more modules only through its manifest's `modules`
map, never by a search; the cache is `$HOME/.funl/pkg`, overridable by `FUNL_CACHE`; the lock file
records the hash of the extracted tree; `funl add github.com/owner/repo[@version]` edits the manifest
surgically and writes it last; `funl vendor` copies the resolved graph into `vendor/` in the cache's
layout. The manifest is `package.funl`, read as data, as slate's `package.sl` is.

**A package cannot carry a native**, as in slate: a package is FunL source, and a native is code in
the `funl` binary. What this means for a third party is under [native modules](#who-can-add-one).

## Built-in modules: `funl:name`

**As slate does, there are two kinds, and they differ in one place.** A built-in module either has
FunL source, carried in the binary as a `raw"""` block in a `*_source.sysl` file and loaded exactly as
a file is, following its own imports; or it has none, and arrives as an export list over natives. The
cycle check, the export-name check and its "did you mean" are the same for both.

**The natives behind a module register where they are written**, through the open registry FunL
already has (`native_fn.sysl`: `register`, `register_generator`, `register_constant`) and an
`install_` line in `natives.sysl` per file. **The registry is keyed by name and the builtins are
one scope**, so a module's natives are registered under a prefixed name — `json_parse`,
`sqlite_open` — into the scope only built-in modules compile in, and the module exports them under
the short names. That is slate's `slate:actor` arrangement, forced for slate's reason: `run`, `open`,
`read` and `parse` are each wanted by more than one module and by programs.

**A module rather than a global is decided by whether the word is one a program wants for its own**,
as slate decides it. `read_file`, `now`, `run`, `env` and `parse` all are, so all live in modules and
none is added to the global builtins.

**What stays native is what FunL cannot do at all**, as slate's rule reads: a binding to a C library,
or to sysl's standard library. Anything FunL code can express is written in FunL and carried as
source.

**A table in one file names the modules**: `builtin_module`, `builtin_module_names`,
`builtin_has_source`, `builtin_source` and each module's export list, slate's `stdlib.sysl`. A test
checks that every name an export list claims is one the scope actually has, since a module nobody
imports is never compiled.

### Features keep the core link line short

**As slate does: a native module whose library is not on every machine is a package feature, named
for the module it serves**, never for the library under it. Its dependency is `optional = true` in
the root `package.hocon`; its files are `#if feature_x` after the `module` line; its `install_` line,
its arm in the module table, its tests and its import strings in `tests_kit.sysl` are gated with it.
**A link directive is never pruned**, so a library bound in the core would sit on the link line of
every `funl` and `prolog` build whether a program used it or not; a feature is how it stays off.

**`left_out_module` is the diagnostic**, as in slate: ``funl:http` is not in this build -- it is
behind the `http` feature, so build with `--features http``, rather than "FunL has no module called".

**Both shapes are gated**: `sysl test .` and `sysl test . --no-default-features`, the second being
the one that finds a missed gate.

**The release is unchanged by the features**: the shipped `funl` is built with `default`. The
`prolog` member has no FunL front end and imports no `funl:` module, so it depends on no feature.

### Who can add one

**A native module is added to this repository**, as slate's are: a `*.sysl` file registering the
natives, the wrapped org package in `package.hocon` (behind a feature if its library is not
everywhere), the module table's rows, and tests. A third party writing **FunL** publishes a package; a
third party needing a **C library** opens a pull request here, or first writes the sysl binding as an
org package and then the native module over it. This is slate's line exactly, and slate's `pg` is the
case that shows its cost: a package cannot open a door the language never opened.

## Values that cross

As slate does, with FunL's kinds in place of slate's:

| sysl side | FunL value |
|---|---|
| integer, real, string, bool | `Int`/`Big`, `Real`, `Str`, `true`/`false` |
| absent (`None`, SQL `NULL`, JSON `null`) | `undefined` |
| a struct of plain data (a `stat`, a row, a response) | an immutable map keyed by field name, so `row.title` reads |
| a list or slice | a list |
| a JSON object, a header table | an immutable map |
| a live resource (a connection, a statement, a child process) | a handle — see the first decided question |
| an iterator or stream (rows, lines, directory entries) | a generator — see the second |
| bytes | see the eighth decided question |

A resource is closed by an explicit `close()`, as slate's are, and the collector's finalizer closes
any the program forgot, as slate's do.

## The first batch

Each sketch is short; the module's page under `docs/library/` will carry the whole surface.

### `funl:fs` — core, over sysl's `sysl.fs`

```
import { read_file, write_file, lines, read_dir, stat, exists, mkdir } from funl:fs

text = read_file( "notes.txt" )
write_file( "copy.txt", text )
for line <- lines( "notes.txt" ) do write( line )
for name <- read_dir( "docs" ) if stat( "docs/" + name ).kind == #file do write( name )
if not exists( "cache" ) then mkdir( "cache" )
if read_file( "missing.txt" ) then write( #there ) else write( #absent )
```

`lines` and `read_dir` are generators (the second decided question); a missing file is failure (the
third).

> **Built — a name that is not UTF-8.** A file's text that is not UTF-8 fails `read_file` and
> `lines`, as `decode` does. A directory holding a *name* that is not UTF-8 is a fault instead,
> `system_error` with sysl's "not UTF-8 at byte N": `read_dir` cannot give the name as a string,
> and failing would say the directory is not there. No byte-string form of `read_dir` is offered;
> the design names none.

### `funl:json` — core, over `sh.sysl.json`

```
import { parse, stringify } from funl:json

v = parse( '{"name": "funl", "tags": ["vm", "prolog"]}' )
write( v.name, v.tags(0) )
write( stringify({name: "funl", version: [0, 0, 2]}) )
if not parse( "{oops" ) then write( #malformed )
```

An object is a map, an array a list, `null` `undefined`. `sysl-lang/json` is pure sysl, with no C
library, so it costs no link line.

> **Built — what the sketch left open.** `stringify(v, indent)` lays the text out (0 to 16 spaces;
> 0 is compact), so a native may take a bounded range of arguments (`register_between`), and Prolog
> sees one predicate per count, `json:stringify/2` and `/3`. A list, a tuple, a range with an end, an
> array and a buffer write as an array; a rational as the nearest real; a map's keys must be strings
> (or atoms). Malformed text fails, as the third decided question says, so `parse` carries no
> position; a function, a set, another atom, an endless range or a value holding itself faults.

### `funl:process` — core, over `sysl.process`, `sysl.env`, `sysl.args`

```
import { args, env, run, exit } from funl:process

for a <- args do write( a )
home = env("HOME") | "/tmp"
r = run( "git", ["status", "--short"] )
write( r.status, r.out )
if r.status != 0 then exit( 1 )
```

`env` and `args` are in `funl:process`, as they are in `slate:process`. An unset variable is failure,
so `|` gives the default.

> **Built — what the sketch left open.** `args` is a value, as the sketch writes it: a constant
> builtin (`register_constant`), the list of the driver's arguments after the program's name (applying a constant, `args(0)`, is applying
> its value, as it is for a variable holding the same value), which
> `run_source` puts on the machine before compiling. `run(program, args?, options?)` captures both
> streams and gives a map of `out`, `err` and exactly one of `status`, `signal` or `timed_out`, so the
> two ways a child can end without a status are failures to read rather than sentinels; a program
> not installed fails, one that may not be run faults with `permission_error(execute, source_sink,
> P)`. The options are `cwd`, `env`, `inherit_env` and `timeout`, and **`env` adds to this program's environment
> rather than replacing it**, which is `sysl.process`'s rule (slate's replaces). `run_lines` is
> `run`'s standard output as a generator of lines. `exit(status)` is `halt/1`'s `Exits`, 0 to 255,
> uncaught by `catch`. `run` and `run_lines` take one to three arguments (`optional_past`, the
> generating twin of `register_between`), so Prolog sees `process:run/2`, `/3` and `/4`.
> `env()` with no argument is a map of every variable (`sysl.env.vars`). The option `inherit_env`
> (default `true`, as `sysl.process`'s) set to `false` gives the child exactly the `env` option's
> variables instead of adding them.

### `funl:time` — core, over `sysl.time`

```
import { now, monotonic } from funl:time

t0 = monotonic()
write( monotonic() - t0 )
write( now() )
```

slate's instant, duration and calendar kinds come later, on the same `sysl.time`.

> **Decided — what `funl:time` is before it has kinds.** A point in time is a whole number of
> milliseconds since 1970 (`now()`, the argument of `format` and `fields`, the answer of `parse` and
> `make`); `monotonic()` is milliseconds as a real from an unnamed origin; `sleep(ms)` blocks. An offset
> is whole minutes east of UTC and, left out, is UTC. The surface is `now monotonic sleep format parse
> fields make local_offset`; `parse` of text that is not a timestamp and `make` of a date that does not
> exist fail; a wrong-kind argument is a fault. The page is `docs/library/time.md`.

### `funl:http` — the `http` feature, over `sysl-lang/curl`

```
import { fetch } from funl:http
import { parse, stringify } from funl:json

resp = fetch( "https://example.com/data.json" )
if resp.status == 200 then write( parse(resp.body).name )
resp = fetch( "https://example.com/api", {method: "POST", body: stringify({q: 1})} )
```

`fetch` is slate's name. Here it blocks; the promise-answering form comes with
[`async` and `await`](#async-and-await). **A feature because libcurl is not on every machine** a `funl` is built on, and its link
line would otherwise sit under every build; on by default, so the release has it.

> **Built — what the sketch left open.** `fetch(url)` and `fetch(url, options)` are one native
> taking one or two arguments (`register_between`), which Prolog sees as `http:fetch/2` and
> `http:fetch/3`. The options are `method` (a string or an atom, any case), `headers`, `body` (a
> string or bytes; a body with no method is a `POST`, and one on `GET` or `HEAD` is refused) and
> `timeout` in seconds; any other key is `domain_error(fetch_option, K)`. The response is a map of
> `status`, `url`, `headers` (lower-cased names; a repeated header joined with `", "`, and
> `set-cookie` a list, as slate's), `body` and `bytes`: `body` is the text and is absent when the
> body is not UTF-8 (not well-formed, question 3), `bytes` is always there. A non-2xx status is a
> response; a request that cannot be done is `system_error`, and a URL that is not one
> `domain_error(url, U)` (question 3's fault side). The `prolog` member takes `funl-vm` with
> `default_features = false`, so its executable links no libcurl.

### `funl:sqlite` — core, over `sysl-lang/sqlite3`

```
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table notes (id integer primary key, title text)" )
db.run( "insert into notes (title) values (?)", "a note" )
for row <- db.query( "select id, title from notes order by id" ) do write( row.id, row.title )
write( [row.title | row <- db.query("select title from notes")] )
if db.query( "select 1 from notes where title = ?", "x" ) then write( #found )
db.close()
```

slate's surface: `sqlite(path)` answers the database, everything else is a method on it.
**`query` is a generator of rows**, so a search over a table is an ordinary FunL search, and a query
with no rows fails. **Not a feature, as in slate**: SQLite is the system's own library on macOS and a
standard package elsewhere, and slate carries it unconditionally for that reason.

> **Built — what the decision left open.** The handle is a `sqlite database`; `exec(sql)` runs every
> statement in the text and answers `()`; `run` answers `{changes, last_insert_rowid}`. Every error
> SQLite reports is `system_error` with SQLite's sentence; a parameter SQLite cannot hold is
> `type_error(sql_value, X)`, an integer past 64 bits `representation_error(max_integer)`, the wrong
> number of parameters `domain_error(sql_parameters, N)`; `true`/`false` bind as 1/0. Closing the
> database ends every query still running over it. **A row is a map, and `x <- e` draws a map
> whole** (language.md, Decided 2026-10-08), so the example's `for row <- db.query(...)` gives each
> row as written. Prolog cannot call a handle's methods, so the module is
> FunL's alone for now.

## `async` and `await`

> **Decided (user, 2026-10-08): `async` and `await` as slate does**, from slate's
> `docs/reference/asynchrony.md` and the coroutine section of its `CLAUDE.md`, translated to FunL.

```
import { sleep } from funl:async

async def work( name, ms, turns ) =
    i = 0
    while i < turns do
        await sleep( ms )
        i += 1
    name + " finished"

async def main() =
    a = work( "a", 8, 3 )
    b = work( "b", 20, 2 )
    write( "started both" )
    write( await a )
    write( await b )

main()
```

As slate does:

- **An `async` function answers a promise**, and `await p` waits for one. Everything above the first
  `await` runs before the caller sees the promise; everything below runs after the caller has moved on.
- **A program's own body is an async context**, so `await` is legal at the top level, and the program
  ends when the top level has settled and nothing else is pending. An imported module's top level
  settles before a line of its importer runs.
- **`await` belongs to the function it is written in**: an `await` in a plain `def` or lambda is
  refused before the program runs.
- **Two queues in a fixed order**: one for what happened outside (a timer, a socket, a file), and one,
  drained to empty between turns of the first, for a value a suspended call is now owed. A settled
  promise still resumes through the queue.
- **A failed promise raises where it is awaited**, as the same fault; a fault nothing awaited is the
  program's failure, reported against its line, and a call whose promise was thrown away fails at once.
- **`sleep`, `resolve`, `reject`, `pending`, `settle`, `fail`** make promises, and **timers keep the
  program alive**.
- **The promise-shaped I/O lives in a module, `funl:async`**, beside the blocking forms of milestone 9
  rather than replacing them (decided question 10).

**A promise is an opaque value**, unifying by identity in Prolog as a handle does. Prolog cannot
`await`; a FunL `async` function called from Prolog unifies its last argument with the promise.

### What the machine needs: a task is a machine set aside

**slate's coroutine is a whole `Machine` parked and later unparked, and FunL's is the same.** FunL's
precondition is already in place: [there is no host recursion in the instruction
loop](vm.md#the-machines-state), so a suspended computation is data — the operand stack, the control
stack of marks and choice points with its saved cells, the trail, the frame chain, the scan state and
the regex captures. A **task** is exactly those fields, in their own `Machine`:

- **Calling an `async` function starts a task**: a fresh machine whose first frame is the call, run at
  once until it settles or reaches an `await` of an unsettled promise; the caller gets the promise.
  Both call paths, a direct call and `apply`, route an `async` function there, as slate's
  `start_async` does, so no `await` ever lands under a sysl frame.
- **`Await` on an unsettled promise parks the machine**: it is moved to a `parked` table, the awaited
  promise left standing on its operand stack (a root, as slate keeps it there), and suspension leaves
  the instruction loop as an `Err` signal, the way a fault already does.
- **The loop resumes a task** by unparking its machine and writing the settled value over that slot.
- **The collector's roots gain every parked machine**, each walked exactly as the running one is — a
  new row in the [roots table](implementation.md#the-collector-and-its-roots).
- **Icon's co-expression is the nearest relative**: a separate control stack resumed from outside.
  What FunL's generators never needed — resumption from anywhere but backtracking inside their own
  bounded expression — is precisely what a task adds.

> **Built — tasks (milestone 10, part 1), and what the decisions left open.** `task.sysl` holds the
> `Machine` (the VM's machine fields, moved out whole by `park` and back by `unpark`), `start_async`,
> `Await` and `Settle`. **Starting a task never re-enters the loop**: the caller is parked with the
> promise already where the call's value goes, the task's first frame returns into a one-instruction
> chunk, `Settle`, and the same `drive` runs whichever machine is current. A task gives way to the
> machine that started it at its first stop; a task the queue resumed gives way to the next ready one.
> `open_frame` is the one place a call becomes a frame, so a direct call, `CallValue`, a tail call
> (never reusing the frame), a builtin's callback and a Prolog call all start the task. A minimal
> queue of ready machines stands in for the loop until kairos: it is drained whenever nothing else
> runs, and when the top level halts with tasks still owed a turn the top level is parked until it is
> empty. As slate does: `await` of a value that is not a promise is that value and still takes a turn;
> a settled promise still goes round the queue; `async` is written before `def` (any clause makes the
> whole function `async`, as `export` does) or before a lambda, and a relation is refused; the program
> ends, without a word, when nothing is left to run though a task still waits; a fault no `await` met
> is the run's fault, reported once the queue is empty. **FunL's own answers, slate having no
> failure**: an `async` function whose body fails settles its promise as failed, and `await` of it
> fails where it is written, as the call would have; a promise prints as `<promise pending>`,
> `<promise 42>`, `<promise failed>` or `<promise faulted: …>`, and `x is promise` tests for one. A
> fault after an `await` is raised again where the promise is awaited (part 3 owes the rest of it).
> **The stamp is still one counter for every machine** -- per-task stamps and the refusal to bind
> another task's variable come with the loop.

> **Built — the loop and `funl:async` (milestone 10, part 2).** `events.sysl` holds kairos's loop,
> made by a program's first timer over the host driver, in callback form: a timer's callback runs no
> FunL, it moves its promise from `Vm.timed` to `Vm.fired` and stops the loop, so `run` returns at the
> end of that pass. **The ready queue is the microtask queue and is drained to empty first**; only
> then are the promises the last pass fired kept, all of them, in deadline order -- **a turn of the
> loop is a pass, as slate's is**, so two timers due together are both kept before either waiting task
> runs (checked against slate, which prints that order) -- and with none fired the loop runs until one
> fires. A set timer keeps the program alive: the top level halting is parked while one is set. kairos
> sets a timer from its last pass's reading, so the clock is read once per stretch of FunL between
> passes, at its first timer, and the difference added: a wait starts when it was asked for, and two
> timers set in one stretch with one delay are still due together. `funl:async` has `sleep`,
> `resolve`, `reject`, `pending` and `settle`; `reject(x)` faults its promise with the error `error(x)`
> raises, so a `catch` around the `await` sees it, and one nothing awaits is the run's fault. `funl:`
> takes the reserved word `async` as a module name. **Left for later**: `fail(p, message)` cannot be
> imported by that name, `fail` being reserved; the promise-shaped `fetch`, `read_file`, `write_file`
> and `run`, with kairos's `uv` feature behind FunL's `async`; and per-task trails and stamps.

> **Built — what a task keeps to itself, and faults nothing awaits (milestone 10, part 3).** The
> `Machine` now carries its stamp, its task number (0 the top level, then 1, 2, … in the order tasks
> start) and its function's chunk, so **each task has its own trail and stamp**: a task starts at
> stamp 0, and parking moves both out with the rest. A `VarObj` records the task that made it, and
> **`bind` refuses another task's variable**: it leaves it unbound, the unification fails, and the
> instruction that tried is replaced by the fault `permission_error(bind, variable, X)` (the design
> names no term; this is Prolog's for an operation the culprit does not allow), its message naming
> both tasks -- `task 1 (`grab`) cannot bind a logic variable the top level made`, a finished task by
> its number alone. Two unbound variables of different tasks unify by binding the running task's
> own. A trial unification (`unifiable/3`, `subsumes_term/2`) binds nothing that lasts and is not
> refused. **A call written as a statement of its own, or before `&` or a body's next line, is
> thrown away**: the compiler emits `Orphan` after it, which marks a promise as one nothing can await,
> and its fault ends the run at once -- already faulted, at the `Orphan`; later, at the next turn of
> the queue -- as slate's `Discard` does, so a fault in a server's task is not held until a loop that
> never drains. Every other unawaited fault is still reported when everything has settled, the
> first first. **A FunL failure is not reported**: the design makes a *fault* nothing awaited the
> program's failure, and a thrown-away call that fails is a statement that failed. A `catch` around
> an `await` is a `Trap` on the parked control stack, so it catches a fault raised after the resume.

> **Built — `settle_error(p, x)` (user decision, 2026-10-08).** slate's `fail(p, message)` is
> `settle_error` in FunL, because `fail` is a FunL keyword and cannot be imported by that name. It
> faults pending promise `p` with the error `error(x)` raises, exactly as `reject(x)` makes an
> already-faulted promise, and mirrors `settle(p, v)` otherwise: settling a promise that is no longer
> pending changes nothing, and a first argument that is not a promise faults as `settle`'s does. An
> `await` re-raises the fault where it is awaited (a `catch` sees it), and one nothing awaits is
> reported as any unawaited fault is.

## The prelude — the first module written in FunL

> **Decided (user, 2026-10-08) — the prelude is the first module written in FunL, and it is
> auto-imported, as Haskell's Prelude is.** It is a Haskell-style list library written fresh in
> FunL, carried in the binary as a source module (the `raw"""` mechanism of the built-in modules),
> and every program starts with its exports in scope. **A program's own names shadow it** by the
> builtin shadowing rule: a `val`, parameter, pattern variable or local `def` of a prelude name
> hides it where in scope, and a top-level `def` is the program's definition in its place. A
> program that wants no prelude name at all simply defines its own.

**The contents**, in the order Haskell's Prelude and `Data.List` give them:

| group | names |
|---|---|
| pairs | `fst`, `snd` |
| ends of a list | `head`, `tail`, `last`, `init` |
| folds | `foldl`, `foldl1`, `foldr`, `foldr1`, `product`, `maximum`, `minimum`, `maxBy`, `minBy` |
| transforming | `map`, `filter`, `concat`, `concatMap`, `reverse`, `sort`, `sortBy`, `nub`, `group` |
| slicing | `take`, `drop`, `takeWhile`, `dropWhile`, `splitAt`, `span`, `spanNot` (Haskell's `break`), `partition` |
| combining | `zip`, `zip3`, `zipWith`, `zipWith3`, `unzip` |
| producing | `iterate`, `forever` (Haskell's `repeat`), `replicate` |
| asking | `any`, `all`, `elem`, `lookup` |

**Generators replace lazy streams.** Where a Haskell function is lazy on an infinite list, the FunL
version is a generator, or draws one. Two facts about FunL decide the shape. **A call's arguments
backtrack**: `f(g())` already calls `f` once per value `g` produces, so applying a function to every
value of a generator needs no `map` at all. **A generator therefore cannot be handed over as an
argument** — `take(3, iterate(f, 1))` would call `take` once per value `iterate` produces — and the
prelude takes it as a **thunk**, a zero-argument function: `take(3, () -> iterate(f, 1))`. The
functions that can meet an endless input (`map`, `filter`, `take`, `takeWhile`) follow one rule:

- **a collection in, a list out** — a list, string, set, array or finite range: `take(2, [5, 6, 7])`
  is `[5, 6]`;
- **a thunk in, a generator out** — the thunk's values are drawn with `x <- xs()`, which draws every
  value, and the function generates its answers one at a time and stops as soon as it has what it
  needs, so `take(3, () -> iterate(f, 1))` does not run the endless generator past three values. A
  list is wanted by writing `[x | x <- take(3, () -> iterate(f, 1))]`.

`iterate` and `forever` themselves are generators (a generator body, a `yield` per value, then the
recursive call), the two producers that never end. An unbounded range `a..` is already a lazy list
value and goes in as a collection. The functions that need the whole input (`reverse`, `sort`,
`last`, `zip`, the folds) take a collection and answer a list or a value.

**The source below is the prelude's own**, other than the natives named afterwards. It runs as it
stands (`mapList` is what the `map` entry calls, below):

```
def
  fst( (a, _) ) = a
  snd( (_, b) ) = b

  head( x:_ ) = x
  tail( _:xs ) = xs
  last( [x] ) = x
  last( _:xs ) = last( xs )
  init( [_] ) = []
  init( x:xs ) = x:init( xs )

  foldl( f, z, [] ) = z
  foldl( f, z, x:xs ) = foldl( f, f(z, x), xs )
  foldl1( f, x:xs ) = foldl( f, x, xs )
  foldr( f, z, [] ) = z
  foldr( f, z, x:xs ) = f( x, foldr(f, z, xs) )
  foldr1( f, [x] ) = x
  foldr1( f, x:xs ) = f( x, foldr1(f, xs) )

  product( xs ) = foldl( (a, b) -> a*b, 1, xs )
  maximum( xs ) = foldl1( (a, b) -> max(a, b), xs )
  minimum( xs ) = foldl1( (a, b) -> min(a, b), xs )
  maxBy( f, xs ) = foldl1( (a, b) -> if f(b) > f(a) then b else a, xs )
  minBy( f, xs ) = foldl1( (a, b) -> if f(b) < f(a) then b else a, xs )

  mapList( f, xs ) = if xs is function then mapDraw( f, xs ) else [f(x) | x <- xs]
  mapDraw( f, xs )
    for x <- xs()
      yield f(x)
    fail

  filter( p, xs ) = if xs is function then filterDraw( p, xs ) else [x | x <- xs if p(x)]
  filterDraw( p, xs )
    for x <- xs() if p(x)
      yield x
    fail

  take( n, xs ) = if xs is function then takeDraw( n, xs ) else takeList( n, xs )
  takeList( n, xs ) = [x | x <- takeDraw( n, () -> xs )]
  takeDraw( n, xs )
    var k = 0
    for x <- xs()
      if k >= n then break
      k++
      yield x
    fail

  takeWhile( p, xs ) = if xs is function then takeWhileDraw( p, xs ) else [x | x <- takeWhileDraw( p, () -> xs )]
  takeWhileDraw( p, xs )
    for x <- xs()
      if not p(x) then break
      yield x
    fail

  dropWhile( p, [] ) = []
  dropWhile( p, x:xs ) = if p(x) then dropWhile( p, xs ) else x:xs
  drop( n, xs ) = if n <= 0 then xs else dropOne( n, xs )
  dropOne( n, [] ) = []
  dropOne( n, _:xs ) = drop( n - 1, xs )
  splitAt( n, xs ) = (take(n, xs), drop(n, xs))
  span( p, xs ) = (takeWhile(p, xs), dropWhile(p, xs))
  spanNot( p, xs ) = span( x -> not p(x), xs )
  partition( p, xs ) = (filter(p, xs), filter(x -> not p(x), xs))

  concat( xss ) = [x | xs <- xss, x <- xs]
  concatMap( f, xs ) = [y | x <- xs, y <- f(x)]

  zip( xs, ys ) = zipWith( (a, b) -> (a, b), xs, ys )
  zip3( xs, ys, zs ) = zipWith3( (a, b, c) -> (a, b, c), xs, ys, zs )
  zipWith( f, xs, ys ) = zipWithArrays( f, array([x | x <- xs]), array([y | y <- ys]) )
  zipWithArrays( f, a, b ) = [f(a(i), b(i)) | i <- 0..<min( a.length, b.length )]
  zipWith3( f, xs, ys, zs ) = zipWith3Arrays( f, array([x | x <- xs]), array([y | y <- ys]), array([z | z <- zs]) )
  zipWith3Arrays( f, a, b, c ) = [f(a(i), b(i), c(i)) | i <- 0..<min( a.length, min(b.length, c.length) )]
  unzip( ps ) = ([a | (a, _) <- ps], [b | (_, b) <- ps])

  iterate( f, x )
    yield x
    iterate( f, f(x) )
  forever( x )
    yield x
    forever( x )
  replicate( n, x ) = [x | _ <- 1..n]
  reverse( xs ) = foldl( (acc, x) -> x:acc, [], xs )

  sortBy( lt, xs ) = qsort( lt, xs )
  qsort( lt, [] ) = []
  qsort( lt, p:xs ) = qsort( lt, [x | x <- xs if lt(x, p)] ) + [p] + qsort( lt, [x | x <- xs if not lt(x, p)] )
  sort( xs ) = sortBy( (a, b) -> a < b, xs )

  all( p, xs ) = [] == [x | x <- xs if not p(x)]
  elem( x, xs ) = x in xs
  lookup( k, ps )
    for (key, v) <- ps if key == k
      return v
    fail
  nub( xs ) = foldl( (acc, x) -> if x in acc then acc else acc + [x], [], xs )
  group( [] ) = []
  group( x:xs ) = (x:takeWhile( y -> y == x, xs )) : group( dropWhile( y -> y == x, xs ) )
```

**Success and failure stand in for `Maybe`.** `lookup(k, ps)` answers the value, or fails when no
pair has the key, so `lookup(k, ps) | default` supplies a default; `elem` and `all` succeed or fail,
as a comparison does (a success carries a value, so they are tested with `if`, never printed);
`head([])` and `last([])` have no matching clause and are the ordinary "argument match not found"
fault, which is a mistake in the program, not a condition.

**Natives and source.** The source above is the prelude's definition of each name; which names are
also written as core natives for speed is the second prelude question below, and the tests for a
native check it against this source on the same inputs.

**Interactions with the builtins.**

- **`sum` is already native** and stays: `sum(xs)` over a list or a range. `product`, `maximum` and
  `minimum` are the prelude's list forms. **`min(a, b)` and `max(a, b)` are the builtin two-argument
  forms**; the prelude's `maximum([..])` calls them, and neither builtin gains a list form.
- **`map` is a builtin already**: `map(m)` and `map(m, default)` make a mutable map. The prelude's
  `map(f, xs)` is the same name and arity as `map(m, default)` and is settled in the first prelude
  question below.
- **`any` is a builtin of one argument** (the scanning charset `any(c)`); the prelude's `any(p, xs)`
  has two, and the name/arity namespace keeps them apart. **`all`, `elem` and `lookup`** are free.
- **A builtin is a value** (P4, decided below): `filter(odd, xs)` and `foldl(max, 0, xs)` work,
  a variadic builtin's value passing on however many arguments it is given.
- **`repeat` and `break` are keywords**, so Haskell's `repeat` is `forever` and Haskell's `break`
  is `spanNot`.
- **`length` is the field `.length`**, not a function, so the prelude defines no `length`;
  `reverse`, `sort`, `last` and the rest have no builtin of their name today.

> **Decided (user, 2026-10-08) — P1, `map` already names a builtin.** `map(m)` and `map(m, default)` make a
> mutable map, and a prelude `def map(f, xs)` would shadow the two-argument builtin in every program.
> **Decision:** **one `map/2`, and its first argument decides**: a function maps (the
> `mapList` above), anything else is the constructor with a default. No program that is valid today
> changes meaning, since a function is not a collection to build a map from. Implemented as a core
> native that forwards to the prelude's own `map` (the `mapList` above, exported under its name):
> the builtin hands its call on to that function value, so `map` passed as a value forwards too.

> **Decided (user, 2026-10-08) — the renames.** Haskell's `repeat` is `forever` and its `break` is
> `spanNot`, since `repeat` and `break` are FunL keywords.

> **Decided (user, 2026-10-08) — P2, which are core natives.** **Decision:** as recommended below.
> The natives are `reverse`, `sort` (a stable merge sort by `<`), `sortBy`, `concat`, `replicate`,
> `elem`, `last`, `init`, `drop`, `zip`, `zip3`, `zipWith` and `zipWith3`; the tests check each
> against the source above on the same inputs. **`sortBy`, `zipWith` and `zipWith3` call their
> function back** through the machine's [calling builtins](vm.md#builtins-that-call-back): each call
> is a bounded expression, so it gives **its first result only** and leaves no choice point behind.
> `sortBy` is the stable merge sort of `sort`, asking `lt(right, left)` of each pair it orders, so it
> is O(n log n) on any input where the `qsort` above is quadratic on sorted input; its comparator
> counts as a condition does — `false` and failure both mean "not less". `zipWith` keeps the
> source's answer for a place where `f` fails (none: the place is left out), and differs from the
> source in one respect, deliberately: a generator `f` gives one value per place, its first, where
> the comprehension above would collect them all. A fault in a callback is an ordinary fault,
> catchable around the builtin or inside the callback. **Where a builtin and the prelude share a name at different arities, the call's count
> decides** (`any(c)` the scanning builtin, `any(p, xs)` the prelude's), and a Prolog predicate the
> program loads shadows the prelude at its arity. **Prolog reaches the prelude as a built-in module**,
> `:- import("funl:prelude")` and `prelude:f/(n+1)`, and a FunL file may import `funl:prelude` by
> name to reach what its own definition shadows.
>
> **Recommendation (as made):** natives for speed — `reverse`,
> `sort` and `sortBy` (a stable merge sort in sysl, the comparator called back as `findall` does),
> `concat`, `replicate`, `elem`, `last`, `init`, `drop` and the `zip` family (one pass over
> arrays), plus the existing `sum`, `min` and `max`. Source carried in the binary — the rest: `fst`,
> `snd`, `head`, `tail`, the folds and the folds' derivatives, `filter`, `take`, `takeWhile`,
> `dropWhile`, `splitAt`, `span`, `spanNot`, `partition`, `concatMap`, `unzip`, `iterate`,
> `forever`, `all`, `lookup`, `nub`, `group`, `any/2`, `maxBy`, `minBy` and `mapList` — because
> each calls back into FunL, which a native can only do through the callback machinery, and
> generator-bodied ones cannot be natives at all.

> **Open question P3 — a generator of tuples cannot be taken apart.** `x <- e` draws every value of
> `e` and iterates a value that is a collection, so `[a | (a, b) <- pairs()]` over a generator that
> yields `(1, "a")` draws `1` and `"a"` and matches neither against the pattern (the answer is
> `[]`), and `[p | p <- pairs()]` flattens the pairs. A list of tuples is fine
> (`[a | (a, b) <- [(1, "a")]]` is `[1]`). **Recommendation:** make `zip` and its family answer
> lists, which is what the source above does and why they need finite inputs (a program zips
> `0..` with a list by taking the list's length first), and decide the language question
> separately: **a tuple pattern on the left of `<-` matches each generated value whole when that
> value is a tuple**, and only an unmatched value is iterated. Until then no prelude function yields
> tuples.

> **Decided (user, 2026-10-08) — P4, builtins as values.** **Decision:** a builtin named without a
> call is a function value when it takes a fixed number of arguments (`abs`, `odd`, `sum`, every
> function of a built-in module), so `filter(odd, xs)` works. The value calls the builtin with its
> arguments as a direct call would; a generating builtin still generates and fails through it, and a
> call through it with the wrong number of arguments faults as any function value's does
> (`'abs' takes 1 argument and was given 2`), where a direct call is refused when compiled. **It
> follows that `import * as fs from funl:fs` names a value**: the map from each export to its
> value, as a FunL module's is (§ "A module is a value"). A builtin that takes any number of
> arguments (`max`, `min`, `map`, `write`, `find`) is a value too: the value passes on however many
> arguments it is given, and the builtin's own run-time count check answers (the same fault as a
> direct call's), so `foldl(max, 0, xs)` works. A name that is not a value (a relation, Prolog's
> `consult`) keeps the "called rather than used as a value" refusal.

## Questions decided

These are the places FunL could not copy slate as it stands, because FunL is a different kind of
language. All eleven were decided by the user on 2026-10-08, each as the recommendation made for it.

> **Decided (user, 2026-10-08) — what a handle is.** slate's resources are objects with methods; FunL has no
> objects and no methods, only maps, records and closures.
> **Decision:** a new opaque value kind, `Handle`: a `gc` object holding the sysl resource, a
> kind tag and a closed flag, with a finalizer that releases the resource. `h.name(args)` on a
> handle is looked up in a per-kind method table — slate's `properties_of` — and a method is an
> ordinary native taking the handle first. A call on a closed handle is a fault. Prolog sees a
> handle as opaque and unifying by identity, the term mapping already decided for maps and closures.
> FunL has no `using`, so there is no scoped form: `close()`, with the finalizer as the backstop.

> **Built — what the decision left open.** A handle prints as
> `<kind handle>`, `<closed kind handle>` once closed; `x is handle` tests for one; `close()` is
> supplied by the runtime for every kind and is idempotent; a closed handle's other methods fault
> with `existence_error(handle, H)` (Prolog's closed-stream error), and a method the kind lacks with
> `existence_error(method, Name)`. **A method may fail** ("no such row"), so its builtin answers a
> value or failure. `x.name(args)` compiles to `MethodOf`/`CallMethod`: on anything but a handle it
> reads the field before the arguments and calls it, exactly as before.

> **Decided (user, 2026-10-08) — iterators as generators.** slate answers a whole array (`db.query`) or a
> promise; FunL's natural answer is a generator, and backtracking into it is what makes rows, lines
> and directory entries searchable. The registry's generating builtin carries only a `long` cursor.
> **Decision:** a native generator's cursor names a per-call iterator state held in a
> `Handle` standing on the operand stack beside the cursor, so the collector sees it. Backtracking
> into the call steps the iterator; running out is failure. **Each call starts afresh** (a new
> statement step, a new directory read), so a generator re-entered by a later call never sees
> another's position. A bounded context that commits abandons the choice point; the abandoned state
> is released by its finalizer, or at once by `close()` on the handle it came from. Where a list is
> wanted, the program writes a comprehension or `findall`.

> **Built — what the decision left open.** A third way to run a builtin,
> `register_stepping(name, arity, start, step)`: the handle `start` makes takes the cursor's cell,
> and running out closes it at once. A **method** generates the same way,
> `register_stepping_method(kind, name, arity, start, step)`, so `stmt.rows()` is a generator: its
> first run moves the handle into `MethodOf`'s `()` cell and the cursor takes the handle's, and a
> handle closed while its method generates ends the generation.

> **Decided (user, 2026-10-08) — failure or fault for a native's error.** slate's rule is that text from outside
> the program is an answer and a mistake the program made itself is a fault. FunL has failure,
> which slate does not.
> **Decision:** **failure for "no such thing", "no more" and "not well-formed"** — a missing
> file, an unset variable, no rows, malformed JSON — because each is a condition a program tests
> with `if` and `|`, exactly as a comparison fails. **A fault for everything else** — a permission
> refused, an I/O error, a network failure, bad SQL, a call on a closed handle — raised as the ISO
> error term (`permission_error`, `existence_error`, `system_error`, `syntax_error`) a Prolog
> builtin would raise, so `catch/3` catches it from Prolog.

> **Decided (user, 2026-10-08) — FunL catches a fault with slate's postfix `catch`.** slate writes `db.query(sql) catch e -> []`;
> in FunL a fault stops the program, and only Prolog's `catch/3` stops one. A program that calls
> `fetch` must be able to survive the network being down.
> **Decision:** add slate's postfix form, `e catch err -> recovery`, to FunL before
> `funl:http` lands, compiled over the `Catch` entry `catch/3` already uses, with `err` bound to the
> error term. This settles the old question of FunL's `catch` syntax. It is new syntax and its
> grammar is written in the language chapter when it is built, not here.

> **Decided (user, 2026-10-08) — exporting and importing relations.** A relation is many `def` clauses, and
> FunL's name/arity namespace means `sides` may be a relation of two arguments and a function of
> one at once.
> **Decision:** **`export` on any clause exports the whole procedure** — every clause of that
> name and arity, wherever written — and a `def` block written under `export def` exports every
> procedure in it. **An import names a name, and brings every arity of it.** A qualified call
> `geometry.sides(#triangle, n)` through `import * as` is resolved by the compiler against the
> module's export list, so a relation called qualified is still a static call with indexing, not a
> map lookup at run time.

> **Decided (user, 2026-10-08) — what Prolog sees.** Prolog has one flat predicate namespace, shared with FunL,
> and `:- import("file.funl")` today brings in every definition the file has.
> **Decision:** **Prolog's `:- import` of a FunL file sees its exports**, as a FunL importer
> does, and the existing tests and pages that import FunL from Prolog gain `export` lines. **A
> built-in module is reached with `:- import("funl:json").` and called module-qualified**,
> `json:parse(Text, Value)`, with `:` (already 600 `xfy` in the operator table); the function `f/n`
> is the predicate `json:f/(n+1)` and a generator gives one solution per value, the rules
> [the Prolog chapter](prolog.md) already has. Module-qualified, so a native module never takes a
> name out of Prolog's one namespace.

> **Decided (user, 2026-10-08) — `import "file.pl"`.** A Prolog file has no exports, so slate's braces have
> nothing to select from.
> **Decision:** keep `import "file.pl"` exactly as it is — every predicate of the file enters
> the shared namespace — as the one form for a Prolog file; `import { x } from "file.pl"` is
> refused, naming the form that works.

> **Decided (user, 2026-10-08) — bytes.** slate has a `bytes` kind, and a BLOB, an HTTP body and `readBytes`
> answer it; FunL's `Buffer` is a buffer of values.
> **Decision:** add a `Bytes` value kind in milestone 9, before `funl:sqlite` and `funl:http`:
> immutable, indexed from 0 like every FunL collection, `!b` generating each byte as an integer, and
> the decoding to text explicit. Until it exists, a BLOB column and a binary body are faults naming
> the missing kind.

> **Built — what the decision left open.** No literal syntax: `bytes(ints)`, `bytes(string)`
> (UTF-8) and `decode(b)`, which fails on bytes that are not UTF-8 (not well-formed, question 3). It
> prints as `bytes([1, 2, 255])`; `b(i)`, `b(range)`, `!b`, `in`, `.length`, `+`, equality and
> byte-by-byte order, `is bytes`. Prolog sees an atomic value that unifies with one holding the
> same bytes.

> **Decided (user, 2026-10-08) — Bytes in Prolog.** A `Bytes` value is an atomic, opaque value to
> Prolog, as a `Handle` is, not a code list.
> **Decision:** `atomic/1` is true of it; `atom/1`, `number/1`, `compound/1` and `callable/1` are
> false; it unifies only with a `Bytes` holding the same bytes; `compare/3` and `msort/2` place it
> after numbers, atoms and strings and order two byte by byte; `write` prints it as FunL does,
> `bytes([1, 2])`.

> **Decided (user, 2026-10-08) — `await` and backtracking.** slate's `await` happens once; in FunL, failure
> after an expression normally goes back into it for another value.
> **Decision:** **`await` is bounded**, like `return`: it produces exactly one value and is
> never resumed by backtracking. It pushes no choice point, so failure after it passes back over it
> to the choice points before it, and the I/O is never redone. Choice points made before the
> `await` stay in the parked machine and are live after it resumes, so **a generator body may
> `await`**: each value it yields may have waited, and backtracking into the generator resumes it
> after its last `yield`, as it would without the `await`.

> **Decided (user, 2026-10-08) — the blocking forms and the promise forms.** slate's file and network calls are
> promise-shaped only; FunL's milestone 9 ships blocking ones, which a script wants and which come
> first.
> **Decision:** the blocking forms stay where they are, and the promise-answering ones are
> the same names in **`funl:async`** — `sleep`, `fetch`, `read_file`, `write_file`, `run` — so a file
> chooses by its import which kind it has, and neither module changes the other's meaning.

> **Decided (user, 2026-10-08) — the loop, and a logic variable two tasks share.** slate drives libuv by
> hand; the org now has `sysl-lang/kairos`, an event loop with libuv's shape whose `uv` feature
> adds libuv sockets and files and whose timers need no library at all.
> **Decision:** **FunL's loop is kairos**: tasks are FunL machines, so the loop is used in
> callback form (a timer or a source settles a promise and queues the task), its host driver
> carries `sleep` and timers in the core with no link line, and kairos's `uv` feature sits behind a
> FunL feature named for the module, `async`, on in `default`. **Each task has its own trail and
> stamp**, so a logic variable made by one task and bound by another could be undone by neither
> correctly: binding a variable another task made is refused as a fault naming both tasks, and a
> value crossing through a promise is settled as it stands.

**Not planned**: a relation view of an SQLite table (`notes(id, title)` as a predicate). It is the
obvious thing a logic language would want, and it is a second design on top of this one; it waits
for a program that needs it.
