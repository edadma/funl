---
title: funl:fs
weight: 10
---

# `funl:fs` — files and directories

`funl:fs` reads and writes files, walks directories and takes paths apart. A path is a string, read
relative to the directory the program runs in; the programs on this page run from the root of the
FunL repository, and read files kept beside the page, under [`fs/`](fs/).

## Importing it

`import { ... } from funl:fs` takes the names it lists:

```funl
import { read_file, exists } from funl:fs

write( read_file("docs/library/fs/poem.txt") )
write( exists("docs/library/fs/poem.txt") )
```

```output
Roses are red,
violets are blue.

true
```

`import * as fs from funl:fs` reaches every name through `fs`:

```funl
import * as fs from funl:fs

write( fs.read_file("docs/library/fs/letters/a.txt") )
```

```output
hello

```

The names are the module's, not builtins: in a file that does not import `read_file`, there is no
`read_file`.

```funl
write( read_file("docs/library/fs/poem.txt") )
```

```error
`read_file` is not defined
```

`fs` itself is a value: a map from each name the module exports to that function. A function taken
from it is called later like any other, and a generator such as `lines` still generates.

```funl
import * as fs from funl:fs

base = fs.basename
write( base("docs/library/fs/poem.txt") )
write( fs("extension")("docs/library/fs/poem.txt") )
lines = fs.lines
every write( lines("docs/library/fs/poem.txt") )
```

```output
poem.txt
txt
Roses are red,
violets are blue.
```

## What fails and what faults

**A file or directory that is not there is failure**, so a program tests for one with `if` or gives
a default with `|`, as it does for any other expression that fails. Bytes that are not UTF-8 text
fail `read_file` and `lines` the same way.

```funl
import { read_file } from funl:fs

write( read_file("docs/library/fs/nothing.txt") | "(no such file)" )
if read_file( "docs/library/fs/nothing.txt" ) then write( #there ) else write( #absent )
```

```output
(no such file)
absent
```

**Anything else that goes wrong is a fault**: a permission refused raises
`permission_error(Action, source_sink, Path)` — `Action` being `open` for reading and writing a file,
`access` for reading a directory, `create` for `mkdir` and `modify` for `remove` and `rename` — and
any other error raises `system_error`, each inside the usual `error(Formal, Context)` term. Nothing
caught, the fault stops the program:

```funl
import { read_file } from funl:fs

write( read_file("docs/library/fs/poem.txt/inside") )
```

```error
'read_file' cannot open `docs/library/fs/poem.txt/inside`: not a directory
```

`catch` catches it, with the error term:

```funl
import { read_file } from funl:fs

write( read_file("docs/library/fs/poem.txt/inside") catch e -> e )
```

```output
error(system_error, context(_G2, "'read_file' cannot open `docs/library/fs/poem.txt/inside`: not a directory"))
```

## Whole files

| function | what it does |
|---|---|
| `read_file(path)` | the file's text, a string |
| `write_file(path, text)` | makes or replaces the file with string `text` |
| `append_file(path, text)` | adds `text` to the end of the file, making it if it is not there |
| `read_bytes(path)` | the file's bytes, a byte string |
| `write_bytes(path, b)` | makes or replaces the file with byte string `b` |
| `append_bytes(path, b)` | adds `b` to the end of the file |

The writing functions answer `()`, and fail when the directory the file would be in is not there.

```funl
import { mkdir, write_file, append_file, read_file, remove, exists } from funl:fs

mkdir( "docs/library/fs/out/drafts" )
write_file( "docs/library/fs/out/drafts/note.txt", "first\n" )
append_file( "docs/library/fs/out/drafts/note.txt", "second\n" )
write( read_file("docs/library/fs/out/drafts/note.txt") )
remove( "docs/library/fs/out/drafts/note.txt" )
remove( "docs/library/fs/out/drafts" )
remove( "docs/library/fs/out" )
write( exists("docs/library/fs/out") | #gone )
```

```output
first
second

gone
```

Bytes are a [byte string](../reference/data.md); text and bytes never convert silently, so a file
that is not UTF-8 is read with `read_bytes`:

```funl
import { write_bytes, read_bytes, read_file, remove } from funl:fs

write_bytes( "docs/library/fs/raw.bin", bytes([72, 105, 255]) )
write( read_bytes("docs/library/fs/raw.bin") )
write( read_file("docs/library/fs/raw.bin") | "(not UTF-8 text)" )
remove( "docs/library/fs/raw.bin" )
```

```output
bytes([72, 105, 255])
(not UTF-8 text)
```

## The lines of a file

`lines(path)` is a generator: each value is one line, without its `\n` (or a `\r` before it), and
backtracking into it takes the next. Each call reads the file afresh, and a file with no lines gives
none.

```funl
import { lines } from funl:fs

every write( lines("docs/library/fs/poem.txt") )
write( [l | l <- [lines("docs/library/fs/poem.txt")]] )
write( lines("docs/library/fs/poem.txt") )
```

```output
Roses are red,
violets are blue.
["Roses are red,", "violets are blue."]
Roses are red,
```

A line is a string, and `x <- e` iterates a string it draws, so a `for` or a comprehension that
wants each line whole draws from the generator in a list, `l <- [lines(path)]`, as
[Generators](../reference/generators.md) explains.

## Directories

`read_dir(path)` is a generator of the names in a directory — `.` and `..` left out — in sorted
order. Each call reads the directory afresh, and a bounded expression that takes one name abandons
the rest:

```funl
import { read_dir, read_file, join } from funl:fs

val dir = "docs/library/fs/letters"

every write( read_dir(dir) )
write( [name | name <- [read_dir(dir)]] )
write( read_dir(dir) )
for name <- [read_dir(dir)] if read_file(join(dir, name)) == "dear\n" do write( name )
```

```output
a.txt
b.txt
c.txt
["a.txt", "b.txt", "c.txt"]
a.txt
b.txt
```

| function | what it does |
|---|---|
| `mkdir(path)` | makes the directory, and every directory above it that is not there yet; one already there is no error |
| `remove(path)` | removes a file, or a directory that is empty; fails when nothing is there |
| `rename(from, to)` | moves a file or directory, replacing what `to` named; fails when `from` is not there |
| `cwd()` | the directory the program runs in |

```funl
import { rename, read_file, write_file, remove, exists } from funl:fs

write_file( "docs/library/fs/old.txt", "moved" )
rename( "docs/library/fs/old.txt", "docs/library/fs/new.txt" )
write( exists("docs/library/fs/old.txt") | #gone, read_file("docs/library/fs/new.txt") )
remove( "docs/library/fs/new.txt" )
write( remove("docs/library/fs/new.txt") | "nothing to remove" )
```

```output
gone, moved
nothing to remove
```

## Asking about a path

`exists(path)`, `is_file(path)` and `is_dir(path)` give `true` or fail. `stat(path)` is an immutable
map of what the filesystem keeps about a path: `kind` — `#file`, `#dir`, `#link` or `#other` —
`size` in bytes, and `modified`, the time of the last change in whole seconds since 1970.

```funl
import { stat, is_file, is_dir } from funl:fs

val s = stat( "docs/library/fs/poem.txt" )
write( s.kind, s.size )
write( stat("docs/library/fs/letters").kind )
write( is_file("docs/library/fs/poem.txt"), is_dir("docs/library/fs/poem.txt") | false )
write( stat("docs/library/fs/nothing.txt") | "no such file" )
```

```output
file, 33
dir
true, false
no such file
```

## Paths

`join(a, b)` puts `b` under `a`, or is `b` itself when `b` is absolute. `basename(path)`,
`dirname(path)` and `extension(path)` take a path apart, failing when it has no such part. None of
the four looks at the filesystem.

```funl
import { join, basename, dirname, extension } from funl:fs

val p = join( "docs/library", "fs/poem.txt" )
write( p )
write( basename(p), dirname(p), extension(p) )
write( extension("README") | "(none)" )
write( join("anything", "/usr/lib") )
```

```output
docs/library/fs/poem.txt
poem.txt, docs/library/fs, txt
(none)
/usr/lib
```

## From Prolog

A Prolog file loaded by FunL reaches the module with `:- import("funl:fs").` and calls it qualified
by the module's name. A function of `n` arguments is the predicate `fs:f/(n+1)`, whose last argument
is unified with the function's value, and a generator gives one solution for each value:

```prolog
:- import("funl:fs").

:- fs:read_file("docs/library/fs/letters/a.txt", T), write(T).
:- findall(N, fs:read_dir("docs/library/fs/letters", N), Ns), write(Ns), nl.
:- ( fs:exists("docs/library/fs/nothing.txt", _) -> write(there) ; write(absent) ), nl.
:- catch(fs:read_file("docs/library/fs/poem.txt/inside", _), error(E, _), (write(E), nl)).
:- fs:is_file('docs/library/fs/poem.txt', Y), write(Y), nl.

read_file(_, mine).

:- read_file("docs/library/fs/poem.txt", W), write(W), nl.
```

```output
hello
[a.txt,b.txt,c.txt]
absent
system_error
true
mine
```

A path may be an atom as well as a string, as Prolog usually names a file. Qualified, the module's
names take nothing out of Prolog's one namespace: `fs:read_file/2` and the program's own
`read_file/2` are two predicates.
