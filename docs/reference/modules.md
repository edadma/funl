---
title: Modules
weight: 115
---

# Modules

A FunL file is a **module**. Another file can use what the module writes `export` in front of, and
nothing else. The programs on this page import files kept beside it, under
[`modules/`](modules/).

## Exporting

`export` goes in front of a `def`, a `val`, a `var` or a `data`. The module
[`modules/geometry.funl`](modules/geometry.funl) is:

```
export data shape = circle(r) | square(s)

export def area( circle(r) ) = 3 * r * r
export def area( square(s) ) = s * s

export def sides( #square, 4 )
export def sides( #triangle, 3 )

export val unit = 1

def helper( x ) = x

write( 'geometry is loaded' )
```

- `export` on one clause exports the whole definition: every clause of that name, wherever it is
  written.
- `export def` in front of an indented block of clauses exports every definition in the block.
- `export data` exports the type's name and every constructor.
- `export val` exports every name its pattern declares.
- `helper` has no `export`, so no other file can reach it.

`import` and `export` are keywords only at the start of a top-level line. Anywhere else they are
ordinary names:

```funl
val import = 1
val export = 2
write( import + export )
```

```output
3
```

## Importing names

`import { ... } from "file"` takes the names it lists. `as` gives a name another name in the
importing file. The path is read relative to the file the import is written in:

```funl
import { area, circle, square, sides as edges, unit, shape } from "modules/geometry.funl"

free s, n

write( area(circle(2)), area(square(3)), unit )
every edges( s, n ) do write( s, n )
write( circle(1) is shape )
```

```output
geometry is loaded
12, 9, 1
square, 4
triangle, 3
circle(1)
```

An imported relation is still a relation: `edges` gives each solution, as `sides` does in its own
file.

Asking for a name the module does not export is refused, and the message lists what it does export:

```funl
import { helper } from "modules/geometry.funl"
```

```error
it exports `shape`, `circle`, `square`, `area`, `sides`, `unit`
```

## Importing the whole module

`import * as name from "file"` names the whole module. `name.x` is the module's export `x`:

```funl
import * as geometry from "modules/geometry.funl"

free s

write( geometry.area(geometry.square(3)), geometry.unit )
every geometry.sides( s, 4 ) do write( s )
```

```output
geometry is loaded
9, 1
square
```

The module itself is an immutable map from each exported name to its value. Functions, variables
and constructors are values, so they are in the map. Relations are not values, so they are not in
the map; `geometry.sides(...)` is still a call of the relation, made directly:

```funl
import * as geometry from "modules/geometry.funl"

write( geometry )
val f = geometry.area
write( f(geometry.circle(1)) )
```

```output
geometry is loaded
{"circle": <constructor circle(r)>, "square": <constructor square(s)>, "area": <function area>, "unit": 1}
3
```

A qualified name is checked when the program is compiled, as any name is. A name the module does not
export is refused:

```funl
import * as geometry from "modules/geometry.funl"

write( geometry.helper(1) )
```

```error
`docs/reference/modules/geometry.funl` does not export `helper`
```

A call with the wrong number of arguments is refused too:

```funl
import * as geometry from "modules/geometry.funl"

write( geometry.area(1, 2) )
```

```error
`geometry.area` takes 1 argument, and this call gives 2
```

A relation used as a value is refused:

```funl
import * as geometry from "modules/geometry.funl"

write( geometry.sides )
```

```error
`geometry.sides` is a relation or a builtin, which is called rather than used as a value
```

A qualified constructor is a pattern wherever a pattern is written: a function's parameter, a
relation's head, the left of a `val`, and a generator. `geometry.circle(r)` matches the same terms
`circle(r)` does where `circle` is imported by name.

```funl
import * as geometry from "modules/geometry.funl"

def describe( geometry.circle(r) ) = 'a circle of radius ' + r
def describe( geometry.square(s) ) = 'a square of side ' + s

def round( geometry.circle(_) )

write( describe(geometry.square(2)) )
val geometry.circle(r) = geometry.circle(5)
write( describe(geometry.circle(r)) )
if round( geometry.circle(r) ) then write( 'round' )
write( [s | geometry.square(s) <- [geometry.square(1), geometry.circle(2), geometry.square(3)]] )
```

```output
geometry is loaded
a square of side 2
a circle of radius 5
round
[1, 3]
```

A qualified pattern is checked when the program is compiled. It names a constructor the module
exports, with as many fields as the constructor has:

```funl
import * as geometry from "modules/geometry.funl"

def side( geometry.square(s, t) ) = s
```

```error
the constructor `geometry.square` takes 1 field, and this pattern gives 2
```

A function or a relation is not a constructor, so no pattern matches it:

```funl
import * as geometry from "modules/geometry.funl"

def f( geometry.area(x) ) = x
```

```error
`geometry.area` is not a constructor, so a pattern cannot match it
```

## Types and constructors

A `data` type's name and its constructors belong to the file that declares them. Another file uses
them by importing them, by name or qualified. After `is`, `m.name` names the type or constructor
`name` that the module imported as `m` exports. A record prints with its constructor's own name,
never a qualified one:

```funl
import * as geometry from "modules/geometry.funl"
import { square } from "modules/geometry.funl"

val c = geometry.circle(2)
write( c is geometry.shape, c is geometry.circle )
if c is square then write( "a square" ) else write( "not a square" )
```

```output
geometry is loaded
circle(2), circle(2)
not a square
```

A type the file did not import is not a type there:

```funl
import { circle } from "modules/geometry.funl"

write( circle(1) is shape )
```

```error
`shape` is not a type
```

A constructor the file did not import is not defined there:

```funl
import { shape } from "modules/geometry.funl"

write( square(1) )
```

```error
`square` is not defined
```

After `is`, a qualified name the module does not export is refused:

```funl
import * as geometry from "modules/geometry.funl"

write( 1 is geometry.oval )
```

```error
`docs/reference/modules/geometry.funl` does not export `oval`
```

Two modules may each have a type of one name. [`modules/boxes.funl`](modules/boxes.funl) declares
`export data shape = box(w, h)`, and its `shape` is a different type from `geometry.funl`'s:

```funl
import * as geometry from "modules/geometry.funl"
import * as boxes from "modules/boxes.funl"

val b = boxes.box(2, 3)
write( b is boxes.shape, boxes.area(b) )
if b is geometry.shape then write( "geometry" ) else write( "not geometry" )
```

```output
geometry is loaded
box(2, 3), 6
not geometry
```

A file cannot declare a type of the name an import declares:

```funl
import { shape } from "modules/geometry.funl"

data shape = dot
```

```error
the type `shape` is imported, and this file cannot also declare it
```

Constructors are different. A record is known by its constructor's name and number of fields
wherever it was made, Prolog included, so two constructors with one name and one number of fields
cannot be declared anywhere in a program, in one file or in two
([Data](data.md#data-types-and-constructors)). [`modules/rings.funl`](modules/rings.funl) declares
`export data ring = circle(r)`, so it cannot be loaded beside `geometry.funl`:

```funl
import { area } from "modules/geometry.funl"
import { ring } from "modules/rings.funl"
```

```error
the constructor `circle` of 1 field is already declared at docs/reference/modules/geometry.funl
```

The same name with another number of fields is another constructor, and a file reaches it only where
it sees it. [`modules/pair.funl`](modules/pair.funl) exports `point` of two fields and
[`modules/mark.funl`](modules/mark.funl) `point` of one. Imported as below, `point` is the first and
`mark.point` the second, and `is point` tests for the first only:

```funl
import { point } from "modules/pair.funl"
import * as mark from "modules/mark.funl"

write( point(1, 2), mark.point(7) )
write( if mark.point(7) is point then "a point" else "not a point" )
```

```output
point(1, 2), point(7)
not a point
```

So `point(7)` there is refused, as it would be with no `point` of one field anywhere:

```funl
import { point } from "modules/pair.funl"
import * as mark from "modules/mark.funl"

write( point(7) )
```

```error
the constructor `point` takes 2 fields, and this call gives 1
```

A module that declares both exports both under the one name.
[`modules/points.funl`](modules/points.funl) declares `point` of one field and of two:

```funl
import { point } from "modules/points.funl"

def size( point(_) ) = 1
def size( point(_, _) ) = 2

write( size(point(7)), size(point(1, 2)), point(7) is point )
```

```output
1, 2, point(7)
```

## Imports run first, and once

Every import is resolved before anything runs. Each imported module is compiled first, and its top
level runs before the importing file's statements. However many files import a module, its top
level runs once. Here [`modules/both.funl`](modules/both.funl) imports `geometry.funl` as well:

```funl
import { floor_area } from "modules/both.funl"
import { area, circle } from "modules/geometry.funl"

write( floor_area(4), area(circle(1)) )
```

```output
geometry is loaded
16, 3
```

A module is known by its file, not by how a path spells it: two paths reaching one file, through
`..` or through a symbolic link, import one module, and its top level still runs once:

```funl
import { area } from "modules/geometry.funl"
import * as g from "../reference/modules/geometry.funl"

write( area(g.circle(1)) )
```

```output
geometry is loaded
3
```

## Exports are a snapshot

A module's exports are taken when its top level finishes. [`modules/counter.funl`](modules/counter.funl)
exports a `var` and a function that changes it:

```
export var count = 0

export def bump() = (count += 1)
```

The importing file sees the value `count` had when the module finished loading:

```funl
import { count, bump } from "modules/counter.funl"
import * as counter from "modules/counter.funl"

bump()
write( bump(), count, counter.count )
```

```output
2, 0, 0
```

## Imported names

An imported name is a top-level name of the importing file. **It shadows a builtin of the same
name**, as a top-level `def` does. **The file cannot define the same name**, or use it for a
top-level variable:

```funl
import { area } from "modules/geometry.funl"

def area( x ) = x
```

```error
`area` is imported from `docs/reference/modules/geometry.funl`, and this file cannot also define it
```

```funl
import { unit } from "modules/geometry.funl"

var unit = 2
```

```error
`unit` is imported, and cannot also be a variable
```

`as` avoids the clash:

```funl
import { area as size, square } from "modules/geometry.funl"

def area( x ) = x

write( size(square(2)), area(5) )
```

```output
geometry is loaded
4, 5
```

Two imports cannot declare the same name:

```funl
import { unit } from "modules/geometry.funl"
import { area as unit } from "modules/geometry.funl"
```

```error
`unit` is imported twice
```

## A circle of imports

A file cannot import a file that is still waiting for it.
[`modules/left.funl`](modules/left.funl) imports `right.funl`, which imports `left.funl`. The error
names every file in the circle:

```funl
import { left } from "modules/left.funl"
```

```error
this import closes a circle: `docs/reference/modules/left.funl` imports `docs/reference/modules/right.funl` imports `docs/reference/modules/left.funl`
```

An error inside a module is reported against the module's own file and line, under the import that
loaded it.

## What an import can name

The thing after `from` names the module in one of three ways:

- **a quoted path** is a file: `.funl` and `.lfunl` files are FunL modules, and `.pl` files are
  Prolog;
- **a bare word** names a package;
- **`funl:name`** names one of FunL's own modules.

No packages or modules of FunL's own are available yet, so a bare word or a `funl:name` is refused:

```funl
import { parse } from funl:json
```

```error
unknown module `funl:json`
```

A file name written without quotes is a bare word, not a file:

```funl
import { area } from geometry.funl
```

```error
a file is imported by a quoted path: `"geometry.funl"`
```

A path with any other extension is refused:

```funl
import "notes.txt"
```

```error
`notes.txt` is neither a FunL module nor a Prolog file
```

`import "file.funl"` with no names runs the module's top level and declares nothing:

```funl
import "modules/geometry.funl"

write( 'after' )
```

```output
geometry is loaded
after
```

A literate module (`.lfunl`, see [source files](source-files.md)) is imported the same way, and only
its indented lines are the program. [`modules/scaling.lfunl`](modules/scaling.lfunl) exports
`scaled`:

```funl
import { scaled } from "modules/scaling.lfunl"

write( scaled(2) )
```

```output
20
```

## Prolog files

A Prolog file has no exports, so it is imported whole with `import "file.pl"`, and every predicate
it defines becomes a relation of the importing file ([Prolog](prolog.md)). Asking a Prolog file for
names is refused, and the message gives the form that works:

```funl
import { parent } from "prolog/family.pl"
```

```error
a Prolog file is imported whole, every predicate it defines with it: `import "prolog/family.pl"`
```

A Prolog file is loaded once in a program, however many of its files import it, and its directives
run once. As with a module, a file is known by its real path. Here
[`modules/greeter.funl`](modules/greeter.funl) imports [`modules/greeting.pl`](modules/greeting.pl)
too, whose directive writes a line as it loads:

```funl
import "modules/greeting.pl"
import { greet } from "modules/greeter.funl"
import "modules/../modules/greeting.pl"

write( greet("world") )
```

```output
greeting.pl is loaded
hello, world
```

A Prolog program that imports a FunL module with `:- import("file.funl").` sees only the module's
exports. `helper` is not exported, so to Prolog it is no predicate:

```prolog
:- import("modules/geometry.funl").

:- area(square(3), A), write(A), nl.
:- catch(helper(1, X), error(E, _), (write(E), nl)).
```

```output
geometry is loaded
9
existence_error(procedure,helper/2)
```
