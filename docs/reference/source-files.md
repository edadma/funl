---
title: Source files
weight: 90
---

# Source files

A FunL program is a file named `.funl`. It is run by naming it:

```sh
funl program.funl
```

`funl` also runs a literate program, `.lfunl`, and a Prolog program, `.pl`. A Prolog file is loaded
and its directives run; one with no `:- initialization(Goal).` directive then starts the Prolog top
level ([Prolog](prolog.md#the-prolog-executable)).

## Literate FunL

**A file named `.lfunl` is literate FunL**: a Markdown document whose lines indented four columns are
the program, and everything else is prose. The file's name decides, and nothing else does: the same
text in a `.funl` file is read as FunL from its first line, prose and all.

- **Indented blocks are one program.** Prose between two indented blocks does not separate them, so a
  name declared in one is in scope in the next.
- **A fenced block is an illustration** (` ``` ` or `~~~`) and never runs, whatever it contains.
- **An indented block under a list item is prose**, as it is in Markdown.
- **A tab in the indentation is refused**, and so is a fence that is never closed.

The prose is blanked, not removed, so every diagnostic names the line and column of the document as
it stands.

This document is a program:

````lfunl greeting.lfunl
# Greeting

This document is a program. The indented lines run:

    name = 'world'

Prose between two indented blocks does not separate them, so `name` is still in scope:

    write( "hello, $name" )

A fenced block is an illustration and never runs:

```
write( 'not run' )
```

- a list item's indented block is prose:

    write( 'not run either' )
````

Saved as `greeting.lfunl`, it runs with `funl greeting.lfunl` and prints

```output
hello, world
```

A mistake in a literate file is reported at its place in the document. In

````lfunl mistake.lfunl
# A mistake

Some prose.

    x = 1
    write( y )
````

the undefined `y` is reported at line 6, column 12, where it stands in the file. The line quoted is the
file's own line, indent included, and the caret is under the column named:

```error
error: `y` is not defined
 --> mistake.lfunl:6:12
  |
6 |     write( y )
  |            ^
```

A tab in the indentation is refused:

````lfunl tabs.lfunl
# Tabs

	write( 'indented by a tab' )
````

```error
error: a tab in the indentation of a literate file — what makes a line program text is four columns of indent, and a tab is as wide as whatever happens to be displaying it, so this line is code in one editor and prose in another
 --> tabs.lfunl:3:1
```

So is a fence that is never closed:

````lfunl open.lfunl
# An open fence

    write( 'runs' )

```
write( 'shown' )
````

```error
error: this fence is never closed, so everything below it is an illustration and none of it is compiled — close it, or indent the lines that are meant to run
 --> open.lfunl:5:1
```

A Prolog program reads a literate FunL file too, with `:- import`, and calls its exported functions
as relations whose last argument is the result. That takes two files, so the two below are shown rather
than run here. With `scaling.lfunl`

````lfunl scaling.lfunl
# Scaling

The scale every function reads.

    val scale = 10

And the one Prolog calls.

    export def scaled( n ) = n * scale
````

this Prolog program, run with `funl main.pl`, prints `20`:

```prolog
:- import("scaling.lfunl").
:- scaled(2, X), write(X), nl.
```

## The preprocessor

**A line whose first character past its indentation is `$` is a directive**, read before the line
around it is compiled. Directives are Icon's:

| directive | what it does |
|---|---|
| `$define name text` | from here on, `name` stands for the tokens of `text` |
| `$undef name` | `name` no longer stands for anything |
| `$ifdef name` | the lines up to the matching `$else` or `$endif` are compiled only when `name` is defined |
| `$ifndef name` | ... only when it is not |
| `$else` | the other branch of the `$ifdef` or `$ifndef` above it |
| `$endif` | the end of the conditional |
| `$include "file"` | the tokens of `file`, read relative to the file the line is in, stand where the line does |
| `$line n "file"` | the next line is line `n` of `file` in diagnostics; the name may be left off |
| `$error text` | the program is refused, with `text` as the diagnostic |

A `$` anywhere else on a line is what it always was: inside a string it interpolates, and outside one it
begins nothing.

### `$define`

**Substitution is by token, never by character.** A defined name is replaced where it is written as
a name, so it is not replaced inside a string, a comment or a longer name; and the replacement is the
definition's tokens as they are, with no parentheses put around them:

```funl
$define LIMIT 2 + 3
$define GREETING "hello"
val LIMITS = 10
write( LIMIT * 2 )
write( GREETING, "GREETING", LIMITS )
```

```output
8
hello, GREETING, 10
```

`LIMIT * 2` is `2 + 3 * 2`, and `LIMITS` is a name of its own.

A definition's text may use other defined names, which are replaced in turn; a name inside its own
definition, directly or through another, is left as it is rather than replaced for ever. Below, the
`x` that `SHOW` brings is replaced by `x * 10` once, and that `x` is the `val`; the `$x` inside the
string is in a string, so it is the `val` too:

```funl
val x = 4
$define x x * 10
$define SHOW write( "$x", x )
SHOW
```

```output
4, 40
```

A definition with nothing after the name defines the name as nothing at all. Defining a name again
with different text is refused -- `$undef` it first; defining it again with the same text is allowed.
A definition takes no parameters, so `$define f(x) ...` is refused; with a space before `(`, the
parenthesis is part of the text.

```funl
$define N 1
$define N 2
```

```error
error: `N` is defined already, as different text
```

### Conditional compilation

`$ifdef name` and `$ifndef name` choose lines by whether `name` is defined, and nest. The lines a
false branch covers are skipped unread: they need not be FunL at all, and their indentation opens and
closes no block.

**Every program starts with these names defined**, each standing for `1`:

| name | defined |
|---|---|
| `_FUNL` | always |
| `_CO_EXPRESSIONS`, `_LARGE_INTEGERS` | always |
| `_MACOS`, `_LINUX` | on that platform |
| `_UNIX` | on a POSIX system, macOS and Linux among them |
| `_HTTP`, `_ASYNC`, `_PROFILE` | when `funl` was built with that feature (`http` and `async` are in a default build) |

```funl
def platform() =
$ifdef _FUNL
    "FunL"
$else
    this line is never read
$endif
write( platform() )
$ifndef _NOT_DEFINED
write( _FUNL + 1 )
$endif
```

```output
FunL
2
```

An `$else` or `$endif` with no conditional open is refused, as is a conditional the file does not
close before it ends: each file closes its own.

### `$include`

**`$include "file"` puts the file's tokens where the line is**, read relative to the directory of the
file holding the line. Unlike `import`, it brings text rather than a module: a `val` or `def` in the
included file is the including file's own, from that line on, and a `$define` in it is in force after
the line. A `.lfunl` file is tangled first, as it is when run.

The `$include` line is a line of the program for indentation: written at the left margin it closes
every block open above it, and indented it stands in the block it is indented into.

**A diagnostic about an included file's text names that file**, its line and its column, and quotes
its line. With `shapes.funl` holding

```funl
def area( r ) = 3 * r * radius
```

a program including it is refused with

```
error: `radius` is not defined
 --> shapes.funl:1:25
  |
1 | def area( r ) = 3 * r * radius
  |                         ^^^^^^
```

**A file that is being included already cannot be included again inside itself**, since that would
never end; the refusal names the chain of files.

### `$line` and `$error`

`$line n "file"` says the next line is line `n` of `file` -- what a program that writes FunL says, so a
diagnostic points into what it was written from. `$error text` refuses the program with `text`, which
together with a conditional says a program cannot be compiled here:

```funl
$line 40 "spec.funl"
write( unknown )
```

```error
error: `unknown` is not defined
  --> spec.funl:40:8
```

```funl
$ifndef _FUNL
$error this program needs FunL
$endif
$ifdef _FUNL
$error this program is not for FunL
$endif
```

```error
error: this program is not for FunL
```

In a literate file the directives are lines of the program like any other: indented four columns.
