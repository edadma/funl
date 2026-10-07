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

A Prolog program reads a literate FunL file too, with `:- import`, and calls its functions as
relations whose last argument is the result. That takes two files, so the two below are shown rather
than run here. With `scaling.lfunl`

````lfunl scaling.lfunl
# Scaling

The scale every function reads.

    val scale = 10

And the one Prolog calls.

    def scaled( n ) = n * scale
````

this Prolog program, run with `funl main.pl`, prints `20`:

```prolog
:- import("scaling.lfunl").
:- scaled(2, X), write(X), nl.
```
