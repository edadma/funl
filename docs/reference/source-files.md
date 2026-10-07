---
title: Source files
weight: 90
---

# Source files

A FunL program is a file named `.funl`. It is run by naming it:

```sh
funl program.funl
```

`funl` also runs a literate program, `.lfunl`, and a Prolog program, `.pl`.

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

````markdown
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

```
hello, world
```

A mistake in a literate file is reported at its place in the document. In

````markdown
# A mistake

Some prose.

    x = 1
    write( y )
````

the undefined `y` is reported at line 6, column 12, where it stands in the file:

```
error: `y` is not defined
 --> mistake.lfunl:6:12
```

A Prolog program reads a literate FunL file too, with `:- import`, and calls its functions as
relations whose last argument is the result. With `scaling.lfunl`

````markdown
# Scaling

The scale every function reads.

    val scale = 10

And the one Prolog calls.

    def scaled( n ) = n * scale
````

this Prolog program, run with `funl main.pl`, prints `20`:

```
:- import("scaling.lfunl").
:- scaled(2, X), write(X), nl.
```
