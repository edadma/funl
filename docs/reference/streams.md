---
title: Standard streams
weight: 85
---

# Standard streams

A program has three standard streams: the input it reads with `read` and `reads`, the output it
writes with `write` and `writes`, and the error stream it writes with `ewrite`, `ewrites` and `stop`.
These are builtins; none needs an import.

## `write` and `writes`

`write(a, b, ...)` writes its arguments, separated by `, `, and ends the line. `writes` writes them
and does not: the next write to the same stream carries on from where it stopped, so a line is built
in pieces.

```funl
writes( "one" )
writes( "two", 2 )
write( "three" )
writes()
write( "next line" )
```

```output
onetwo, 2three
next line
```

A newline inside the text ends a line as it should:

```funl
writes( "a\nb" )
write( "c" )
```

```output
a
bc
```

Output goes out as the program writes it, not when the program ends, and a line still open when the
program ends stays open: no newline is added for it. Before a read of standard input, what has been
written is pushed out, so a prompt written with `writes` is showing when the read waits:

```funl
writes( "name? " )
write( "hello, " + (read() | "nobody") )
```

```output
name? hello, nobody
```

At a terminal, the prompt stays in front of what is typed.

## `read` and `reads`

`read()` is the next line of standard input without its newline. At the end of the input it fails, so
`while (l <- read())` runs once for each line. `reads(n)` is the next `n` characters, counted as
characters rather than bytes and taken across line ends, newlines included; near the end of the input
it gives what is left, and once nothing is left it fails. `read` and `reads` share one stream: after
`reads(2)` takes the start of a line, `read()` takes the rest of it. A final line with no newline is
read as if it had one.

`reads(n)` faults unless `n` is an integer of at least 1. Input that is not UTF-8 faults.

The programs on this page run with no input, so every read fails:

```funl
write( read() | "no input" )
write( reads(5) | "no input" )
```

```output
no input
no input
```

A program that echoes its input in capitals is a loop over the lines:

```funl
while (l <- read())
  write( upper(l) )
```

## `ewrite`, `ewrites` and `stop`

`ewrite` and `ewrites` are `write` and `writes` for standard error. `stop(a, ...)` writes its
arguments and a newline to standard error and ends the program with status 1; nothing catches it, as
nothing catches `halt`.

```funl
ewrite( "warning:", "empty input" )
if read() == undefined then stop( "no input" )
```

Each goes out as it is written, standard output pushed out ahead of anything written to standard
error, so a terminal showing both shows them in the order the program wrote them. Prolog's `write/1`,
`nl/0` and `format/2` write to the same standard output as `write`, in the order they run.
