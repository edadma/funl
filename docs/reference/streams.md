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

A line still open when the program ends is ended for it.

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

Standard output and standard error are collected separately while the program runs, and `funl` prints
the output first and the error lines when the program ends.
