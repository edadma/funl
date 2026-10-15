---
title: funl:term
weight: 20
---

# `funl:term` — the keyboard, a key at a time

`funl:term` reads the keyboard one key at a time, without waiting for the end of a line, as Icon's
`getch`, `getche` and `kbhit` do.

| function | what it does |
|---|---|
| `getch()` | the next key typed, a one-character string, not echoed |
| `getche()` | the same, echoed to standard output |
| `kbhit()` | `()` where a key has been typed and not yet read, failure where none has |

**Each call puts the terminal in raw mode for as long as it takes, and puts it back**, so nothing
the program does between calls sees a changed terminal: `read()` still reads whole lines, edited as
the terminal edits them. A key that is more than one byte, `é`, is one character. What the program has
written is shown before `getch` waits, so a prompt appears first.

```funl
import { getch } from funl:term

writes( "Continue? [y/n] " )
write( if getch() == "y" then "yes" else "no" )
```

**A key `kbhit()` finds waiting is the next `getch()`'s**, so a loop can do its work and look at the
keyboard between steps without stopping:

```funl
import { getch, kbhit } from funl:term

n = 0
while not kbhit() do n = n + 1
write( "stopped by '${getch()}' after $n steps" )
```

**Where standard input is not a terminal** — a file, a pipe — the keys are the program's input, a
character at a time, as `reads(1)` takes them, and no key is ever waiting, so `kbhit()` fails. Where
the input has ended, `getch()` and `getche()` fail. The programs on these pages have no keyboard and
an input that has ended:

```funl
import { getch, getche, kbhit } from funl:term

write( getch() | "no key" )
write( getche() | "no key" )
write( kbhit() | "nothing waiting" )
```

```output
no key
no key
nothing waiting
```

The names are the module's: in a file that does not import `getch`, there is no `getch`.

```funl
write( getch() )
```

```error
`getch` is not defined
```
