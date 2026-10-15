---
title: Keywords
weight: 80
---

# Keywords

A **keyword** is a name written with `&` in front of it, `&pos`, and it stands for something the
language, the machine or the host knows: the scan's position, a named cset, a constant, the time of
day. `&` touching the first letter of a name makes one token, so keywords are a namespace of their
own, and a program's own names cannot shadow them:

```funl
def area( pi ) = &pi * 2 ^ 2
write( area(3) )
```

```output
12.566370614359172
```

| keyword | what it is |
|---|---|
| `&subject`, `&pos` | the scan's subject and position, assignable ([scanning](scanning.md#subject-and-pos)) |
| `&lcase`, `&ucase`, `&letters`, `&digits`, `&ascii`, `&cset` | the csets `lowercase`, `uppercase`, `letters`, `digits`, `ascii` and `cset_all` |
| `&null`, `&fail` | `undefined`, and `fail` |
| `&pi`, `&e`, `&phi` | the constants `pi`, `e` and `phi` |
| `&random` | the random generator's seed, assignable |
| `&time`, `&clock`, `&date`, `&dateline` | the processor time used, in milliseconds; the time of day, the date, and both as a person writes them |
| `&version`, `&host`, `&progname` | FunL's version, the machine's name, the program's file |
| `&features` | generates what this FunL has: `"large integers"`, `"co-expressions"`, the platform, and each optional feature built in |
| `&file`, `&line` | the source file and line the keyword is written on |
| `&level` | how many calls deep the running code is |
| `&trace` | while not zero, each call, return, suspension, resumption and failure of a function is written to standard error; assignable ([tracing](#tracing)) |
| `&dump` | while not zero, the program's end writes `display()`'s report to standard error; assignable ([looking inside](#looking-inside)) |
| `&collections`, `&allocated` | how many times the collector has run, and the bytes the program has allocated in the collected heap |
| `&storage`, `&regions` | the bytes in use in the collected heap, and the heap's size ([looking inside](#looking-inside)) |
| `&current`, `&source`, `&main` | the running co-expression, the one that activated it, and the one the program started in ([co-expressions](coexpressions.md#current-source-and-main)) |

## `&` and conjunction

Conjunction is `&` with a space after it. `&` touching a name after an operand reads as a keyword
where an operator belongs, and is refused with the spelling that was meant:

```funl
x = 1
y = 2
write( x &y )
```

```error
`&y` touching its name is the keyword `&y`, and a keyword cannot follow an operand
```

A name that is not a keyword is refused too:

```funl
write( &colour )
```

```error
`&colour` is not a keyword
```

## Values

The cset keywords are the named csets, and the constants are the constants:

```funl
write( &digits, &lcase.length, 'Q' in &ucase )
write( &pi, &e, &phi )
write( if /&null then 'undefined' else 'defined', if &fail then 'succeeded' else 'failed' )
```

```output
cset("0123456789"), 26, Q
3.141592653589793, 2.718281828459045, 1.618033988749895
undefined, failed
```

`&random` is the seed the generator started from, as `random_seed()` answers it. Assigning it
restarts the generator, so a run that sets it repeats:

```funl
&random = 2026
a = [?100, ?100, ?100]
&random = 2026
write( if a == [?100, ?100, ?100] then 'repeated' else 'differs', &random )
```

```output
repeated, 2026
```

Only `&subject`, `&pos`, `&random`, `&trace` and `&dump` can be assigned, and only the scan keywords
with `<-`:

```funl
&pi = 3
```

```error
`&pi` cannot be assigned
```

## The clocks and the host

`&time` is the processor time the program has used, in milliseconds. `&clock` is the host's time of
day, `14:05:09`; `&date` its date, `2026/10/15`; and `&dateline` both as a person writes them,
`Thursday, October 15, 2026  2:05 pm`. `&host` is the machine's name and `&progname` the file `funl`
was given to run.

```funl
write( if &time is integer then 'ms' else '?', &clock.length, &date.length, &date(4) )
write( if &host is string then 'named' else '?' )
```

```output
ms, 8, 10, /
named
```

`&version` names FunL and its version. `&features` generates `"large integers"` and `"co-expressions"`, then the platform
FunL was built for, then each optional feature of this build — `http`, `async`, `profile`:

```funl
write( if &version ? match('FunL version ') then 'FunL' else '?' )
write( &features )
```

```output
FunL
large integers
```

## Where the code is

`&line` is the line the keyword is written on, and `&file` the file — both fixed when the program is
compiled. `&level` is how deep in calls the running code is: 1 at the top level and one more inside
each call. A call in tail position takes its caller's place, so it is counted at its caller's depth.

```funl
def depth() = &level
def twice() = depth() + 0
write( &line, &level, depth(), twice() )
```

```output
3, 1, 2, 3
```

## Tracing

`&trace` is 0 until a program assigns it. While it is not zero, every call of a function, and what
became of it, is written to standard error, one line each; a positive `&trace` falls by one with
each line, so `&trace = 20` shows the next twenty, and a negative one, `&trace = -1`, traces until it
is set back to 0. In the transcripts on this page, a line marked `stderr:` went to standard error.

```funl
def fact( 0 ) = 1
def fact( n ) = n * fact( n - 1 )
&trace = -1
write( fact(2) )
```

```output
stderr: docs/reference/keywords.md:4 | fact(2)
stderr: docs/reference/keywords.md:2 | | fact(1)
stderr: docs/reference/keywords.md:2 | | | fact(0)
stderr: docs/reference/keywords.md:1 | | | fact returned 1
stderr: docs/reference/keywords.md:2 | | fact returned 1
stderr: docs/reference/keywords.md:2 | fact returned 2
2
```

A line is Icon's: the file and line of the code the event happened at, a `| ` for each call the
event is inside, and the event. A value is shown as `image` writes it. The events, and where each is
placed, are:

| line | when | placed at |
|---|---|---|
| `f(1, "a")` | `f` is called, with these arguments | the call |
| `f returned 2` | `f` gives its value and can give no other | what gave the value |
| `f suspended 2` | `f` gives a value and can be asked for another: a generator | the `yield` |
| `f resumed` | failure goes back into `f` for its next value | what failed back into it |
| `f failed` | failure leaves `f` for good: it gives no value, or no more | what failed in it |

So `fact(2)` above is placed on line 4, where the program calls it, and each `fact(n - 1)` on line
2, where the second clause does. A call written over several lines is placed on the line it begins
on, and each of its arguments on its own. A function another function calls — `map`'s function
argument, say — is placed at that call, in the prelude if that is where it is.

A generator suspends with each value, is resumed for the next, and fails when it has none left:

```funl
def two()
  yield 'a'
  yield 'b'
&trace = -1
every write( two() )
```

```output
stderr: docs/reference/keywords.md:5 | two()
stderr: docs/reference/keywords.md:2 | two suspended "a"
a
stderr: docs/reference/keywords.md:5 | two resumed
stderr: docs/reference/keywords.md:3 | two suspended "b"
b
stderr: docs/reference/keywords.md:5 | two resumed
stderr: docs/reference/keywords.md:3 | two failed
```

The same lines place a fault nothing catches: `funl: half at prog.funl:2: 4 / 0 divides by zero`
names the function the fault was raised in and the file and line of what raised it.

Functions, lambdas and the prelude's functions are traced, since they are all FunL; a builtin is
not, nor is Prolog. A call in tail position takes its caller's place, as it does for `&level`, so
the caller's return is not shown: the callee's return is the caller's.

## Looking inside

`display()` writes to standard error the named variables of the function running — its parameters
and locals, each as `image` writes it — and then those of the top level. A name not yet given a value
is `undefined`.

```funl
total = 10
def share( n )
  part = total / n
  display()
  part
write( share(4) )
```

```output
stderr: share local identifiers:
stderr:    n = 4
stderr:    part = 5/2
stderr: 
stderr: top-level identifiers:
stderr:    total = 10
5/2
```

Setting `&dump` to anything but 0 writes the same report, headed `Termination dump:`, when the
program ends — however it ends, a fault included, and then about the function the fault was raised in.

`collect()` runs the collector now. `&collections` is how many times it has run, and `&allocated`
how many bytes the program has allocated in the collected heap since it started — one number each,
where Icon's give one for each of its regions, since FunL allocates in one, the heap. How often the
collector runs depends on the heap, so a program compares these rather than printing them:

```funl
before = &collections
collect()
write( if &collections > before then 'collected' else 'not', if &allocated > 0 then 'counted' else 'none' )
```

```output
collected, counted
```

`&storage` is how many bytes are in use in the heap now: those its objects held when the collector
last ran, and all made since. `&regions` is the heap's size in bytes, which it never grows past —
a program whose live data outgrows it faults with `heap exhausted`. Icon's generate a number for
each of its regions, string and block; FunL's heap is one region, so each is one number. A value
dropped is still counted in `&storage` until the collector runs:

```funl
before = &storage
squares = [x * x | x <- 1..100]
made = &storage
squares = ()
collect()
write( if made > before then 'grew' else 'not', if &storage < made then 'freed' else 'kept', if &regions >= made then 'fits' else 'over' )
```

```output
grew, freed, fits
```
