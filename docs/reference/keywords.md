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
| `&features` | generates what this FunL has: `"large integers"`, the platform, and each optional feature built in |
| `&file`, `&line` | the source file and line the keyword is written on |
| `&level` | how many calls deep the running code is |

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

Only `&subject`, `&pos` and `&random` can be assigned, and only the scan keywords with `<-`:

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

`&version` names FunL and its version. `&features` generates `"large integers"`, then the platform
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
