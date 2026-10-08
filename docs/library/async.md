---
title: funl:async
weight: 25
---

# `funl:async` — timers and promises to await

`funl:async` makes [promises](../reference/async.md) that do not come from calling an `async`
function: `sleep` answers one a timer keeps, and `resolve`, `reject`, `pending` and `settle` make and
answer them by hand.

```funl
import { sleep } from funl:async

async def work( name, ms, turns ) =
  i = 0
  while i < turns do
    await sleep( ms )
    i += 1
  name + " finished"

async def main() =
  a = work( "a", 8, 3 )
  b = work( "b", 20, 2 )
  write( "started both" )
  write( await a )
  write( await b )

main()
```

```output
started both
a finished
b finished
```

Both calls are waiting on timers at once: `a` comes up at 8, 16 and 24 milliseconds and `b` at 20 and
40, so the whole program takes about 40 milliseconds, not 64.

## `sleep`

`sleep(ms)` answers a promise kept with `()` once `ms` milliseconds, whole or fractional, have passed.
**A timer keeps the program alive**: the program does not end while one is set, whether or not
anything awaits its promise.

```funl
import { sleep } from funl:async

async def later() =
  await sleep( 30 )
  write( "later" )

later()
sleep( 60 )
write( "the top level is done" )
```

```output
the top level is done
later
```

[`funl:time`](time.md) has a `sleep` of the same name that blocks: the whole program stops for the
wait. A file says which it means by which module it imports it from, and may have both under two
names:

```funl
import { sleep } from funl:async
import { monotonic, sleep as block } from funl:time

start = monotonic()
await sleep( 5 )
block( 20 )
await sleep( 10 )
write( if monotonic() - start >= 35 then "waited" else "early" )
```

```output
waited
```

A wait starts when it is asked for, however long the program ran before asking.

## The order things run in

There are two queues. One holds the calls a promise that has been kept owes a turn; the other is the
timers. **The first is run until it is empty before a timer is looked at**, so a call that awaited
something already there goes before a timer that is already due, even one of zero milliseconds:

```funl
import { sleep, resolve } from funl:async

async def timed() =
  await sleep( 0 )
  write( "timer" )

async def ready() =
  await resolve( 1 )
  write( "resumed" )

timed()
ready()
write( "the top level goes on" )
```

```output
the top level goes on
resumed
timer
```

**Timers that come due together are kept together**, in the order they were set, and only then do
the calls waiting on them run. Two timers set one after the other with the same delay are due
together:

```funl
import { sleep } from funl:async

async def chain( name ) =
  await sleep( 10 )
  write( "$name timer" )
  await ()
  write( "$name after" )

chain( "a" )
chain( "b" )
```

```output
a timer
b timer
a after
b after
```

## Making a promise

| | |
|---|---|
| `resolve(x)` | a promise already kept with `x` |
| `reject(x)` | a promise already faulted with the error `error(x)` raises |
| `pending()` | a promise nobody has answered yet |
| `settle(p, x)` | keep pending promise `p` with `x` |

`resolve` and `reject` stand for something that has already happened; `pending` and `settle` for
something that has not. Every call waiting on a pending promise is owed a turn once it is settled.
Settling a promise that is no longer pending changes nothing.

```funl
import { pending, settle } from funl:async

door = pending()

async def guest() =
  write( "waiting" )
  write( "let in with " + (await door) )

guest()
write( door )
settle( door, "a key" )
settle( door, "another key" )
write( door )
```

```output
waiting
<promise pending>
<promise "a key">
let in with a key
```

**`await` of a rejected promise raises the error**, so a `catch` around the `await` sees it:

```funl
import { resolve, reject } from funl:async

write( await resolve( 5 ) )
write( (await reject( "no such row" )) catch e -> e.message )
```

```output
5
no such row
```

A rejected promise nothing ever awaits is the program's fault, reported once everything else has
run:

```funl
import { reject } from funl:async

reject( "nobody waited" )
write( "the top level is done" )
```

```error
nobody waited
```

`settle` wants a promise:

```funl
import { settle } from funl:async

settle( 42, 1 )
```

```error
'settle' wants a promise and was given the integer 42
```
