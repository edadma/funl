---
title: funl:async
weight: 25
---

# `funl:async` — timers, I/O and promises to await

`funl:async` makes [promises](../reference/async.md) that do not come from calling an `async`
function: `sleep` answers one a timer keeps; `read_file`, `write_file`, `run` and `fetch` answer one
the file, the child or the network keeps ([I/O that answers a promise](#io-that-answers-a-promise));
and `resolve`, `reject`, `pending`, `settle` and `settle_error` make and answer them by hand.

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
timers and the I/O. **The first is run until it is empty before a timer is looked at**, so a call that awaited
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
| `settle_error(p, x)` | fault pending promise `p` with the error `error(x)` raises |

`resolve` and `reject` stand for something that has already happened; `pending`, `settle` and
`settle_error` for something that has not. Every call waiting on a pending promise is owed a turn once it is settled.
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
run -- or at once, when the call that made it was a statement of its own, nothing being able to
await it then ([faults](../reference/async.md#faults)):

```funl
import { reject } from funl:async

p = reject( "nobody waited" )
write( "the top level is done" )
```

```error
nobody waited
```

**`settle_error(p, x)` faults a promise that is still pending**, as `reject(x)` makes one already
faulted: every call waiting on it raises the error where it awaits, and a `catch` there sees `x`'s
text as the message. Settling a promise that is no longer pending changes nothing, and one faulted
that nothing awaits is the program's fault, as a rejected one is.

```funl
import { pending, settle_error } from funl:async

door = pending()

async def guest() =
  write( (await door) catch e -> "refused: " + e.message )

g = guest()
settle_error( door, "locked" )
await g
```

```output
refused: locked
```

`settle` and `settle_error` want a promise:

```funl
import { settle } from funl:async

settle( 42, 1 )
```

```error
'settle' wants a promise and was given the integer 42
```

## I/O that answers a promise

`read_file`, `write_file`, `run` and `fetch` are the promise-answering twins of the blocking forms in
[`funl:fs`](fs.md), [`funl:process`](process.md) and [`funl:http`](http.md). Each starts its work and
answers a promise at once, so the program goes on while the disk, the child or the network does what
it was asked, and several can be in flight together:

```funl
import { read_file, run } from funl:async

poem = read_file( "docs/library/fs/poem.txt" )
echo = run( "/bin/echo", ["from", "a", "child"] )
write( "both started" )
write( await poem )
write( (await echo).out )
write( "both done" )
```

```output
both started
Roses are red,
violets are blue.

from a child

both done
```

| | |
|---|---|
| `read_file(path)` | a promise of the file's text |
| `write_file(path, text)` | a promise kept with `()` once the file is made or replaced with string `text` |
| `run(program, args, options)` | a promise of the map `funl:process`'s `run` gives: `out`, `err`, and `status`, `signal` or `timed_out`; `args` and `options` are optional, and the options are the same |
| `fetch(url, options)` | a promise of the response `funl:http`'s `fetch` gives; `options` is optional, and the options are the same |

The names are the same as the blocking ones, so a file chooses which it has by the module it imports
them from:

```funl
import { write_file, read_file } from funl:async
import { remove } from funl:fs

await write_file( "docs/library/async-note.txt", "kept for later" )
write( await read_file( "docs/library/async-note.txt" ) )
remove( "docs/library/async-note.txt" )
```

```output
kept for later
```

A response comes back as the blocking `fetch` would give it:

```funl
import { fetch } from funl:async
import { cwd, join } from funl:fs

r = await fetch( "file://" + join(cwd(), "docs/library/http/greeting.txt") )
write( r.body )
write( r.status )
```

```output
Hello from a file.

0
```

**What is not there fails the promise, and `await` fails where it is written**, exactly as the
blocking call would fail: no such file, no directory to write the file into, no such program. Text
that is not UTF-8 fails a `read_file` as well. So `| default` and `if` test an `await` as they test a
call:

```funl
import { read_file, run } from funl:async

write( await read_file("docs/library/fs/nothing.txt") | "(no such file)" )
write( await run("funl-no-such-program") | "not installed" )
```

```output
(no such file)
not installed
```

**Anything else that goes wrong faults the promise**, with the term the blocking call would raise --
`permission_error` for a file that may not be read or a program that may not be run, `system_error`
for the rest, a network that is down among them -- and `await` raises it where it is written, so a
`catch` there sees it:

```funl
import { run } from funl:async

write( (await run("docs/library/process/notes.txt")) catch e -> e )
```

```output
error(permission_error(execute, source_sink, "docs/library/process/notes.txt"), context(_G2, "'run' cannot start `docs/library/process/notes.txt`: permission denied"))
```

**A mistake in the arguments is raised by the call itself**, before any work starts, since it is the
program's own and no answer could change it:

```funl
import { read_file } from funl:async

p = read_file( 42 )
```

```error
'read_file' wants a path, a string, and was given the integer 42
```

**Anything in flight keeps the program alive**, as a timer does: the program does not end while a
file is being read or a child is running, whether or not anything awaits its promise.

`run`'s `inherit_env: false` gives the child only the variables `env` names, and none at all where
`env` is left out, as the blocking `run` does. **A bare program name is found on the same `PATH` as the
blocking `run` finds it on**: the `PATH` in `env`, else this program's own where `inherit_env` is
`true`, else the system's default path.

These four are behind the `async` feature, which is on unless a build turns it off; `fetch` is
behind `http` as well. A build without them still has `sleep` and the promise makers, and answers an
import of one of the four with the feature it needs.
