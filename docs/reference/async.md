---
title: async and await
weight: 120
---

# `async` and `await`

A function written `async def` answers a **promise** when it is called: a value standing for the
answer the function will give. Its body runs at once, up to its first `await`; then the caller
carries on with the promise, and the rest of the body runs later. `await p` waits for promise `p`
and is its value.

```funl
async def work( name, turns ) =
  i = 0
  while i < turns do
    write( "$name turn $i" )
    await ()
    i += 1
  name + " finished"

async def main() =
  a = work( "a", 3 )
  b = work( "b", 2 )
  write( "started both" )
  write( await a )
  write( await b )

main()
write( "main returned" )
```

```output
a turn 0
b turn 0
started both
main returned
a turn 1
b turn 1
a turn 2
a finished
b finished
```

Each call of `work` printed its first line before `main` went on, and `main` printed `started both`
before either call had finished. A call that waits is set aside, and the program goes on with
whatever can run; the two calls then take turns, each `await` letting the other have one.

## Promises

A promise prints as what it has come to so far: `<promise pending>` until the call has answered,
then `<promise` and the answer `>`. `p is promise` tests for one.

```funl
async def double( x ) = (await x) * 2

p = double( 21 )
write( p )
write( await p )
write( p )
write( p is promise, 42 is promise | "42 is not a promise" )
```

```output
<promise pending>
42
<promise 42>
<promise 42>, 42 is not a promise
```

**`await` of anything that is not a promise is that value**, as `await x` above is `x`. It still
waits its turn, so a program cannot tell which it awaited by watching what runs next.

## When the waiting ends

A call that waits goes back into a queue when what it waits for is ready, and the queue is run in
order. **A promise that has already been kept still goes round the queue**, behind whatever was
waiting before it:

```funl
async def now( x ) = x

async def first() =
  write( "first awaits a kept promise" )
  write( await done )
  write( "first resumes" )

async def second() =
  write( "second awaits a plain value" )
  write( await 7 )
  write( "second resumes" )

done = now( "kept" )
first()
second()
write( "the top level goes on" )
write( await done )
write( "the top level resumes" )
```

```output
first awaits a kept promise
second awaits a plain value
the top level goes on
kept
first resumes
7
second resumes
kept
the top level resumes
```

**The top level may `await`**, as the last lines show: it waits as a call does. The program ends
once the top level has finished and nothing waiting can run any more. A call still waiting for a
promise nothing will keep does not hold the program open; the lines below its `await` never run. A
timer does hold it open: [`funl:async`](../library/async.md)'s `sleep` answers a promise a timer
keeps, and a call waiting for that one runs when the timer fires.

```funl
var p = undefined

async def selfish() =
  await ()
  write( "waiting for itself" )
  await p
  write( "never printed" )

p = selfish()
write( "the top level is done" )
```

```output
the top level is done
waiting for itself
```

An imported file's top level finishes, `await`s and all, before a line of the file importing it
runs.

## `await` belongs to its own function

`await` is written in an `async` function, or at the top level. One written in a function that is
not `async` -- a lambda inside an `async` function included -- is refused before the program runs:

```funl
async def outer( xs ) =
  map( x -> await x, xs )
```

```error
`await` is written in an `async` function or at the top level
```

A lambda is made `async` by writing the word before it, and then answers a promise as an `async def`
does. Every way of calling a function starts it the same way -- by name, through a value, from a
builtin that calls it back:

```funl
async def double( x ) = (await x) * 2

twice = double
plus_one = async x -> (await x) + 1

write( await twice( 2 ) )
write( await plus_one( 4 ) )
write( zipWith( async (a, b) -> a + b, [1, 2], [10, 20] ) )
```

```output
4
5
[<promise 11>, <promise 22>]
```

Only a function can be `async`. A relation answers by unifying, and has no value to promise:

```funl
async def parent( #tom, #bob )
```

```error
`parent` is a relation, and only a function is `async`
```

## Failure and backtracking

**An `async` function that fails gives a promise that fails**, and `await` of it fails where it is
written, as the call itself would have if the function were not `async`. Such a promise prints as
`<promise failed>`.

```funl
async def small( n ) = (await n) < 5

p = small( 9 )
write( await p | "small(9) failed" )
write( p )
write( await small( 2 ) )
```

```output
small(9) failed
<promise failed>
5
```

**`await` is bounded**, as `return` is: it produces one value and is never backtracked into. A
failure after it goes back past it, to whatever could produce another value before it, so what was
awaited is not waited for again. What could produce another value before the `await` still can once
the waiting is over -- a generator may be asked for its next value with an `await` between each:

```funl
var asked = 0

async def ask( n ) =
  asked += 1
  write( "asked $n" )
  n

def three() =
  yield 1
  yield 2
  yield 3

async def main() =
  for x <- three() do
    write( "got " + await ask( x ) )

  every write( (10 | 20) + await ask( 1 ) )

  write( await ask( 5 ) == 6 | "not six" )
  write( "$asked asks" )

main()
```

```output
asked 1
got 1
asked 2
got 2
asked 3
got 3
asked 1
11
asked 1
21
asked 5
not six
6 asks
```

`(10 | 20)` gave its second value after the `await` to its right had waited; the `every` then made
that `await` again, with a new call, as it makes every part of its expression again after a
backtrack. `await ask( 5 ) == 6` failed, and `ask( 5 )` was not called a second time.

## Faults

**A fault in an `async` function is raised again where its promise is awaited**, so a `catch` there
catches it:

```funl
async def boom( n ) =
  await n
  error( "boom $n" )

write( (await boom( 1 )) catch e -> e.message )
```

```output
boom 1
```

**A fault nothing awaits is the program's fault.** It is reported once everything else has had its
turn, and the program ends with it:

```funl
async def boom() =
  write( "boom starts" )
  await ()
  1 / 0

boom()
write( "the top level ends" )
```

```error
1 / 0 divides by zero
```
