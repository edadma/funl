---
title: Co-expressions
weight: 22
---

# Co-expressions

A generator is resumed only by backtracking inside the expression it is written in, so two
generators cannot be stepped in lockstep, and a generator cannot be handed to another part of the
program to be pulled from later. A **co-expression** can: `create e` sets the expression `e` aside,
with a control stack of its own, and `@c` runs it until it produces its next result.

```funl
val words = create !['the', 'quick', 'fox']
val nums = create !(1..)

while w = @words
  writes( @nums, ' ', w, '\n' )
```

```output
1 the
2 quick
3 fox
```

## `create` and `@`

`create e` runs nothing: it makes the co-expression, and `@c` activates it. `e` runs until it
produces a result, which is the value of `@c`, and the co-expression waits there. The next `@c`
resumes `e` for its next result. When `e` has no more, `@c` fails, and so does every `@c` after:

```funl
val c = create (write( 'running' ) & !['a', 'b'])
write( 'made' )
write( @c )
write( @c )
write( if @c then 'more' else 'over' )
```

```output
made
running
a
b
over
```

**`@c` produces one value**: it is bounded, and failure after it does not go back into `c`. Only
another `@c` asks for the next result:

```funl
val c = create !(1..5)
every x = @c do write( x )
write( @c )
```

```output
1
2
```

`create` reads everything up to the end of the expression, `|` included, so `create !xs | !ys` is one
co-expression over the alternation. `@`, like `!`, binds tighter than any other operator.

## Generating with `!`

`!c` generates the results of activating `c` until it has no more, and so does a binding `x <- c`.
A co-expression is something to pull from, so a co-expression made from another is a pipeline:

```funl
def even( n ) = if n % 2 == 0 then n else fail
def evens( c ) = create even( !c )
def squares( c ) = create (!c) ^ 2

val pipe = squares( evens( create !(1..10) ) )
every write( !pipe )

for n <- create 1 to 3
  write( n )
```

```output
4
16
36
64
100
1
2
3
```

## Transmission

`x @ c` activates `c` and hands it `x`, which becomes the value of the `@` inside `c` at which `c`
last gave up control. Co-expressions are symmetric: any one can activate any other, and
**`&source`** is the one that activated the running one, so `@&source` gives control back and waits
for the next value:

```funl
def consumer()
  repeat
    val line = @&source
    write( 'got ' + line )

val c = create consumer()
@c
'first' @ c
'second' @ c
```

```output
got first
got second
```

Values go both ways. A producer hands each value to `&source` and gets the reply as the value of its
own `@`:

```funl
def producer()
  every i = 1 to 3 do
    write( 'sent ' + str( i ) + ', got back ' + str( i @ &source ) )
  'done' @ &source

val p = create producer()
writes( @p, '\n' )
writes( 'ack1' @ p, '\n' )
writes( 'ack2' @ p, '\n' )
writes( 'ack3' @ p, '\n' )
```

```output
1
sent 1, got back ack1
2
sent 2, got back ack2
3
sent 3, got back ack3
done
```

A value with nowhere to go is dropped: on the first activation there is no `@` inside `c` yet, and
when `c` is resumed for its expression's next result, the expression was suspended at a result, not
at an `@`. `@c` transmits `undefined`.

## The creator's locals

`create` copies the locals of the function it is written in, so the co-expression and its creator
do not see each other's later assignments. Anything further out -- the locals of an enclosing
function -- is shared:

```funl
def outer()
  var shared = 0

  def inner( mine )
    val c = create |(shared += 1, mine += 10)
    write( @c, @c, mine )
    mine = 100
    write( @c )

  inner( 1 )
  write( shared )

outer()
```

```output
(1, 11), (2, 21), 1
(3, 31)
3
```

So each co-expression made in a loop keeps the turn it was made on:

```funl
cs = buffer()

for xs <- [[1, 2], [3], [4, 5, 6]]
  cs += create !xs

write( [@c | c <- cs], [@c | c <- cs] )
```

```output
[1, 3, 4], [2, 5]
```

At the top level, the locals copied are the top level's own names; a top-level function the
co-expression calls still reads the originals.

## Refresh and count

**`^c`** is a new co-expression with `c`'s expression and the locals as they were when `c` was
made, never yet activated; `c` itself is unchanged. **`*c`** is how many results `c` has produced,
and of anything that is not a co-expression, its length:

```funl
def counter( n ) = create |(n += 1)

val c = counter( 0 )
write( @c, @c, @c )

val d = ^c
write( @d, @c, *c, *d, *'abc' )
```

```output
1, 2, 3
1, 4, 4, 1, 3
```

`(^c)` and `(*c)` in parentheses are operator sections, as `(^ 2)` is; `@^c` needs no parentheses.

## `&current`, `&source` and `&main`

**`&main`** is the co-expression the program started in, **`&current`** the one running, and
**`&source`** the one that activated it -- `&main` itself, in `&main` before anything activates it.
A co-expression prints with its number and how far it has gone, and `x is coexpression` tests for
one:

```funl
write( &main, &current == &main )

val q = create (&source, &current)
val (s, c) = @q
write( if s == &main & c == q then 'as named' else 'not', q is coexpression )
```

```output
<co-expression 1 (0 results)>, <co-expression 1 (0 results)>
as named, <co-expression 2 (1 result)>
```

Activating the running co-expression produces the value transmitted, at once: `'x' @ &current` is
`'x'`. A result goes to the co-expression that activated the one producing it -- or, where that one
has ended or is itself suspended at a result, to `&main`, which is always waiting at an `@` while
anything else runs.

## Faults

**A fault inside a co-expression ends it, and is raised again at the `@` that activated it**, where
a `catch` can take it. Every later `@` fails:

```funl
val boom = create (1 / 0)
write( (@boom) catch e -> 'caught ' + e.message )
write( if @boom then 'again' else 'over' )
```

```output
caught 1 / 0 divides by zero
over
```

Nothing catching it, the fault is the program's:

```funl
val c = create (1 / 0)
write( @c )
```

```error
1 / 0 divides by zero
```

`@` of something that is not a co-expression is a type error:

```funl
write( @5 )
```

```error
`@` wants a co-expression, and was given the integer 5
```

A co-expression's expression runs on its own, handing its values to whoever activates it, so nothing
in it can leave it: `return`, `yield`, and `break` or `continue` out of `create` are refused:

```funl
def first( xs ) = create (return !xs)
```

```error
`return` cannot be written inside `create`
```

## With `async` tasks

A co-expression is not a task: nothing schedules it, and it runs only when something activates it,
**as part of the task that activates it**. An `await` inside one waits as that task does: the task
is parked inside the co-expression, other tasks run, and when the promise settles the task resumes
there, its activator still waiting at the `@`. `await` is bounded, so a co-expression that awaits
for each result generates the values to await first:

```funl
async def tenfold( x ) =
  await ()
  x * 10

val c = create (i <- !(1..3)) & await tenfold( i )
every write( !c )
```

```output
10
20
30
```

A co-expression belongs to the task that made it. **Activating it from another task is the fault
`permission_error(activate, coexpression, C)`**, as binding another task's logic variable is. Each
task has its own `&main`, the co-expression it started in:

```funl
val c = create !(1..3)

async def steal() =
  await ()
  write( (@c) catch e -> e.message )
  write( 'x' @ &main )

await steal()
write( @c )
```

```output
task 1 (`steal`) cannot activate a co-expression the top level made
x
1
```

## Logic variables

A co-expression may bind any logic variable its task may, and **the binding is recorded on the
co-expression's own trail**: its own backtracking undoes it, and its activator's does not. Failing
back into the co-expression for its next result undoes what the last one bound:

```funl
free x
val c = create x ~ !['a', 'b', 'c']
write( @c, x )
write( @c, x )
```

```output
a, a
b, b
```

A binding made inside a co-expression of a variable from outside it outlasts the activator's
backtracking, as an assignment made there would:

```funl
free y
val d = create y ~ 1
write( (@d & fail) | y )
```

```output
1
```

So a co-expression walks a relation's solutions one activation at a time:

```funl
def
  append([], ys, ys)
  append(h : t, ys, h : r) :- append(t, ys, r)

free xs, ys
val splits = create append(xs, ys, [1, 2]) & (xs, ys)
write( @splits )
write( @splits )
```

```output
([], [1, 2])
([1], [2])
```

A co-expression nothing refers to any more is collected, however far its expression had got: a
generator abandoned half way costs nothing.
