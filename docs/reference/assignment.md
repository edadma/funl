---
title: Assignment and scope
weight: 40
---

# Assignment and scope

`=` stores a value in a variable, `+=` and the other compound forms change one, and `<-` stores a
value that is taken back if the expression it is part of backtracks. Where a name can be seen is
decided by the block it is declared in.

## `=` assigns

`x = e` stores `e`'s value in `x` and produces it. A name assigned at the top level that nothing has
declared is declared by the assignment. Several names take several values at once, and every value
is made before any is stored, so `a, b = b, a` swaps.

```funl
x = 1
write( x )
x = x + 1
write( x )
write( x = 5 )

a, b = 1, 2
a, b = b, a
write( a, b )
```

```output
1
2
5
2, 1
```

A multiple assignment gives as many values as it has names:

```funl
a, b = 1
```

```error
this assigns 2 variables and gives 1 value
```

## Compound assignment, `++` and `--`

`x op= e` is `x = x op e`, and produces the new value; `+=`, `-=`, `*=`, `/=`, `^=`, `\=` and `%=`
are the forms. `x++` and `x--` produce the old value and then change it; `++x` and `--x` change it
and produce the new one.

```funl
var x = 10
x += 5
x -= 3
x *= 2
write( x )
write( x++, x, ++x, x--, --x )
```

```output
24
24, 25, 26, 26, 24
```

Each of these changes a variable that is already in scope, so it cannot be the first thing said
about a name:

```funl
n += 1
```

```error
`n` is not defined
```

## `val` and `var`

`var x = e` declares a variable; `var x` alone declares one whose value is `undefined`. `val x = e`
declares a name whose value never changes, and assigning to it is refused before the program runs.
A name bound by `where` is the same.

```funl
var u
write( u )
val k = 1
write( k )
```

```output
undefined
1
```

```funl
val k = 1
k = 2
```

```error
`k` is a `val`, and cannot be assigned
```

```funl
def f( x ) = (y = x)
  where y = 1
```

```error
`y` is a `val`, and cannot be assigned
```

## Every block has names of its own

An indented block, a parenthesized sequence `(s; s; e)` and a `for`'s bindings are each a block.
`val`, `var` and `free` declare a name in the innermost block. It is in scope from the end of its
declaration to the end of the block, and inside the block it hides a name of the same spelling
outside it.

```funl
x = 1

if true
  val x = x + 10
  write( x )

write( x )
write( (val q = 4; q * q) )

for x <- [7, 8] do write( x )
write( x )
```

```output
11
1
16
7
8
1
```

**A name read after its block has ended reads `undefined`.** A name that no block has declared before
the read is refused.

```funl
if true
  var t = 5
  write( t )

write( t )
```

```output
5
undefined
```

```funl
write( z )
```

```error
`z` is not defined
```

## A builtin's name is free to use

The builtins — `sqrt`, `round`, `log`, `sum`, `sign`, `pi` and the rest — are names outside every
block of the program. A `val`, a `var`, `free`, an assignment, a parameter or a local `def` that uses
one hides the builtin wherever that name is in scope, and the builtin answers everywhere else. A
top-level `def` of a builtin's name is the program's definition, used in place of the builtin.

```funl
write( round(2.6) )
val round = 3
write( round )

log = []
write( log )

if true
  val sqrt = 7
  write( sqrt )

write( sqrt(9) )

def scale( sign ) = sign * 2
write( scale(5), sign(-5) )

def sum( xs ) = 'mine'
write( sum([1, 2]) )
```

```output
3
3
[]
7
3.0
10, -1
mine
```

A name the program itself defines at the top level — a function, a relation, or a Prolog predicate
it imports — is not a builtin, and the top level cannot also make it a variable.

```funl
def parent( #ann, #bob )
val parent = 1
```

```error
`parent` is already a relation, and cannot also be a variable
```

## `=` stores into the nearest variable

`x = e` stores into the nearest `x` in scope: this block's, an enclosing block's, or an enclosing
function's. It declares `x` in the innermost block only when there is no `x` in scope. That is how a
loop changes a counter declared above it, and how a function changes a variable of the program
around it.

```funl
var i = 0

while i < 3
  i = i + 1

write( i )

total = 0

def add( k ) = (total += k)

add( 4 )
add( 5 )
write( total )

if true
  fresh = 'inside'
  write( fresh )

write( fresh )
```

```output
3
9
inside
undefined
```

## `<-` assigns, and undoes itself on backtracking

`x <- e` is **reversible assignment**. It stores `e`'s value in `x` as `=` does, but if the expression
it is part of later fails back past it, `x` gets its previous value back. With `=`, the value stays.

```funl
x = 0
write( (x <- 5) > 10 )
write( x )

y = 0
write( (y = 5) > 10 )
write( y )
```

```output
0
5
```

That makes a trial assignment safe inside a search. Each alternative below is tried with `v` set to
something new, and when an alternative fails, `v` is what it was before the attempt:

```funl
v = 1
write( (v <- 2) & v > 5 | v )
write( v )

w = 0
every (w <- ![1, 2, 3]) & write( w )
write( w )
```

```output
1
1
1
2
3
0
```

When the expression succeeds, the assignment stands:

```funl
used = 0
write( (used <- ![4, 7, 9]) > 8 & used )
write( used )
```

```output
9
9
```

## `in` and `not in`

`x in c` succeeds with `x` when `x` is an element of `c`, and fails otherwise; `x not in c` is the
reverse. `c` is a list, a range, a tuple, a set, an array, a buffer, or a map, whose keys are
searched.

```funl
write( 2 in [1, 2, 3] )
write( 5 in [1, 2, 3] )
write( 5 not in [1, 2, 3] )
write( 2 not in [1, 2, 3] )
if 4 in 1..10 then write( 'in range' )
write( 3 in {1, 3} )
write( #a in {#a: 1} )
write( 1 in {#a: 1} )
```

```output
2
5
in range
3
a
```
