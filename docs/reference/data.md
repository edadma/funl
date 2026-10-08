---
title: Data
weight: 30
---

# Data

FunL's values are numbers, strings, atoms, booleans, `()` and `undefined`, and the collections built
from them: tuples, lists, ranges, maps and sets, and the mutable arrays, buffers, maps and sets.
A program declares records of its own with `data`. Numbers have [their own page](numbers.md).

## Literals

A tuple is written in parentheses, a list in brackets, and a set or map in braces. `x:xs` is the
list whose first element is `x` and whose rest is `xs`.

```funl
write( (1, 2) )
write( [1, 2, 3] )
write( 1 : [2, 3] )
write( {1, 2} )
write( {a: 1, "b": 2} )
write( #name, true, false, () )
```

```output
(1, 2)
[1, 2, 3]
[1, 2, 3]
{1, 2}
{"a": 1, "b": 2}
name, true, false, ()
```

A map's key written as a bare name is a string, so `{a: 1}` and `{"a": 1}` are the same map.

## Comprehensions

A comprehension makes a list, or with braces a set, from the values a generator produces, keeping
those that pass its filter.

```funl
write( [x^2 | x <- 1..10 if x % 2 == 1] )
write( {x \ 2 | x <- 1..5} )
```

```output
[1, 9, 25, 49, 81]
{0, 1, 2}
```

`x <- e` takes every value `e` produces, iterating each one that is a collection and taking any
other as it is, so a comprehension collects what a generator produces
([Generators](generators.md) has the whole rule):

```funl
def squares()
  yield 1
  yield 4
  yield 9

write( [x + 1 | x <- squares()] )
write( [x | x <- ([1, 2] | 3)] )
```

```output
[2, 5, 10]
[1, 2, 3]
```

## `sum`

`sum(c)` adds the elements of a list, range, tuple, set, array or buffer with `+`, so rationals stay
exact and the kinds mix as they do in `+`. The sum of nothing is `0`.

```funl
write( sum([1, 2, 3]) )
write( sum(1..100) )
write( sum([1/2, 1/3]) )
write( sum([1, 2.5]) )
write( sum([]) )
write( sum(array([4, 5, 6])) )
write( sum([x^2 | x <- 1..4]) )
```

```output
6
5050
5/6
3.5
0
15
30
```

An element that is not a number is a fault, and so is something that is not a collection.

```funl
write( sum([1, 'a']) )
```

```error
'sum' wants numbers and was given the string 'a'
```

```funl
write( sum(5) )
```

```error
'sum' wants a list and reached the integer 5
```

## Reaching an element

An element is reached by calling the collection with its index or key, and a map's or record's
field by its name. Indexes start at 0, and an index or key that is not there fails.

```funl
data point(x, y)

m = {a: 1, "b": 2}
p = point(3, 4)

write( [3, 4, 5](1) )
write( (10, 20, 30)(2) )
write( m.a, m("b") )
write( p.x, p("y"), p(1) )
write( [3, 4, 5](3) )
write( m("c") )
write( 'done' )
```

```output
4
30
1, 2
3, 4, 4
done
```

A negative index names no element, so it fails as an index past the end does.

```funl
write( [3, 4, 5](-1) )
write( 'done' )
```

```output
done
```

Calling is the only way to index. Brackets after a value are refused:

```funl
write( [3, 4, 5][1] )
```

```error
`[...]` after a value is not indexing: an element is reached with `c(i)`, counting from 0
```

### Slices

Called with a range, a string, a list, a range, a tuple, an array or a buffer gives its **slice**:
the elements at the indexes the range names, in that order, as a value of the same kind. `a..b`
includes `b`, `a..<b` stops before it, `a..+n` takes `n` elements, `a..` runs to the end, and `by`
steps, backwards too. An array's or a buffer's slice is a new one, so changing it leaves the
original as it was. A string is sliced by characters ([String functions](strings.md#slices)).

```funl
l = [10, 20, 30, 40, 50]
write( l(1..3), l(1..<3), l(1..+2), l(3..), l(0.. by 2), l(4..0 by -2) )
write( (1, 2, 3)(1..), (1..10)(2..4) )
a = array([1, 2, 3])
b = a(0..1)
b(0) = 9
write( a, b )
```

```output
[20, 30, 40], [20, 30], [20, 30], [40, 50], [10, 30, 50], [50, 30, 10]
(2, 3), 3..5
Array(1, 2, 3), Array(9, 2)
```

A slice fails if any index it names is not there, as a single index does. A range that runs to the
end may start at the length, which gives an empty slice.

```funl
l = [1, 2, 3]
write( l(3..) )
write( l(2..3) )
write( l(-1..1) )
write( 'done' )
```

```output
[]
done
```

A map is not sliced: a range is looked up as a key like any other.

```funl
m = map()
m(1..2) = 'r'
write( m(1..2) )
```

```output
r
```

## Mutable collections

`array(n)` is an array of `n` elements, each `undefined` until one is stored. `buffer()` is an
empty buffer that grows at the end. `map()` is an empty mutable map, and `map(m)` and `set(s)` are
mutable copies of a map and a set. Assigning to a field of a mutable map adds that key.

```funl
a = array(3)
a(0) = 'x'
write( a, a.length )

b = buffer()
b += 1
b += 2
write( b )

m = map()
m.k = 1
m("j") = 2
write( m )

s = set( {1, 2} )
s += 3
write( s )
```

```output
Array("x", undefined, undefined), 3
Buffer(1, 2)
MutableMap("k": 1, "j": 2)
MutableSet(1, 2, 3)
```

A map written with braces cannot be changed:

```funl
m = {a: 1}
m.b = 2
```

```error
is an immutable map, and nothing can be stored in it
```

## Maps with a default

`map(c, d)` is a mutable copy of `c` whose missing keys read as `d` instead of failing. Counting
needs no membership test, because a word not yet seen reads as `0` and the assignment adds it:

```funl
val counts = map( {}, 0 )
text = 'the cat and the hat and the bat'

every counts( !split(text) ) += 1
write( counts )
write( counts("zzz"), counts.zzz )
```

```output
MutableMap("the": 3, "cat": 1, "and": 2, "hat": 1, "bat": 1)
0, 0
```

- **Reading a missing key gives the default and adds nothing**, by call syntax and field syntax
  alike.
- **Assigning adds the key**, and an update such as `+=` is a read and then an assignment, so it
  starts from the default and adds the key.
- **The default is not an entry**: `k in m`, `m.length`, `for (k, v) <- m` and `!m` see only the
  keys that were assigned.
- **`map(m)` copies the entries but not the default**, so the copy fails on a missing key.

```funl
c = map( {}, 0 )
c("a") += 2
c("b") += 1
write( c("zzz") )
write( c.length )
write( "zzz" in c )
write( "a" in c )
for (k, v) <- c do write( k, v )
write( map(c)("zzz") )
write( 'done' )
```

```output
0
2
a
a, 2
b, 1
done
```

**The default is one value, never copied.** With `map({}, buffer())` every missing key reads the
*same* buffer, and `m(k) += x` appends to that shared buffer in place without adding `k`. A
collection per key is made by assigning one.

```funl
m = map( {}, buffer() )
m("x") += 5
write( m("y") )
write( m.length )
```

```output
Buffer(5)
0
```

A map written with braces has no default, and a lookup of a missing key in it always fails;
`map({a: 1}, 0)` gives a map with both entries and a default. `map` takes at most two arguments,
and its first must be a collection:

```funl
write( map({}, 0, 1) )
```

```error
'map' takes no argument, a collection, or a collection and a default, and was given 3
```

```funl
write( map(5, 0) )
```

```error
'map' wants a list and reached the integer 5
```

## Bytes

A **byte string** is an immutable run of bytes, each an integer from 0 to 255: binary data, as
opposed to text. `bytes(c)` makes one from a collection of integers, and it prints as that call. It
is indexed from 0 like every collection: `b(i)` is a byte, `b(r)` a slice of a range of indexes,
`!b` and `x <- b` generate each byte in turn, and `b.length` counts them. An index that is not there
fails, as it does for a list. `+` joins two byte strings.

```funl
b = bytes([104, 105, 255])
write( b )
write( b(0), b(2), b(1..2), b.length )
write( [x | x <- b], b(3) | 'none' )
write( b + bytes([0]), bytes(1..4) )
```

```output
bytes([104, 105, 255])
104, 255, bytes([105, 255]), 3
[104, 105, 255], none
bytes([104, 105, 255, 0]), bytes([1, 2, 3, 4])
```

Two byte strings are equal when they hold the same bytes, and they order byte by byte, a shorter one
before a longer one it begins. `x is bytes` tests for one, and `in` looks for a byte.

```funl
write( bytes([1, 2]) == bytes([1, 2]), bytes([1, 2]) < bytes([1, 3]), bytes([1]) < bytes([1, 0]) )
write( bytes([1]) is bytes, 'a' is bytes | 'not bytes' )
write( 104 in bytes([104, 105]), {bytes([1]), bytes([1])} )
```

```output
bytes([1, 2]), bytes([1, 3]), bytes([1, 0])
bytes([1]), not bytes
104, {bytes([1])}
```

A value that is not a byte is an error, and so is joining a byte string to anything but another.

```funl
write( bytes([1, 256]) )
```

```error
a byte is an integer from 0 to 255, and 'bytes' was given the integer 256
```

```funl
write( bytes([1]) + 'a' )
```

```error
'+' joins a byte string to another, and was given the string 'a'
```

Text and bytes never turn into each other by themselves: `bytes(s)` encodes a string as UTF-8, and
`decode(b)` reads UTF-8 back as a string ([Strings](strings.md#bytes-and-decode)).

## Handles

A **handle** is a live resource a built-in module gives a program -- an open connection, a
statement, a running process. It is opaque: it prints as `<kind handle>`, is equal only to itself,
and is reached only through its methods, called as `h.name(args)`. Every handle has `close()`, which
gives the resource back at once; closing it again does nothing, and calling any other method of a
closed handle is an error. A handle the program never closes is closed when nothing can reach it any
more. `x is handle` tests for one. A method may fail, as a comparison does, and may generate, as
any generator does: each call starts afresh and produces its values one at a time on backtracking.

## Records

`data point(x, y)` declares a record constructor, and `data shape = circle(r) | square(s) | blank`
declares a type with three constructors. A constructor with no fields, like `blank`, is a value by
itself.

```funl
data point(x, y)
data shape = circle(r) | square(s) | blank

write( point(1, 2) )
write( circle(2), blank )
write( circle(2).r )
```

```output
point(1, 2)
circle(2), blank
2
```

## `undefined`

A variable declared and never assigned holds `undefined`. `\x` succeeds, producing `x`, if `x` is
defined, and `/x` if it is undefined. Both can be assigned to: `/x = e` assigns only if `x` has no
value, and `\x = e` only if it has one.

```funl
var x
write( x )
write( \x )
write( /x )

/x = 5
/x = 6
write( x )
\x = 7
write( x )

var y
\y = 1
write( y )
```

```output
undefined
undefined
5
7
undefined
```

## Types, and `is`

Every value has a type, and `x is t` tests it. Like a comparison, it **succeeds producing `x`** when
`x`'s value is of type `t` and **fails** otherwise, so it serves as a condition, a guard and a filter
directly.

```funl
data shape = circle(r) | square(s)
data point(x, y)

def kind( x ) | x is number = 'number'
def kind( _ ) = 'something else'

x = 'hi'
if x is string then write( x ) else write( 'not text' )
write( kind(5), kind('5') )
every write( (1 | 'a' | 2.5 | #b) is number )
items = [1, circle(2), 'x', point(0, 0)]
write( [i | i <- items if i is record] )
```

```output
hi
number, something else
1
2.5
[circle(2), point(0, 0)]
```

The type names are these:

| name | the values of that type |
|---|---|
| `number` | every number: `integer`, `rational` and `real` |
| `integer` | an integer of any size, `3`, `10^30` |
| `rational` | an exact fraction that is not an integer, `1/3` |
| `real` | an inexact number, `1.5` |
| `string` | `'abc'` |
| `atom` | `#name`, and a constructor with no fields |
| `boolean` | `true`, `false` |
| `list` | `[]`, a list cell `x:xs`, and a range |
| `range` | `1..10`, `1..<n`, `1..` |
| `tuple` | `(1, 2)` |
| `unit` | `()` |
| `map` | `{a: 1}`, and a mutable map, `map()` |
| `set` | `{1, 2}`, and a mutable set, `set(s)` |
| `array` | `array(n)` |
| `buffer` | `buffer()` |
| `cset` | `cset('aeiou')`, `letters` |
| `bytes` | a byte string, `bytes([1, 2])` |
| `handle` | a resource a built-in module gives, such as an open connection |
| `function` | a function or a lambda, and a constructor with fields |
| `record` | a record, and any other compound term |
| `undefined` | `undefined` |
| `variable` | an unbound logic variable |

```funl
data shape = circle(r) | blank
def f( x ) = x

write( 10^30 is integer, 1/3 is rational, 1.5 is real, 1.5 is number )
write( 6/3 is rational )
write( 'abc' is string, #name is atom, blank is atom, true is boolean )
write( [] is list, [1, 2] is list, 1..3 is list, 1..3 is range )
write( [1] is range )
write( (1, 2) is tuple, () is unit )
write( {a: 1} is map, map() is map, {1} is set, set({1}) is set )
write( array(2) is array, buffer() is buffer, cset('aeiou') is cset )
write( f is function, (x -> x) is function, circle is function )
write( circle(1) is record, undefined is undefined )
```

```output
1000000000000000000000000000000, 1/3, 1.5, 1.5
abc, name, blank, true
[], [1, 2], 1..3, 1..3
(1, 2), ()
{"a": 1}, MutableMap(), {1}, MutableSet(1)
Array(undefined, undefined), Buffer(), cset("aeiou")
<function f>, <function lambda>, <constructor circle(r)>
circle(1), undefined
```

### `data` types and constructors

A `data` type is tested by its name, and each constructor by its own. A record's type is its
constructor.

```funl
data shape = circle(r) | square(s) | blank
data point(x, y)

write( circle(2) is shape )
write( circle(2) is circle )
write( square(1) is circle )
write( blank is shape )
write( point(1, 2) is point )
```

```output
circle(2)
circle(2)
blank
point(1, 2)
```

A type's name and its constructors belong to the file that declares them, and another file reaches
them only by importing them ([Modules](modules.md#types-and-constructors)).
[`modules/geometry.funl`](modules/geometry.funl) declares `export data shape = circle(r) | square(s)`:

```funl
import { circle, shape } from "modules/geometry.funl"

write( circle(1) is shape )
write( 3 is shape )
```

```output
geometry is loaded
circle(1)
```

```funl
import { circle } from "modules/geometry.funl"

write( circle(1) is shape )
```

```error
`shape` is not a type
```

A record is known by its constructor's name and number of fields, so a term of that shape is that
record however it was made: by a relation's head, by unification, or by Prolog. It is `is point`, it
matches a `point(a, b)` pattern, and its fields are read by name. The examples load
[`data/points.pl`](data/points.pl), whose `corner/1` holds `point(1, 2)` and whose `built/1` makes
`point(3, 4)` with `=..`.

```funl
import "data/points.pl"
data point(x, y)

def origin( point(0, 0) )
def sum( point(a, b) ) = a + b

free p, q, r
corner( p )
built( q )
origin( r )
write( p is point, p.x, p.y, sum(p) )
write( q is point, q.x, q.y, sum(q) )
write( r is point, r.x, r.y, sum(r) )
```

```output
point(1, 2), 1, 2, 3
point(3, 4), 3, 4, 7
point(0, 0), 0, 0, 0
```

A term with the same name and a different number of fields is a `record`, as every compound term
is, but it is not a `point` and has no fields named `x` and `y`. The file's `solid/1` holds
`point(1, 2, 3)`:

```funl
import "data/points.pl"
data point(x, y)

free s
solid( s )
write( s is record )
write( s is point )
write( s.x )
```

```output
point(1, 2, 3)
```

Because a record is known by its name and number of fields, two constructors with one name and one
number of fields cannot be declared in a program, in any block or in a FunL file it imports. The
second declaration is refused, naming where the first is:

```funl
data point(x, y)
data point(a, b)
```

```error
the constructor `point` of 2 fields is already declared at
```

The same name with a different number of fields is a different constructor and is allowed. A call
or a pattern reaches the one with as many fields as it gives:

```funl
data point(x)
data point(x, y)

def size( point(_) ) = 1
def size( point(_, _) ) = 2

write( point(1, 2).y, point(7).x )
write( size(point(7)), size(point(1, 2)) )
```

```output
2, 7
1, 2
```

`is` with the name tests for a record of any of them:

```funl
data point(x)
data point(x, y)

write( point(7) is point, point(1, 2) is point )
```

```output
point(7), point(1, 2)
```

Two such constructors may also belong to two `data` types, and each type has only its own:

```funl
data mark = point(x)
data pair = point(x, y)

write( point(7) is mark, point(1, 2) is pair )
write( if point(1, 2) is mark then "a mark" else "not a mark" )
```

```output
point(7), point(1, 2)
not a mark
```

A call or a pattern giving a number of fields no constructor of that name has is refused:

```funl
data point(x)
data point(x, y)

def size( point(_, _, _) ) = 3
```

```error
the constructor `point` takes 2 fields, and this pattern gives 3
```

A name is looked up first among the program's own `data` types, then among its constructors, and
then among the built-in names, so a program's own `data list` hides the built-in `list` from `is`:

```funl
data list = nil | cons(h, t)

write( [1] is list )
write( nil is list )
write( cons(1, nil) is list )
```

```output
nil
cons(1, nil)
```

### Logic variables

An unbound logic variable is a `variable`. Once it is bound, it is tested by its value.

```funl
free v
write( v is variable )
v ~ 3
write( v is variable )
write( v is integer )
```

```output
_G0
3
```

### How `is` is written

`is` binds as a comparison does, on the level of `==`, `<` and `in`, so `a < b is integer` tests the
comparison's value, `b`, and `x is integer and y is string` needs no parentheses.

```funl
write( 1 < 2 is integer )
write( 1 is integer and 'a' is string )
```

```output
2
a
```

The type is a name, never an expression, and a name that is not a type is refused before the
program runs:

```funl
x = 1
write( x is integr )
```

```error
`integr` is not a type
```

```funl
x = 1
write( x is 4 )
```

```error
expected a type's name after `is`
```
