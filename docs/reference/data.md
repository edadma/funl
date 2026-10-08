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
words = ['the', 'cat', 'and', 'the', 'hat', 'and', 'the', 'bat']

every counts( !words ) += 1
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
