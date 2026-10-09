---
title: The prelude
weight: 5
---

# The prelude — the list library every program sees

The prelude is a library of list functions in the manner of Haskell's, written in FunL and carried
in the `funl` binary. **Every program sees it without importing anything**:

```funl
xs = [3, 1, 4, 1, 5, 9, 2, 6]

write( reverse(xs), sort(xs) )
write( foldl((a, b) -> a + b, 0, xs), product([1, 2, 3, 4]), maximum(xs) )
write( map(x -> x * x, take(3, xs)), filter(even, xs) )
```

```output
[6, 2, 9, 5, 1, 4, 1, 3], [1, 1, 2, 3, 4, 5, 6, 9]
31, 24, 9
[9, 1, 16], [4, 2, 6]
```

| group | names |
|---|---|
| pairs | `fst`, `snd` |
| ends of a list | `head`, `tail`, `last`, `init` |
| folds | `foldl`, `foldl1`, `foldr`, `foldr1`, `product`, `maximum`, `minimum`, `maxBy`, `minBy` |
| transforming | `map`, `filter`, `concat`, `concatMap`, `reverse`, `sort`, `sortBy`, `nub`, `group` |
| slicing | `take`, `drop`, `takeWhile`, `dropWhile`, `splitAt`, `span`, `spanNot`, `partition` |
| combining | `zip`, `zip3`, `zipWith`, `zipWith3`, `unzip` |
| producing | `iterate`, `forever`, `replicate` |
| asking | `any`, `all`, `elem`, `lookup` |

`sum`, `min` and `max` are builtins, not the prelude's, and `length` is the field `.length`.

## A program's own names come first

A prelude name is seen only where nothing of the program's takes it. A `val`, a parameter, a pattern
variable, an assignment or a local `def` of the name hides the prelude's where it is in scope, and a
top-level `def` is the program's definition in its place everywhere in the file:

```funl
val head = "my head"

def bump( last ) = last + 1

def reverse( xs ) = "never mind"

write( head, bump(1), reverse([1, 2]), last([1, 2]) )
```

```output
my head, 2, never mind, 2
```

What a file's own definition hides is still reached through the module's name, `funl:prelude`,
imported as any [built-in module](../reference/modules.md) is:

```funl
import * as prelude from funl:prelude

def reverse( xs ) = "never mind"

write( reverse([1, 2]), prelude.reverse([1, 2]) )
```

```output
never mind, [2, 1]
```

An import, a builtin and a Prolog predicate the program loads each come before the prelude too.
Where a builtin and the prelude share a name at different numbers of arguments, the call's count
decides: `any(c)` is the scanning builtin and `any(p, xs)` the prelude's question.

## Pairs and the ends of a list

```funl
write( fst((1, "one")), snd((1, "one")) )
write( head([7, 8, 9]), tail([7, 8, 9]), last([7, 8, 9]), init([7, 8, 9]) )
write( last(1..4), init(1..4), tail([7]) )
```

```output
1, one
7, [8, 9], 9, [7, 8]
4, [1, 2, 3], []
```

An empty list has no head, tail, last element or front. Asking is a mistake in the program, the
same fault a function with no clause for its argument gives:

```funl
write( last([]) )
```

```error
argument match not found: no clause of 'last' matches ([])
```

## Folds

`foldl(f, z, xs)` combines from the left, starting from `z`; `foldr` from the right. `foldl1` and
`foldr1` start from the list's own first or last element, so they need one. A builtin is a value, so
`foldl(max, 0, xs)` passes `max` itself:

```funl
write( foldl((a, b) -> a - b, 10, [1, 2]), foldr((a, b) -> a - b, 10, [1, 2]) )
write( foldl1((a, b) -> a - b, [10, 1, 2]), foldr1((a, b) -> a - b, [10, 1, 2]) )
write( foldl(max, 0, [3, 7, 2]), product([]), minimum(["pear", "fig", "plum"]) )
write( maxBy(s -> s.length, ["ab", "abc", "x"]), minBy(s -> s.length, ["ab", "abc", "x"]) )
```

```output
7, 9
7, 11
7, 1, fig
abc, x
```

`maximum` and `minimum` are `foldl1` with `max` and `min`, so an empty list is the same mistake:

```funl
write( maximum([]) )
```

```error
no clause of 'foldl1' matches
```

## Transforming

`map` and `filter` take a list, a range, a set or an array and answer a list. `concat` joins what
each element of a list holds, `concatMap` maps and then joins, `nub` drops the repeats and `group`
gathers equal neighbours:

```funl
write( map(x -> x + 1, 1..3), filter(odd, 1..10), map(abs, [-1, 2]) )
write( concat([[1], [2, 3], []]), concatMap(x -> [x, x], [1, 2]) )
write( nub([1, 2, 1, 3, 2]), group([1, 1, 2, 3, 3]) )
```

```output
[2, 3, 4], [1, 3, 5, 7, 9], [1, 2]
[1, 2, 3], [1, 1, 2, 2]
[1, 2, 3], [[1, 1], [2], [3, 3]]
```

**`map` is also the builtin that makes a mutable map.** With a function as the first of two
arguments it maps; with anything else it is the constructor, as it always was:

```funl
m = map([("a", 1)], 0)

write( m("a"), m("b"), map(x -> -x, [1, 2]) )
```

```output
1, 0, [-1, -2]
```

`sort` orders by `<`, numbers by value and strings by their bytes. `sortBy(lt, xs)` orders by a
function that says whether its first argument goes before its second. Both keep equal elements in
the order they came in:

```funl
write( sort([3, 1, 2]), sort(["pear", "fig"]), sort([2, 1.0, 1]) )
write( sortBy((a, b) -> a > b, [3, 1, 2]), sortBy((a, b) -> a.length < b.length, ["ccc", "a", "bb", "b"]) )
```

```output
[1, 2, 3], ["fig", "pear"], [1.0, 1, 2]
[3, 2, 1], ["a", "b", "bb", "ccc"]
```

What `<` cannot compare, `sort` cannot order:

```funl
write( sort([1, "a"]) )
```

```error
'<' cannot compare the string 'a' with the integer 1
```

## Slicing

```funl
write( take(2, [5, 6, 7]), drop(2, [5, 6, 7]), take(9, [5]), drop(9, [5]), drop(1, 1..4) )
write( takeWhile(x -> x < 3, [1, 2, 3, 1]), dropWhile(x -> x < 3, [1, 2, 3, 1]) )
write( splitAt(1, [1, 2, 3]), span(x -> x < 2, [1, 2, 3]), spanNot(x -> x > 1, [1, 2, 3]) )
write( partition(odd, [1, 2, 3, 4]) )
```

```output
[5, 6], [7], [5], [], 2..4
[1, 2], [3, 1]
([1], [2, 3]), ([1], [2, 3]), ([1], [2, 3])
([1, 3], [2, 4])
```

`spanNot` is Haskell's `break`, which is a FunL keyword. `drop` and `replicate` count with an
integer:

```funl
write( drop("two", [1, 2, 3]) )
```

```error
'drop' wants an integer count and was given the string 'two'
```

## Combining

`zip` pairs the elements in the same place, as many as the shorter list has; `zipWith` applies a
function to them instead, and `unzip` takes a list of pairs apart:

```funl
write( zip([1, 2, 3], ["a", "b"]), zip3([1, 2], [3, 4], [5, 6]) )
write( zipWith((a, b) -> a * b, [1, 2], 3..5), zipWith3((a, b, c) -> a + b + c, [1], [2], [3]) )
write( unzip([(1, "a"), (2, "b")]) )
```

```output
[(1, "a"), (2, "b")], [(1, 3, 5), (2, 4, 6)]
[3, 8], [6]
([1, 2], ["a", "b"])
```

An endless range beside a finite list is as long as the list, so `zip(0.., xs)` numbers it:

```funl
write( zip(0.., [#a, #b]), zip3(0.., 10.., [1, 2, 3]) )
write( zipWith((a, b) -> a * b, 1.., [5, 6, 7]) )
```

```output
[(0, a), (1, b)], [(0, 10, 1), (1, 11, 2), (2, 12, 3)]
[5, 12, 21]
```

With no finite input there is no end to stop at, so the call is refused:

```funl
write( zip(0.., 1..) )
```

```error
'zip' was given only endless ranges, such as 0..
```

**The list functions take lists.** A string, a tuple and a map are each one value, not a list of
parts, so `zip`, `zipWith`, `map`, `filter`, `take`, `drop`, `concat`, `reverse`, `sort` and the rest
refuse one rather than answer for a single element:

```funl
write( zip("ab", [1, 2]) )
```

```error
'zip' wants a list and reached the string 'ab'
```

```funl
write( map(x -> x, (1, 2)) )
```

```error
'map' wants a list and reached the tuple (1, 2)
```

```funl
write( take(1, {a: 1}) )
```

```error
'take' wants a list and reached {"a": 1}
```

## Producing, and endless inputs

`replicate(n, x)` is a list of `n` of `x`. `iterate(f, x)` generates `x`, `f(x)`, `f(f(x))` and on
for ever, and `forever(x)` generates `x` for ever (Haskell's `repeat`, also a keyword).

A generator cannot be passed as an argument, since a call's arguments backtrack: `f(g())` calls `f`
once per value `g` gives. **`map`, `filter`, `take` and `takeWhile` take a thunk instead** — a
function of no arguments, whose values they draw — and then generate their own answers, stopping as
soon as they have what they need. A list of them is a comprehension:

```funl
write( replicate(3, "ha") )
write( [x | x <- take(5, () -> iterate(x -> x * 2, 1))] )
write( [x | x <- take(3, () -> forever("again"))] )
write( [x | x <- takeWhile(x -> x < 50, () -> map(x -> x * x, () -> 1..))] )
```

```output
["ha", "ha", "ha"]
[1, 2, 4, 8, 16]
["again", "again", "again"]
[1, 4, 9, 16, 25, 36, 49]
```

## Asking

`any` and `all` succeed or fail, as a comparison does, so they are tested with `if`. `elem(x, xs)`
is `x in xs`, and `lookup(k, pairs)` answers the value paired with `k`, or fails when there is none,
so `|` supplies a default:

```funl
pairs = [(1, "one"), (2, "two")]

write( if any(even, [1, 2]) then "some even" else "none even" )
write( if all(odd, [1, 2]) then "all odd" else "not all odd" )
write( if elem(2, [1, 2]) then "has 2" else "no 2" )
write( lookup(2, pairs), lookup(3, pairs) | "unknown" )
```

```output
some even
not all odd
has 2
two, unknown
```

## From Prolog

A Prolog file loaded by FunL reaches the prelude as it reaches any built-in module: with
`:- import("funl:prelude").`, calling each function qualified, `prelude:f/(n+1)`, its last argument
unified with the function's value. It takes no name out of Prolog's own namespace, where `reverse/2`
and `last/2` are Prolog's list predicates:

```prolog
:- import("funl:prelude").

:- prelude:reverse([1, 2, 3], X), write(X), nl.
:- prelude:sort([3, 1, 2], X), write(X), nl.
:- findall(Y, prelude:take(2, [5, 6, 7], Y), Ys), write(Ys), nl.
:- reverse([1, 2, 3], Z), write(Z), nl.
```

```output
[3,2,1]
[1,2,3]
[[5,6]]
[3,2,1]
```
