---
title: String functions
weight: 75
---

# String functions

FunL takes a string apart in two ways. [String scanning](scanning.md) walks a position through it,
backtracking as it goes. The functions on this page do the everyday jobs in one call: cut a string
into pieces, put pieces together, take spaces off, change case, replace text, and test how a string
begins, ends or what it holds.

A string is Unicode text, and these functions count, compare and change it by **characters**, not
bytes. Every argument a function names as a string must be one; anything else is an error that names
what was given.

## Slices

A string called with a range gives its **slice**, counted by characters from 0, with every form of
range [Data](data.md#slices) shows. A slice reaching past either end fails.

```funl
s = 'naïve café'
write( s(0..4), s(6..), s(2..+3), s(4..0 by -1) )
write( s(6..20) | 'too far' )
```

```output
naïve, café, ïve, evïan
too far
```

## `split`

`split(s)` cuts `s` into its **words**: the runs of characters between spaces. Spaces at either end
make no empty word, a run of spaces cuts once, and a blank string has no words at all. A space here
is any Unicode white space, the tab and the line breaks among them.

```funl
write( split('  the quick  brown fox ') )
write( split('one\ttwo\nthree') )
write( split('   ') )
```

```output
["the", "quick", "brown", "fox"]
["one", "two", "three"]
[]
```

`split(s, sep)` cuts `s` at every place the string `sep` stands in it, left to right. Every piece
is kept, the empty ones too, so the pieces put back together with `sep` between them are `s` again.

```funl
write( split('a,b,,c', ',') )
write( split('a::b::c', '::') )
write( split(',a,', ',') )
write( split('no commas', ',') )
```

```output
["a", "b", "", "c"]
["a", "b", "c"]
["", "a", ""]
["no commas"]
```

With a **cset** as `sep` (see [character sets](scanning.md)), `split` cuts at each character in the
set, so one call splits at several different separators:

```funl
write( split('x1y22z', digits) )
write( split('a, b; c', cset(',;')) )
```

```output
["x", "y", "", "z"]
["a", " b", " c"]
```

An empty separator would cut everywhere and nowhere, and is refused:

```funl
write( split('abc', '') )
```

```error
'split' was given an empty separator
```

## `join`

`join(c, sep)` puts the elements of `c` one after another with the string `sep` between each two.
`c` is a list, a range, a set, an array or a buffer; a tuple, a map, a string or a number is refused. `join(c)` puts nothing between them. An
element that is not a string is written as `write` would write it.

```funl
write( join(['a', 'b', 'c'], ', ') )
write( join(['a', 'b', 'c']) )
write( join(1..4, ' < ') )
write( join(split('  too   many    spaces '), ' ') )
```

```output
a, b, c
abc
1 < 2 < 3 < 4
too many spaces
```

The separator must be a string:

```funl
write( join(['a', 'b'], 0) )
```

```error
'join' wants a string and was given the integer 0
```

What is joined must be a collection of elements, not a single value:

```funl
write( join((1, 2)) )
```

```error
'join' wants a list and reached the tuple (1, 2)
```

## `trim`, `trim_start` and `trim_end`

`trim(s)` is `s` without the spaces at both of its ends; `trim_start(s)` takes them off the start
only, and `trim_end(s)` off the end only. Spaces inside the string stay.

```funl
s = '  two  words  '
write( '[' + trim(s) + ']' )
write( '[' + trim_start(s) + ']' )
write( '[' + trim_end(s) + ']' )
write( '[' + trim('\t\n line \r\n') + ']' )
```

```output
[two  words]
[two  words  ]
[  two  words]
[line]
```

## `upper` and `lower`

`upper(s)` is `s` with every letter in upper case, and `lower(s)` in lower case. The mapping is
Unicode's **simple** case mapping: every character becomes exactly one character, so the result has
as many characters as `s`. That covers accented letters and the other scripts that have case, but it
leaves out the few mappings that change a string's length: `ß` has no single-character upper case,
so `upper` leaves it as it is, never making it `SS`.

```funl
write( upper('héllo wörld') )
write( lower('ÀÉÎ ABC 123') )
write( upper('straße'), upper('жук') )
```

```output
HÉLLO WÖRLD
àéî abc 123
STRAßE, ЖУК
```

## `replace`

`replace(s, old, new)` is `s` with every place `old` stands in it made `new`. The places are found
left to right, and one that has been replaced is not looked inside again.

```funl
write( replace('a-b-c', '-', ' + ') )
write( replace('aaaa', 'aa', 'b') )
write( replace('café', 'é', 'e') )
write( replace('nothing here', 'x', 'y') )
```

```output
a + b + c
bb
cafe
nothing here
```

The string to replace may not be empty:

```funl
write( replace('abc', '', '-') )
```

```error
'replace' was given an empty string to replace
```

## `starts_with`, `ends_with` and `contains`

These test a string, the way a comparison or `odd` tests a number: **`starts_with(s, p)` produces `s`
when `s` begins with `p`, and fails when it does not**. `ends_with(s, p)` asks whether `s` ends with
`p`, and `contains(s, p)` whether `p` stands anywhere in it. Every string begins with, ends with and
holds the empty string.

```funl
write( starts_with('hello', 'he') )
write( starts_with('hello', 'lo') )
write( ends_with('hello', 'lo') )
write( contains('hello', 'ell') )
write( contains('hello', 'z') )
write( contains('hello', '') )
```

```output
hello
hello
hello
hello
```

Because a test fails rather than producing `false`, it is a condition as it stands, and a filter in
a comprehension:

```funl
file = 'notes.txt'
if ends_with(file, '.txt') then write( 'text' ) else write( 'other' )

write( [w | w <- split('one two three four five') if ends_with(w, 'e')] )
write( [w | w <- split('apple banana cherry') if not contains(w, 'n')] )
```

```output
text
["one", "three", "five"]
["apple", "cherry"]
```

The test's arguments must both be strings:

```funl
write( starts_with(5, '5') )
```

```error
'starts_with' wants a string and was given the integer 5
```

## Where a string stands: `find`

`contains` says whether `p` stands in `s`; scanning's `find(p, s)` says **where**. It is a generator
of the positions at which `p` begins, counted from 1, so `every` takes them all and a search takes the
first that suits. With no second argument `find` searches the subject of a scan instead — see [string
scanning](scanning.md).

```funl
every write( find('an', 'banana') )
write( 3 < find('an', 'banana') )
write( find('x', 'banana') | 'not there' )
```

```output
2
4
4
not there
```

## `bytes` and `decode`

`bytes(s)` is the UTF-8 encoding of `s` as a [byte string](data.md#bytes), and `decode(b)` reads a
byte string as UTF-8 text. A character outside ASCII takes more than one byte, so the two lengths
can differ.

```funl
b = bytes('héllo')
write( b, b.length, 'héllo'.length )
write( decode(b), decode(b(0..0)) )
```

```output
bytes([104, 195, 169, 108, 108, 111]), 6, 5
héllo, h
```

On bytes that are not UTF-8 -- here a slice that cuts `é` in half -- `decode` fails, so a program
tests for text that is not well-formed with `if` or `|`. Anything but a byte string is an error.

```funl
write( decode(bytes('héllo')(0..1)) | 'not text' )
```

```output
not text
```

```funl
write( decode('héllo') )
```

```error
'decode' wants a byte string and was given the string 'héllo'
```

## Putting them together

Counting the words of a line, whatever its case and spacing:

```funl
line = '  The cat and the hat  and THE bat '
val counts = map( {}, 0 )

every counts( !split(lower(line)) ) += 1
write( counts )
write( join([upper(w) | w <- split(line) if starts_with(lower(w), 'th')], ' ') )
```

```output
MutableMap("the": 3, "cat": 1, "and": 2, "hat": 1, "bat": 1)
THE THE THE
```
