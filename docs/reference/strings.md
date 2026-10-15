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

A second argument, a cset or a string of characters, names the characters to take off in place of
spaces; `trim_end('xxhixx', 'x')` is `'xxhi'`.

```funl
write( trim('-+hi+-', '+-') )
write( trim_start('xxhixx', cset('x')) )
write( trim_end('xxhixx', 'x') )
```

```output
hi
hixx
xxhi
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

### Assigning to a section

A string variable's section can be assigned: `s(i..j) = t` stores in `s` a new string, `s` with the
characters the range names replaced by the string `t`, of any length. The range runs forwards one
character at a time, and lies inside the string or starts at its end, so `s(n..<n) = t` inserts at `n`.
`s(i..j) <- t` is the same store, put back when the expression is backtracked over. The string itself
is never changed: another name holding it still sees the old text.

```funl
var s = 'hello world'
t = s
s(0..4) = 'HOWDY'
write( s, t )
s(5..<5) = ','
s(7..) = 'there'
write( s )
write( ((s(0..0) <- 'J') & 1 > 2) | s )
```

```output
HOWDY world, hello world
HOWDY, there
HOWDY, there
```

A section outside the string, one that steps, a replacement that is not a string, and a `val` are
refused:

```funl
var s = 'abc'
s(1..5) = 'x'
```

```error
the section is outside the string 'abc', which has 3 characters
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

## `translate`

`translate(s, from, to)` is `s` with each character that stands in `from` made the character at the
same place in `to`: Icon's `map(s1, s2, s3)`. `from` and `to` hold as many characters as each other. Where
`from` names a character twice, the later place wins.

```funl
write( translate('hello', 'el', 'ip') )
write( translate('abc', 'aa', 'xy') )
write( translate('hello world', 'abcdefghijklmnopqrstuvwxyz', 'nopqrstuvwxyzabcdefghijklm') )
```

```output
hippo
ybc
uryyb jbeyq
```

Strings of different lengths are refused:

```funl
write( translate('abc', 'ab', 'x') )
```

```error
'translate' was given strings of different lengths
```

## `reverse` of a string

`reverse(s)` of a string is the string with its characters in the other order. It is the one list
function that answers a string; the others still refuse it.

```funl
write( reverse('naïve') )
write( reverse([1, 2, 3]) )
```

```output
evïan
[3, 2, 1]
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

## Fitting, repeating and converting

`repl(s, n)` writes `s` `n` times over. `left(s, n, p)`, `center(s, n, p)` and `right(s, n, p)` put
`s` in a field `n` characters wide, the rest made of the pad string `p` (a space when it is left
out), and cut `s` from the side the padding would go on when it is too long. A value that is not a
string is written as `write` would first.

**`left` replicates the pad from the right end of the field and `right` from the left end**, so
the pad is whole at the outer edge; `center` pads each side from its own outer edge, the extra
column going to the right, and cuts the same number from both ends.

```funl
write( repl('ab', 3) )
write( '[' + left('abc', 6) + ']' )
write( right('abc', 10, 'xy') )
write( left('abc', 10, 'xy') )
write( center('ab', 7, '*') )
write( right(42, 6, '0') )
write( left('abcdef', 3), right('abcdef', 3), center('abcde', 2) )
```

```output
ababab
[abc   ]
xyxyxyxabc
abcyxyxyxy
**ab***
000042
abc, def, bc
```

An empty pad is refused:

```funl
write( left('a', 3, '') )
```

```error
'left' was given an empty pad
```

`char(i)` is the one-character string whose Unicode code point is `i`, and `ord(s)` the code
point of the one character of `s`.

```funl
write( char(955), ord('A'), ord(char(128512)) )
```

```output
λ, 65, 128512
```

```funl
write( ord('ab') )
```

```error
'ord' wants a string of one character, and was given one of 2
```

`detab(s, i…)` replaces each tab with the spaces that reach the next tab stop, and `entab(s, i…)`
replaces spaces that reach a tab stop with a tab, except a single blank, which stays a blank (as in
Icon). Stops are columns counted from 1: every 8 columns
(9, 17, …) when none is given. With stops given, the last interval repeats past the last one, and
the first interval runs from column 1. A line break starts a new line at column 1. `image` shows
the result below with its tabs spelled out.

```funl
write( image(detab('a\tb')) )
write( image(entab('a       b')) )
write( image(detab('ab\tc', 5)) )
```

```output
"a       b"
"a\tb"
"ab  c"
```

```funl
write( detab('a', 5, 5) )
```

```error
'detab' wants tab stops in increasing order from column 2, and 5 is not past 5
```

`str(x)` is any value as a string: a string is itself, anything else as `write` shows it.
`image(x)` is the value as source text: a string is quoted and escaped, a cset is `cset("…")`,
and a `$` is doubled as a double-quoted string needs. `type(x)` is the name `is` uses for the kind
of `x`, the most specific one, and a record's constructor name for a record.

```funl
data shape = circle(r)
write( str(12) + str(1.5) )
write( image('say "hi"') )
write( image(cset('cab')) )
write( image(['a', 2]) )
write( type(1), type(true), type('s'), type([1]), type(circle(2)) )
```

```output
121.5
"say \"hi\""
cset("abc")
["a", 2]
integer, boolean, string, list, circle
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
