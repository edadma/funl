---
title: String scanning
weight: 70
---

# String scanning

`s ? e` **scans** the string `s`: it evaluates `e` with `s` as the **subject** and a **position** at
its start, and its value is `e`'s. Inside `e`, a family of builtins read the subject and move the
position, and every movement is undone when the program backtracks past it.

## Positions

A position falls between two characters. Position `1` is before the first character, and position
`0` is after the last; a negative position counts back from the end. A scan starts at `1`.

A position is not an element index. An index names a character and counts from `0`, so `s(0)` is
the first character; a position names the gap before one, so the first character is the text
between positions `1` and `2`:

```funl
val s = 'hello'
write( s(0), s ? tab(2) )
write( s(1), s ? (tab(2) & tab(3)) )
write( s(4), s ? (tab(-1) & tab(0)) )
```

```output
h, h
e, e
o, o
```

`move(n)` moves `n` characters and produces the text it passed over. `tab(i)` moves to position `i`
and produces the text between. Each fails when the position it names is outside the subject.

```funl
'hello world' ? write( move(5) )
'hello world' ? write( tab(3) )
'hello world' ? (tab(3) & write( tab(0) ))
'hello world' ? write( tab(-1) )
'abc' ? write( move(10) )
write( 'abc' ? move(2) )
```

```output
hello
he
llo world
hello worl
ab
```

`pos(i)` succeeds, producing the position, when the scan is at position `i`, and fails otherwise. A
scan whose expression fails fails.

```funl
'abc' ? write( pos(1) )
'abc' ? write( pos(0) )
'abc' ? (move(1) & write( pos(2) ))
write( 'abc' ? fail )
write( 'done' )
```

```output
1
2
done
```

## Looking ahead

Several builtins look at the subject from the position without moving, and produce a position for
`tab` to move to. A **character set** argument is a cset, such as `digits` or `letters`, or a string
standing for the set of its characters.

| builtin | produces |
|---|---|
| `upto(c)` | each position at which a character of `c` occurs |
| `many(c)` | the position after the longest run of characters in `c`; fails if there is none |
| `any(c)` | the position after the next character, if it is in `c` |
| `match(s)` | the position after `s`, if the subject continues with `s` |
| `find(s)` | each position at which `s` occurs |
| `bal(c1, c2, c3, s, i, j)` | each position of `c1` balanced in `c2`, `c3` (see below) |

```funl
'hello world' ? write( upto('o') )
'hello world' ? write( many('hel') )
'hello world' ? write( any('h') )
'hello world' ? write( any('e') )
'hello world' ? write( match('hell') )
'hello world' ? write( match('world') )
'hello world' ? write( find('wor') )
'2026-10-07' ? write( tab(many(digits)) )
write( digits )
```

```output
5
5
2
5
7
2026
cset("0123456789")
```

`upto` and `find` are generators: asked again, they produce the next position.

```funl
'hello world' ? every write( upto('o') )
'a,b;c' ? every write( upto(',;') )
'banana' ? every write( find('an') )
```

```output
5
8
2
4
2
4
```

Together they take a string apart. This writes each word, stepping over the spaces between:

```funl
text = 'the quick  brown fox'

text ?
  while write( tab(upto(' ')) )
    tab( many(' ') )

  write( tab(0) )
```

```output
the
quick
brown
fox
```

### `=s`: tab past a string

`=s` is `tab(match(s))`: if the subject continues with `s` it moves past it and produces `s`, and it
fails otherwise. Like `tab`, it is undone when the program backtracks past it.

```funl
'key=value' ? (='key' & ='=' & write( tab(0) ))
'key=value' ? (if ='val' then write( 'yes' ) else write( 'no' ))
```

```output
value
no
```

### `bal`: balanced positions

`bal(c1, c2, c3, s, i, j)` generates each position in `s[i:j]` at which a character of `c1` stands
with the text before it, from `i`, balanced in the openers `c2` and the closers `c3`. The search
stops at a closer that has no opener. Every argument may be left out (or be `undefined`): `c1` is
then every character, `c2` is `(`, `c3` is `)`, `s` is the subject with `i` the scan position, and `j`
the end.

```funl
'f(a,b),g,h' ? every write( bal(',') )
'f(a,b),g,h' ? write( tab(bal(',')) )
every write( bal(';', '[', ']', 'a[;]b;c') )
```

```output
7
9
f(a,b)
6
```

## Character sets

A **cset** is a set of characters: `cset(s)` makes one of the characters of string `s`, and five are
named.

| name | holds |
|---|---|
| `letters` | `A`–`Z` and `a`–`z` |
| `digits` | `0`–`9` |
| `alphanum` | the letters and the digits |
| `ascii` | the 128 ASCII characters |
| `cset_all` | every character |

`+` is union, `-` difference and `**` intersection, as they are on [sets](data.md#union-difference-and-intersection),
with a cset or a string on the right. `x in c` holds when `x` is a one-character string in `c`.
`c.length` is how many characters `c` holds, and `!c` generates them in ascending order, each a
one-character string.

```funl
vowels = cset('aeiou')
write( vowels + 'y', letters ** 'a1b2', digits - '0123' )
write( cset('banana').length, [c | c <- cset('banana')] )
if 'e' in vowels then write( 'a vowel' )
```

```output
cset("aeiouy"), cset("ab"), cset("456789")
3, ["a", "b", "n"]
a vowel
```

`complement(c)` is every character not in `c`. A character is any Unicode one, so a complement is
kept as "every character but these", written `complement(cset(…))`, and the operators, `in` and
the scanning builtins all read it as that. It has no length and cannot be listed: `c.length` and
`!c` on one fault with `domain_error(finite, c)`.

```funl
others = complement(letters)
'hello, wörld' ? write( tab(upto(others)) )
write( 'é' in others, others ** '1a', others + 'a' - 'é' )
write( complement(others) == letters, cset_all == complement('') )
```

```output
hello
é, cset("1"), complement(cset("ABCDEFGHIJKLMNOPQRSTUVWXYZbcdefghijklmnopqrstuvwxyzé"))
cset("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"), complement(cset(""))
```

```funl
write( complement('a').length )
```

```error
the complement complement(cset("a")) has no length: it holds every character but the ones it names
```

## Backtracking puts the position back

When an expression fails and the program backtracks past a `tab` or a `move`, the position goes
back to where it was. That is what lets alternation try a second way of reading the subject from
the same place:

```funl
"asdf," ? tab(upto(',') + 1) & write( move(1) ) | write( tab(upto('.')) )
"asdf,zxvc." ? tab(upto(',') + 1) & write( move(1) ) | write( tab(upto('.')) )
"asdf." ? tab(upto(',') + 1) & write( move(1) ) | write( tab(upto('.')) )
```

```output
z
asdf
```

The first subject has nothing after its comma and no `.`, so it prints nothing. The third has no
comma, so the second alternative runs, from the start.

Here `tab(3)` moves, `pos(1)` then fails, and the second alternative starts again from position 1:

```funl
'abcdef' ? (tab(3) & pos(1) | write( move(1) ))
```

```output
a
```

## Leaving a scan

However control leaves `s ? e` — with its value, by failing, or by `return`, `break`, `continue` or
`yield` — the subject and position that were in force before it are in force again. A scan inside another one does not
disturb the outer one:

```funl
'outer' ? (write( 'inner' ? move(2) ) & write( move(2) ))

def first( s ) = s ? return move(1)

write( first('abc') )
'xyz' ? (first('abc') & write( move(1) ))
```

```output
in
ou
a
x
```

`break`, `continue` and `yield` leave a scan the same way. A `break` or `continue` in a loop body
leaves the scans inside the loop, and the loop's own subject and position are in force again:

```funl
'outer' ?
  for i <- 1..3
    'inner' ? (move(1) & (if i == 2 then continue))
    write( move(1) )
  write( tab(0) )

'outer' ?
  for i <- 1..3
    'inner' ? (move(1) & (if i == 2 then break))
    write( move(1) )
  write( tab(0) )
```

```output
o
u
ter
o
uter
```

A `yield` from inside a scan gives the caller its own scan back. Resuming the generator gives it the
scan again, at the position it had:

```funl
def g()
  'abc' ?
    move(1)
    yield move(1)
    yield tab(0)

'outer' ? (tab(2) & every write( g(), tab(0) ))
```

```output
b, uter
bc, uter
```

## `&subject` and `&pos`

The [keywords](keywords.md) `&subject` and `&pos` are the scan's subject and position. Outside
every scan they are `""` and `1`.

```funl
write( &subject == '', &pos )
'hello' ? (tab(3) & write( &subject, &pos ))
```

```output
, 1
hello, 3
```

Both can be assigned. `&pos = i` moves to position `i`, written as any position is — `0` the end,
a negative one counting back — and fails when `i` is outside the subject. `&subject = s` starts
scanning `s` at position 1; outside a scan it sets the subject the scanning builtins work on, and a
scan puts it back when it ends.

```funl
'abcdef' ?
    &pos = -2
    write( tab(0) )
    write( if (&pos = 9) then 'moved' else 'stayed', &pos )

&subject = 'outer'
write( tab(3) )
'inner' ? write( &subject )
write( &subject, &pos )
```

```output
ef
stayed, 7
ou
inner
outer, 3
```

`&pos = i` and `&subject = s` are ordinary assignments, which backtracking does not undo. **`&pos
<- i` is the reversible one**: failure passing back over it puts the position back, as it puts back
a `tab`.

```funl
'abcdef' ?
    tab(2)
    ((&pos <- 5) & fail) | write( 'failed back', &pos )
    ((&pos = 5) & fail) | write( 'kept', &pos )
```

```output
failed back, 2
kept, 5
```

An assignment that is not a position or not a string is a fault:

```funl
'abc' ? (&pos = 'x')
```

```error
'&pos' wants an integer and was given the string 'x'
```

## `?=`

`s ?= e` scans `s` and assigns the scan's value back to it. When the scan fails, `s` keeps its value.

```funl
line = '  indented'
line ?= (tab(many(' ')) & tab(0))
write( line )

word = 'xyz'
word ?= fail
write( word )
```

```output
indented
xyz
```

## Patterns

A **pattern** is a matcher inside a scan. It matches at the position — not further on — produces
the text it matched, and moves the position past it. A regex literal, written between backquotes,
is a pattern; [Regular expressions](regex.md) has their syntax.

```funl
'2026-10-07' ? (write( `[0-9]+` ) & move(1) & write( `[0-9]+` ))
'abc' ? write( `ab` )
'abc' ? write( `b` )
```

```output
2026
10
ab
```

**A pattern backtracks with everything around it.** When what follows it fails, the program asks
the pattern for its next way of matching, and the position goes back with it:

```funl
'aaab' ? (`a*` & match('ab') & write( 'matched' ))
'aaab' ? (write( `a*` ) & write( match('ab') ))
```

```output
matched
aaa
aa
5
```

A pattern is matched against a scan's subject, so one evaluated outside every scan is a fault:

```funl
write( `a+` )
```

```error
no scan is running
```

### Combinators

Patterns can also be built from **combinators**, which build the same patterns a regex literal
does:

| combinator | matches |
|---|---|
| `string(s)` | the text `s` |
| `ccls(c)` | one character in the character set `c` |
| `opt(p)` | `p` or nothing, preferring `p` |
| `rep(p)` | `p` any number of times, as many as it can |
| `rep1(p)` | `p` one or more times, as many as it can |
| `repn(n, p)` | `p` exactly `n` times |
| `ropt(p)`, `rrep(p)`, `rrep1(p)` | as `opt`, `rep` and `rep1`, but as few times as they can |

```funl
'123cc' ? write( rep(ccls(digits)) ) & write( rep(string('c')) )

if 'aaa12cc' ? tab(many('a')) & write( repn(2, ccls(digits)) ) & write( rep1(string('c')) )
  write( 'match' )

w = 'ab'
'ababc' ? write( rep(string(w)) )
```

```output
123
cc
12
cc
match
abab
```

The reluctant forms take the least they can, and more only when the program backtracks into them:

```funl
'xy' ? (write( opt(string('x')) ) & write( move(1) ))
'xy' ? (write( ropt(string('x')) ) & write( move(1) ))
'aaab' ? (write( rrep(string('a')) ) & write( tab(0) ))
```

```output
x
y

x

aaab
```

A combinator's argument that repeats must itself be a pattern, and a plain string is not one:

```funl
'aaa' ? write( rep('a') )
```

```error
`rep` repeats a pattern, and this is not one
```

A combinator's pattern is built when the program is compiled, so `repn`'s count is an integer
written in the program:

```funl
n = 2
'1234' ? write( repn(n, ccls(digits)) )
```

```error
`repn` takes its count as an integer written in the program
```
