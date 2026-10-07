---
title: Regular expressions
weight: 30
---

# Regular expressions

A regular expression is written between backticks, `` `a(b|c)*` ``, or built from the pattern
combinators. Either way it is a **matcher inside a scan**: inside `s ? e` it matches the subject at
the current position, produces the text it matched, and moves the position past it. When it cannot
match there, it fails.

## Matching at the position

A pattern matches where the scan position is, not somewhere further on. Its value is the text it
matched, and the position has moved past that text, so `tab(0)` afterwards gives what is left:

```funl
'abc123' ? (write(`[a-z]+`) & write(tab(0)))
write('abc123' ? `[0-9]+` | 'not at the start')
```

```output
abc
123
not at the start
```

To match further on, move the position there first — here with `tab(upto(digits))`, which moves
to the first digit:

```funl
write('price 42' ? (tab(upto(digits)) & `\d+`))
```

```output
42
```

A pattern used where no scan is running is a fault:

```funl
write(`a`)
```

```error
a regex matches the subject of a scan `s ? e`, and no scan is running
```

## A pattern backtracks with the program

A pattern is part of the expression around it. If something after it fails, the pattern is asked for
its **next** way of matching, in order, and the position moves back with it. Alternatives are tried
left to right and the first that lets the whole expression succeed is the one taken — leftmost-first
backtracking, as in JavaScript, not the longest match:

```funl
write('xyz' ? `x|xy|xyz`)
every write('abc' ? `a|ab|abc`)
```

```output
x
a
ab
abc
```

Here the pattern first matches `abcd` as `a` then `bcd`, and `tab(0)` gives the empty rest. `every`
asks for another result, and the pattern's next way of matching is `ab` then `c`, leaving `d`:

```funl
every 'abcd' ? (write(`(a|ab)(c|bcd)`) & write(tab(0)))
```

```output
abcd

abc
d
```

## Characters and classes

An ordinary character matches itself. A backslash before a character that is not a letter or a digit
makes it literal: `\.`, `\[`, `\\`. `\Q...\E` makes everything between literal.

| escape | matches |
|---|---|
| `\t` `\n` `\r` `\f` `\e` `\a` | tab, newline, carriage return, form feed, escape, bell |
| `\xhh`, `\x{h...}` | the character with that code in hexadecimal |
| `\0oo` | the character with that code in octal |
| `\cX` | control-X |
| `\d` `\w` `\s` | an ASCII digit; an ASCII letter, digit or `_`; a space, tab, newline, carriage return, form feed or vertical tab |
| `\D` `\W` `\S` | any character the lower-case form does not match |
| `.` | any character but a newline (any character at all under `s`) |

```funl
write('a.b' ? `a\.b`)
write('a+b' ? `\Qa+b\E`)
write('AB' ? `\x41\x{42}`)
write('x7_ ' ? `\w\d\w\s`)
write('a\nb' ? `a.b` | 'a dot stops at a newline')
```

```output
a.b
a+b
AB
x7_ 
a dot stops at a newline
```

A **character class** `[...]` matches one character from a set: characters, ranges `a-z`, the
escapes `\d` `\w` `\s` and their complements, and POSIX classes `[:alpha:]`, `[:^alpha:]`. A `^`
first negates it; a `]` first, or a `-` first or last, is literal. The POSIX classes are `alpha`,
`digit`, `alnum`, `word`, `upper`, `lower`, `space`, `blank`, `punct`, `print`, `graph`, `cntrl`,
`xdigit` and `ascii`, each over ASCII. A range may run past ASCII.

```funl
write('x-_]' ? `[a-z][-0-9][\w][]]`)
write('a1 ' ? `[[:alpha:]][[:^alpha:]][\s]`)
write('é' ? `[à-ÿ]`)
write('Q' ? `[^a-z\d]`)
```

```output
x-_]
a1 
é
Q
```

Positions count characters, not bytes, so `.` steps over one character however it is encoded:

```funl
write('ñaña' ? `(ña)+`)
write('añb' ? (`a.` & tab(0)))
```

```output
ñaña
b
```

## Repetition

| quantifier | repeats | greedy | lazy | possessive |
|---|---|---|---|---|
| `*` | any number of times | `p*` | `p*?` | `p*+` |
| `+` | at least once | `p+` | `p+?` | `p++` |
| `?` | at most once | `p?` | `p??` | `p?+` |
| `{n}` | exactly `n` times | `p{n}` | | |
| `{n,}` | at least `n` times | `p{n,}` | `p{n,}?` | `p{n,}+` |
| `{n,m}` | from `n` to `m` times | `p{n,m}` | `p{n,m}?` | `p{n,m}+` |

A **greedy** repetition takes as many turns as it can first, and gives one back each time it is
asked for another way of matching. A **lazy** one takes as few as it can first, and takes one more
each time. A **possessive** one takes as many as it can and gives nothing back.

```funl
every write('aaa' ? `a{1,3}`)
every write('aaa' ? `a{1,3}?`)
write('aaa' ? `a+a`)
write('aaa' ? `a++a` | 'possessive gives nothing back')
```

```output
aaa
aa
a
a
aa
aaa
aaa
possessive gives nothing back
```

A brace that does not begin a count is an ordinary character:

```funl
write('aaaa' ? `a{2}`)
write('aaaa' ? `a{2,}`)
write('a{2' ? `a{2`)
```

```output
aa
aaaa
a{2
```

## Groups, captures and backreferences

| group | does |
|---|---|
| `(p)` | a capture group, numbered by its `(` counting from 1 |
| `(?<name>p)` | a capture group with a name, numbered like the others |
| `(?:p)` | groups without capturing |
| `(?>p)` | an atomic group: `p` keeps the first way it matches |
| `(?#...)` | a comment, matching nothing |

A **backreference** matches the text its group captured: `\1`, `\2` and so on, `\g1`, `\g{1}`, `\g{-1}` for the group opened last before it, and `\k<name>`,
`\k{name}`, `\k'name'` or `\g{name}` by name. Under `i` it ignores case. **A backreference to a group
that has captured nothing fails.**

```funl
write('abab' ? `(ab)\1`)
write('xyxy' ? `(?<pair>xy)\k<pair>`)
write('aAa' ? `(?i)(a)\1\g{-1}`)
write('b' ? `(a)?b\1` | 'the group captured nothing')
```

```output
abab
xyxy
aAa
the group captured nothing
```

An **atomic group** commits to the first way its contents match; nothing after it can make it try
another:

```funl
write('aab' ? `(?:a|aa)b`)
write('aab' ? `(?>a|aa)b` | 'the atomic group kept a')
```

```output
aab
the atomic group kept a
```

A backreference to a group the pattern does not have is refused before the program runs:

```funl
'a' ? `(a)\2`
```

```error
this refers to group 2, and the pattern has only 1
```

## Anchors

| anchor | holds |
|---|---|
| `^` | at the start of the subject (under `m`, also just after a newline) |
| `$` | at the end of the subject or just before a newline that ends it (under `m`, also just before any newline) |
| `\A` | at the start of the subject |
| `\z` | at the end of the subject |
| `\Z` | at the end of the subject or just before a newline that ends it |
| `\b` | between a word character and a non-word character, or at an end next to a word character |
| `\B` | wherever `\b` does not hold |

Anchors hold at a place in the **subject**, not at the place the scan started:

```funl
write('ab' ? (tab(2) & `^b`) | 'not at the start')
write('ab\n' ? `ab$`)
write('ab\n' ? `ab\z` | 'not the very end')
write('a b' ? `a\b \bb`)
write('ab' ? `a\b` | 'no boundary')
```

```output
not at the start
ab
not the very end
a b
no boundary
```

## Flags

| flag | effect |
|---|---|
| `i` | ignore case, for letters beyond ASCII too |
| `m` | multiline: `^` and `$` hold at each line |
| `s` | `.` matches a newline |
| `x` | extended: whitespace is ignored and `#` starts a comment to the end of the line; `\ ` is a space |

`(?flags)` turns flags on for the rest of the group it is written in, and `(?flags:p)` for `p`
alone. A `-` turns the flags after it off: `(?i-s)`.

```funl
write('HeLLo' ? `(?i)hello`)
write('HeLLo' ? `(?i:h)ELLO` | 'case is back')
write('HeLLO' ? `(?i)h(?-i)eLLO`)
write('é' ? `(?i)É`)
write('a\nb' ? `(?s)a.b`)
write('x\ny' ? `(?m)x$\n^y`)
write('aaab' ? `(?x) a +  b # the a's, then b`)
```

```output
HeLLo
case is back
HeLLO
é
a
b
x
y
aaab
```

An unknown flag is refused where it is written:

```funl
'a' ? `(?iq)a`
```

```error
`q` is not a flag
```

## Lookahead

`(?=p)` holds where `p` matches next, and `(?!p)` where it does not. Neither moves the position,
and each keeps the first way `p` matches.

```funl
'price: 10' ? (write(`\w+(?=:)`) & write(tab(0)))
write('abc' ? `(?!b)\w` | 'no')
write('bc' ? `(?!b)\w` | 'no')
```

```output
price
: 10
a
no
```

## Lookbehind

`(?<=p)` holds where `p` matches **ending** at the position, and `(?<!p)` where it does not. Neither
moves the position, and each keeps the first way `p` matches.

**A lookbehind is matched right to left from the position, as in JavaScript.** It is not matched by
trying start points behind the position, so `p` may be any pattern at all: unbounded repetition,
alternatives of different lengths, backreferences, other lookarounds.

```funl
write('cost:   1500' ? (tab(upto(digits)) & `(?<=:\s*)\d+`))
write('total 1500' ? (tab(upto(digits)) & `(?<=:\s*)\d+`) | 'no colon before it')
write('ab' ? (tab(2) & `(?<!a)b`) | 'an a comes before')
```

```output
1500
no colon before it
an a comes before
```

Because the body is read right to left, everything inside it resolves the way a right-to-left match
does:

- **alternatives are tried in order, nearest the position first**, so `(?<=(a|aa))` takes `a` where
  both would fit;
- **a repetition counts its turns outward from the position**, so a repeated group keeps the turn
  furthest to the left;
- **a group to the right is matched before a backreference to its left**, so in `(?<=\1(\w))` the
  backreference sees what the group captured;
- **an atomic group is entered from its right end**.

Here the group inside the first lookbehind captured `a`, not `aa`, so `\1` after it matches one
`a`; and the repeated group in the second kept its turn furthest left, `a`, not `b`:

```funl
'aabaa' ? (tab(3) & write(`(?<=(a|aa))b\1`))
'abcab' ? (tab(3) & write(`(?<=(a|b){2})c\1`))
```

```output
ba
ca
```

And here the group on the right is matched first, so the backreference on its left can use it:

```funl
write('aax' ? (tab(3) & `(?<=\1(\w))x`))
write('abx' ? (tab(3) & `(?<=\1(\w))x`) | 'b is not a twice')
```

```output
x
b is not a twice
```

## The pattern combinators

A pattern can also be built with calls, which make the same patterns a literal does. They take a
value the program computes, where a literal can only say what is written in it:

| combinator | matches |
|---|---|
| `string(s)` | the string `s` |
| `ccls(c)` | one character of the cset or string `c` |
| `rep(p)` | `p` any number of times, greedily — `p*` |
| `rep1(p)` | `p` at least once, greedily — `p+` |
| `opt(p)` | `p` at most once, greedily — `p?` |
| `repn(n, p)` | `p` exactly `n` times — `p{n}` |
| `rrep(p)`, `rrep1(p)`, `ropt(p)` | the lazy forms — `p*?`, `p+?`, `p??` |

Each `p` is a regex literal or another combinator call.

```funl
val sep = ', '

'123cc' ? (write(rep(ccls(digits))) & write(rep(string('c'))))
write('a, b' ? (`a` & string(sep) & `b`))
write('aeixab' ? rep1(ccls('aeiou')))
write('b' ? rep1(string('a')) | 'rep1 wants one')
write('aaaa' ? repn(3, `a`))
every write('aaa' ? rrep1(string('a')))
```

```output
123
cc
b
aei
rep1 wants one
aaa
a
aa
aaa
```

What a combinator repeats has to be a pattern, and `repn`'s count is written in the program as an
integer, because the pattern is built when the program is compiled:

```funl
'a' ? rep('a')
```

```error
`rep` repeats a pattern, and this is not one
```

```funl
val n = 2
'a' ? repn(n, `a`)
```

```error
`repn` takes its count as an integer written in the program
```

A program that defines a function with a combinator's name calls its own:

```funl
def rep( x ) = x + 1

write( rep(1) )
```

```output
2
```

## A malformed pattern is refused before the program runs

The complaint points into the pattern, at the part that is wrong:

```funl
'a' ? `ab(c`
```

```error
this group is not closed
```

```funl
'a' ? `a|*b`
```

```error
this quantifier has nothing to repeat
```

```funl
'a' ? `[z-a]`
```

```error
this range runs backwards
```
