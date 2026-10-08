---
title: Prolog
weight: 40
---

# Prolog

Standard Prolog runs on the same machine as FunL. A predicate written in a `.pl` file and a relation
written in a `.funl` file are the same kind of thing, so each language calls the other directly, and
a term passes between them unchanged.

Prolog is reached three ways: a FunL program imports or consults a `.pl` file; `funl file.pl` loads
one from the command line; and the separate `prolog` executable is a standard Prolog with an ISO top
level and no FunL in it.

The Prolog programs on this page are whole files. A program runs as it loads: each `:- Goal.`
directive is run in its place, and so is everything a directive writes.

## Loading Prolog from FunL

`import "file.pl"` loads a Prolog file when the FunL program is compiled. The path is read relative
to the file that imports it. The file's predicates are then **relations** in FunL, and are called
like any other: with `free` variables, under `every` for each solution, under `if` for the first.

The examples on this page load two small files kept beside it:
[`prolog/family.pl`](prolog/family.pl), a few `parent/2` facts and `grandparent/2`, and
[`prolog/shapes.funl`](prolog/shapes.funl), some FunL definitions.

```funl
import "prolog/family.pl"

free g
every grandparent(#tom, g) do write( g )

free p, c
if parent(p, #liz) then write( p )
write( findall(c, parent(#bob, c)) )
```

```output
ann
pat
tom
[ann, pat]
```

A Prolog atom is a FunL symbol, so `tom` in the file is `#tom` in FunL. A predicate is called with
exactly the arguments its clauses have, and any other number is refused before the program runs:

```funl
import "prolog/family.pl"

free x
if grandparent(x) then write( x )
```

```error
the Prolog predicate `grandparent` has no clauses of 1 argument
```

`consult(file)` loads a Prolog file while the program runs, and runs its directives then. Its path
is read relative to the working directory. What it loads is there for Prolog loaded after it; a
predicate FunL calls by name comes from an `import`, which the compiler can see.

```funl
write( #before )
consult( "nowhere.pl" )
write( #after )
```

```error
consult cannot read nowhere.pl
```

From the command line, `funl family.pl` loads a Prolog file, runs its directives, and then runs the
goal of each `:- initialization(Goal).` directive in it. A file with such a directive ends there; a
file without one then reads queries at the same top level the `prolog` executable has, below, with
FunL present.

## The `prolog` executable

`prolog` is standard Prolog alone: the Prolog reader, the Prolog builtins and an ISO top level, with
no FunL syntax, no FunL builtins and no FunL values. A Prolog program runs the same in it as through
`funl`.

`prolog family.pl` loads the file and then reads queries. An answer that leaves another to find
waits: `;` asks for the next, and anything else stops. An answer with nothing left to find ends in a
`.`, and a query with no answer says `false.`. An error no `catch/3` handles is printed, and the next
query is read. `halt.` ends the session with status 0 and `halt(N).` with status `N`; the end of the
input ends it with status 0.

Given these lines on its input:

```
parent(tom, X).
;
grandparent(tom, G).
;
;
X is 1 / 0.
halt.
```

`prolog family.pl` answers:

```
X = bob
X = liz.
G = ann
G = pat
false.
uncaught exception: error(evaluation_error(zero_divisor),_)
```

`consult('other.pl').` loads another file from the top level. `import/1`, which loads a FunL file
from Prolog, needs FunL, so in `prolog` it is an unknown procedure.

## Directives and `initialization`

A directive runs in its place as the file loads. `:- initialization(Goal).` instead runs `Goal` once
the whole file is loaded:

```prolog
main :- write('loaded, and now main'), nl.

:- initialization(main).
:- write('a directive runs as the file loads'), nl.
```

```output
a directive runs as the file loads
loaded, and now main
```

A directive that fails, or raises an error nothing catches, is reported with a warning naming it, and
the load goes on with the next:

```prolog
:- write(before), nl.
:- X is 1 / 0, write(X), nl.
:- fail.
:- write(after), nl.
```

```output
before
Warning: directive raised 1 / 0 divides by zero: X is 1 / 0, write(X), nl
Warning: directive failed: fail
after
```

## The reader

Tokens are ISO's. A variable begins with a capital letter or `_`. An atom is a name beginning with a
lower-case letter, a run of symbol characters, a quoted `'atom'`, or one of `[]`, `{}`, `!`, `;` and
`,`. Integers are unbounded, and may be written `0'c` (a character's code), `0x1F`, `0o17` or
`0b101`. `%` begins a comment to the end of the line, and `/* */` encloses one.

```prolog
:- X = 0'a, write(X), nl.
:- X = 0x1F, write(X), nl.
:- X = 0o17, write(X), nl.
:- X = 0b101, write(X), nl.
:- X = 1.5e3, write(X), nl.
:- X = 123456789012345678901234567890, write(X), nl.
:- X = 'hello world', writeq(X), nl.
:- X = "a string", (string(X) -> write(string) ; write(other)), nl.
:- X = 1 + 2 * 3, write_canonical(X), nl.
:- X = - 1, write_canonical(X), nl.
:- X = -1, write_canonical(X), nl.
% a comment to the end of the line
/* and a comment between delimiters */
```

```output
97
31
15
5
1500.0
123456789012345678901234567890
'hello world'
string
+(1,*(2,3))
-(1)
-1
```

A rational is written `NrM`, as `writeq/1` writes one, so what is written reads back as the same
number; `14r4` is `7r2`, and a denominator of 1 leaves an integer. Back-quoted text, `` `abc` ``, is a
list of character codes.

```prolog
:- X = 7r2, writeq(X), nl.
:- X = 14r4, writeq(X), nl.
:- X is 7r2 + 1r2, writeq(X), nl.
:- X = `abc`, writeq(X), nl.
```

```output
7r2
7r2
4
[97,98,99]
```

### Operators

`op(Priority, Type, Name)` adds an operator, or changes one, for the clauses read after it.
Priorities run from 0 to 1200, and the types are `xfx`, `xfy`, `yfx`, `fy`, `fx`, `xf` and `yf`. A
term written in canonical form, `+(1, 2)`, needs no operator at all.

```prolog
:- op(700, xfx, ===>).

rule(a ===> b).

:- rule(R), writeq(R), nl, write_canonical(R), nl.
:- op(200, xfy, ^^).
:- X = (a ^^ b ^^ c), write_canonical(X), nl.
```

```output
a===>b
===>(a,b)
^^(a,^^(b,c))
```

`current_op(Priority, Type, Name)` enumerates the table, `,` included, in every mode. An argument
that is bound must be of its kind: a priority from 0 to 1200 or an operator type, or it is a
`domain_error`, and a name that is not an atom is a `type_error`.

```prolog
:- current_op(P, T, mod), writeq(op(P, T, mod)), nl.
:- findall(T, current_op(_, T, -), Ts), msort(Ts, S), writeq(S), nl.
:- catch(current_op(1201, _, _), error(E, _), (writeq(E), nl)).
```

```output
op(400,yfx,mod)
[fy,yfx]
domain_error(operator_priority,1201)
```

### Double-quoted text

**A double-quoted `"text"` is a string**, the same value as a FunL string. The `double_quotes` flag
changes that for the clauses read after it: `codes` reads a list of character codes, `chars` a list
of one-character atoms, `atom` an atom, and `string` a string again.

```prolog
show :- X = "ab", writeq(X), nl.

:- show.
:- set_prolog_flag(double_quotes, codes).

codes :- X = "ab", writeq(X), nl.

:- set_prolog_flag(double_quotes, chars).

chars :- X = "ab", writeq(X), nl.

:- codes, chars, show.
```

```output
"ab"
[97,98]
[a,b]
"ab"
```

A file that cannot be read is refused before any of it runs, and the message says where:

```prolog
p(X) :- X = (1 + ).
```

```error
a term was expected here
```

### Reading terms

`read(T)` reads the next term from the input, up to the `.` that ends it, and `read_term(T, Options)`
does the same with the options `variable_names(Vs)`, `variables(Vs)` and `singletons(Vs)`. At the
`prolog` top level the input is the lines typed after the query; elsewhere it is standard input. At
the end of the input the term read is `end_of_file`.

`term_to_atom(T, A)` reads atom `A` as a term, its `.` optional, or, when `A` is unbound, writes `T`
as `writeq/1` does. Text that does not read raises `syntax_error(What)`, whose message says where
and what was expected there.

```prolog
:- term_to_atom(T, 'point(X, Y, X)'), T = point(1, 2, Z), writeq(T/Z), nl.
:- term_to_atom((p :- q, r), A), writeq(A), nl.
:- catch(term_to_atom(_, 'foo('), error(E, _), (writeq(E), nl)).
```

```output
point(1,2,1)/1
'p:-q,r'
syntax_error('a term was expected, and the input ended')
```

## Control

| construct | meaning |
|---|---|
| `A, B` | `A`, then `B` |
| `A ; B` | `A`, or else `B` |
| `C -> T ; E` | `T` if `C` succeeds (taking its first solution), else `E` |
| `C *-> T ; E` | `T` for every solution of `C`, or `E` if it has none |
| `\+ G`, `not(G)` | succeeds when `G` fails |
| `!` | commits the clause to the choices made so far |
| `call(G)`, `call(G, A1, …)` | runs `G`, with any extra arguments added, as a goal of its own |
| `once(G)`, `ignore(G)` | `G`'s first solution; `G` once, or succeed anyway |
| `forall(C, A)` | `A` holds for every solution of `C` |
| `catch(G, C, R)`, `throw(B)` | below, under errors |
| `true`, `fail`, `false` | succeed; fail; fail |

```prolog
color(red).
color(green).
color(blue).

first_color(C) :- color(C), !.

sign(X, S) :- ( X > 0 -> S = positive ; X < 0 -> S = negative ; S = zero ).

:- first_color(C), write(C), nl.
:- sign(5, A), sign(-2, B), sign(0, C), write([A, B, C]), nl.
:- forall(color(C), atom(C)), write('every color is an atom'), nl.
:- \+ color(pink), write('no pink'), nl.
:- findall(C, (color(C) *-> true ; C = none), L), write(L), nl.
:- findall(C, (color(pink) *-> C = yes ; C = none), L), write(L), nl.
:- once(color(C)), write(C), nl.
:- ignore(color(pink)), write(ignored), nl.
:- findall(X, between(1, 5, X), L), write(L), nl.
```

```output
red
[positive,negative,zero]
every color is an atom
no pink
[red,green,blue]
[none]
red
ignored
[1,2,3,4,5]
```

### Meta-calls

`call/1` runs a term as a goal, and `call/2` to `call/8` add arguments to it first. A goal may hold
control constructs. **A cut inside `call` cuts only the called goal**, while a cut written directly
in `,`, `;` or `->` cuts the clause it is in.

```prolog
color(red).
color(green).
color(blue).

:- findall(C, call((color(C), !)), L), write(L), nl.
:- findall(C, (color(C), call(!)), L), write(L), nl.
:- findall(C, (call(color, C), C \= green), L), write(L), nl.
:- G = (color(X), X \= red), findall(X, call(G), L), write(L), nl.
:- P = format("~w and ~w~n"), call(P, [salt, pepper]).
```

```output
[red]
[red,green,blue]
[red,blue]
[green,blue]
salt and pepper
```

### Halting

`halt` ends the program with status 0, and `halt(N)` with status `N`, from wherever the goal is: a
query, a directive, a clause body many calls deep, or a predicate a FunL program called. What the
program wrote before it is written out first, and nothing after it runs. **`catch/3` does not catch
a halt.** Here the program ends with status 1:

```prolog
check(X) :- X > 0, write(fine), nl.
check(_) :- write(stopping), nl, halt(1).

:- check(5).
:- catch(check(-2), _, write(caught)).
:- write(never), nl.
```

```output
fine
stopping
```

The status must be an integer:

```prolog
:- catch(halt(two), error(E, _), (write(E), nl)).
```

```output
type_error(integer,two)
```

## Dynamic predicates

`:- dynamic(Name/Arity).` declares a predicate whose clauses can be changed while the program runs.
`assertz/1` (and `assert/1`) adds a clause at the end, `asserta/1` at the beginning, `retract/1`
removes the first clause that unifies, and `retractall/1` every clause whose head unifies. A
predicate first given a clause by `assert` is dynamic too.

**A call sees the clauses as they were when it began**, so a loop that adds clauses to the predicate
it is walking does not walk the new ones.

```prolog
:- dynamic(counter/1).
counter(0).

bump :- retract(counter(N)), N1 is N + 1, assertz(counter(N1)).

:- bump, bump, counter(N), write(N), nl.

:- dynamic(item/1).
item(1).
item(2).

:- forall(item(X), (Y is X * 10, assertz(item(Y)))), findall(X, item(X), L), write(L), nl.
:- asserta(item(0)), findall(X, item(X), L), write(L), nl.
:- retract(item(10)), findall(X, item(X), L), write(L), nl.
:- retractall(item(_)), findall(X, item(X), L), write(L), nl.
:- assertz((double(X, Y) :- Y is X * 2)), double(4, D), write(D), nl.
:- clause(double(3, R), Body), Body = (R is E), write(E), nl, call(Body), write(R), nl.
```

```output
2
[1,2,10,20]
[0,1,2,10,20]
[0,1,2,20]
[]
8
3*2
6
```

A predicate a file defines without declaring it dynamic, and a builtin, cannot be changed. A dynamic
predicate with no clauses fails, while calling a predicate nothing defines is an error:

```prolog
fixed(1).

try(G) :- catch(G, error(E, _), (write(E), nl)).

:- try(assertz(fixed(2))).
:- try(assertz(atom_length(a, 1))).
:- try(retract(fixed(1))).
:- dynamic(empty/0).
:- (empty -> write(yes) ; write(no)), nl.
:- try(missing).
```

```output
permission_error(modify,static_procedure,fixed/1)
permission_error(modify,static_procedure,atom_length/2)
permission_error(modify,static_procedure,fixed/1)
no
existence_error(procedure,missing/0)
```

## Terms shared with FunL

**Most kinds of value are the same in both languages.** A Prolog atom is a FunL symbol, an integer is
an integer (unbounded in both), a float is a FunL real, a string is a string, a list is a list, and
a compound term is a compound term. A FunL `data` record is a compound, so `point(1, 2)` made in FunL
unifies with `point(X, Y)` in Prolog. A FunL rational is a number to Prolog, and arithmetic takes it
as it is.

The kinds only FunL has are these to Prolog:

- **a tuple** `(a, b)` is the compound `tuple(a, b)`, in a goal, in a clause head, and in a FunL
  tuple pattern that a Prolog caller passes `tuple(...)` to;
- **a map, an array, a buffer and a function** are opaque: each unifies only with itself and
  is written as `<map>`, `<function>` and so on;
- **`()` and `undefined`** are constants that unify only with themselves; neither is `[]` or an atom.

The FunL definitions in [`prolog/shapes.funl`](prolog/shapes.funl) are:

```
data point(x, y)

def sides(#square, 4)
def sides(#triangle, 3)

def corner() = point(1, 2)
def pair(a) = (a, a * 2)
def upto(n) = 1 to n
def inverse(n) = 1 / n
def ages() = {alice: 30}
def nothing() = ()
def unset() = undefined
def yes() = true
def no() = false
```

```prolog
:- import("prolog/shapes.funl").

:- forall(sides(S, N), (write(S-N), nl)).
:- findall(K, upto(4, K), L), write(L), nl.
:- corner(P), P = point(X, Y), write(P), write(' '), write(X/Y), nl.
:- pair(3, T), T = tuple(A, B), write(T), write(' '), write(A+B), nl.
:- inverse(4, R), S is R * 2 + 1, write(R), write(' '), write(S), nl.
:- ages(M), write(M), nl.
:- ages(M), ages(M2), (M = M2 -> write(same) ; write(different)), nl.
:- nothing(U), write(U), nl, (U = [] -> write(nil) ; write(not_nil)), nl.
:- unset(U), write(U), nl, (U = undefined -> write(atom) ; write(not_atom)), nl.
```

```output
square-4
triangle-3
[1,2,3,4]
point(1,2) 1/2
tuple(3,6) 3+6
1r4 3r2
<map>
different
()
not_nil
undefined
not_atom
```

**FunL's `true` and `false` are the atoms `true` and `false`**, one value in both languages. A boolean
a FunL function answers unifies with the atom, is an atom to `atom/1` and every atom builtin, takes
its place among the atoms in the standard order, and is written as the atom; called as a goal, it is
the control construct `true` or `false`:

```prolog
:- import("prolog/shapes.funl").

:- yes(T), (T = true -> write(unifies) ; write(differs)), nl.
:- no(F), (F = true -> write(unifies) ; write(differs)), nl.
:- yes(T), atom(T), atom_length(T, N), T =.. L, write(N-L), nl.
:- yes(T), no(F), msort([b, T, a, F], S), write(S), nl.
:- yes(T), (call(T) -> write(succeeds) ; write(fails)), nl.
:- no(F), (call(F) -> write(succeeds) ; write(fails)), nl.
:- (atom(1) -> write(atom) ; write(not_atom)), nl.
```

```output
unifies
differs
4-[true]
[a,b,false,true]
succeeds
fails
not_atom
```

The other way round, the atom `false` is FunL's `false`, so it fails a condition wherever it came
from, and `#true` is the value `true` is:

```funl
if #false then write( 'taken' ) else write( 'not taken' )
write( true == #true, #false is boolean )
```

```output
not taken
true, false
```

## Calling FunL from Prolog

`:- import("file.funl").` loads a FunL file into a Prolog program, reading the path relative to the
Prolog file. The FunL file's top level runs as it loads. Its definitions are predicates:

- **a relation** of `n` arguments, such as `sides` above, is the predicate `sides/2`;
- **a function `f` of `n` parameters is the predicate `f/(n+1)`**: its last argument is unified with
  the result, and a function that generates values gives one solution for each, as `upto/2` does
  above.

A FunL function's arguments are looked at as FunL values, so an unbound one where FunL needs a value
is an `instantiation_error`. **An error raised in FunL code is the same ISO error term** a Prolog
builtin would raise, and `catch/3` catches it:

```prolog
:- import("prolog/shapes.funl").

try(G) :- catch((G, write(G)), error(E, _), write(E)), nl.

:- try(inverse(0, _)).
:- try(inverse(_, _)).
:- try(inverse(foo, _)).
:- try(inverse(5, _)).
```

```output
evaluation_error(zero_divisor)
instantiation_error
type_error(number,foo)
inverse(5,1r5)
```

**Prolog and FunL share one predicate for each name and arity.** A Prolog predicate and a FunL
definition that would be the same predicate are refused when the second is loaded, naming both:

```prolog
:- import("prolog/shapes.funl").

sides(circle, 0).
```

```error
Prolog and FunL share one predicate for each name and arity
```

The same refusal holds the other way round, for a FunL definition after an import of a Prolog file:

```funl
import "prolog/family.pl"

def parent( a ) = a
```

```error
`parent/2` cannot be defined twice
```

## Errors and exceptions

`catch(Goal, Catcher, Recovery)` runs `Goal`. `throw(Ball)` copies its ball and passes control back
to the nearest `catch` whose catcher unifies with it, undoing every binding made since that `catch`
began, and then runs its recovery goal.

```prolog
:- catch(throw(oops), oops, (write(recovered), nl)).
:- catch(catch(throw(inner), outer, write(wrong)), inner, (write(right), nl)).
:- catch((X = 1, throw(ball(X))), ball(Y), (write(Y), nl)).
:- catch((X = 1, throw(ball)), ball, (var(X) -> write(undone) ; write(kept))), nl.

try(G) :- catch(G, error(E, _), (write(E), nl)).

:- try(_ is foo + 1).
:- try(_ is _ + 1).
:- try(_ is 1 / 0).
:- try(atom_length(_, _)).
:- try(arg(x, f(a), _)).
:- try(functor(_, foo, -1)).
:- try(call(1)).
:- try(nope(1, 2)).
```

```output
recovered
right
1
undone
type_error(evaluable,foo/0)
instantiation_error
evaluation_error(zero_divisor)
instantiation_error
type_error(integer,x)
domain_error(not_less_than_zero,-1)
type_error(callable,1)
existence_error(procedure,nope/2)
```

**Every error is an ISO error term** `error(Formal, Context)`: `instantiation_error`,
`type_error(Type, Culprit)`, `domain_error(Domain, Culprit)`, `existence_error(procedure, Name/Arity)`,
`permission_error(Action, Type, Culprit)`, `evaluation_error(Error)` and the rest. The context holds
a sentence saying what went wrong.

The `unknown` flag decides what a call of a predicate nothing defines does: `error`, the default,
raises the `existence_error`; `fail` fails; `warning` fails after saying so on standard error.

```prolog
try(G) :- catch((G -> write(yes) ; write(no)), error(E, _), write(E)), nl.

:- try(undefined_here).
:- set_prolog_flag(unknown, fail).
:- try(undefined_here).
```

```output
existence_error(procedure,undefined_here/0)
no
```

## Arithmetic

`X is Expr` evaluates `Expr` and unifies `X` with the result; `=:=`, `=\=`, `<`, `>`, `=<` and `>=`
compare two expressions by value. An unbound variable in an expression is an
`instantiation_error`, and a name that is not an arithmetic function is
`type_error(evaluable, Name/Arity)`.

**`/` on two integers is exact where it divides and a float where it does not**, as ISO has it:
`8 / 2` is `4` and `7 / 2` is `3.5`. `//` divides rounding toward zero and `div` rounding down; `rem`
takes the sign of the dividend and `mod` the sign of the divisor. `**` gives a float, and `^` on two
integers an exact integer.

```prolog
show(E) :- X is E, format("~w = ~w~n", [E, X]).

:- show(7 / 2).
:- show(8 / 2).
:- show(7 // 2).
:- show(-7 // 2).
:- show(-7 div 2).
:- show(-7 mod 2).
:- show(-7 rem 2).
:- show(2 ** 10).
:- show(2 ^ 100).
:- show(2 ** -1).
:- show(sqrt(16)).
:- show(max(2, 3.0)).
:- show(gcd(12, 18)).
:- show(msb(1000)).
:- show(truncate(-2.5)).
:- show(round(2.5)).
:- show(5 xor 3).
:- show(1 << 4).
```

```output
7/2 = 3.5
8/2 = 4
7//2 = 3
-7//2 = -3
-7 div 2 = -4
-7 mod 2 = 1
-7 rem 2 = -1
2**10 = 1024.0
2^100 = 1267650600228229401496703205376
2** -1 = 0.5
sqrt(16) = 4.0
max(2,3.0) = 3.0
gcd(12,18) = 6
msb(1000) = 9
truncate(-2.5) = -2
round(2.5) = 3
5 xor 3 = 6
1<<4 = 16
```

The arithmetic functions:

| kind | functions |
|---|---|
| operators | `+`, `-` (both also prefix), `*`, `/`, `//`, `mod`, `rem`, `div`, `**`, `^` |
| bits | `/\`, `\/`, `xor`, `\`, `<<`, `>>`, `msb` |
| sign and size | `abs`, `sign`, `min`, `max`, `gcd`, `copysign` |
| conversion | `float`, `integer`, `float_integer_part`, `float_fractional_part`, `truncate`, `round`, `ceiling`, `floor` |
| powers and roots | `sqrt`, `exp`, `log` |
| trigonometry | `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2` |
| constants | `pi`, `e`, `epsilon` |
| random | `random` |

**`random(N)` is an integer drawn from 0 to `N - 1`**, `N` a positive integer. The generator is
seeded from the operating system, so each run draws differently, and only the range can be shown:

```prolog
:- X is random(6), integer(X), X >= 0, X < 6, write(in_range), nl.
:- catch(_ is random(0), error(E, _), (write(E), nl)).
```

```output
in_range
domain_error(positive_integer,0)
```

### Rationals

**FunL's rationals are numbers in Prolog arithmetic** and are written as FunL writes them, `7r2`. A
FunL function can hand one to Prolog, as `inverse/2` did above, and the `prefer_rationals` flag
makes `/` on two integers give one: a rational where the integers do not divide, and an integer where
they do. A rational is a `number` and is `rational`, compares with other numbers by value, and mixed
with a float gives a float.

```prolog
:- X is 7 / 2, write(X), nl.
:- set_prolog_flag(prefer_rationals, true).
:- X is 7 / 2, write(X), nl.
:- X is 7 / 2 + 1 / 2, write(X), nl, (integer(X) -> write(integer) ; true), nl.
:- X is 1 / 3, (rational(X) -> write(rational) ; true), nl, (number(X) -> write(number) ; true), nl.
:- X is 1 / 3, (X < 0.5 -> write(less) ; true), nl, (X =:= 2 / 6 -> write(equal) ; true), nl.
:- X is 1 / 3 * 1.5, write(X), nl.
```

```output
3.5
7r2
4
integer
rational
number
less
equal
0.5
```

## Standard order

`==`, `\==`, `@<`, `@>`, `@=<`, `@>=`, `compare/3`, `msort/2` and `sort/2` order terms in the
standard order: variables first, then numbers by value (an integer and a float of equal value put
the float first), then atoms by their text, then strings by their text, then compound terms by
arity, then name, then arguments from left to right.

`msort/2` keeps duplicates, `sort/2` removes them, and `sort/4` sorts on a key with an order
(`@<`, `@>`, `@=<` or `@>=`, the last two keeping duplicates). `keysort/2` sorts `Key-Value` pairs by
key and keeps pairs with equal keys in their order. `predsort/3` sorts with a comparison predicate.

```prolog
:- msort([b, 2, "s", f(x), 1.0, a, g(a, b), 1, f(y), 0.5], L), print(L), nl.
:- msort([c, a, b, a], L), write(L), nl.
:- sort([c, a, b, a], L), write(L), nl.
:- sort(0, @>=, [1, 3, 2, 3], L), write(L), nl.
:- keysort([b-1, a-2, b-0, a-1], L), write(L), nl.
:- compare(O, 1, 1.0), write(O), nl.
:- compare(O, f(z), g(a, b)), write(O), nl.
:- (1 == 1.0 -> write(same) ; write(different)), nl.
:- (1 =:= 1.0 -> write(equal) ; write(unequal)), nl.
:- (X @< a -> write(variables_first) ; true), nl.
```

```output
[0.5,1.0,1,2,a,b,"s",f(x),f(y),g(a,b)]
[a,a,b,c]
[a,b,c]
[3,3,2,1]
[a-2,a-1,b-1,b-0]
>
<
different
equal
variables_first
```

## All solutions

`findall/3` collects every solution, and `findall/4` puts a tail after them. `bagof/3` and `setof/3`
fail when there is none, and give one answer for each binding of the variables the template does
not mention, unless they are named with `^`; `setof` sorts and removes duplicates. `aggregate_all/3`
takes `count`, `sum(E)`, `max(E)`, `min(E)`, `bag(E)` and `set(E)`.

```prolog
age(peter, 7).
age(ann, 11).
age(pat, 8).
age(tom, 5).
age(mike, 11).

:- findall(N-A, age(N, A), L), write(L), nl.
:- findall(N, age(N, 11), L, [end]), write(L), nl.
:- setof(A-N, age(N, A), L), write(L), nl.
:- forall(bagof(N, age(N, A), L), (write(A-L), nl)).
:- setof(N, A^age(N, A), L), write(L), nl.
:- (bagof(N, age(N, 99), L) -> write(L) ; write(none)), nl.
:- aggregate_all(count, age(_, _), C), write(C), nl.
:- aggregate_all(sum(A), age(_, A), S), write(S), nl.
:- aggregate_all(max(A), age(_, A), M), write(M), nl.
:- aggregate_all(bag(A), age(_, A), B), write(B), nl.
:- aggregate_all(set(A), age(_, A), T), write(T), nl.
```

```output
[peter-7,ann-11,pat-8,tom-5,mike-11]
[ann,mike,end]
[5-tom,7-peter,8-pat,11-ann,11-mike]
5-[tom]
7-[peter]
8-[pat]
11-[ann,mike]
[ann,mike,pat,peter,tom]
none
5
42
11
[7,11,8,5,11]
[5,7,8,11]
```

## Grammar rules

A clause written `Head --> Body` is a grammar rule, translated into an ordinary clause as the file is
read. In the body, a list is the tokens it matches, a string matches its characters' codes, `{ Goal }`
is a goal run as it is, and `!`, `,`, `;`, `->` and `\+` mean what they mean in a clause.
`phrase(Rule, List)` holds when `Rule` matches the whole of `List`, and `phrase(Rule, List, Rest)`
when it matches a beginning of it, leaving `Rest`.

```prolog
greeting --> [hello], name.

name --> [world].
name --> [prolog].

anbn --> [].
anbn --> [a], anbn, [b].

digits([D|T]) --> digit(D), digits(T).
digits([D]) --> digit(D).

digit(D) --> [D], { D >= 0'0, D =< 0'9 }.

sum(S) --> number(N), sum_rest(N, S).

sum_rest(Acc, S) --> "+", !, number(N), { Acc1 is Acc + N }, sum_rest(Acc1, S).
sum_rest(S, S) --> [].

number(N) --> digits(Ds), { number_codes(N, Ds) }.

:- (phrase(greeting, [hello, world]) -> write(yes) ; write(no)), nl.
:- findall(N, phrase(greeting, [hello, N]), L), write(L), nl.
:- (phrase(anbn, [a, a, b, b]) -> write(yes) ; write(no)), nl.
:- (phrase(anbn, [a, b, b]) -> write(yes) ; write(no)), nl.
:- atom_codes('12+30+4', Cs), phrase(sum(S), Cs), write(S), nl.
:- atom_codes('42 rest', Cs), phrase(number(N), Cs, Rest), atom_codes(R, Rest), writeq(N-R), nl.
```

```output
yes
[world,prolog]
yes
no
46
42-' rest'
```

## `format/1` and `format/2`

`format(Format, Args)` writes `Format` with each directive replaced; `Args` is a list, or a single
argument that is not one. `format(Format)` takes no arguments. The format is an atom, a string, or a
list of codes or characters.

| directive | writes |
|---|---|
| `~w`, `~p`, `~q` | a term as `write/1`, `print/1`, `writeq/1` write it |
| `~a` | an atomic value's text |
| `~d`, `~Nd`, `~D` | an integer; with `N` digits after a decimal point; in groups of three |
| `~Nf`, `~Ne`, `~Ng` | a number with `N` digits (6 by default), fixed, exponent, or shortest |
| `~s` | a string, or a list of codes or characters |
| `~c`, `~Nc` | a character code, `N` times |
| `~Nr`, `~NR` | an integer in radix `N`, in lower or upper case |
| `~n`, `~~` | a newline; a `~` |
| `~i` | nothing: skips an argument |
| `~t`, `~N\|`, `~N+` | fill, a column stop at column `N`, a column `N` past the previous stop |

`*` in place of `N` takes the number from the arguments, and `` `c `` before `t` fills with `c`.

```prolog
:- format("plain text~n").
:- format("~w and ~a~n", [salt, pepper]).
:- format("~w~n", hello).
:- format("~q ~p~n", ['A b', "text"]).
:- format("~d items, ~2d, ~D~n", [42, 1234, 1234567]).
:- format("~4f ~e ~g~n", [3.14159, 2.5, 1.5]).
:- format("~s ~c~n", [[104, 105], 33]).
:- format("~8r ~16R~n", [255, 255]).
:- format("~a~t~10|~a~n", [left, right]).
:- format("~t~a~10|~a~n", [right, x]).
:- format("~t~d~6|~n", [42]).
:- format("~`-t~20|~n").
:- format("~a~t~8+~a~t~8+~a~n", [name, age, city]).
:- format("100~~ ~i~w ~*c~n", [skipped, shown, 3, 0'x]).
```

```output
plain text
salt and pepper
hello
'A b' "text"
42 items, 12.34, 1,234,567
3.1416 2.500000e+00 1.5
hi !
377 FF
left      right
     rightx
    42
--------------------
name    age     city
100~ shown xxx
```

A format that goes wrong writes nothing and raises an error:

```prolog
try(G) :- catch(G, error(E, _), (write(E), nl)).

:- try(format("~d~n", [abc])).
:- try(format("~w ~w~n", [one])).
```

```output
type_error(integer,abc)
format(not enough arguments)
```

## The builtin set

Every predicate there is, by group. Those marked *(library)* are written in Prolog and loaded into
every machine as ordinary clauses; a program may define one of them itself, and its own definition
replaces the library's. Every other one is built in, and a program cannot redefine it.

| group | predicates |
|---|---|
| control | `true/0`, `fail/0`, `false/0`, `!/0`, `,/2`, `;/2`, `->/2`, `*->/2`, `\+/1`, `not/1`, `call/1-8`, `once/1`, `ignore/1`, `forall/2`, `catch/3`, `throw/1`, `halt/0`, `halt/1`, `between/3`, `repeat/0` *(library)* |
| unification and comparison | `=/2`, `\=/2`, `unify_with_occurs_check/2`, `==/2`, `\==/2`, `@</2`, `@>/2`, `@=</2`, `@>=/2`, `=@=/2`, `\=@=/2`, `compare/3` |
| type tests | `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `float/1`, `rational/1`, `atomic/1`, `compound/1`, `callable/1`, `is_list/1`, `string/1`, `ground/1` |
| terms | `functor/3`, `arg/3`, `=../2`, `copy_term/2`, `term_variables/2` |
| arithmetic | `is/2`, `=:=/2`, `=\=/2`, `</2`, `>/2`, `=</2`, `>=/2`, `succ/2`, `plus/3` |
| database | `assert/1`, `asserta/1`, `assertz/1`, `retract/1` *(library)*, `retractall/1` *(library)*, `abolish/1`, `clause/2`, `dynamic/1`, `current_predicate/1` *(library)* |
| all solutions | `findall/3`, `findall/4`, `bagof/3`, `setof/3`, `aggregate_all/3` *(library)* |
| atoms and text | `atom_codes/2`, `atom_chars/2`, `char_code/2`, `atom_length/2`, `atom_concat/3`, `sub_atom/5`, `atom_number/2`, `number_codes/2`, `number_chars/2`, `upcase_atom/2`, `downcase_atom/2`, `term_to_atom/2` |
| strings | `atom_string/2`, `string_chars/2`, `string_codes/2`, `string_length/2`, `string_concat/3`, `sub_string/5`, `split_string/4`, `string_upper/2`, `string_lower/2` |
| lists *(library)* | `append/2`, `append/3`, `member/2`, `memberchk/2`, `nth0/3`, `nth1/3`, `reverse/2`, `last/2`, `delete/3`, `select/3`, `selectchk/3`, `subtract/3`, `intersection/3`, `union/3`, `permutation/2`, `flatten/2`, `numlist/3`, `sum_list/2`, `sumlist/2`, `max_list/2`, `min_list/2`, `list_to_set/2`, `exclude/3`, `include/3`, `partition/4`, `maplist/2-5`, `foldl/4-6`, `sort/4`, `predsort/3` |
| lists, built in | `length/2`, `msort/2`, `sort/2`, `keysort/2` |
| global variables | `nb_setval/2`, `nb_getval/2`, `b_setval/2`, `b_getval/2` |
| output | `write/1`, `writeln/1`, `print/1`, `writeq/1`, `write_canonical/1`, `write_term/2`, `nl/0`, `tab/1`, `put_char/1`, `format/1`, `format/2` |
| input | `read/1`, `read_term/2` |
| flags and operators | `op/3`, `current_op/3` *(library)*, `set_prolog_flag/2`, `current_prolog_flag/2` *(library)* |
| grammar rules | `phrase/2`, `phrase/3` *(library)*, `dcg_translate_rule/2` |
| loading | `consult/1` |

Four directives are carried out by the loader rather than called: `:- dynamic(…)`, `:- op(…)`,
`:- initialization(Goal)`, and, where FunL is present, `:- import(File)`.

The string predicates are SWI-Prolog's. Each takes any atomic value as text and makes a string where
its atom counterpart makes an atom; a given argument is compared by its text, so an atom may stand
for a string. `split_string/4` cuts at each separator character and strips the padding characters
from both ends of every piece; `string_concat/3` and `sub_string/5` with their parts unbound give
every answer in turn.

```prolog
:- split_string("SWI-Prolog, 7.0", ",", " ", L), writeq(L), nl.
:- forall(string_concat(A, B, "ab"), (writeq(A + B), nl)).
:- sub_string("hello world", B, _, 0, world), atom_string(A, "hi"), writeq(B - A), nl.
:- upcase_atom('héllo', U), string_lower("ABC", S), writeq(U / S), nl.
```

```output
["SWI-Prolog","7.0"]
""+"ab"
"a"+"b"
"ab"+""
6-hi
'HÉLLO'/"abc"
```

The flags `set_prolog_flag/2` changes are `unknown` (`error`, `fail`, `warning`), `double_quotes`
(`string`, `codes`, `chars`, `atom`), `prefer_rationals` (`false`, `true`) and `debug` (`off`, `on`).
The others describe the machine and cannot be changed: `bounded` is `false`, `max_integer` and
`min_integer` are the 64-bit range, `integer_rounding_function` is `toward_zero`, and `max_arity`.
