---
title: Relations
weight: 60
---

# Relations

FunL has logic programming built in. A **logic variable** is a value not known yet; **unification**
makes two values the same by filling such variables in; and a **relation** is a definition made of
facts and rules whose clauses are all tried, one after another, as alternatives.

## Logic variables

`free x` declares `x` as a new, unbound logic variable (`free x, y` declares several). An unbound
variable prints as `_G` followed by a number, unique within the run, so two of them can be told
apart. Once bound, a variable stands for its value everywhere: printing, arithmetic, comparison and
indexing all see the value.

```funl
free x
write( x )
x ~ 5
write( x )
write( x + 1 )
```

```output
_G0
5
6
```

A logic variable is declared before it is used. A name that is neither declared with `free` nor
defined some other way is refused:

```funl
write( y ~ 1 )
```

```error
a logic variable is declared before it is used: `free y`
```

An unbound variable given to an operation that needs a value is an **instantiation error**, which
stops the program and names the operation:

```funl
free x
write( x + 1 )
```

```error
instantiation error: '+' was given an unbound variable
```

## Unification: `~`

`a ~ b` succeeds if the two can be made equal by binding variables, and fails otherwise. Like a
comparison, it produces its **right** operand. Variables on either side are bound, and a variable
inside a list is bound as readily as one standing alone:

```funl
free y, z
write( [y, 2] ~ [3, z] )
write( y )
write( z )
```

```output
[3, 2]
3
2
```

Unification compares **terms**, so the kinds have to agree: `1 ~ 1.0` fails, though `1 == 1.0`
holds. A variable unified with itself succeeds. A bound variable unifies only with what it is bound
to, and a unification that fails part way through leaves no binding behind:

```funl
free a
write( 1 ~ 1.0 )
write( 1 == 1.0 )
if a ~ a then write( 'same' )
write( [a, 1] ~ [2, 3] )
a ~ 4
write( a ~ 5 )
write( a )
```

```output
1.0
same
4
```

## Atoms

An **atom** is a bare name, written `#` and the name: `#don`. Two atoms unify when they are the same
name. `write` prints an atom without its `#`. A constructor of a `data` type that has no fields is an
atom too, so after `data colour = red | green`, `red` is `#red`.

```funl
data colour = red | green

write( #don )
write( #don ~ #don )
write( #don ~ #anne )
write( red ~ #red )
write( green ~ #red )
write( 'done' )
```

```output
don
don
red
done
```

## Facts and rules

A `def` with no body is a **fact**. A `def` whose head is followed by `:-` and a goal is a **rule**.
Either makes the name a **relation**: when it is called, every clause is tried in the order written,
and each one that succeeds is a solution. A relation called from ordinary code is a generator, so
`every` visits each solution:

```funl
def female(#anne)
def male(#don)
def parent(#don, #randy)
def parent(#don, #anne)
def parent(#rosie, #anne)

def mother(x, y) :- female(x) & parent(x, y)
def father(x, y) :- male(x) & parent(x, y)

free c, p
every parent(#don, c) do write( c )
every parent(p, #anne) do write( p )
every father(#don, c) do write( c )
```

```output
randy
anne
don
rosie
randy
anne
```

The clauses of a relation may be spread across a file, as above, or gathered under one `def`:

```funl
def
  edge(#a, #b)
  edge(#b, #c)
  path(x, y) :- edge(x, y) | edge(x, z) & path(z, y)

free n
every path(#a, n) do write( n )
```

```output
b
c
```

**A definition is one kind or the other.** A name defined both by `=` clauses and by facts or rules is
refused:

```funl
def f(1) = 'one'
def f(#a)
```

```error
`f` mixes a function clause with relation clauses
```

### The body is one goal

A rule's body is one expression. Inside it, `&` and `and` are conjunction, `|` and `or` are
disjunction. A long body is written as an indented block, and **each line of the block is a
conjunct**, as if the lines were joined by `&`:

```funl
def
  parent(#don, #randy)
  parent(#don, #anne)
  parent(#rosie, #randy)
  parent(#rosie, #anne)

  full_siblings(a, b) :-
    parent(f, a) & parent(f, b)
    parent(m, a) & parent(m, b)
    a != b & f != m

free s
every full_siblings(#randy, s) do write( s )
```

```output
anne
anne
```

`anne` is found twice: once with `f` as don and `m` as rosie, and once the other way round.

### Variables in a relation

Inside a relation, a name means one of these, in order:

1. a parameter of the head is a variable of the clause;
2. a name that is defined at the top level — a function, a relation, a `data` constructor — means
   that definition;
3. any other name is a new logic variable of the clause, unbound each time the clause is entered:
   `z` in `path` above, `f` and `m` in `full_siblings`;
4. `_` is a new anonymous variable at each place it is written.

A clause variable written only once draws a warning, because it is usually a misspelling of another
one. Starting the name with `_` (`_child`) says the single use is intended, and draws none:

```funl
def parent(#don, #randy)
def parent(#don, #anne)
def has_child(x) :- parent(x, _child)

write( has_child(#don) )
write( has_child(#randy) )
write( 'done' )
```

```output
()
done
```

## Head unification is two-way

A clause's head is **unified** with the call's arguments, not matched one way. A constant in the head
binds an unbound argument, and a list or a term in the head builds the structure the caller's
variable is bound to. So one relation answers several questions, depending on which arguments are
given:

```funl
def
  append([], ys, ys)
  append(h : t, ys, h : r) :- append(t, ys, r)

free xs, ys, r
if append([1], [2, 3], r) then write( r )
every append(xs, ys, [1, 2]) do write( (xs, ys) )
```

```output
[1, 2, 3]
([], [1, 2])
([1], [2])
([1, 2], [])
```

A term in a head is written like a call, `point(x, _)`, and needs no declaration:

```funl
def point_x(point(x, _), x)

free p
if point_x(p, 5) then write( p )
```

```output
point(5, _G2)
```

## Calling a relation from FunL

**A relation call produces `()`, and its results are its bindings.** Each solution binds the
caller's variables; asking for the next solution undoes those bindings first.

In a bounded position, such as the test of an `if`, the first solution is taken and its bindings
stay. After an `every`, every binding it made has been undone:

```funl
def parent(#don, #randy)
def parent(#don, #anne)

free c
write( parent(#don, #randy) )
write( parent(#don, #rosie) )
if parent(#don, c) then write( c )
write( c )

free d
every parent(#don, d) do write( d )
write( d ~ #nobody )
```

```output
()
randy
randy
randy
anne
nobody
```

The last line shows `d` unbound again after the `every`: it unifies with anything.

## Calling a function from a relation

Inside a rule body, a function call is evaluated as FunL evaluates it: as a goal, its failure fails
the conjunction, and as an operand, its value is used. A comparison is a goal like any other.

```funl
def
  total([]) = 0
  total(h : t) = h + total(t)

def sum_is(xs, n) :- n ~ total(xs)
def big(x) :- x > 100

free s
if sum_is([1, 2, 3], s) then write( s )
write( big(150) )
write( big(50) )
write( 'done' )
```

```output
6
()
done
```

A function's parameters are matched one way, so a function given an unbound variable where its
pattern has to look inside the argument does not bind it: that is an instantiation error, naming the
function and the argument.

```funl
def
  size([]) = 0
  size(_ : t) = 1 + size(t)

def len(xs, n) :- n ~ size(xs)

free a, n
if len(a, n) then write( n )
```

```error
`size` was given an unbound variable as argument 1, which its pattern has to look inside
```

### A function as a predicate

In a goal — a rule body, or the goal of `findall`, `bagof` or `setof` — a function of `n` parameters
may also be called as a relation of `n + 1` arguments: the extra last argument is unified with the
function's result. A function that generates gives one solution for each value it produces.

```funl
def upto(n) = !(1..n)
def twice(x) = x * 2

def doubled(e) :- upto(3, n) & twice(n, e)

free k
every doubled(k) do write( k )
```

```output
2
4
6
```

## Negation and choice inside a relation

`not g` is negation as failure: it succeeds when `g` has no solution. It is sound only when `g`'s
variables are bound by the time it runs — `not parent(x, #anne)` with `x` unbound asks whether
nobody is anne's parent, not for an `x` who is not.

`if c then t else e` in a body tries `c`, and **commits to its first solution**: when `c` succeeds,
`t` runs with `c`'s bindings and `e` is never tried.

```funl
def parent(#don, #randy)
def parent(#don, #anne)

def childless(x) :- not parent(x, _)
def kind(x, k) :- if parent(x, _) then k ~ #parent else k ~ #child

write( childless(#randy) )
write( childless(#don) )

free k
every kind(#don, k) do write( k )
every kind(#anne, k) do write( k )
```

```output
()
parent
child
```

`kind(#don, k)` gives one solution, not two, though don has two children: the test of the `if` is
bounded.

## There is no cut

A relation written in FunL has no cut. `if … then … else` commits where a cut would, and a bounded
position takes only the first solution. `!` is FunL's generator operator, and is not a goal:

```funl
def p(1)
def q(x) :- p(x) & !
```

```error
a FunL relation has no cut
```

## Collecting all solutions

`findall(t, g)` runs the goal `g` to exhaustion and gives a list of `t` as it stood at each solution,
in order. `bagof(t, g)` is the same, except that it fails when `g` has no solution. `setof(t, g)`
sorts the list and removes its duplicates.

```funl
def parent(#don, #randy)
def parent(#don, #anne)
def parent(#rosie, #anne)

free c, x
write( findall(c, parent(#don, c)) )
write( findall(c, parent(x, c)) )
write( setof(c, parent(x, c)) )
write( findall(c, parent(#nobody, c)) )
write( bagof(c, parent(#nobody, c)) )
write( 'done' )
```

```output
[randy, anne]
[randy, anne, anne]
[anne, randy]
[]
done
```

## A family tree

The relations below describe a family, and the questions at the end are answered by search.

```funl
def
  female(#anne)
  female(#rosie)
  female(#emma)
  female(#olivia)
  female(#mia)

  male(#randy)
  male(#don)
  male(#liam)
  male(#logan)
  male(#aiden)

  parent(#don, #randy)
  parent(#don, #anne)
  parent(#rosie, #randy)
  parent(#rosie, #anne)
  parent(#liam, #don)
  parent(#olivia, #don)
  parent(#liam, #mia)
  parent(#olivia, #mia)
  parent(#emma, #rosie)
  parent(#logan, #rosie)
  parent(#emma, #aiden)
  parent(#logan, #aiden)

def
  ancestor(x, y) :- parent(x, y) | parent(x, p) & ancestor(p, y)
  father(x, y) :- male(x) & parent(x, y)
  siblings(x, y) :- parent(p, x) & parent(p, y) & x != y
  uncle(u, n) :- male(u) & siblings(u, p) & parent(p, n)
  grandparent(x, y) :- parent(x, p) & parent(p, y)

free who
every father(who, _) do write( who )
every grandparent(who, #randy) do write( who )
every uncle(who, #randy) do write( who )
if father(who, #mia) then write( who )

free a
write( setof(a, ancestor(a, #randy)) )
```

```output
don
don
liam
liam
logan
logan
liam
olivia
emma
logan
aiden
aiden
liam
[don, emma, liam, logan, olivia, rosie]
```

The answers include the duplicates. `father(who, _)` has one solution for each child, so don, liam
and logan are each a father twice; aiden is randy's uncle once through each of his two parents.
