---
title: Logic programming in FunL
weight: 30
---

# Logic programming in FunL

**FunL already has Prolog's search core and lacks its data.** Goal-directed evaluation *is*
depth-first search with chronological backtracking; what Prolog adds on top of it is a kind of value
that is not known yet — the logic variable — and an operation that makes two values the same by
filling such variables in — unification. This chapter adds those two things to FunL, and then adds
facts and rules as a second way to write a `def`.

| Prolog | FunL already has |
|---|---|
| a goal succeeds or fails | every expression succeeds or fails |
| `,` conjunction | `&` and `and` |
| `;` disjunction | `\|` and `or` |
| `\+ G` negation as failure | `not e` |
| choice points and backtracking | the control stack, exactly |
| `(C -> T ; E)` | `if c then t else e` — the condition is already bounded |
| `once(G)` | any bounded position |
| `forall(C, A)` | `not (c & not a)`, or `every` |
| **logic variables** | **missing** |
| **unification** | **missing** — parameter matching is one-way |
| **a trail** | half: `x <- e` is undone, but [interleaved with the choice points](vm.md#a-separate-trail-because-a-commit-is-not-an-undo) |
| **clauses tried as alternatives** | **missing** — `def` clauses are committed choice |

The languages that have done this before are worth naming, because each settled one of the
questions below: **Curry** (functional logic, `free` variables, narrowing), **Oz/Mozart** (dataflow
variables, `=` as unification, `:=` for cells) and **Verse** (failure-driven control in a functional
language, with unification at the core).

## Logic variables

```
struct VarObj
    value: Value            // the binding, or Undefined-with-a-flag while unbound
    bound: bool
    stamp: u64              // the machine stamp when the variable was made
```

**A variable is unbound until unification binds it, and a binding is undone by backtracking** — the
[trail chapter](vm.md#the-trail) has the mechanism, and the stamp is what decides whether a binding
needs recording at all. Every operation that inspects a value first **dereferences** it: follows
bound variables until it reaches something that is not one. Arithmetic, comparison, printing,
indexing and pattern matching all dereference; only unification and the type tests `var(x)` /
`nonvar(x)` look at a variable as a variable.

**An unbound variable reaching an operation that needs a value is an instantiation error**, raised
as an ordinary FunL error that names the operation: `instantiation error: '+' was given an unbound
variable`. It is never a stack overflow, never a silent failure, and never `undefined`.

An unbound variable prints as `_` followed by a number unique within the run — `_G17` — so two
different variables can be told apart in output.

## Unification has its own operator

**`=` is assignment and `==` is equality, so unification needs a third spelling.**

> **Decided — the unification operator.**
> **Decision: `~`.** It is free in FunL's lexer, it is one character in a position (relation
> bodies) where it is written constantly, and "is like" is a fair reading of what it does.
> **Rejected:** `=:=`, Curry's spelling, which a Prolog programmer reads as *arithmetic*
> equality; a builtin `unify(a, b)`, which reads badly as the most common goal of all; or making `=`
> mean unification inside a relation body only, which gives one symbol two meanings depending on
> which side of `:-` it is written.

`a ~ b` succeeds, producing `b` like every FunL comparison, if the two can be made equal by binding
variables, and fails otherwise. The algorithm, iterative with an explicit work list so that a deep
term cannot overflow the sysl stack:

```
unify(a, b)
    push (a, b)
    while the work list is not empty
        (x, y) = pop, both dereferenced
        if x and y are the same variable or the same heap object: continue
        if x is an unbound variable: bind(x, y); continue
        if y is an unbound variable: bind(y, x); continue
        kinds differ: fail
        atoms, integers, reals: equal by value, or fail
        strings: equal by content, or fail
        compounds: same functor and arity, then push each pair of arguments
        tuples, conses: same shape, then push each pair
        arrays, maps, sets, closures, cursors: identical objects, or fail
```

Four rules in it are deliberate, and each is a defect of the 2019 engine that it closes:

- **A variable unified with itself succeeds at once.** The identity test comes before anything else,
  so `X ~ X` cannot loop.
- **Kinds are compared before values, so `1 ~ 1.0` fails.** Unification is about terms being the
  same term, and an integer and a real are different terms; `1 == 1.0` is FunL's numeric equality
  and still succeeds.
- **When two unbound variables meet, the younger is bound to the older**, by stamp. The binding of a
  younger variable usually needs no trail entry, and the older one outlives it.
- **There is no occurs check by default**, as in every Prolog; `unify_with_occurs_check` does one.

## Atoms

**A Prolog fact talks about named things, and FunL has no literal for a bare name** — `don` written
in a program is a variable.

> **Decided — how is an atom written?**
> **Decision: `#don`**, an atom literal; `#` is unused by FunL's lexer, and the atom is the
> same interned value Prolog's `don` is, so a fact written in either syntax is the same fact. In
> addition, **a nullary `data` constructor is an atom**: after `data colour = red | green`, `red`
> is the atom `#red`, which is the typed way to write a closed set of names.
> **Rejected:** strings (`parent("don", "randy")`), which work today and are a different kind
> from Prolog's atoms, so they do not unify with what a Prolog file says; or only `data`
> constructors, which makes a quick fact base wait on a declaration of every name in it.

## Facts and rules

**A `def` with no body is a fact. A `def` whose head is followed by `:-` is a rule.** Both make the
definition a **relation**: its clauses are alternatives, all of them tried, rather than committed
choice.

```
def female(#anne)
def male(#don)
def parent(#don, #randy)

def mother(x, y) :- female(x) & parent(x, y)
def ancestor(x, y) :- parent(x, y) | parent(x, p) & ancestor(p, y)
```

> **Decided — how is a rule marked?**
> **Decision: `:-` after the head, and no body for a fact**, as above. It reuses the one
> symbol every reader of Prolog already knows, needs no keyword, and leaves `def` meaning "define a
> name" for both kinds. **Rejected: a `rel` keyword** in place of `def` for relations —
> `rel ancestor(x, y) = parent(x, y) | …` — which marks the kind at the start of the line and lets
> the body keep `=`, at the cost of a second defining word and of facts needing it too.

**A definition is one kind or the other.** A name whose clauses mix `=` bodies with facts or `:-`
rules is refused at compile time, naming both clauses — a reader cannot tell which selection rule
applies, and neither can the compiler. Clauses of a relation may be spread across a file like
Prolog's, or gathered under one `def` block like FunL's mutually recursive functions.

### The body is one goal

**A rule's body is one expression, never a sequence of statements.** A statement is bounded — its
`Unmark` discards every choice point made inside it — so a body written as statements could never
be backtracked into for a second solution. A long body is written as an indented block, and **each
line of a rule body block is a conjunct**, joined by an implicit `&`:

```
def full_siblings(a, b) :-
  parent(f, a) & parent(f, b)
  parent(m, a) & parent(m, b)
  a != b & f != m
```

is one conjunction of six goals. Inside a rule body, `|` and `or` are disjunction, `not` is negation
as failure, and `if c then t else e` is Prolog's `(C -> T ; E)`: the condition is bounded, so it
commits to its first solution, exactly as `->` does.

### Variables in a relation

**In a relation, a name that is not otherwise in scope is a logic variable of the clause**, created
fresh each time the clause is entered:

1. a head parameter is a clause variable;
2. a name that resolves to something in an enclosing scope — a function, a relation, a `val` — means
   that thing, by FunL's ordinary lexical rule;
3. any other name is a fresh, unbound variable local to the clause: `p` in `ancestor`, `f` and `m`
   in `full_siblings`;
4. `_` is a new anonymous variable at each occurrence.

**A clause variable that appears exactly once is warned about**, as every Prolog does, because it is
almost always a misspelling of another one. A name starting with `_` (`_rest`) is the way to say the
single occurrence is intended.

**The rule is the user's suggestion and the reason is that Prolog's convention cannot be borrowed.**
Prolog tells variables from atoms by capital letters; FunL's names are ordinary identifiers, so the
only information available is whether the name is already bound somewhere — and in a relation, an
unbound name that meant `undefined` would make every rule that introduces an intermediate variable
silently wrong.

## How a relation is compiled

**Clause selection is a `Choice` per clause, and the head is unified, not matched.**

```
ancestor/2:
    Choice(c2)
    // clause 1:  ancestor(x, y) :- parent(x, y)
    Frame(2)                         // slots: x, y
    GetVar(0, arg0); GetVar(1, arg1)
    Call parent(slot0, slot1)
    Return
c2:
    // clause 2:  ancestor(x, y) :- parent(x, p) & ancestor(p, y)
    Frame(3)                         // slots: x, y, p
    GetVar(0, arg0); GetVar(1, arg1); NewVar(2)
    Call parent(slot0, slot2); Pop
    TailCall ancestor(slot2, slot1)
```

The last clause has no `Choice` before it, so a relation that reaches its last clause leaves nothing
behind for itself. `Return` (never `ReturnCommit`) leaves the body's choice points in place, so the
caller sees a generator; the [`Restore` rule](vm.md#the-operand-stack-and-why-backtracking-has-to-copy-part-of-it)
makes that safe exactly as it does for a generating function.

### Head unification is two-way

A head argument is compiled by what it is:

| head argument | instruction | when the caller's argument is unbound | when it is bound |
|---|---|---|---|
| first occurrence of a variable | `GetVar(slot, arg)` | the slot shares the caller's variable | the slot holds the value |
| later occurrence | `GetValue(slot, arg)` | unify | unify |
| an atom, a number, a string | `GetConst(k, arg)` | **bind the caller's variable to `k`** | compare |
| `point(a, b)`, `[h \| t]`, a tuple | `GetCompound(f/n, arg)` + its arguments | **build the term with fresh variables and bind** | check the functor, then unify the arguments |

**The third and fourth rows are what FunL's one-way parameter matching cannot do**: `parent(x,
#randy)` called with `x` unbound finds the facts whose second argument is `#randy` and binds `x` to
each first argument in turn, which a function's parameter pattern never does to its caller.

**First-argument indexing is the first optimisation and it is about determinism as much as speed.**
When the first argument is bound, the relation jumps straight to the clauses whose first head
argument could match it, keyed by atom, number or functor. A call that can match only one clause
then leaves no choice point at all — so `parent(#liam, c)` is a generator over two clauses rather
than a walk of twelve, and a recursive relation over a list runs in constant control-stack space.

## Calling across the line

**Functions and relations call each other through the same `Call` instruction; what is called
decides what happens.**

### A relation called from ordinary FunL code

**A relation call is a generator whose value is `()` and whose results are its bindings.** Each
solution binds the caller's variables; backtracking for the next solution unbinds them first.

```
free f
every father(f, _) do write(f)          ;; don, don, liam, liam, logan, logan

if father(f, #mia) then write(f)        ;; liam
```

In a bounded position the first solution is taken and its bindings stay, so after the `if` above
`f` is still `#liam`. After the `every`, every binding it made has been undone and `f` is unbound
again.

#### Logic variables in ordinary code

> **Decided — how does FunL code make a logic variable?**
> **Decision: `free x`** (and `free x, y`), Curry's word for exactly this declaration. It
> declares a name in the enclosing scope whose value is a new unbound variable. Inside relation
> bodies no declaration is needed, by the [scoping rule](#variables-in-a-relation) above.
> **Rejected:** a builtin, `val x = fresh()`, which needs no new word and reads as a function
> call that has a side effect; or treating every undeclared name in *any* call to a relation as
> free, which makes a typo in ordinary code a silent logic variable.

### A function called from a relation

**Inside a rule body, a function call is an expression, evaluated as FunL evaluates it**: as a goal
its failure fails the conjunction, and as an operand its value is used.

```
def total(xs, n) :- n ~ sum(xs)
def big(x) :- x > 100                  ;; a comparison is a goal like any other
```

**A function's parameters are matched one way, so a function given an unbound variable where its
pattern has to look inside the argument raises an instantiation error** naming the function and the
parameter. It does not bind the variable — a function never changes its caller's values — and it
does not fail silently, which would make "no such clause" and "called too early" indistinguishable.

### A function seen as a predicate, and a relation seen as a generator

**A FunL function `f` of `n` parameters is the predicate `f/(n+1)`**: the extra last argument is
unified with the result, and if `f` generates, each value is one solution. This is how a Prolog
clause calls FunL (the [Prolog chapter](prolog.md#calling-funl-from-prolog) has the syntax), and how
`findall` collects a FunL generator's values.

**A relation `r/n` seen from FunL is a generator of bindings**, as above. The two views are one
mechanism seen from opposite sides: both are code that leaves choice points on the control stack
when it returns, and both are resumed by failing into them.

## Negation, cut and collecting solutions

**`not g` is negation as failure**, and it is sound only when `g` is ground — `not parent(x, #mia)`
with `x` unbound asks "does nobody have mia as a child", not "find an `x` that is not mia's parent".
This is Prolog's rule and FunL inherits it unchanged; the singleton warning catches the commonest
mistake, a variable that appears only under a `not`.

### Cut in a relation

> **Decided — is there a cut in FunL relations?**
> **Decision: no `!` in FunL syntax.** `!` is already FunL's generator operator, and every use
> of cut that is not a hack is covered by constructs FunL has: `if c then t else e` is `->`, a
> bounded position is `once`, and first-argument indexing removes the choice points that cut was
> most often written to kill. Standard Prolog syntax has `!` and runs on the same `CutClause`
> instruction. **Rejected:** a keyword (`commit`) meaning Prolog's cut; or `!` as cut when it
> stands alone as a conjunct, which is unambiguous to the parser and confusing to a reader.

### All solutions

`findall(t, g)`, `bagof(t, g)` and `setof(t, g)` are compile-time forms — `g` must not be evaluated
before they run — that drive `g` to exhaustion and collect a copy of `t` at each solution:

```
free c
write( findall(c, parent(#don, c)) )        ;; [#randy, #anne]
```

The copy is taken because the bindings that made `t` are undone as soon as the next solution is
asked for. The list being built is held in an operand cell for the duration, never in a sysl local,
so a collection in the middle of a long `findall` sees it.

## The family tree, natively

This is the program the [first milestone](milestones.md) runs: the 2019 repository's
`examples/family_tree`, written in FunL.

```
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
  relation(x, y) :- ancestor(a, x) & ancestor(a, y) & x != y
  ancestor(x, y) :- parent(x, y) | parent(x, p) & ancestor(p, y)

  mother(x, y) :- female(x) & parent(x, y)
  father(x, y) :- male(x) & parent(x, y)
  daughter(x, y) :- female(x) & parent(y, x)
  son(x, y) :- male(x) & parent(y, x)
  siblings(x, y) :- parent(p, x) & parent(p, y) & x != y
  full_siblings(a, b) :-
    parent(f, a) & parent(f, b)
    parent(m, a) & parent(m, b)
    a != b & f != m
  sister(x, y) :- female(x) & siblings(y, x)
  brother(x, y) :- male(x) & siblings(y, x)
  uncle(u, n) :- male(u) & siblings(u, p) & parent(p, n)
  aunt(a, n) :- female(a) & siblings(a, p) & parent(p, n)
  grandparent(x, y) :- parent(x, p) & parent(p, y)
  grandmother(x, y) :- female(x) & grandparent(x, y)
  grandfather(x, y) :- male(x) & grandparent(x, y)

free who
every father(who, _) do write(who)          ;; don don liam liam logan logan
every grandparent(who, #randy) do write(who) ;; liam olivia emma logan
every uncle(who, #randy) do write(who)      ;; aiden aiden
if father(who, #mia) then write(who)        ;; liam
```

**The expected answers include the duplicates, and that is the test.** `father(F, _)` has one
solution per child, so don, liam and logan are each a father twice; the 2019 repository's README
shows each once, which is not what a Prolog answers and not what this program may print. `x != y`
is FunL's inequality on two values that are ground by the time it runs, which is the same test
Prolog's `\=` makes in this program.
