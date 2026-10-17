---
title: Prolog on the same machine
weight: 40
---

# Prolog on the same machine

**Standard Prolog source runs on the FunL machine, compiled by a second front end to the same
instructions FunL relations compile to.** A predicate defined in a `.pl` file and a relation defined
in a `.funl` file are the same kind of thing on the control stack, so each calls the other with no
bridge, no conversion of terms and no second engine. The [logic chapter](logic.md) is the
mechanism; this chapter is the second syntax, the builtin set it aims for, and the failure modes of
a Prolog engine that its design rules out.

**Prolog is built on this machine, not beside it.** A Prolog on a separate machine with its own
instruction set has failure modes (listed at the end) that come from that separation rather than
from slips in its code, so Prolog here is built on a machine whose design already rules them out.

## Loading Prolog

```
consult("family.pl")                    ;; from FunL: load and compile the file's clauses

import "family.pl"                      ;; the same, at compile time, like any import
```

and from the command line, `funl family.pl` reads a file by its extension and starts the
interactive top level, or runs `:- initialization(main).` if the file has one. **A Prolog file's
directives run at load time** (`:- dynamic`, `:- op`, `:- initialization`), exactly as in any Prolog.

## A Prolog with no FunL in it

**There is one machine, and a standalone Prolog is a second front end to it.** The VM is unified:
one instruction set and single-level dispatch — one match over the instruction kind in one run loop,
as slate's interpreter does it. Some instructions are emitted only for Prolog code (clause
indexing, cut to a clause's barrier), some only for FunL (generators, scanning, regex) and most are
shared (choice points, the trail, frames, calls, unification); all of them live in the same enum and
the same loop, with no layering and no extension seam.

**The standalone Prolog is that machine with the Prolog reader, the Prolog builtins and an ISO top
level** (`?- goal.`, `;` for the next answer, `consult/1`); its front end never emits a FunL-only
instruction. **Later it also runs FunL**: `:- import("geometry.funl")` from a Prolog file or the top
level brings in the FunL front end and the file's functions become predicates, as [Calling FunL from
Prolog](#calling-funl-from-prolog) describes. Because the machine is one machine, that is a matter of
which front ends are linked in, never of a second VM or a bridge. It knows only standard Prolog:
no FunL syntax, no FunL builtins, no FunL value kinds reachable from it. The same Prolog program
runs identically in it and through `funl family.pl`; the difference is only what else is loaded.

**It is `fprolog`, which is the `funl` executable started under that name.** The machine is a library
at the repository root (package `funl-vm`, module `io.github.edadma.funl`), and the driver is a
member over it — `funl/`, a `main.sysl` and a manifest — because a program has
one `main` and a package whose root carried an entry file would bring it into every program that
depends on it. `sysl build -p funl` writes `funl/funl`; installing makes `fprolog` a symlink to it, and
`main` looks at the basename of its own name to choose. `sysl test .` at the root tests every member. The top level itself is `toplevel.sysl` in the library,
over a machine made by `prolog_vm()`, whose `funl_front_end` is off: there `:- import(File)` is no
directive, and a goal `import(File)` is an unknown procedure. `funl/main.sysl` only supplies the
terminal — lines through `sh.sysl.linenoise`, plain lines with no prompt from a pipe.

```
$ fprolog family.pl
?- parent(tom, X).
X = bob
;
X = liz.
?- X is 1 / 0.
uncaught exception: error(evaluation_error(zero_divisor),_)
?- halt.
```

A query runs under `catch/3` as a chunk that ends in `Halt` with its variables on the operand stack
(`compile_toplevel`); `;` fails back into the same run (`resume`). An answer with no choice point
left ends in `.` and asks for nothing; anything but `;` stops at the answer given. `halt.` and
`halt(N).` end the session with that status, as does the end of input with 0.

## The reader

**The reader is an operator-precedence parser over an operator table that changes at run time**,
because `op/3` lets a program add operators while it is being read. `sh.sysl.parsing`'s `pratt`
asks a callback for each token's binding power, and the callback here consults the table — so a
directive `:- op(700, xfx, ===>).` takes effect for the clauses after it with no change to the loop.
ISO's priorities (0–1200) and types (`xfx`, `xfy`, `yfx`, `fy`, `fx`, `xf`, `yf`) map onto the
left/right powers the loop already takes; a term written in canonical form, `+(1, 2)`, needs no
table at all.

**Tokens follow ISO**: a variable starts with a capital or `_`; an atom is a lower-case name, a run
of symbol characters, a quoted `'atom'`, or one of `[]`, `{}`, `!`, `;`, `,`; numbers include
`0'c`, `0x1F`, `0o17`, `0b101` and floats; `%` and `/* */` are comments. SWI-Prolog's number
syntax is read too: `16'1F` in any base from 2 to 36, digit groups (`1 000 000`, `1_000_000`, a `_`
followed by layout or a comment), rationals `7r2`, and `1.0Inf`, `-1.0Inf` and `1.5NaN`.

**Characters beyond ASCII are classified by their Unicode general category, as SWI-Prolog does**
(`pl_class.sysl`, the one place the reader and `writeq` both ask). An unquoted name starts with a
lowercase, titlecase or caseless letter (`Ll`, `Lt`, `Lo`, `Lm`: `été`, `σ`, `日本`); a variable with an
uppercase letter (`Lu`) or `_`. Either continues with letters, decimal digits, combining marks, connector
punctuation such as `‿`, or `_`. A symbol character (`Sm`, `Sc`, `Sk`, `So`) joins the ASCII ones into a
run, so `€` is a name, `+€` is one name, and `€x` is the name `€` followed by `x`. Any other character
(`²`, `«`) cannot begin a token. `writeq` writes an atom bare exactly when the reader reads it back as
that atom, and quotes any other; inside quotes it writes `\a \b \f \n \r \t \v` as those escapes and any
other control, separator, format, private-use or unassigned character as `\x<hex>\` in upper case.

**A double-quoted string is a string**, not a list of codes — the `double_quotes` flag defaults to
`string`, as in SWI-Prolog 7 and later. That is the setting under which a Prolog string and a FunL
string are the same value. A program that wants ISO's `codes` sets the flag.

## How a clause compiles

**A clause compiles exactly as a FunL rule does**: `Choice` between clauses, the head by
[two-way unification](logic.md#head-unification-is-two-way), the body as calls. The control
constructs map onto the machine's own instructions:

| Prolog | instructions |
|---|---|
| `A, B` | `A; Pop; B` |
| `A ; B` | `Choice(b); A; Branch(end); b: B; end:` |
| `C -> T ; E` | `SaveHeight(s); Choice(e); C; CutSlot(s); T; Branch(end); e: E; end:` |
| `C *-> T ; E` | as `->`, but instead of `CutSlot(s)` a `SoftCut(s)` disarms the `Choice(e)` entry — turns it into a `MarkThrough` — so `C`'s other solutions survive and `E` can no longer run |
| `\+ G` | `Mark(ok); G; Unmark; Fail; ok:` |
| `!` | `CutClause` — back to `frame.cut` |
| `call(G)`, `call(G, A…)` | a new frame for `G`, so a cut inside `G` is local to it |
| `catch(G, C, R)` | a `Catch` entry around `G` (below) |
| `true`, `fail`, `false` | nothing, `Fail`, `Fail` |

**A cut is transparent through `,`, `;` and `->` and opaque through `call/N`**, which is ISO and
falls out of where the barrier is kept: `CutClause` reads the frame, and `,`, `;` and `->` make no
frame, while `call/N` does.

### Dynamic predicates

**`assertz`, `asserta` and `retract` change a predicate's clause list, and a call sees the list as
it was when the call began** — the logical update view, which ISO requires. Each clause carries the
generation at which it was added and the generation at which it was retracted; a call records the
generation when it starts and skips clauses outside it. A clause added by `assert` is compiled when
it is added, so a dynamic predicate runs the same instructions a static one does.

### Meta-calls

`call(G)` with `G` an atom or a simple compound is a lookup of `name/arity` and a `Call`. A goal
containing control constructs, `call((A ; B))`, is compiled into a small chunk on the spot and run in
a new frame. Nothing is cached until a measurement says it should be.

## The term mapping

**Most kinds are shared outright**: a Prolog atom is a FunL atom, an integer is an integer from the
same tower (unbounded on both sides), a float is a real, a string is a string, a list is a list, a
compound term is a compound — and a FunL `data` record *is* a compound, so `point(1, 2)` built in
FunL unifies with `point(X, Y)` in Prolog. A rational from FunL is a number to Prolog arithmetic as
well, as SWI-Prolog's rationals are. `true` and `false` are atoms on both sides.

> **Decided — the term mapping for FunL-only kinds.**
> **Decision:** a **tuple** `(a, b)` is the compound `tuple(a, b)`, so a FunL function
> answering a pair can be destructured by a Prolog head; **arrays, buffers, maps, sets, closures and
> cursors are opaque** — they unify only with themselves and print as `<map>`, `<function>` and so
> on — because their contents can change and a term that changes under a binding is not a term;
> **`()` and `undefined`** are two distinct constants that unify only with themselves.
> *Revised for immutable maps (user, 2026-10-09, [map patterns](language.md#map-patterns) M5):* an
> immutable map cannot change, so it unifies with another by content — the same keys, then the
> values unified. It still prints `<map>`; a mutable map stays opaque.
> **Rejected:** tuples as `','(a, b)` (the conjunction functor, which some systems use and every
> reader trips on); or converting maps to association lists at the boundary, which copies on every
> call and silently breaks identity.

## Calling FunL from Prolog

**A FunL function `f` of `n` parameters is the predicate `f/(n+1)`**, its last argument unified with
the result, each generated value one solution:

```
:- import("geometry.funl").

area_of(Shape, A) :- area(Shape, A).          % area/1 in FunL is area/2 here
squares(L) :- findall(Y, (between(1, 5, X), square(X, Y)), L).   % square/1 in FunL is square/2 here
```

**Prolog and FunL share one namespace keyed by name and arity**, and a function of `n` parameters
occupies the key `name/(n+1)`. A Prolog predicate and a FunL function that would occupy the same key
are refused when the second is loaded, naming both — the alternative is one silently shadowing the
other, and which one depends on load order. A file that loads again replaces what it defined itself,
as consulting a file again does, and a file imported twice is loaded once. The library written in
Prolog gives way to a program's own definition in either language. A builtin's key is never
redefined: a Prolog file's clauses for one are refused, as ISO's `permission_error(modify,
static_procedure, …)` has it, and so is a FunL export on one when a Prolog file imports the module:
the import raises the same permission error, naming the builtin's `Name/Arity`, and the module counts
as not imported. A FunL program's own definitions are not imported and stay FunL's own under
their names — Prolog keeps the builtin, and no load order can change which one either side sees.

A FunL function's arguments are dereferenced before the call and must be bound where its patterns
look inside them; an unbound one is an `instantiation_error` naming the function, raised as an
ordinary Prolog exception.

**From FunL, a Prolog predicate is a relation** and is called like one — `every parent(x, #mia) do
…` — with nothing to declare.

## Errors and exceptions

**`catch/3` pushes a `Catch` entry; `throw/1` copies the ball, then unwinds the control stack to the
nearest `Catch` whose catcher unifies with it**, undoing the trail on the way exactly as failure
would, and runs the recovery goal in the catch's frame. An uncaught ball reaching the top level is
printed with the goal that raised it.

**Every runtime error is an ISO error term**: `error(type_error(evaluable, foo/0), context(is/2,
_))`, `error(instantiation_error, …)`, `error(existence_error(procedure, foo/2), …)`. FunL's own
runtime errors are raised as the same terms, so a Prolog `catch` around a call into FunL catches a
FunL type error. FunL grows a `catch` of its own, slate's postfix `e catch err -> recovery` (decided,
user, 2026-10-08, in [modules](modules.md#questions-decided)); it uses the `Catch` entry already here.

## Arithmetic and order

**`is/2` evaluates with Prolog's operator meanings over FunL's tower.** The difference that matters
is `/`: in Prolog `7 / 2` is `3.5` (ISO; with the `prefer_rationals` flag, `7r2`), where FunL's
`7 / 2` is the rational `7/2`. Each syntax compiles its own `/`; the numbers are the same numbers.
`//`, `mod`, `rem`, `div`, `abs`, `sign`, `min`, `max`, `gcd`, `msb`, `**`, `^`, the bit operators,
`sqrt`, the trigonometric and exponential functions, `float`, `integer`, `truncate`, `round`,
`ceiling`, `floor` and `random` are the set.

**An unbound variable inside an arithmetic expression is an `instantiation_error`**, and an atom
that is not an evaluable is a `type_error(evaluable, Name/Arity)`.

> **Decided — infinities and NaN (user, 2026-10-08).**
> **Decision:** Prolog follows SWI-Prolog's three flags. `float_overflow` (`error` | `infinity`),
> `float_zero_div` (`error` | `infinity`) and `float_undefined` (`error` | `nan`) each default to
> `error`, ISO's behaviour: `evaluation_error(float_overflow)`, `zero_divisor` and `undefined`. Set,
> arithmetic answers the infinity or the NaN, written as SWI-Prolog writes it — `1.0Inf`, `-1.0Inf`,
> `1.5NaN` — and the reader reads those back as floats; the evaluable atoms `inf` and `nan` are the
> two values whatever the flags say. Integer division by zero stays `zero_divisor`, and rounding an
> infinity or a NaN to an integer stays `evaluation_error(undefined)` (where SWI-Prolog answers the
> float itself, which is no integer). FunL's own arithmetic is separate and unchanged.
> **Rejected:** IEEE results always (ISO conformance is the default here); a single on/off switch
> (SWI-Prolog's three flags are what a program written for it sets).

**Standard order of terms** — what `@<`, `compare/3`, `sort/2` and `msort/2` use — is: variables
(by age), then numbers (by value; an integer and a float of equal value put the float first), then
strings (by text), then atoms (by text), as SWI-Prolog orders them, then compounds (by arity, then name, then arguments left to
right). FunL-only kinds come after compounds, in a fixed order by kind and then by identity, so a
sort of mixed terms is total and repeatable.

## Input and output: streams

**Prolog's I/O is ISO's stream model (13211-1 §7.10, §8.11–8.13) built over `funl:fs`'s file
handles.** A stream term *is* a `file` handle (`native_file.sysl`): opaque, written `<file handle>`
(`<closed file handle>` once closed), unifying only with itself. ISO leaves the stream term to the
implementation; SWI-Prolog's is `<stream>(0x…)`, equally unreadable. `user_input`, `user_output`
and `user_error` are the machine's standard handles, the very ones FunL's `&input`, `&output` and
`&errout` name, so Prolog and FunL output reach standard output in the order they were written, and
`user_input`'s lookahead is the machine's own pending input, shared with FunL's `read()` and
`reads(n)`. No second file layer: Prolog opens, reads, writes, flushes, seeks and closes through the
handle's file; what Prolog keeps beside it is the stream's record (`pl_streams.sysl`) — aliases,
mode, type, `eof_action`, whether it repositions, its file name, its position, whether it is past its
end, and the characters (or the byte) read from the file and not yet taken.

**A stream argument is a stream or an alias.** Unbound is `instantiation_error`; an atom that is no
alias, a closed stream, or a `file` handle Prolog did not open is `existence_error(stream, S)`;
anything else is `domain_error(stream_or_alias, S)`. Where ISO says *stream* and not *stream or
alias* — `current_input/1`, `current_output/1`, `stream_property/2` — anything but a variable or a
stream is `domain_error(stream, S)`, an alias atom included (SWI-Prolog takes the alias there and
raises `existence_error` for any other atom; ISO is followed). The aliases `user_input`,
`user_output` and `user_error` are permanent; `alias(A)` of `open/4` adds one, several are allowed,
and `close` releases them.

**`open(Source, Mode, S)` and `open/4`.** `Source` is an atom or a string naming a file (SWI-Prolog
takes both; ISO leaves the sources to the implementation); `Mode` is `read`, `write` (made, or
emptied) or `append`. The options are ISO's four: `type(text|binary)`, `reposition(Bool)`,
`alias(Atom)` and `eof_action(error|eof_code|reset)`. Their defaults are implementation-defined, and
are SWI-Prolog's: `type(text)`, `eof_action(eof_code)`, and `reposition(true)` for a file opened to
read or write. A file opened to append does not reposition — every write goes at its end — so
`reposition(true)` there is `permission_error(open, source_sink, reposition(true))`. The errors, all
ISO's and checked before anything is opened: an unbound `Source`, `Mode`, option list, list tail or
option is `instantiation_error`; a `Mode` that is no atom `type_error(atom, M)`, one that is no mode
`domain_error(io_mode, M)`; options that are no list `type_error(list, O)`; anything else in it, an
option of a known name with an argument it does not take included, `domain_error(stream_option, O)`
(SWI-Prolog ignores an unknown option and type-checks the argument; ISO is followed); a bound `S`
`uninstantiation_error(S)` (Cor. 2); a `Source` that is not an atom or string
`domain_error(source_sink, Source)`; an alias already in use `permission_error(open, source_sink,
alias(A))`; no such file `existence_error(source_sink, Source)`; a file that may not be opened
`permission_error(open, source_sink, Source)`. `stream_property(S, file_name(F))` is the absolute
path, as in SWI-Prolog.

**`close(S)` and `close(S, Options)`** write out what the stream holds and close the handle. Closing
the current input or output makes `user_input` or `user_output` current again; closing a standard
stream does nothing, as ISO has it. The one option is `force(Bool)`; `force(true)` drops an error the
close meets. Anything else is `domain_error(close_option, O)`.

**Stream selection**: `current_input/1`, `current_output/1`, `set_input/1`, `set_output/1`,
`flush_output/0-1`. `set_input` of an output stream is `permission_error(input, stream, S)`;
`set_output` and `flush_output` of an input stream `permission_error(output, stream, S)`.

**`stream_property(S, P)`** enumerates every open stream's properties, in the order `file_name(F)`
(files only), `mode(M)`, `input` or `output`, each `alias(A)`, `position(P)`, `end_of_stream(E)`
(input streams only), `eof_action(A)`, `reposition(B)`, `type(T)`. A `P` that is not one of these
forms is `domain_error(stream_property, P)`. The standard streams are `type(text)`,
`reposition(false)`, and `eof_action(reset)` for `user_input` — so a program reads on after the
terminal's end-of-file, as SWI-Prolog's does — and `eof_action(eof_code)` for the other two.

**Positions** are SWI-Prolog's term, `'$stream_position'(Chars, Line, LinePos, Bytes)`: characters
and bytes taken from the start, the line counted from 1, and the characters into the line. Every
stream keeps one; `set_stream_position(S, P)` moves a repositionable stream — a file's — back to a
position its `position/1` property gave, dropping the lookahead and anything past its end. A `P`
that is no position term is `domain_error(stream_position, P)`; a stream that does not reposition is
`permission_error(reposition, stream, S)`.

**End of stream is ISO's three states.** A stream is `not` at its end while it has more to give,
`at` its end once the next read would find none, and `past` its end once a read has answered
`end_of_file` (`-1` for a code or a byte). A file looks ahead to tell `at` from `not`; `user_input`
says `not` until a read finds the end, since looking would wait for the terminal (ISO leaves when to
look to the implementation; SWI-Prolog does not look either). `at_end_of_stream/0-1` does look, and
waits. A read past the end does what the stream's `eof_action` says: `error` raises
`permission_error(input, past_end_of_stream, S)`, `eof_code` answers the end again, `reset` reads on.
`at_end_of_stream` of an output stream is `permission_error(input, stream, S)`, as in SWI-Prolog (ISO
gives an output stream no `end_of_stream` property and says nothing of the call).

**Characters are Unicode, read and written as UTF-8**, as the rest of FunL's Prolog is: `get_char`,
`peek_char` and `put_char` take one-character atoms, `get_code`, `peek_code` and `put_code` code
points. A text stream's bytes that are not UTF-8 are `representation_error(character)`. Bytes are
`get_byte`, `peek_byte`, `put_byte` on a binary stream, `0..255`. A text read of a binary stream is
`permission_error(input, binary_stream, S)`, a byte read of a text stream `permission_error(input,
text_stream, S)`, and the same with `output` for the writes; reading an output stream is
`permission_error(input, stream, S)` and writing an input stream `permission_error(output, stream,
S)`. The argument checks are ISO's, stricter than SWI-Prolog's, which fails where ISO raises: an
input argument that is neither unbound nor a character or `end_of_file` is `type_error(in_character,
C)`; one neither unbound nor an integer `type_error(integer, C)`, and an integer that is neither a
code nor `-1` `representation_error(in_character_code)`; a byte argument that is no byte or `-1`
`type_error(in_byte, B)`. On the way out an unbound argument is `instantiation_error`; `put_char` of
anything but a character `type_error(character, C)`; `put_code` of a non-integer `type_error(integer,
C)` and of an integer that is no code `representation_error(character_code)`; `put_byte` of anything
but a byte `type_error(byte, B)`.

**The one-argument forms are the current streams'.** `write/1` and its family, `nl/0`, `tab/1`,
`put_char/1`, `format/1-2` write to the current output; `read/1` and `read_term/2` read from the
current input, a line at a time until the lines taken hold the `.` that ends a clause. The end
token's layout character is taken with it, and the rest waits for the next read; a read that does
not parse consumes what it took.

**Term I/O on a named stream (ISO §8.14).** `write/2`, `writeq/2`, `print/2`, `write_canonical/2`,
`write_term/3`, `read/2` and `read_term/3` take a stream or alias first and do what their shorter
forms do on it; the shorter forms *are* these on the current stream, one code path each. The stream
argument raises what every stream argument raises (above); term output to an input stream is
`permission_error(output, stream, S)`, to a binary stream `permission_error(output, binary_stream,
S)`; a term read from an output stream `permission_error(input, stream, S)`, from a binary stream
`permission_error(input, binary_stream, S)`, and past the end of an `eof_action(error)` stream
`permission_error(input, past_end_of_stream, S)`. ISO lists a builtin's errors without ordering them;
the options are checked before the stream, as the one-argument-shorter forms already did. `print/2`
is `print/1`'s output — quoted, `'$VAR'(N)` as a variable name — since FunL has no `portray/1`.

**Character conversion (ISO §8.14.5–6, flag §7.11.2.1).** The machine keeps one table of
conversions, empty at the start. `char_conversion(In, Out)` makes the reader read `In` as `Out`;
`char_conversion(C, C)` removes `C`'s conversion. `current_char_conversion(In, Out)` enumerates the
conversions in the order they were made, only those where `In` and `Out` differ — so a character
with none is not reported converted to itself (GNU Prolog's reading; SWI-Prolog enumerates every
character). The errors are ISO's, which differ between the two: `char_conversion` of an unbound
argument is `instantiation_error`, and of anything but a one-character atom
`representation_error(character)`; `current_char_conversion` of a bound argument that is no
one-character atom is `type_error(character, C)` (SWI-Prolog and GNU Prolog raise the type error for
both; ISO is followed). **The table applies only while the `char_conversion` flag is `on`**; its
values are ISO's `on` and `off` (SWI-Prolog's are `true` and `false`), and its default, which ISO
leaves to the implementation, is `off`. Turning it off keeps the table.

**What the reader converts** is every character of Prolog text — a file consulted, a term read, a
top-level query, the text of `term_to_atom/2` and its kin — **except within a quoted token**:
single-quoted atoms, double- and back-quoted text, the character of `0'c`, and comments. A character
the table turns into a quote opens a quoted token as the quote would. GNU Prolog also converts within
double quotes and after `0'`; ISO's rule is that a quoted token is read as written, and it is
followed. The conversion changes no position: a diagnostic points into the text as written. While a
file loads, a directive that is `char_conversion/2` or `set_prolog_flag(char_conversion, _)` takes
effect at once, for the clauses after it, as `op/3` and `double_quotes` do (a conjunction holding
one runs only in its place, as one holding `op/3` does); the flag is put back when the
file has been read and set again in its place among the directives, as `double_quotes` is, while the
table is not put back (the table is the machine's, as the operator table is).

**`discontiguous/1` (ISO §7.4.2.3)** takes what `dynamic/1` takes — `Name/Arity`, a conjunction
`a/1, b/2` or a list `[a/1, b/2]` — with `dynamic/1`'s errors for each indicator. It declares nothing
FunL needs: the loader gathers a predicate's clauses wherever they stand in the file, and gives no
warning for clauses that are not together, so there is no warning to suppress. It is a directive and
a goal alike, as in SWI-Prolog.

**`ensure_loaded(File)` (ISO §7.4.2.8)** consults `File` unless this machine has already loaded it —
by `consult/1` or by `ensure_loaded/1` — and is a no-op if it has. Files are told apart by their
absolute path (the working directory before a relative one) as the filesystem resolves it — every
`.`, `..` and symbolic link taken out — so `./a.pl`, `a.pl`, `sub/../a.pl` and a link to it are one
file; a path to no file has its `.` and `..` parts and repeated separators taken out by string
surgery instead. `consult/1` loads a file again whether
or not it was loaded, as before; a file whose load was refused is not counted as loaded. Its errors
are `consult/1`'s. It is a directive, run in its place among a file's directives, and a goal alike.

**A stream to memory** is what `with_output_to/2` and `format/3` write into: a stream record whose
handle is a `file` handle with no file behind it (every file method finds its file gone), and which
keeps what is written in the record (`PlStream.held`). It is the current output while the goal runs,
so `current_output/1` names it and every output builtin writes to it, the column `format/2` counts
from included. `with_output_to(Sink, Goal)` runs `Goal` once; on success the text is unified with
the sink — `atom(A)`, `string(S)`, `codes(Cs)`, `codes(Cs, Tail)`, `chars(Cs)`, `chars(Cs, Tail)` —
and on success, failure and a throw alike the current output is put back. A stream or an alias is a
sink too (SWI-Prolog takes one): the goal runs with it the current output. `format(Out, F, A)` is
`format(F, A)` run so, onto a stream or into a sink. Their errors are SWI-Prolog's: an unbound sink
`instantiation_error`, an atom that is no alias `existence_error(stream, A)`, an input stream
`permission_error(output, stream, S)`, any other term `type_error(output, T)`.

## Cleanup

**`setup_call_cleanup(Setup, Goal, Cleanup)` runs `once(Setup)`, then `Goal`, and runs `Cleanup`
exactly once when `Goal` is finished with**: it exits leaving nothing to resume, it fails, it
raises, or the choice points it left are cut away — by `!`, or by the end of `once/1`, `\+`,
`->`, `findall/3` and the like. `call_cleanup(Goal, Cleanup)` is the same with no setup. These are
SWI-Prolog's rules, and its answers are matched case by case (`tests_prolog_cleanup.sysl`).

**The goal runs under a `Cleanup` entry on the control stack**, in a chunk of its own
(`'$call_cleanup'/2`, `cleanup.sysl`), as `catch/3`'s goal runs under a `Catch`:

    Frame(1); SaveHeight(0); PushCleanup(f); LoadArg(0); CallGoal(0); ExitCleanup(0); Return
    f: PushConst('$cleanup_once'); LoadArg(1); CallGoal(1); Pop; Fail
    t: PushConst('$cleanup_quiet'); LoadArg(1); CallGoal(1); Pop; ResumeThrow

Each way the goal can be finished with reaches the entry its own way, and the machine needed one new
mechanism for the third:

- **Failure** pops the entry as it pops a `Choice`, and resumes at `f`: the cleanup, then failing on.
  The trail is undone to the entry's height first, so the cleanup sees none of the goal's bindings.
- **A throw** unwinding past the entry sets itself aside — the ball, the frame it was thrown in, the
  traps it had passed — and resumes at `t`; `ResumeThrow` takes it up again exactly where it
  stopped, so the cleanup's frame cannot catch what the thrower's callers could not. Bindings are
  undone here too.
- **A commit** — `cut_to`, which every cut, every end of a bounded expression and `ExitCleanup`
  itself go through — queues the cleanup goal of each `Cleanup` entry it discards, the innermost
  first. **The instruction loop runs the queue before the next instruction**: it calls a small
  runner chunk in a frame of its own, whose `ReturnBare` returns to that next instruction leaving
  nothing on the operand stack (or, where the instruction failed, fails on). The commit keeps the
  goal's bindings, and so does the cleanup. A query the top level stops at is committed in `settle`,
  outside the loop, so it runs the queue as a run of its own.

`ExitCleanup` decides "nothing to resume": when no `Choice` or `Mark` stands above the entry, it
commits the entry away. On backtracking into the goal it runs again, so a goal whose last solution
is deterministic is cleaned up at that exit, as in SWI-Prolog. After an exit, a failure or a commit
the cleanup runs as `once/1` would, its failure ignored and its error raised in the goal's place;
after a throw its error is ignored and the goal's ball goes on. The goal and the cleanup are checked
before the setup runs: an unbound one is `instantiation_error`, one that is no callable term
`type_error(callable, G)`; the setup's errors are `once/1`'s.

**`setarg/3` changes a compound in place, undone on backtracking** (a `Setarg` trail entry, undone
like a binding), and `nb_setarg/3` stores a copy for good. **A term a stored clause shares with every
call is refused**: `assert/1` keeps a clause's ground parts as one term that every call answers
rather than copies (`pl_db.sysl`), so changing such a term would change the clause. Those terms are
marked shared (`CompoundObj.shared`), and `setarg/3` on one, or on a FunL record, is
`permission_error(modify, shared_term, T)`; a `copy_term/2` of it may be changed. SWI-Prolog copies
a clause's terms on every call, so the case does not arise there.

## The builtin set

**ISO core first, then the SWI-Prolog library predicates programs actually reach for.** Grouped as
the standard groups them:

| group | predicates |
|---|---|
| control | `call/1-8`, `not/1`, `\+/1`, `once/1`, `ignore/1`, `forall/2`, `catch/3`, `throw/1`, `halt/0-1`, `between/3`, `repeat/0`, `setup_call_cleanup/3`, `call_cleanup/2` |
| unification and comparison | `=/2`, `\=/2`, `unify_with_occurs_check/2`, `==/2`, `\==/2`, `@</2` and the other three, `compare/3` |
| type tests | `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `float/1`, `atomic/1`, `compound/1`, `callable/1`, `is_list/1`, `string/1`, `ground/1` |
| terms | `functor/3`, `arg/3`, `=../2`, `copy_term/2`, `term_variables/2`, `setarg/3`, `nb_setarg/3` |
| arithmetic | `is/2`, `=:=/2`, `=\=/2`, `</2`, `>/2`, `=</2`, `>=/2`, `succ/2`, `plus/3` |
| database | `assert/1`, `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`, `abolish/1`, `clause/2`, `dynamic/1`, `predicate_property/2` (`built_in`, `dynamic`, `static`, `defined`, `number_of_clauses(N)`) |
| all solutions | `findall/3`, `findall/4`, `bagof/3`, `setof/3` (with `^`), `aggregate_all/3` (`count`, `sum`, `max`, `bag`, `set`) |
| atoms and strings | `atom_codes/2`, `atom_chars/2`, `char_code/2`, `atom_length/2`, `atom_concat/3`, `sub_atom/5`, `atom_number/2`, `number_codes/2`, `atom_string/2`, `string_concat/3`, `string_chars/2`, `string_codes/2`, `split_string/4`, `sub_string/5`, `upcase_atom/2`, `term_to_atom/2`, `atomic_list_concat/2-3` (splitting where the list is not whole), `char_type/2`, `code_type/2` |
| lists (library) | `append/3`, `member/2`, `memberchk/2`, `length/2`, `nth0/3`, `nth1/3`, `reverse/2`, `last/2`, `msort/2`, `sort/2`, `sort/4`, `predsort/3`, `sum_list/2`, `max_list/2`, `min_list/2`, `max_member/2`, `min_member/2`, `list_to_set/2`, `exclude/3`, `include/3`, `maplist/2-5`, `foldl/4-6`, `nb_getval/2`, `b_getval/2` |
| input and output | `write/1-2`, `writeln/1`, `print/1-2`, `write_canonical/1-2`, `writeq/1-2`, `write_term/2-3`, `nl/0`, `tab/1-2`, `format/1-3`, `with_output_to/2`, `read_term/2-3`, `read/1-2`, `read_line_to_string/2`, `read_line_to_codes/2` |
| streams | `open/3-4`, `close/1-2`, `current_input/1`, `current_output/1`, `set_input/1`, `set_output/1`, `flush_output/0-1`, `stream_property/2`, `at_end_of_stream/0-1`, `set_stream_position/2` |
| character and byte I/O | `get_char/1-2`, `get_code/1-2`, `peek_char/1-2`, `peek_code/1-2`, `put_char/1-2`, `put_code/1-2`, `nl/1`, `get_byte/1-2`, `peek_byte/1-2`, `put_byte/1-2` |
| flags and operators | `op/3`, `current_op/3`, `set_prolog_flag/2`, `current_prolog_flag/2`, `char_conversion/2`, `current_char_conversion/2` |
| grammar rules | `-->` translation at load time, `phrase/2-3` |
| loading | `consult/1`, `ensure_loaded/1`, `discontiguous/1` |
| system | `statistics/2` (`runtime`, `cputime`, `process_cputime`, `walltime`, `real_time`, `epoch`; FunL counts no inferences, so there is no `inferences`), `get_time/1` |

**The probe** (`tests_prolog_probe.sysl`) calls every ISO builtin (13211-1 and its corrigenda) and
every predicate of this table with fresh arguments, and fails naming any that raises
`existence_error(procedure, _)`; a predicate added here is added to its list.

**The library predicates are written in Prolog** wherever that is natural (`append`, `member`,
`reverse`, the `maplist` family), loaded from a source file carried in the binary, so they are
ordinary clauses a reader can look at. A predicate that needs the machine — `findall`, `copy_term`,
`assert`, arithmetic, the I/O — is native.

## Failure modes of a Prolog engine, and what rules each one out

Every row is a regression test named for the failure, written before the feature it guards.

| failure mode | its usual cause | what in this design prevents it |
|---|---|---|
| a cut leaks into the caller | cut and mark points in global registers | the barrier lives in the frame (`frame.cut`) or a slot; nothing else can write it |
| a nested `->` corrupts the outer one | the same global register, overwritten by the inner | each `->` saves its barrier in its own slot |
| `call/1` behaves as `once/1` | the meta-call was bounded | `call/1` is a frame with its own cut barrier and no mark; its choice points survive the return |
| `==` fails on compound terms | identity compared instead of structure | `==` walks both terms; two variables are `==` only if they are the same variable |
| `X == X` fails | the variable compared to its dereferenced self | `==` dereferences both sides first, and a variable is identical to itself |
| `X = X` loops | binding a variable to itself made a cycle | unification tests identity before anything else |
| no standard order of terms | not implemented | `compare/3` implements the order above, and `sort/2` and `@<` use it |
| `1 = 1.0` succeeds | numbers compared by value | unification compares kinds first |
| an unbound variable in arithmetic overflows the stack | evaluation recursed on an unbound variable | evaluation dereferences and raises `instantiation_error` |
| `5 is 2+3` is a compile error | the compiler expected a variable on the left of `is` | `is` unifies its left side with the result; a number there is an ordinary comparison |
| `findall` is a stub | not implemented | `findall` is native, with the copy taken in an operand cell |
| `asserta` does nothing | not implemented | `asserta`/`assertz` share one clause store, with generations |
| no `catch`, `retract`, `assertz`, `bagof`, `setof` | not implemented | all in the builtin set above |

## How conformance is checked

**The INRIA ISO conformance suite (`inriasuite`) is the oracle for the builtins**: it is a file of
goals with their expected outcomes — solutions, failure, or a specific error term — written against
the standard, by people who did not write this implementation. It runs as a suite here once the
reader and the core builtins exist, and every expected outcome it records is either met or listed,
with the reason, in a file the test reads.

**The classic benchmark programs** — naive reverse, queens, the dev branch's `classic.prolog` — are
run for their answers rather than their speed, and the family tree runs in both syntaxes with one
expected output.
