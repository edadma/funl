---
title: funl:process
weight: 20
---

# `funl:process` — this program, and other programs

`funl:process` gives a program what it was started with — its arguments and its environment — lets
it end with a status, and runs other programs: `run` waits for one and gives back what it wrote and
how it ended, and `run_lines` generates what it wrote a line at a time. The programs on this page run
from the root of the FunL repository and start programs every POSIX system has, `/bin/echo`,
`/bin/sh` and `/bin/ls`.

## Importing it

`import { ... } from funl:process` takes the names it lists:

```funl
import { run } from funl:process

r = run( "/bin/echo", ["hello", "world"] )
write( r.status )
write( r.out )
```

```output
0
hello world

```

`import * as process from funl:process` reaches every name through `process`:

```funl
import * as process from funl:process

write( process.run("/bin/echo", ["through", "the", "module"]).out )
```

```output
through the module

```

The names are the module's, not builtins: in a file that does not import `run`, there is no `run`.

```funl
write( run("/bin/echo") )
```

```error
`run` is not defined
```

## The program's arguments

`args` is the list of what followed the program's name on the command line, each a string:
`funl report.funl june "all regions"` gives `["june", "all regions"]`. It is a value, not a
function. The programs on this page are started with no arguments, so here it is empty:

```funl
import { args } from funl:process

write( args )
for a <- args do write( a )
```

```output
[]
```

```funl
import { args } from funl:process

write( args() )
```

```error
`args` is a value, and is used without calling it
```

## The environment

`env(name)` is what the variable `name` is set to. **A variable that is not set is failure**, so `if`
tests for one and `|` gives a default:

```funl
import { env } from funl:process

if env( "PATH" ) then write( #path_is_set )
write( env("FUNL_NOT_SET_ANYWHERE") | "(unset)" )
```

```output
path_is_set
(unset)
```

A value that is not UTF-8 text fails the same way.

## Ending the program

`exit(status)` ends the program at once, with `status` — 0 to 255 — as its exit status. What it
wrote before is kept; nothing after runs, and `catch` does not stop it.

```funl
import { exit } from funl:process

write( "checking" )
exit( 3 )
write( "not reached" )
```

```output
checking
```

Without `exit`, a program that ran ends with 0, and one that was refused or faulted with 1.

## Running a program

`run(program, args)` starts `program` with the list of strings `args`, waits for it to end, and gives
a map. `program` is found on `PATH` when it names no directory, and `args` may be left out.

- `out` is what the program wrote to its standard output, and `err` what it wrote to its standard
  error, both captured whole.
- `status` is its exit status, where it exited.

```funl
import { run } from funl:process

r = run( "/bin/sh", ["-c", "echo partial; echo trouble >&2; exit 2"] )
write( r )
```

```output
{"out": "partial
", "err": "trouble
", "status": 2}
```

**A program that ran is an answer whatever its status**: a non-zero `status` is a value to test, not
a failure of `run`. **Nothing goes through a shell**: each argument reaches the program exactly as it
is written, spaces, `*`, `$` and `;` included. A program that wants the shell runs `/bin/sh -c`, as
above.

```funl
import { run } from funl:process

write( run("/bin/echo", ["*", "two words", '$HOME', "a; echo b"]).out )
```

```output
* two words $HOME a; echo b

```

### How it ended

Of `status`, `signal` and `timed_out`, the map holds **only the one that happened**, so reading
another fails and `|` gives a default. `signal` is the number of the signal that killed the program;
`timed_out` is `true` for a program stopped for running past its `timeout`.

```funl
import { run } from funl:process

killed = run( "/bin/sh", ["-c", 'kill -9 $$'] )
write( killed.signal )
write( killed.status | "no status" )

slow = run( "/bin/sh", ["-c", "sleep 10"], {timeout: 200} )
write( slow.timed_out )
```

```output
9
no status
true
```

### Options

`run(program, args, options)` takes a map of options, each optional:

| option | what it does |
|---|---|
| `cwd` | the directory the program runs in |
| `env` | a map of variables added to this program's environment for it |
| `timeout` | how many milliseconds it may run before it is stopped; 0, the default, is no limit |

```funl
import { run } from funl:process

write( run("/bin/ls", [], {cwd: "docs/library/process"}).out )
write( run("/bin/sh", ["-c", 'echo $GREETING'], {env: {GREETING: "hello"}}).out )
```

```output
notes.txt

hello

```

A call gives one to three arguments, and the compiler refuses any other count:

```funl
import { run } from funl:process

write( run("/bin/echo", ["a"], {}, "extra") )
```

```error
`run` takes 1 to 3 arguments, and this call gives 4
```

## A program's output, line by line

`run_lines(program, args, options)` runs the program as `run` does and generates each line it wrote
to its standard output, without the `\n` or a `\r` before it. How it ended and what it wrote to its
standard error are not looked at.

```funl
import { run_lines } from funl:process

for line <- run_lines( "/bin/sh", ["-c", "echo one; echo two; echo three"] ) do write( line )
write( [name | name <- run_lines("/bin/ls", ["docs/library/fs"])] )
```

```output
one
two
three
["letters", "poem.txt"]
```

## What fails and what faults

**A program that is not there is failure**, for `run` and `run_lines` alike, so a program tests for
one with `if` or gives a default with `|`:

```funl
import { run } from funl:process

write( run("funl-no-such-program") | "not installed" )
if run( "funl-no-such-program" ) then write( #ran ) else write( #missing )
```

```output
not installed
missing
```

**Anything else that goes wrong is a fault**: a file that is there and may not be run raises
`permission_error(execute, source_sink, Program)`; any other error starting one raises
`system_error`; and a mistake in the call — an argument that is not a string, options that are not
a map, an option `run` does not have — raises the `type_error` or `domain_error` saying so. Nothing
caught, the fault stops the program:

```funl
import { run } from funl:process

write( run("docs/library/process/notes.txt") )
```

```error
'run' cannot start `docs/library/process/notes.txt`: permission denied
```

`catch` catches it, with the error term:

```funl
import { run } from funl:process

write( run("docs/library/process/notes.txt") catch e -> e )
write( run("/bin/echo", [], {colour: "red"}) catch e -> e )
```

```output
error(permission_error(execute, source_sink, "docs/library/process/notes.txt"), context(_G2, "'run' cannot start `docs/library/process/notes.txt`: permission denied"))
error(domain_error(process_option, "colour"), context(_G5, "'run' has no option `colour`: its options are `cwd`, `env` and `timeout`"))
```

## From Prolog

A Prolog file loaded by FunL reaches the module with `:- import("funl:process").` and calls it
qualified by the module's name. A function of `n` arguments is the predicate `process:f/(n+1)`, whose
last argument is unified with its value, so `run` is `process:run/2`, `/3` and `/4`, and `args` is
`process:args/1`. `run`'s map is opaque to Prolog; a generator gives one solution for each value:

```prolog
:- import("funl:process").

:- findall(L, process:run_lines("/bin/sh", ["-c", "echo one; echo two"], L), Ls), write(Ls), nl.
:- ( process:env("FUNL_NOT_SET_ANYWHERE", _) -> write(set) ; write(unset) ), nl.
:- ( process:run("funl-no-such-program", _) -> write(ran) ; write(missing) ), nl.
:- catch(process:run("docs/library/process/notes.txt", _), error(E, _), (write(E), nl)).
:- process:args(A), write(A), nl.
```

```output
[one,two]
unset
missing
permission_error(execute,source_sink,docs/library/process/notes.txt)
[]
```

## Every name

| name | what it is |
|---|---|
| `args` | the list of the program's arguments |
| `env(name)` | what variable `name` is set to; fails where it is unset |
| `exit(status)` | ends the program with `status` |
| `run(program, args, options)` | runs a program and waits for it: a map of `out`, `err`, and `status`, `signal` or `timed_out` |
| `run_lines(program, args, options)` | runs a program and generates each line of its output |
