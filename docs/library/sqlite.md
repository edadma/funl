---
title: funl:sqlite
weight: 20
---

# `funl:sqlite` — SQLite databases

`funl:sqlite` opens SQLite databases and runs SQL against them. It exports one name, `sqlite`; the
database it answers is a handle, and everything else is a method on that handle.

## Opening a database

`sqlite(path)` opens the database in the file at `path`, making the file when it is not there.
`sqlite(":memory:")` opens a database that lives only as long as its handle. `db.close()` closes
it, and closing a closed database does nothing.

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
write( db )
db.close()
write( db )
db.close()
```

```output
<sqlite database handle>
<closed sqlite database handle>
```

A database in a file keeps what was written to it after it is closed:

```funl
import { sqlite } from funl:sqlite
import { remove } from funl:fs

db = sqlite( "docs/library/notes.db" )
db.exec( "create table notes (title text)" )
db.run( "insert into notes values (?)", "kept" )
db.close()

again = sqlite( "docs/library/notes.db" )
write( again.query("select title from notes").title )
again.close()
remove( "docs/library/notes.db" )
```

```output
kept
```

A database the program forgets to close is closed by the collector once nothing reaches it.

## Running SQL

| method | what it does |
|---|---|
| `db.exec(sql)` | runs every statement in `sql`, in turn, and answers `()` |
| `db.run(sql, params...)` | runs one statement with its parameters, and answers a map of `changes`, the rows it changed, and `last_insert_rowid` |
| `db.query(sql, params...)` | generates the rows of one statement, each a map from column name to value |

Each `?` in the SQL is a parameter, filled from the arguments after the SQL, in order. A
parameter is never part of the SQL's text, so a value that holds a quote or a semicolon is only a
value.

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table notes (id integer primary key, title text); insert into notes (title) values ('first')" )
write( db.run("insert into notes (title) values (?)", "it's; second") )
write( db.run("update notes set title = upper(title)").changes )
db.close()
```

```output
{"changes": 1, "last_insert_rowid": 2}
2
```

## Rows

`query` is a generator: each value it gives is one row, and backtracking into it takes the next.
A row is an immutable map from each column's name to its value, so `row.title` reads a column.
**A query with no rows fails**, so a program tests for a row with `if` or gives a default with `|`.

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table notes (id integer primary key, title text)" )
db.run( "insert into notes (title) values (?)", "a note" )
db.run( "insert into notes (title) values (?)", "another" )

every write( db.query("select title from notes order by id").title )
write( db.query("select count(*) as n from notes").n )
if db.query( "select 1 from notes where title = ?", "x" ) then write( #found ) else write( #absent )
write( db.query("select title from notes where id > ?", 9) | "no such note" )
db.close()
```

```output
a note
another
2
absent
no such note
```

A row is a map, and `x <- e` draws the entries of a map one by one, so a binding that wants each
row whole puts the query in a list, `row <- [db.query(...)]`, as it would any generator of
collections:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table notes (id integer primary key, title text)" )
db.run( "insert into notes (title) values (?)", "a note" )
db.run( "insert into notes (title) values (?)", "another" )

for row <- [db.query("select id, title from notes order by id")] do write( row.id, row.title )
write( [row.title | row <- [db.query("select title from notes order by title desc")]] )
db.close()
```

```output
1, a note
2, another
["another", "a note"]
```

**Each call of `query` starts afresh**, with a statement of its own, so two queries over one table
nest:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table n (x integer); insert into n values (1); insert into n values (2)" )
write( [(a.x, b.x) | a <- [db.query("select x from n")], b <- [db.query("select x from n")]] )
db.close()
```

```output
[(1, 1), (1, 2), (2, 1), (2, 2)]
```

Closing the database ends every query still running over it:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table n (x integer); insert into n values (1); insert into n values (2); insert into n values (3)" )
for row <- [db.query("select x from n order by x")] do
  write( row.x )
  if row.x == 2 then db.close()
```

```output
1
2
```

## Values

| SQLite | FunL |
|---|---|
| `INTEGER` | an integer |
| `REAL` | a real |
| `TEXT` | a string |
| `BLOB` | bytes |
| `NULL` | `undefined` |

A parameter may be any of those, and `true` and `false` are bound as 1 and 0, which is how SQLite
keeps a boolean. What a column gives back is what the row holds, whatever the column was declared:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
write( db.query("select ? as i, ? as r, ? as s, ? as b, ? as n, ? as t", 7, 2.5, "text", bytes([0, 255]), undefined, true) )
db.close()
```

```output
{"i": 7, "r": 2.5, "s": "text", "b": bytes([0, 255]), "n": undefined, "t": 1}
```

## Faults

**Every error SQLite reports is a fault** — SQL that does not compile, a table that is not there, a
constraint broken, a file that will not open — raised as `system_error` with SQLite's own sentence.
Nothing caught, the fault stops the program:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
write( db.query("select x from nowhere") )
```

```error
'query' failed: no such table: nowhere
```

`catch` catches it, with the error term:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
db.exec( "create table t (id integer primary key)" )
db.run( "insert into t values (?)", 1 )
write( db.run("insert into t values (?)", 1) catch e -> e )
write( db.exec("not sql") catch e -> #refused )
```

```output
error(system_error, context(_G2, "'run' failed: UNIQUE constraint failed: t.id"))
refused
```

A parameter SQLite cannot hold is a `type_error`, an integer past 64 bits a `representation_error`,
the wrong number of parameters a `domain_error`, and a method called on a closed database an
`existence_error`:

```funl
import { sqlite } from funl:sqlite

db = sqlite( ":memory:" )
write( db.query("select ? as x", [1]) catch e -> e )
write( db.query("select ? as x") catch e -> e )
db.close()
write( db.exec("select 1") catch e -> e )
```

```output
error(type_error(sql_value, [1]), context(_G2, "'query' cannot bind the list [1]: SQLite holds integers, reals, strings, bytes and undefined"))
error(domain_error(sql_parameters, 0), context(_G5, "'query': this SQL takes 1 parameter and was given 0"))
error(existence_error(handle, <closed sqlite database handle>), context(_G8, "'exec' was called on the handle <closed sqlite database handle>"))
```
