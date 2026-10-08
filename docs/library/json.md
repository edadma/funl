---
title: funl:json
weight: 20
---

# `funl:json` — reading and writing JSON

`funl:json` reads JSON text into FunL values and writes FunL values as JSON text. It has two names:
`parse` and `stringify`.

## Reading

`parse(text)` is the value the JSON document `text` holds. An object becomes a map keyed by strings,
an array a list, and `null` is `undefined`:

```funl
import { parse } from funl:json

v = parse( '{"name": "funl", "tags": ["vm", "prolog"], "version": null}' )
write( v.name, v.tags(0), v.version )
write( v )
```

```output
funl, vm, undefined
{"name": "funl", "tags": ["vm", "prolog"], "version": undefined}
```

`true` and `false` are FunL's `true` and `false`. A number without a point or an exponent is an
integer, of any size; one with either is a real:

```funl
import { parse } from funl:json

write( parse('[true, false, 42, 123456789012345678901234567890, 2.5, 1e3, "caf\\u00e9"]') )
```

```output
[true, false, 42, 123456789012345678901234567890, 2.5, 1000.0, "café"]
```

The map an object becomes is immutable, and keeps its keys in the order the document gives them. A
key written twice keeps the last value.

## Writing

`stringify(value)` is `value` as compact JSON text, with no space anywhere. A map is an object, and a
list, a tuple, a range with an end, an array or a buffer is an array; `undefined` is `null`:

```funl
import { stringify } from funl:json

write( stringify({name: "funl", version: [0, 0, 2], released: undefined}) )
write( stringify((1, "two")) )
write( stringify([1/4, 2 ^ 70, 1..3]) )
```

```output
{"name":"funl","version":[0,0,2],"released":null}
[1,"two"]
[0.25,1180591620717411303424,[1,2,3]]
```

JSON has no fractions, so a rational is written as the nearest real. An integer of any size is
written with all its digits.

`stringify(value, indent)` lays the text out for a person to read: one member or element to a line,
`indent` spaces a level, from 0 to 16. An indent of 0 is the compact form. An empty object or array
stays on one line:

```funl
import { stringify } from funl:json

write( stringify({name: "funl", tags: ["vm", "prolog"], empty: []}, 2) )
```

```output
{
  "name": "funl",
  "tags": [
    "vm",
    "prolog"
  ],
  "empty": []
}
```

## What fails and what faults

**Text that is not JSON is failure**, so a program tests for it with `if` or gives a default with `|`.
The whole text must be one JSON value, with only whitespace around it:

```funl
import { parse } from funl:json

write( parse("{oops") | "(not JSON)" )
if parse( "[1, 2,]" ) then write( #read ) else write( #malformed )
```

```output
(not JSON)
malformed
```

**A value JSON cannot hold is a fault**: a function, a set, an atom other than `true` and `false`, a
map with a key that is not a string, an endless range, or a value that holds itself. A value of the
wrong kind raises `type_error(json, Culprit)`, which `catch` catches:

```funl
import { stringify } from funl:json

write( stringify({a: {1, 2}}) catch e -> e )
```

```output
error(type_error(json, {1, 2}), context(_G2, "'stringify' cannot write {1, 2} as JSON, which holds maps, lists, strings, numbers, booleans and undefined"))
```

Nothing caught, the fault stops the program:

```funl
import { stringify } from funl:json

write( stringify(x -> x) )
```

```error
'stringify' cannot write a function as JSON
```

## From Prolog

A Prolog file loaded by FunL reaches the module with `:- import("funl:json").` and calls it qualified
by the module's name: `json:parse(Text, Value)`, `json:stringify(Value, Text)` and
`json:stringify(Value, Indent, Text)`. Text that is not JSON makes `json:parse/2` fail:

```prolog
:- import("funl:json").

:- json:parse("{\"name\": \"funl\", \"tags\": [\"vm\"]}", V), json:stringify(V, T), write(T), nl.
:- json:stringify([1, 2], 2, T), write(T), nl.
:- ( json:parse("{oops", _) -> write(read) ; write(malformed) ), nl.
:- catch(json:stringify(foo, _), error(E, _), (write(E), nl)).
```

```output
{"name":"funl","tags":["vm"]}
[
  1,
  2
]
malformed
type_error(json,foo)
```
