---
title: funl:http
weight: 20
---

# `funl:http` — fetching a URL

`funl:http` is an HTTP client: `fetch` sends a request and answers the response once the whole of it
has arrived. It speaks HTTP and HTTPS, follows redirects, and reads `file://` URLs too, which is how
the programs on this page run without a network: they fetch files kept beside the page, under
[`http/`](http/), from the root of the FunL repository.

## Importing it

```funl
import { fetch } from funl:http
import { cwd, join } from funl:fs

r = fetch( "file://" + join(cwd(), "docs/library/http/greeting.txt") )

write( r.status )
write( r.body )
write( r.bytes.length )
```

```output
0
Hello from a file.

19
```

`import * as http from funl:http` reaches it as `http.fetch`. The name is the module's, not a
builtin: in a file that does not import `fetch`, there is no `fetch`.

```funl
write( fetch("file:///etc/hosts") )
```

```error
`fetch` is not defined
```

## The response

`fetch(url)` answers an immutable map, so `r.status` reads:

| key | what it holds |
|---|---|
| `status` | the HTTP status, an integer; `0` for a `file://` URL, which has none |
| `url` | where the response came from, which differs from the URL asked for after a redirect |
| `headers` | a map from each header's name, lower-cased, to its value |
| `body` | the body as text — absent when the body is not UTF-8 |
| `bytes` | the body as bytes, always |

**A status of 404 or 500 is a response like any other**: the server answered, and `r.status` says
what it said. **A header that repeats has its values joined with `", "`**, which HTTP allows for
every header but one: `set-cookie`, whose values carry commas of their own, is a list of the lines
as they arrived.

**A body that is not UTF-8 has no `body`**, so reading it fails, and `|` gives a default; `bytes` is
always there:

```funl
import { fetch } from funl:http
import { cwd, join } from funl:fs

r = fetch( "file://" + join(cwd(), "docs/library/http/four.bin") )

write( r.bytes )
write( r.body | "(not text)" )
write( decode(r.bytes(1..3)) )
```

```output
bytes([137, 80, 78, 71])
(not text)
PNG
```

## Options

`fetch(url, options)` takes a map of options, each optional:

| option | what it does |
|---|---|
| `method` | the method, a string or an atom in any case — `"PUT"`, `#delete`; `GET` when not given |
| `headers` | a map of request headers, each value a string or an integer |
| `body` | the request body, a string or bytes; a body with no `method` is a `POST` |
| `timeout` | how long the whole transfer may take, in seconds; 30 when not given |

A request to a server is written the same way as everything on this page, and is not run here,
since the page's programs reach no network:

```funl
import { fetch } from funl:http

r = fetch( "https://api.example.com/notes", {headers: {"Content-Type": "application/json"},
                                             body: '{"title": "a note"}', timeout: 5} )
if r.status == 201 then write( r.headers.location )
```

An option `fetch` does not know, and a body on a `GET`, are refused rather than ignored:

```funl
import { fetch } from funl:http

write( fetch("file:///x", {colour: "red"}) catch error(e, _) -> e )
write( fetch("file:///x", {method: "GET", body: "x"}) catch e -> e.message )
```

```output
domain_error(fetch_option, "colour")
'fetch' cannot send a body with GET
```

## What faults

**A request that cannot be done is a fault**: no network, a host that does not resolve, a connection
refused, a timeout, a certificate that does not check out, a `file://` file that is not there. Each
raises `system_error` with what went wrong, inside the usual `error(Formal, Context)` term; a URL
that is not one raises `domain_error(url, URL)`. Nothing caught, the fault stops the program:

```funl
import { fetch } from funl:http

write( fetch("file:///no/such/file.txt") )
```

```error
'fetch' cannot fetch `file:///no/such/file.txt`: Couldn't read a file:// file
```

`catch` catches it, so a program survives the network being down:

```funl
import { fetch } from funl:http

write( fetch("file:///no/such/file.txt") catch error(system_error, _) -> #unreachable )
write( fetch("htp://example.com") catch error(e, _) -> e )
```

```output
unreachable
domain_error(url, "htp://example.com")
```

## A build without it

`funl:http` is behind the `http` feature, because it links libcurl, which not every machine a `funl`
is built on has. A `funl` built with `--no-default-features` has no `funl:http`, and an import of it
says so — *`funl:http` is not in this build -- it is behind the `http` feature, so build with
`--features http`*. The released `funl` has it.

## From Prolog

A Prolog file loaded by FunL reaches the module with `:- import("funl:http").` and calls it
qualified, as `http:fetch/2` and, with options, `http:fetch/3`. The response is a map, which Prolog
sees as an opaque value to hand back to FunL:

```prolog
:- import("funl:fs").
:- import("funl:http").

:- fs:cwd(D), fs:join(D, "docs/library/http/greeting.txt", P), string_concat("file://", P, U),
   http:fetch(U, R), write(R), nl.
:- catch(http:fetch("file:///no/such/file.txt", _), error(E, _), (write(E), nl)).
```

```output
<map>
system_error
```
