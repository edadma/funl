---
title: Icon parity
weight: 58
---

# Icon parity

**FunL does everything Icon 9.5 does** ([decided](language.md#icon-parity), user, 2026-10-14). This
chapter holds only what of Icon is not built yet: files, and the three keywords that name the
standard streams as files. Everything else Icon has — its control structures, procedures,
co-expressions, scanning, strings, csets, structures, numbers, keywords, errors and preprocessor —
is built, with the decisions behind it in [The language § Icon parity](language.md#icon-parity) and
the language itself in the [reference](../reference/README.md). When this is built too, the chapter
goes.

## Input and output

**Icon:** `open(s, mode)` opens a file (`"r"`, `"w"`, `"a"`, `"b"` read and write, `"c"` create,
`"t"`/`"u"` translated or not, `"p"` a pipe to or from a shell command) and **fails** when it cannot.
`read(f)` is the next line without its newline, failing at the end; `reads(f, n)` the next `n`
characters; `!f` generates the lines. `write(x…)` and `writes(x…)` write their arguments — to the
file an argument names, else `&output` — with or without a newline. `seek(f, i)`, `where(f)`,
`flush(f)`, `close(f)` (a pipe's exit status). `&input`, `&output`, `&errout` are the standard
files. `getch`, `getche` and `kbhit` read the keyboard unbuffered.

**FunL today:** whole files and the lines of a file, in `funl:fs`; the standard streams through
`read`, `reads`, `write`, `writes`, `ewrite`, `ewrites` and `stop`
([streams](../reference/streams.md)); no open file.

**To build** (milestone 11, item 9):

- **`open(path, mode)` in `funl:fs`** answers a file `Handle` or fails, as `funl:fs` fails "for no
  such thing" ([decided](modules.md#questions-decided)); a mode string as Icon's, `"p"` included,
  run over `funl:process`. A file is a `Handle` with the methods `f.read()`, `f.reads(n)`,
  `f.write(x…)`, `f.writes(x…)`, `f.seek(i)`, `f.where()`, `f.flush()`, `f.close()`, and `!f`
  generates its lines. Positions in `seek` and `where` are Icon's, 1 before the first byte.
- `getch()`, `getche()` and `kbhit()` in a new `funl:term`, which puts the terminal in raw mode
  around each call.

> **Decided (user, 2026-10-15) — I17, writing to a file: a handle's method, `f.write(x)`**, as every
> handle in FunL is used, and `write` stays "print these". It is how `funl:sqlite`'s handles already
> read, where Icon's rule — a file among `write`'s arguments redirects the rest — would change what
> `write(h)` prints for every handle.

## Keywords

| keyword | FunL |
|---|---|
| `&input`, `&output`, `&errout` | adopted: the three standard streams as the file handles [item 9](#input-and-output) makes |
