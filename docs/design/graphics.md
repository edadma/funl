---
title: Graphics
weight: 59
---

# Graphics

**FunL has Icon 9.5's graphics facilities** — windows, drawing, text and fonts, colours and
palettes, images, events, clipping, several windows at once and the shared contexts of `Clone` and
`Couple` — as a native module, `funl:graphics` (user, 2026-10-15, question I21 of
[Icon parity](language.md#icon-parity)). The reference is Griswold, Jeffery and Townsend, *Graphics
Programming in Icon*. As with the rest of the parity work, **parity is of capability, not of
spelling**: an Icon graphics program has a FunL translation about as short that draws the same
picture and answers the same events.

The questions this chapter leaves open are G1 to G12, collected at the end and in the
[README](README.md#open-questions).

## What it stands on

**The drawing is `sysl-lang/plutovg`; the window on screen is `sysl-lang/sdl3`.** The two are
joined the way `sysl-lang/cairo-sdl3-demo` joins cairo to SDL: one buffer of ARGB32 pixels, drawn
by the vector library and copied into an SDL texture when the window is shown.

The org has four candidates for the drawing, and the survey chose by what Icon needs:

| package | what it is | for FunL |
|---|---|---|
| `plutovg` 0.2.1 | 2D vector graphics into memory; C vendored, links nothing; its own TrueType rasterizer; stb_image for loading; `write_png`; 38 pixel tests | **chosen**: no link line, so a hidden canvas (below) works in every build, tests included; text and image loading need no second binding |
| `cairo` 0.3.1 | the same drawing model, plus PDF, SVG and PostScript surfaces | needs `brew install cairo` and a link line; its extra surfaces are nothing Icon has |
| `sdl3`'s renderer | rectangles, lines, points, textures, `debug_text` | no arcs, curves, polygons, line widths, dashes or fonts beyond 8x8; drawing in a GPU target makes every `Pixel` a read-back |
| `syslui-sdl`, `imui` | widget toolkits | a layer above what Icon offers |

**`sdl3` 0.3.6 supplies everything a window needs**: `create_window`, title, position, size,
`raise`, the event queue with `wait_event_timeout`, `push_event`, keyboard, mouse and modifier state,
window events (`WindowResized`, `WindowExposed`, `WindowCloseRequested`), cursors, warping the
pointer, and a texture to present the frame through. **Its tests run on SDL's dummy video driver**,
which makes windows and renderers with no display, so a window test need not open one on screen.

### What the packages lack — findings for the sysl org

These are reported to the org, not routed around ([Rule 9](#closed-by-design)):

- **F2 — kairos has no way to combine two sources that each want to block.** A program that has a
  window on screen and an `async` task reading a file is waiting on libuv and on SDL's event queue at
  once. kairos's `Driver` has one `idle_until`; a driver that waits on SDL with a timeout and takes in
  libuv's finished work through the inner driver's `poll` is generic machinery, and belongs in
  kairos (a driver wrapping another with an extra source) rather than privately in FunL.
- **F3 — plutovg writes PNG only** (`write_png`; JPEG output is listed as not bound). Icon's
  `WriteImage` writes GIF, and on some builds JPEG and PNG. PNG is enough for parity of capability
  (see [G8](#images)); the finding is recorded so the gap is known, not because FunL waits on it.
- **F4 — plutovg cannot write a PNG into memory.** `plutovg_surface_write_to_png_stream` (and its
  JPEG twin) take a `plutovg_write_func_t` callback and a closure, and are in the vendored header and
  absent from `plutovg.sysl`, whose only PNG writer is `write_png(path)`. `image_bytes` waits on it;
  writing a scratch file and reading it back would be the workaround [Rule 9](#closed-by-design)
  forbids.

## The window

**The window handle, its canvas and context, `open` with `canvas: #hidden`, `clone`, `couple`,
`close`, `uncouple`, the attribute map and colour specifications are built** — see the library page,
[`funl:graphics`](../library/graphics.md). What remains of this section is for the later items.

**Setting `canvas: #normal` later shows a hidden canvas**, in a build that has windows (item 7); today
it faults, as opening one does.

### The attributes still to come

The map, its failures and its faults are built, with the attributes whose meaning needs nothing from
a later item. **Each later item adds its own attributes to the same table**, with Icon's meaning and
values in the form the library page gives (numbers `Int`, pairs tuples, `on`/`off` booleans,
enumerations symbols, the rest strings):

| attribute | item |
|---|---|
| `image` (a file loaded at open, the canvas sized to it) | 4, images |
| `pos`, `posx`, `posy`, `pointer`, `pointerx`, `pointery`; read-only `displaywidth`, `displayheight` | 7, windows on screen |

Not adopted: `display` (an X server's name), `gamma`, `iconpos`, `iconlabel`, `iconimage`,
`windowlabel` separate from `label` — they describe X11, and SDL has no meaning for them.

## Drawing

**The drawing methods, their coordinates, `linestyle`, `fillstyle`, `pattern`, `clip` and
`copy_area` are built** — see [the library page](../library/graphics.md#drawing). What remains is
for the later items:

| Icon | FunL |
|---|---|
| `DrawString(x, y, s)` | `w.draw_string(x, y, s)` — `y` is the baseline, as Icon's |
| `TextWidth(s)` | `w.text_width(s)` |
| `Raise()`, `Lower()` | `w.raise()`, `w.lower()` |
| `WDefault(prog, opt)` | not adopted: X resources ([out of scope](#out-of-scope)) |

**`drawop: #reverse` is built** ([G7](#the-questions-collected)) — see [the library
page](../library/graphics.md#drawing-in-reverse).

### Coordinates

**Coordinates on a hidden canvas are built** ([the library page](../library/graphics.md#coordinates),
G5 decided). **On a high-density display the canvas is measured in points, not device pixels** ([G6](#the-questions-collected)).
A window opened at `size: (400, 300)` is 400 by 300 points on screen; its surface has `density`
times as many pixels in each direction, and drawing is scaled by it, so lines and text are sharp on a
retina screen. `pixel(x, y)` answers the colour at the point's top-left device pixel, and
`write_image` writes the device pixels. A hidden canvas has `density` 1, always, so what a test
reads is the same on every machine.

### Colours

**Colour specifications, `color_value` and `pixel` are built** ([the library
page](../library/graphics.md#colours)), and so are `fg`'s `"transparent"` and `"opaque"` modifiers
([painting styles](../library/graphics.md#painting-styles)). The window itself has no alpha.

**Palettes are module functions**: `palette_chars(p)`, `palette_color(p, s)`, `palette_key(p, c)`,
`palette_grays(p)` over Icon's `c1`–`c6` and `g2`–`g256`, written in FunL and carried as source
([the native rule](modules.md#built-in-modules-funlname): only what FunL cannot do is native).

**`NewColor`, `FreeColor` and `Color`** — mutable colour-map entries — answer as Icon's do on a
true-colour display: `new_color` fails. Icon's book allows exactly this, and no display FunL runs on
has a colour map.

## Text and fonts

**Fonts, their metrics, `draw_string` and the window as a text terminal — `write`, `writes`, `read`
and `reads` with `echo` — are built**; see the library page,
[`funl:graphics`](../library/graphics.md#text-and-fonts).

## Images

| Icon | FunL |
|---|---|
| `ReadImage(s, x, y, p)` | `w.read_image(file, x, y)` — draws the file at `x, y`; fails when the file is missing or not an image |
| `WriteImage(s, x, y, wd, ht)` | `w.write_image(file, x, y, wd, ht)` — the rectangle (the whole canvas by default) as a PNG |
| `DrawImage(x, y, spec)` | `w.draw_image(x, y, spec)` — Icon's image strings, `"wd,palette,data"` and the bi-level `"wd,#hex"` form, `~` transparent and `\377` as Icon has them |
| `open(…, "image=f")` | the `image` attribute at open, sized to the image |

**Reading takes what plutovg's stb_image takes**: PNG, JPEG, BMP, GIF (its first frame), TGA, PSD,
HDR, PIC and PNM ([G8](#the-questions-collected)). **Writing is PNG** whatever the file is called; a
name ending in another known image extension faults with `domain_error(image_format, Ext)` rather than
writing PNG bytes under a misleading name (F3). `read_image`'s palette argument, which reduces an
image to an Icon palette, is not adopted: it exists for colour-mapped displays.

**`w.image_bytes(x, y, wd, ht)`** answers the same PNG as a [`Bytes`](modules.md#questions-decided)
value without a file — a FunL addition, small, and what a test or a program sending an image over
`funl:http` wants. **It waits on F4**: plutovg's binding cannot yet encode a PNG into memory.

## Events

**The queue, `enqueue`, `event()`, `pending()`, `active()`, the event codes and keywords, `&window`
and `inputmask` are built on hidden canvases** — see the library page's
[events](../library/graphics.md#events). What remains for windows on screen (item 7):

- **`event()` and `active()` block while no event is queued**, showing the canvas first (they
  [flush](#when-the-screen-is-updated)); on a hidden canvas, which nothing but `enqueue` fills, they
  fault instead.
- **The screen fills the same queue `enqueue` does**, under the same `inputmask`: SDL's keys (the
  symbols named for Icon's `keysyms.icn`), buttons, motion, `#wheelup` … `#wheelright`, and `#close`
  from the title bar (FunL's; Icon's book leaves it to the window manager — a program that never reads
  it keeps its window until it closes it).
- `WFlush(w)`, `WSync(w)` are `w.flush()`, `w.sync()`; `WDelay(w, i)` is `w.delay(ms)`, which
  flushes, then waits.

### Events and the loop

**`event()` blocks the machine that calls it, as `funl:fs`'s blocking forms do.** A program with no
`async` in it is Icon's program and needs nothing more.

**The promise form is a method, `w.next_event()`**, answering a promise of an **event map** —
`{code, x, y, row, col, interval, control, shift, meta, released, window}` — so a task awaits a click
while others run ([G11](#the-questions-collected)). It does not set the keywords: an awaited value is
settled as it stands ([the loop's rule](modules.md#questions-decided)), and a map read after the
`await` cannot be overwritten by another task's event, where a keyword could. `event()` and
`next_event()` take from the one queue, each event once, in order.

**The window's events reach FunL through kairos.** While a window is on screen the loop's driver waits
on SDL's queue (`wait_event_timeout`) as well as on its timers and on libuv, moves each SDL event onto
its canvas's queue (routed by window id), and settles the oldest `next_event()` promise waiting on
that canvas. **Nothing the loop calls runs FunL**, as before: the driver only queues and settles. The
driver that waits on two sources is F2.

**SDL's video calls are made on the process's main thread**, which is the thread FunL's instruction
loop and kairos run on; macOS refuses them from any other. FunL starts no thread for graphics.

### When the screen is updated

Drawing goes into the canvas's surface, and **the surface is shown — copied into the SDL texture and
presented — when the program waits**: at `flush()`, `sync()`, `delay()`, `event()`, `pending()`,
`active()`, `read()`, an `await`, and when the program ends with a window still open (it then waits
for the window to be closed, as Icon's `WDone` does, unless the program called `close()`). `sync()` is
`flush()` plus waiting until SDL has presented the frame. Between those, a hundred `draw_point`s
cost one copy, not a hundred — Icon's book gives exactly this latitude ("output may be buffered").

**A resize** keeps the canvas's pixels where they were, fills the new area with `bg`, and queues
`#resize`, as Icon's does; `resize: false` (the default, Icon's) stops the user from resizing.

## Testing without a screen

**A window cannot be opened from a session that is not at the console** — on this Mac anything that
starts `NSApplication` from a `Background` shell never returns (`~/CLAUDE.md`). So the suite must be
able to test every facility with no window on screen, and it can:

- **Drawing, text, images and contexts** are tested on `canvas: #hidden`, which is a plutovg surface
  and nothing else. A test draws, then reads back with `pixel` and compares colour strings at chosen
  points — that a fill stopped at the clip, that a dash left a gap, that `#reverse` drawn twice
  restores the pixel, that `"fixed"` at size 13 puts ink in a known cell — or compares
  `image_bytes()` of a small region against bytes held as a literal in the test. The surface is
  deterministic because plutovg is vendored at a fixed version and a hidden canvas has density 1.
- **Events** are tested on a hidden canvas through `enqueue`: the test queues a press, a drag and a
  release, and the program under test reads them with `event()` and `next_event()` and sees the
  keywords and maps Icon's would.
- **The on-screen path** — SDL window, texture, SDL events routed by window id, resize — is tested
  with SDL's dummy video driver (`SDL_VIDEO_DRIVER=dummy`, which `sysl-lang/sdl3`'s own sixty-four
  tests use): the window is made and drawn into memory, and SDL events are injected with
  `push_event`. Before the item that builds it starts, it proves the dummy driver opens from a
  `Background` shell on macOS without starting `NSApplication`; if it does not, that is reported
  before any test is written that would hang the gate.
- **Nothing in the suite opens a real window.** An example under `examples/` may, and is run by hand
  from a console shell (`launchctl managername` says `Aqua`).

## Closed by design

- **Nothing is worked around.** A capability a sysl package lacks (F1, F2) is a finding sent to the
  org and built there; the module does not reach past the binding into C.
- **Drawing never runs FunL inside the loop**, and the loop never runs inside a machine: SDL events
  are queued by the driver and read by FunL code, as timers are.
- **A closed canvas is closed for every handle on it.** A clone kept after `close()` faults on its
  next call rather than drawing into a freed surface.
- **Prolog reaching no window is not a gap**: Prolog sees a window as an opaque handle and cannot
  call its methods, as with [SQLite](modules.md#funlsqlite--core-over-sysl-langsqlite3); FunL is
  the language this module is for.

## Out of scope

- **Icon's program library**: the `vidgets`, VIB, the dialogs (`Notice`, `OpenDialog`,
  `TextDialog`, `ColorDialog`, `SelectDialog`, `ToggleDialog`) and the turtle procedures are Icon
  *programs*, in the IPL, not built-in facilities. Any of them can be written in FunL over this
  module and published as a package.
- **X11 specifics**: `WDefault` (X resources), `display`, `gamma`, icon attributes, colour maps
  (`new_color` fails, as on any true-colour display).
- **Writing GIF or JPEG** (F3); reading every frame of an animated GIF.
- **Unicon's additions** (3D graphics, `WSection`, textures, alpha in colours): FunL aims at Icon
  9.5, not Unicon.
- **Printing** and any non-raster output (cairo's PDF and SVG): not Icon's.

## The questions, collected

| | question | recommendation |
|---|---|---|
| G1 | methods on the window, or functions taking it (with an implicit `&window`) | **Decided (user, 2026-10-15)**: methods, as every handle has; `&window` kept as an assignable keyword, no implicit-window function forms |
| G2 | what `close()` closes | **Decided (user, 2026-10-15)**: the canvas, as `WClose`, closing every handle on it; `uncouple()` closes only the one |
| G3 | how attributes are read and set | **Decided (user, 2026-10-15)**: a map with natural values: `set(m)`, `get(name)`, `attributes()`; no `"name=value"` strings |
| G4 | the default font | **Decided (user, 2026-10-15)**: a permissively licensed monospaced face carried in the binary as `"fixed"` — JetBrains Mono Regular, OFL 1.1, at 10 points — so text is the same everywhere and in tests |
| G5 | anti-aliasing | **Decided (user, 2026-10-15)**: accept plutovg's; integer coordinates stroked through pixel centres (filled shapes on the grid) so points, axis lines and rectangles are pixel-exact |
| G6 | coordinates on a high-density screen | **Decided (user, 2026-10-15)**: points, with the surface at `density` times the pixels; a hidden canvas always density 1 |
| G7 | `drawop: #reverse` | **Decided (user, 2026-10-15)**: a coverage mask exclusive-ored into the pixels, not anti-aliased |
| G8 | image formats | read what stb_image reads; write PNG only, refusing another extension |
| G9 | key releases | **Decided (user, 2026-10-15)**: reported under `inputmask` `"k"`, with the same code and `&released` succeeding |
| G10 | whose are `&x`, `&y`, `&control`, … | **Decided (user, 2026-10-15)**: the running machine's (each task its own), set by `event()` |
| G11 | the promise form of `Event()` | a method, `w.next_event()`, answering a promise of an event map; it sets no keywords |
| G12 | module and feature | **Decided (user, 2026-10-15)**: one module, `funl:graphics`, in the core over vendored plutovg (hidden canvases work in every build); on-screen windows behind a `graphics` feature over `sdl3`, on in `default`; without it, opening a visible window faults naming the feature |

**G12 in full.** plutovg's C is vendored and links nothing, so the drawing half costs no link line
and can be in the core, where `--no-default-features` builds and tests it. SDL3 is not on every
machine, so the on-screen half is a feature, named for the module it serves as the
[feature rule](modules.md#features-keep-the-core-link-line-short) requires. Opening `canvas: #normal`
in a build without it faults with `left_out_module`'s sentence, ``a window on screen is not in this
build -- it is behind the `graphics` feature``. In `default` because a graphics module that opens no
window by default would surprise everyone; its cost is that the Homebrew formula gains `depends_on
"sdl3"`. The alternative, the whole module behind the feature, would leave the drawing untested by
the `--no-default-features` run.
