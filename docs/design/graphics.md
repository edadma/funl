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

- **F1 — plutovg's font-face cache is not bound.** `plutovg_font_face_cache_*` (create, `add_file`,
  `get` by family/bold/italic, `load_dir`, `load_sys`) and `plutovg_canvas_set_font_face_cache` are
  in the vendored header and absent from `plutovg.sysl`. Icon's fonts are named by family —
  `"sans,bold,14"` — so FunL needs exactly the system lookup the cache gives. Without it the module
  could only take a font file's path.
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
- **F5 — sdl3 does not bind `SDL_MaximizeWindow`, `SDL_MinimizeWindow` or `SDL_RestoreWindow`.**
  Icon's `canvas=maximal` and `canvas=iconic` are those calls, and they fault naming them until the
  package binds them. (`SDL_SyncWindow` is unbound too; `sync()` does not need it, since a frame is
  on screen once `SDL_RenderPresent` returns.)
- **F6 — SDL has no call that lowers a window.** `SDL_RaiseWindow` has no opposite in SDL3, so
  Icon's `Lower()` (`w.lower()` in the table below) is not built: whether FunL drops it or reaches
  each platform's own call is a question for the user.

## The window

**The window handle, its canvas and context, `open` with `canvas: #hidden`, `clone`, `couple`,
`close`, `uncouple`, the attribute map and colour specifications are built** — see the library page,
[`funl:graphics`](../library/graphics.md). **So is the window on screen** — the `graphics` feature,
showing on wait, `flush`, `sync`, `delay`, `raise`, `pos`, `pointer`, the display's size, density,
a resize keeping the picture, several windows, and the wait at the program's end — on
[Windows on screen](../library/graphics-screen.md). What remains of this section is for the later
items.

### The attributes still to come

The map, its failures and its faults are built, with the attributes whose meaning needs nothing from
a later item. **Each later item adds its own attributes to the same table**, with Icon's meaning and
values in the form the library page gives (numbers `Int`, pairs tuples, `on`/`off` booleans,
enumerations symbols, the rest strings):

| attribute | item |
|---|---|
| `pattern` (Icon's pattern specifications, and its failure) | 2, drawing |
| `image` (a file loaded at open, the canvas sized to it) | 4, images |
| `font`, `leading`, `x`, `y`, `row`, `col`; read-only `ascent`, `descent`, `fheight`, `fwidth` | 5, fonts and text |

`pointer` is read as item 7 built it, paired with `pointerx` and `pointery`: where the pointer is, as
`pos` is where the window is. Icon's `pointer` -- the pointer's *shape* -- has no attribute here yet:
which shape names FunL takes is open, and SDL's dummy driver cannot make a system cursor to test one.

**The default size, 80 columns by 12 lines of the default font, is 480 by 156 until item 5** carries
`"fixed"`; item 5 computes it from that face's metrics, as Icon's `wopen` does.

Not adopted: `display` (an X server's name), `gamma`, `iconpos`, `iconlabel`, `iconimage`,
`windowlabel` separate from `label` — they describe X11, and SDL has no meaning for them.

## Drawing

Every Icon drawing function is a method; a function that takes a repeated group of arguments
(`DrawLine(x1, y1, x2, y2, x3, y3, …)`, `DrawPoint(x1, y1, x2, y2, …)`, `DrawRectangle` with several
rectangles) takes the same flat list, so a variadic call ([I5](../reference/functions.md#variadic-functions))
or a spread passes a computed list.

| Icon | FunL |
|---|---|
| `DrawPoint(x, y, …)` | `w.draw_point(x, y, …)` |
| `DrawLine(x1, y1, x2, y2, …)` | `w.draw_line(…)` — a polyline through every point |
| `DrawSegment(x1, y1, x2, y2, …)` | `w.draw_segment(…)` — separate segments, four numbers each |
| `DrawRectangle`, `FillRectangle` | `w.draw_rectangle(x, y, wd, ht, …)`, `w.fill_rectangle(…)`; `fill_rectangle()` with none fills the whole canvas |
| `DrawPolygon`, `FillPolygon` | `w.draw_polygon(x1, y1, …)`, `w.fill_polygon(…)` |
| `DrawCircle(x, y, r, theta, alpha, …)` | `w.draw_circle(…)`, `w.fill_circle(…)` — `theta` and `alpha` in radians, as Icon's |
| `DrawArc(x, y, wd, ht, theta, alpha, …)` | `w.draw_arc(…)`, `w.fill_arc(…)` — an arc of the ellipse in the box |
| `DrawCurve(x1, y1, …)` | `w.draw_curve(…)` — Icon's smooth curve through every point, a closed one when the first point is the last |
| `DrawString(x, y, s)` | `w.draw_string(x, y, s)` — `y` is the baseline, as Icon's |
| `EraseArea(x, y, wd, ht)` | `w.erase_area(…)` — fills with `bg`; with no arguments, the whole canvas |
| `CopyArea(w1, w2, x, y, wd, ht, x2, y2)` | `w1.copy_area(w2, x, y, wd, ht, x2, y2)` — `w2` may be `w1`, and an overlapping copy is done through a scratch surface |
| `Clip(x, y, wd, ht)`, `Clip()` | `w.clip(x, y, wd, ht)`, `w.clip()` — also the `clip*` attributes |
| `Fg(s)`, `Bg(s)`, `Font(s)`, `Pattern(s)`, `DrawOp(s)` | the attributes; `w.set({fg: s})` |
| `TextWidth(s)` | `w.text_width(s)` |
| `Raise()`, `Lower()` | `w.raise()`, `w.lower()` |
| `WDefault(prog, opt)` | not adopted: X resources ([out of scope](#out-of-scope)) |

**A width or height may be negative**, which Icon defines as extending left or up from the point; a
missing `wd`/`ht` means "to the edge of the canvas", also Icon's.

**`linestyle`** strokes with plutovg's dash; `#striped` draws the gaps in `bg`. **`fillstyle`** and
**`pattern`** take Icon's pattern specifications — the named ones (`"black"`, `"gray"`,
`"checkers"`, `"grains"`, …) and the bit-pattern strings `"#8,…"` and `"8,#…"` — made into a small
tiled plutovg texture: `#textured` paints the ones in `fg` and the zeros in `bg`, `#masked` paints
the ones only.

**`drawop: #reverse`** is Icon's rubber-band operation: drawing the same thing twice restores what
was there. plutovg's `Xor` operator is Porter-Duff's, not a bitwise one, so the module draws the
shape into a scratch coverage mask and exclusive-ors `fg ^ bg` into the pixels the mask covers
([G7](#the-questions-collected)). A shape drawn under `#reverse` is not anti-aliased, since a
partial pixel cannot be undone by a second exclusive-or.

### Coordinates

**Coordinates are numbers, `Int` or `Real`**, measured from the canvas's top left, `x` right and
`y` down, with `dx`/`dy` added first as Icon's are. **An integer coordinate names a pixel**, and the
module draws through the pixel's centre (it adds 0.5 before handing plutovg the point), so a
`draw_point`, a horizontal or vertical line, and a rectangle with integer corners touch exactly the
pixels Icon's would, with no anti-aliased fringe. A diagonal line or an arc is anti-aliased — plutovg
has no aliased mode, and Icon's book leaves the pixels of a slanted line to the display
([G5](#the-questions-collected)).

**On a high-density display the canvas is measured in points, not device pixels** ([G6](#the-questions-collected)).
A window opened at `size: (400, 300)` is 400 by 300 points on screen; its surface has `density`
times as many pixels in each direction, and drawing is scaled by it, so lines and text are sharp on a
retina screen. `pixel(x, y)` answers the colour at the point's top-left device pixel, and
`write_image` writes the device pixels. A hidden canvas has `density` 1, always, so what a test
reads is the same on every machine.

### Colours

**Colour specifications, `color_value` and `pixel` are built** ([the library
page](../library/graphics.md#colours)). Icon's `"transparent"` and `"opaque"` modifiers are taken for
`fg` (painting at half and full coverage being what a 24-bit display can show of them), which is
drawing's, item 2: today `fg` refuses them as it refuses any string that is no colour. The window
itself has no alpha.

**Palettes are module functions**: `palette_chars(p)`, `palette_color(p, s)`, `palette_key(p, c)`,
`palette_grays(p)` over Icon's `c1`–`c6` and `g2`–`g256`, written in FunL and carried as source
([the native rule](modules.md#built-in-modules-funlname): only what FunL cannot do is native).

**`NewColor`, `FreeColor` and `Color`** — mutable colour-map entries — answer as Icon's do on a
true-colour display: `new_color` fails. Icon's book allows exactly this, and no display FunL runs on
has a colour map.

## Text and fonts

**A font is Icon's specification, `"family,style,…,size"`**: the families `"mono"`, `"typewriter"`,
`"sans"`, `"serif"` (and any installed family by name), the styles `bold` and `italic`, a size in
points. The family is looked up in the system's fonts through plutovg's font-face cache (F1), and an
unknown family **fails** `set({font: …})`, as `Font` does.

**`"fixed"` — Icon's default font — is a font carried in the `funl` binary** ([G4](#the-questions-collected)),
a permissively licensed monospaced TrueType face. It is the default for every window and the only
font a test uses, so text draws the same pixels on a machine with no fonts installed and on one with
thousands.

The **metrics** are the read-only attributes `ascent`, `descent`, `fheight`, `fwidth`, from
plutovg's `metrics`, and `text_width(s)` is plutovg's `text_extents`. `leading` is the line spacing
the text cursor moves by.

**The window is also a text terminal**, as Icon's is: a cursor at `row`/`col` (character cells of
`fwidth` by `leading`) or `x`/`y`, moved by `w.goto_rc(r, c)` and `w.goto_xy(x, y)`;
`w.write(…)` and `w.writes(…)` draw at the cursor and advance it, scrolling the canvas when it passes
the bottom; `w.read()` and `w.reads(n)` take keys from the event queue, echoing them while `echo` is
on, until a newline or `n` characters. These are the stream methods [I17](icon.md#input-and-output)
gives a file handle, so a procedure that writes to "a stream" writes to a window unchanged.

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
[events](../library/graphics.md#events) — and so are a window on screen's own events, which reach the
same queue ([Windows on screen](../library/graphics-screen.md#events-from-the-screen)).

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
| G4 | the default font | a permissively licensed monospaced face carried in the binary as `"fixed"`, so text is the same everywhere and in tests |
| G5 | anti-aliasing | accept plutovg's; integer coordinates drawn through pixel centres so points, axis lines and rectangles are pixel-exact |
| G6 | coordinates on a high-density screen | **Decided (user, 2026-10-15)**: points, with the surface at `density` times the pixels; a hidden canvas always density 1 |
| G7 | `drawop: #reverse` | built, as a coverage mask exclusive-ored into the pixels, not anti-aliased |
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
