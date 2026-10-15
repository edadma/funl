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
- **F3 — plutovg writes PNG only** to a file (`write_png`; `jpg_bytes` encodes a JPEG in memory). Icon's
  `WriteImage` writes GIF, and on some builds JPEG and PNG. PNG is enough for parity of capability
  (see [G8](#the-questions-collected)); the finding is recorded so the gap is known, not because FunL
  waits on it.
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
a later item. **Each later item added its own attributes to the same map**, with Icon's meaning and
values in the form the library page gives (numbers `Int`, pairs tuples, `on`/`off` booleans,
enumerations symbols, the rest strings).

Every attribute that table named is now built: `image` with the images, the font's with the text,
and the screen's with the windows on screen.

`pointer` is read as item 7 built it, paired with `pointerx` and `pointery`: where the pointer is, as
`pos` is where the window is. Icon's `pointer` -- the pointer's *shape* -- has no attribute here yet:
which shape names FunL takes is open, and SDL's dummy driver cannot make a system cursor to test one.

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
G5 decided), and so are points at `density` on a window on screen ([G6](#the-questions-collected),
[Windows on screen](../library/graphics-screen.md#density)). What remains is item 4's:
`write_image` writes the device pixels, and any new code that reads or writes a surface's bytes
directly works in device pixels, `density` to the point, as `pixel`, `copy_area` and scrolling do.

### Colours

**Colour specifications, `color_value` and `pixel` are built** ([the library
page](../library/graphics.md#colours)), and so are `fg`'s `"transparent"` and `"opaque"` modifiers
([painting styles](../library/graphics.md#painting-styles)), the palettes and `new_color`
([palettes](../library/graphics.md#palettes)). The window itself has no alpha.

## Text and fonts

**Fonts, their metrics, `draw_string` and the window as a text terminal — `write`, `writes`, `read`
and `reads` with `echo` — are built**; see the library page,
[`funl:graphics`](../library/graphics.md#text-and-fonts).

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
| G4 | the default font | **Decided (user, 2026-10-15)**: a permissively licensed monospaced face carried in the binary as `"fixed"` — JetBrains Mono Regular, OFL 1.1, at 10 points — so text is the same everywhere and in tests |
| G5 | anti-aliasing | **Decided (user, 2026-10-15)**: accept plutovg's; integer coordinates stroked through pixel centres (filled shapes on the grid) so points, axis lines and rectangles are pixel-exact |
| G6 | coordinates on a high-density screen | **Decided (user, 2026-10-15)**: points, with the surface at `density` times the pixels; a hidden canvas always density 1 |
| G7 | `drawop: #reverse` | **Decided (user, 2026-10-15)**: a coverage mask exclusive-ored into the pixels, not anti-aliased |
| G8 | image formats | **Decided (user, 2026-10-15)**: read what stb_image reads; write PNG only, refusing another image format's extension with `domain_error(image_format, Ext)` |
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
