---
title: funl:graphics on screen
weight: 23
---

# Windows on screen

A window that is not opened with `canvas: #hidden` is **shown on screen**, in a window of the
platform's own, as Icon's `WOpen` shows one. Everything on [the `funl:graphics` page](graphics.md)
holds for it too: it is a canvas and a context, its attributes are a map, and `pixel` reads it back.
This page is what being on screen adds.

Windows on screen are behind the `graphics` feature, over SDL, which a `funl` build has unless it was
built with `--no-default-features`. A build without it keeps every hidden canvas and faults on any
window on screen, naming the feature.

## Opening and showing

`open(title, attrs)` with no `canvas`, or with `canvas: #normal`, makes the window at the size and
place its attributes ask for, titled `title`:

```funl
import { open } from funl:graphics

w = open( "hello", {size: (200, 100), bg: "light blue"} )
write( w.get("canvas"), w.get("size"), w.get("label") )
w.close()
```

```output
normal, (200, 100), hello
```

**What is drawn reaches the screen when the program waits**, not at every drawing call: at
`w.flush()`, `w.sync()`, `w.delay(ms)`, a wait on the [event loop](async.md), and the program's end.
Between those, a hundred drawing calls cost one copy to the screen, the latitude Icon's book gives
("output may be buffered").

| method | what it does |
|---|---|
| `w.flush()` | shows every window's canvas as it now is, and answers `w` |
| `w.sync()` | the same: the frame is on screen by the time it answers |
| `w.delay(ms)` | shows the canvases, then waits `ms` milliseconds, the windows answering their user meanwhile; answers `w` |
| `w.raise()` | brings `w`'s window in front of the others, and answers `w` |

On a hidden canvas each of them answers `w` and shows nothing, so a program written for the screen
runs unchanged on a hidden canvas.

```funl
import { open } from funl:graphics

w = open( "paced", {size: (40, 30)} )
write( w.flush(), w.delay(10), w.raise() )
w.close()
```

```output
<window handle>, <window handle>, <window handle>
```

**A program that ends with a window still on screen keeps it there until its user closes it**, as
Icon's `WDone` waits; a program that called `close()` on its windows, or kept them hidden, ends at
once.

## Hidden, then shown

`canvas` may change after `open`. A hidden canvas set to `#normal` is given a window then, showing
the picture already drawn on it; set to `#hidden` again, the window is taken off the screen and the
canvas kept.

```funl
import { open } from funl:graphics

w = open( "later", {canvas: #hidden, size: (60, 40), pos: (100, 80)} )
write( w.get("canvas"), w.get("pos") )
w.set( {canvas: #normal} )
write( w.get("canvas"), w.get("pos") )
w.set( {canvas: #hidden} )
write( w.get("canvas") )
w.close()
```

```output
hidden, (100, 80)
normal, (100, 80)
hidden
```

`#maximal` and `#iconic`, Icon's other canvas states, fault for now: SDL can maximize and minimize a
window, and the `sdl3` package does not bind those calls yet.

```funl
import { open } from funl:graphics

w = open( "big", {canvas: #maximal} )
```

```error
'open': `#maximal` and `#iconic` need SDL's maximize and minimize, which the `sdl3` package does not bind yet
```

## Where the window is, and the pointer

| attribute | value |
|---|---|
| `pos`, `posx`, `posy` | where the window is on its display, `(x, y)`, in points; setting it moves the window |
| `pointer`, `pointerx`, `pointery` | where the pointer is over the window, `(x, y)`; setting it moves the pointer there |
| `displaywidth`, `displayheight` | read-only: the size of the display the window is on, in points |

A hidden canvas keeps a `pos` it is given for when it is shown; it has no pointer and no display, so
those read `undefined` and setting `pointer` fails.

```funl
import { open } from funl:graphics

w = open( "placed", {size: (80, 60), pos: (40, 50)} )
write( w.get("pos") )
w.set( {posx: 90} )
write( w.get("pos"), w.get("posy") )
w.set( {pointer: (10, 20)} )
write( w.get("pointer") )
if w.get("displaywidth") > 80 then write( "the display is wider than the window" )
w.close()
```

```output
(40, 50)
(90, 50), 50
(10, 20)
the display is wider than the window
```

## Resizing

**A window resized keeps its picture**: the part the old and new sizes share stays, and the new area
is painted in the background colour, as Icon's is. That is so whether the program set `size` or the
window's user dragged its edge, which `resize: true` allows and the default, `false`, does not.

```funl
import { open } from funl:graphics

w = open( "grown", {size: (4, 3), bg: "blue"} )
w.set( {bg: "red", size: (8, 6)} )
write( w.pixel(1, 1, 1, 1), w.pixel(6, 5, 1, 1) )
w.close()
```

```output
0,0,65535, 65535,0,0
```

## Density

**A window is measured in points.** On a high-density display -- a retina screen -- its canvas has
`density` device pixels to the point in each direction, 2 there, and drawing is scaled by it, so lines
and text are sharp. `pixel` answers a point's top-left device pixel. A hidden canvas has density 1
always, so what a test reads back is the same on every machine.

## Several windows

A program may have any number of windows on screen at once, each its own canvas:

```funl
import { open } from funl:graphics

a = open( "left", {size: (30, 30), bg: "red"} )
b = open( "right", {size: (50, 20), bg: "green"} )
write( a.get("label"), a.pixel(5, 5, 1, 1), b.get("label"), b.pixel(40, 10, 1, 1) )
a.close()
b.close()
```

```output
left, 65535,0,0, right, 0,65535,0
```
