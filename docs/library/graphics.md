---
title: funl:graphics
weight: 22
---

# `funl:graphics` — windows, after Icon's

`funl:graphics` gives FunL the graphics facilities of Icon 9.5. A window is a handle, and everything
done with it is a method on that handle. The module exports `open`, which makes a window; `couple`,
which joins two; and `color_value`, which reads a colour.

This page covers windows on a **hidden canvas**: a picture drawn in memory and read back, with no
window on screen, and [text and fonts](#text-and-fonts). Drawing, images and events come with later
parts of the module.

## Hidden canvases

`open(title, attrs)` answers a window titled `title`, with the attributes in the map `attrs`
applied. **`canvas: #hidden` keeps it off the screen.** The canvas is still drawn on and read back
like any other, but it needs no display and no one logged in at one, so it works in every build and
in every test.

```funl
import { open } from funl:graphics

w = open( "sketch", {canvas: #hidden, size: (200, 100), bg: "light gray"} )
write( w )
write( w.get("size"), w.get("label") )
w.close()
write( w )
```

```output
<window handle>
(200, 100), sketch
<closed window handle>
```

**A window on screen is not in this build yet.** Opening one, which is what `open` does when
`canvas` is not `#hidden`, faults and names the feature it needs:

```funl
import { open } from funl:graphics

w = open( "hello" )
```

```error
'open': a window on screen is not in this build -- it is behind the `graphics` feature
```

A canvas opened with no `size` is Icon's default, 80 columns by 12 lines of the font it is opened
with — 480 by 156 in the default font, `"fixed"` ([Text and fonts](#text-and-fonts)). It starts
painted in its background colour.

## Attributes

A window's attributes are a map, where Icon used `"name=value"` strings:

| method | what it does |
|---|---|
| `w.set(m)` | applies every entry of map `m`, in order, and answers `w` |
| `w.get(name)` | answers one attribute |
| `w.attributes()` | answers an immutable map of every attribute |

Numbers are integers, a pair is a tuple like `(200, 100)`, on and off are `true` and `false`, a
choice among names is a symbol like `#dashed`, and colours and the title are strings.

```funl
import { open } from funl:graphics

w = open( "attrs", {canvas: #hidden, size: (40, 30)} )
w.set( {fg: "red", linewidth: 3, linestyle: #dashed} )
write( w.get("fg"), w.get("linewidth"), w.get("linestyle") )
write( w.attributes() )
```

```output
red, 3, dashed
{"size": (40, 30), "width": 40, "height": 30, "label": "attrs", "canvas": hidden, "resize": false, "fg": "red", "bg": "white", "reverse": false, "linewidth": 3, "linestyle": dashed, "fillstyle": solid, "drawop": copy, "clipx": undefined, "clipy": undefined, "clipw": undefined, "cliph": undefined, "dx": 0, "dy": 0, "depth": 24, "density": 1, "font": "fixed", "leading": 13, "ascent": 10, "descent": 3, "fheight": 13, "fwidth": 6, "x": 0, "y": 10, "row": 1, "col": 1}
```

| attribute | value | default |
|---|---|---|
| `size` | `(width, height)` | 80 columns by 12 lines of the font: `(480, 156)` |
| `width`, `height` | an integer from 1 to 32767 | |
| `label` | the title, a string | `open`'s title |
| `canvas` | `#hidden` (`#normal`, `#maximal` and `#iconic` are on-screen states) | |
| `resize` | whether the user may resize the window | `false` |
| `fg`, `bg` | a colour | `"black"`, `"white"` |
| `reverse` | `true` swaps `fg` and `bg`, `false` swaps them back | `false` |
| `linewidth` | an integer, 0 or more | `1` |
| `linestyle` | `#solid`, `#dashed` or `#striped` | `#solid` |
| `fillstyle` | `#solid`, `#textured` or `#masked` | `#solid` |
| `drawop` | `#copy` or `#reverse` | `#copy` |
| `clipx`, `clipy`, `clipw`, `cliph` | the clip rectangle; setting one when there is none starts from the whole canvas, and `undefined` takes it away | `undefined` |
| `dx`, `dy` | the origin, added to every coordinate | `0` |
| `depth` | read-only: bits per pixel | `24` |
| `density` | read-only: device pixels per point, always 1 on a hidden canvas | `1` |
| `font` | a font specification, a string ([Text and fonts](#text-and-fonts)) | `"fixed"` |
| `leading` | an integer: the pixels between one line's baseline and the next | the font's `fheight` |
| `ascent`, `descent` | read-only: how far the font rises above the baseline and falls below it | `10`, `3` |
| `fheight`, `fwidth` | read-only: the font's height, `ascent + descent`, and its widest character | `13`, `6` |
| `x`, `y` | the text cursor: the left of the next character and its baseline, moved by `dx` and `dy` | `0`, the ascent |
| `row`, `col` | the text cursor in character cells, `fwidth` wide and `leading` high, from 1 | `1`, `1` |

**Changing the size keeps the picture.** The pixels the old and new sizes share stay, and the rest
is painted in the background colour.

**`set` fails, applying nothing, on a value Icon would refuse**: a colour that is no colour, a size
of 0, a negative line width, a symbol that is none of an attribute's choices, a value of the wrong
kind. A key that is no attribute faults with a `domain_error`, and a read-only one with a
`permission_error`, before any value is looked at.

```funl
import { open } from funl:graphics

w = open( "refusals", {canvas: #hidden, size: (40, 30)} )
write( w.set({fg: "red", linewidth: -1}) | #refused )
write( w.get("fg") )
write( w.set({colour: "red"}) catch e -> e )
write( w.set({depth: 8}) catch e -> e )
```

```output
refused
black
error(domain_error(window_attribute, colour), context(_G2, "'set': `colour` is no window attribute"))
error(permission_error(modify, window_attribute, depth), context(_G5, "'set': `depth` is read-only"))
```

`open` fails exactly where `set` would, and faults where `set` would.

## Colours

**A colour is Icon's colour specification, a string**, in one of four forms:

- three numbers from 0 to 65535, `"r,g,b"`;
- `#` and three, six, nine or twelve hexadecimal digits, `"#f00"`, `"#00ff80"`;
- a phrase: a hue, optionally after a second hue or its `-ish` form, a saturation, and a lightness
  that `very` may strengthen, as in `"very dark greenish gray"`.

The hues are `black`, `gray` (or `grey`), `white`, `pink`, `violet`, `brown`, `red`, `orange`,
`yellow`, `green`, `cyan`, `blue`, `purple` and `magenta`. The lightness is `pale`, `light`,
`medium`, `dark` or `deep`, `pale` being very light and `deep` very dark. The saturation is `weak`,
`moderate`, `strong` or `vivid`. Words are separated by a space or a hyphen, in any case.

**`color_value(s)` answers a colour in the form `"r,g,b"`**, with the numbers Icon's `ColorValue`
gives, and fails on a string that is no colour:

```funl
import { color_value } from funl:graphics

every write( color_value("red" | "dark blue" | "very dark greenish gray" | "#00ff80" | "1,2,3") )
write( color_value("plaid") | #no_colour )
```

```output
65535,0,0
0,0,43690
8191,13653,8191
0,65535,32896
1,2,3
no_colour
```

## Reading pixels

**`w.pixel(x, y, wd, ht)` generates the colours of a rectangle**, row by row, each in the
`"r,g,b"` form, so a program compares them as strings. The colours are those when the call starts.
As in Icon, `x` and `y` default to the canvas's corner and are moved by `dx` and `dy`, `wd` and `ht`
default to the canvas's edge, a negative width or height reaches left or up, and a pixel off the
canvas is the background colour.

```funl
import { open } from funl:graphics

w = open( "pixels", {canvas: #hidden, size: (3, 2), bg: "blue"} )
write( w.pixel(0, 0) )
write( [p | p <- w.pixel(2, 1, 2, 1)] )
w.set( {bg: "red", size: (4, 2)} )
write( [p | p <- w.pixel(2, 0, 2, 1)] )
```

```output
0,0,65535
["0,0,65535", "0,0,65535"]
["0,0,65535", "65535,0,0"]
```

A canvas holds eight bits of each component, so a pixel's numbers are multiples of 257: a colour
drawn and read back is `color_value`'s answer where that already is one.

## Text and fonts

**A font is Icon's specification, a string `"family,style,…,size"`**: a family, then any of the
styles `bold` and `italic` and a size in points, in any order. The family is one of

| family | what it is |
|---|---|
| `"fixed"` | the font carried in `funl` itself, the default |
| `"mono"`, `"typewriter"` | a system monospaced face: Menlo and Courier on macOS |
| `"sans"`, `"serif"` | a system proportional face: Helvetica and Times on macOS |
| any other name | the installed family of exactly that name, such as `"Helvetica"` |

and a font that names no size is 10 points. **`"fixed"` is JetBrains Mono Regular**, under the SIL
Open Font License 1.1, carried in the binary so that text draws the same pixels on every machine,
one with no fonts installed included. On Linux the generic families are DejaVu, Liberation or
Nimbus faces, whichever are installed.

**Setting `font` fails, setting nothing, on a specification it cannot read or a family there is no
face of**, and a font that is set sets `leading` to its height. The metrics are read-only
attributes: `ascent` and `descent`, how far the font rises above the baseline and falls below it,
`fheight`, their sum, and `fwidth`, its widest character. **`w.text_width(s)`** is how far `s`
would advance in the window's font.

```funl
import { open } from funl:graphics

w = open( "fonts", {canvas: #hidden} )
write( w.get("size"), w.get("font"), w.get("ascent"), w.get("descent"), w.get("fheight"), w.get("fwidth") )
write( w.text_width("hello") )
w.set( {font: "fixed,20"} )
write( w.get("fheight"), w.get("fwidth"), w.get("leading"), w.text_width("hello") )
write( w.set({font: "fixed,huge"}) | #refused )
write( w.set({font: "no such family"}) | #refused )
write( open("big", {canvas: #hidden, font: "fixed,20"}).get("size") )
```

```output
(480, 156), fixed, 10, 3, 13, 6
30
26, 12, 26, 60
refused
refused
(960, 312)
```

**`w.draw_string(x, y, s, …)` draws each string in the foreground colour** with its baseline
starting at `(x, y)`, moved by `dx` and `dy`, and answers `w`. Only the glyphs are painted, the
clip rectangle confines them, and the text cursor does not move.

```funl
import { open } from funl:graphics

w = open( "label", {canvas: #hidden, size: (20, 13)} )
w.draw_string( 0, 10, "a", 9, 10, "b" )
for y <- [3, 6] do write( join([(if p == "65535,65535,65535" then "." else "#") | p <- w.pixel(0, y, 20, 1)]) )
write( w.draw_string(0, 10) catch e -> e )
```

```output
.........##.........
.#####...##..##.....
error(existence_error(procedure, /(draw_string, 2)), context(_G2, "'draw_string' takes x, y and a string, repeated, and was given 2 arguments"))
```

### The window as a terminal

**A window has a text cursor**, as Icon's has: `x` and `y`, the left of the next character and its
baseline, or `row` and `col`, character cells `fwidth` wide and `leading` high counted from 1. A
window opens with it at row 1, column 1. `w.goto_rc(r, c)` moves it to a cell (each 1 when not
given) and `w.goto_xy(x, y)` to a point (each 0), both answering `w`.

**`w.write(x…)` and `w.writes(x…)` write at the cursor** exactly what `write` and `writes` would
print — `write` separating its arguments with `, ` and ending the line, `writes` running them
together — and answer `()`. Each character's cell is painted in the background colour before the
character is drawn over it, as a terminal shows text. A newline moves the cursor to the start of the
next line, a carriage return to the start of its own line, and a tab to the column after the next
multiple of 8. **A line that would pass the bottom scrolls the canvas up**, the uncovered rows
painted in the background colour.

```funl
import { open } from funl:graphics

w = open( "terminal", {canvas: #hidden, size: (60, 26)} )
w.writes( "x = ", 42 )
write( w.get("row"), w.get("col") )
w.write( "one" )
write( w.get("row"), w.get("col"), w.get("x"), w.get("y") )
w.write( "two" )
w.write( "three" )
write( w.get("row"), w.get("y") )
write( w.goto_rc(2, 4).get("x"), w.get("y") )
```

```output
1, 7
2, 1, 0, 23
2, 23
18, 23
```

After `"two"` and `"three"`, each ending its line, the canvas has scrolled twice, and `"three"` is on
row 1 with the cursor below it on row 2, the last row the canvas holds.

## Clones, couples and closing

**A window is a canvas and a context.** The canvas is the picture, its size and its title; the
context is how drawing is done: the colours, the line and fill styles, the drawing operation, the
clip rectangle and the origin.

- **`w.clone(attrs)`** answers a new window on the same canvas with a copy of `w`'s context, `attrs`
  applied to the copy. It fails, making nothing, where `set` would.
- **`couple(w1, w2)`** answers a new window on `w1`'s canvas sharing `w2`'s context, so a change made
  through either is seen by both.

A clone that changes the size changes it for every window on the canvas, since the size is the
canvas's:

```funl
import { open, couple } from funl:graphics

w = open( "main", {canvas: #hidden, size: (40, 30)} )
red = w.clone( {fg: "red"} )
write( red.get("fg"), w.get("fg") )
red.set( {size: (60, 30)} )
write( w.get("size") )

v = open( "other", {canvas: #hidden, size: (10, 10), fg: "blue"} )
both = couple( w, v )
both.set( {fg: "green"} )
write( v.get("fg"), both.get("size") )
```

```output
red, black
(60, 30)
green, (60, 30)
```

**`w.close()` closes the canvas, and every window on it with it**, as Icon's `WClose` does: a clone's
or a couple's next call faults with an `existence_error`, as any closed handle's does.
**`w.uncouple()` closes `w` alone**, and the canvas closes with its last window. A window the program
forgets is uncoupled by the collector once nothing reaches it, so the canvas stays as long as another
window is still on it.

```funl
import { open } from funl:graphics

w = open( "main", {canvas: #hidden, size: (40, 30)} )
a = w.clone()
b = w.clone()
a.uncouple()
write( a, b.get("size") )
w.close()
write( b.get("size") catch e -> e )
```

```output
<closed window handle>, (40, 30)
error(existence_error(handle, <closed window handle>), context(_G2, "'get' was called on the handle <closed window handle>"))
```
