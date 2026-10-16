---
title: funl:graphics
weight: 22
---

# `funl:graphics` — windows, after Icon's

`funl:graphics` gives FunL the graphics facilities of Icon 9.5. A window is a handle, and everything
done with it is a method on that handle. The module exports `open`, which makes a window; `couple`,
which joins two; `color_value`, which reads a colour; `active`, which finds the windows with
events waiting; and the [palette](#palettes) functions.

This page covers windows on a **hidden canvas**: a picture drawn in memory and read back, with no
window on screen, [drawing](#drawing) on it, [text and fonts](#text-and-fonts), [images](#images)
read, written and drawn, and its events, put on its queue by the program. [Windows on
screen](graphics-screen.md) has the rest of a window's life: shown, moved, resized and closed by its
user, whose keys and clicks reach the same queue.

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

**Without `canvas: #hidden`, `open` shows the window on screen**
([Windows on screen](graphics-screen.md)). That is behind the `graphics` feature, which a `funl`
build has unless it was built with `--no-default-features`; in a build without it, opening a window
on screen faults with a `system_error` saying that a window on screen is not in this build and is
behind the `graphics` feature.

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
{"size": (40, 30), "width": 40, "height": 30, "label": "attrs", "image": undefined, "canvas": hidden, "resize": false, "pos": undefined, "posx": undefined, "posy": undefined, "pointer": undefined, "pointerx": undefined, "pointery": undefined, "cursor": false, "echo": true, "inputmask": "", "fg": "red", "bg": "white", "reverse": false, "linewidth": 3, "linestyle": dashed, "fillstyle": solid, "pattern": "black", "drawop": copy, "clipx": undefined, "clipy": undefined, "clipw": undefined, "cliph": undefined, "dx": 0, "dy": 0, "depth": 24, "density": 1, "displaywidth": undefined, "displayheight": undefined, "font": "fixed", "leading": 13, "ascent": 10, "descent": 3, "fheight": 13, "fwidth": 6, "x": 0, "y": 10, "row": 1, "col": 1}
```

| attribute | value | default |
|---|---|---|
| `size` | `(width, height)` | 80 columns by 12 lines of the font: `(480, 156)` |
| `width`, `height` | an integer from 1 to 32767 | |
| `label` | the title, a string | `open`'s title |
| `image` | an image file, read onto the canvas, which takes its size ([images](#images)) | `undefined` |
| `canvas` | `#hidden`, or `#normal` for a window on screen | `#normal` |
| `resize` | whether the user may resize the window | `false` |
| `pos`, `posx`, `posy` | where the window is on its display, `(x, y)`; a hidden canvas keeps it for when it is shown | `undefined` until set or shown |
| `pointer`, `pointerx`, `pointery` | where the pointer is over the window, `(x, y)`; setting it moves the pointer; a hidden canvas has none, and setting it fails | `undefined` until known |
| `cursor` | whether a window on screen shows a [caret](graphics-screen.md#events-from-the-screen) at the text cursor while the program waits on its keys | `false` |
| `echo` | whether keys read as text are drawn as they are typed | `true` |
| `inputmask` | the events reported beyond the usual: `"m"` motion with no button held, `"k"` a key's release ([events](#events)) | `""` |
| `fg`, `bg` | a colour; `fg` may also be [transparent](#painting-styles) | `"black"`, `"white"` |
| `reverse` | `true` swaps `fg` and `bg`, `false` swaps them back | `false` |
| `linewidth` | an integer, 0 or more | `1` |
| `linestyle` | `#solid`, `#dashed` or `#striped` | `#solid` |
| `fillstyle` | `#solid`, `#textured` or `#masked` | `#solid` |
| `pattern` | a [pattern](#painting-styles): a name or a bit pattern, a string | `"black"` |
| `drawop` | `#copy`, or `#reverse` to [draw in reverse](#drawing-in-reverse) | `#copy` |
| `clipx`, `clipy`, `clipw`, `cliph` | the clip rectangle; setting one when there is none starts from the whole canvas, and `undefined` takes it away | `undefined` |
| `dx`, `dy` | the origin, added to every coordinate | `0` |
| `depth` | read-only: bits per pixel | `24` |
| `density` | read-only: device pixels per point, 2 on a high-density display, always 1 on a hidden canvas | `1` |
| `displaywidth`, `displayheight` | read-only: the size of the display the window is on; a hidden canvas has none | `undefined` |
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

## Drawing

**Every Icon drawing function is a method of the window, and answers the window**, so drawing calls
chain. A function Icon gives a repeated group of arguments takes the same flat list:
`w.draw_line(x1, y1, x2, y2, x3, y3)` is one line through three points.

| method | what it draws |
|---|---|
| `w.draw_point(x, y, …)` | a pixel at each point |
| `w.draw_line(x1, y1, x2, y2, …)` | a line through every point |
| `w.draw_segment(x1, y1, x2, y2, …)` | a separate line for each four numbers |
| `w.draw_rectangle(x, y, wd, ht, …)`, `w.fill_rectangle(…)` | a rectangle's outline, or its inside |
| `w.draw_polygon(x1, y1, …)`, `w.fill_polygon(…)` | a closed polygon; its inside is filled by the even-odd rule |
| `w.draw_circle(x, y, r, theta, alpha, …)`, `w.fill_circle(…)` | a circle about `(x, y)`, or the arc of it from angle `theta` through `alpha` more |
| `w.draw_arc(x, y, wd, ht, theta, alpha, …)`, `w.fill_arc(…)` | an arc of the ellipse in the rectangle |
| `w.draw_curve(x1, y1, …)` | a smooth curve through every point, closed when the first point is the last |
| `w.erase_area(x, y, wd, ht, …)` | the rectangle painted in the background colour |
| `w1.copy_area(w2, x, y, wd, ht, x2, y2)` | the rectangle of `w1` copied to `w2` at `(x2, y2)` |
| `w.clip(x, y, wd, ht)`, `w.clip()` | sets the clip rectangle, or takes it away |

A filled circle or arc is a pie, its inside running to the centre. Angles are in radians, measured
from the positive `x` axis; since `y` grows downwards, a positive angle turns clockwise on the
screen. `theta` defaults to 0 and `alpha` to a whole turn.

**A computed list of numbers is passed by spreading it**, `w.draw_line(..xs)`, as into any call
([functions](../reference/functions.md) § "Spreading arguments"), beside written arguments or not:

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "steps", {canvas: #hidden, size: (8, 5)} )
corners = [(0, 4), (0, 3), (2, 3), (2, 2), (4, 2), (4, 1), (6, 1), (6, 0)]
w.draw_line( ..[n | (x, y) <- corners, n <- [x, y]] ).draw_point( 7, ..[4] )
show( w )
```

```output
......#.
....###.
..###...
###.....
#......#
```

### Coordinates

**Coordinates are numbers, measured from the canvas's top left corner, with `dx` and `dy` added
first.** An integer coordinate names a pixel. A line goes through the centres of the pixels it
names and covers both its end points, so a horizontal or vertical line, a point and a rectangle with
integer corners cover exactly the pixels Icon's would, with no blurred edge. As in Icon,
**`draw_rectangle(x, y, wd, ht)` covers `wd + 1` columns and `ht + 1` rows**, from `x` to `x + wd`,
while **`fill_rectangle(x, y, wd, ht)` covers exactly `wd` by `ht` pixels**. A slanted line, a
circle and a curve are smoothed (anti-aliased) at their edges.

**A rectangle's numbers may be left out**: a missing `x` or `y` is the canvas's corner, a missing
`wd` or `ht` reaches the canvas's edge, so `fill_rectangle()` fills the whole canvas and
`erase_area()` clears it. **A negative width or height reaches left or up** from the point.

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "shapes", {canvas: #hidden, size: (12, 7)} )
w.draw_line( 0, 0, 4, 0, 4, 3 ).draw_point( 0, 6 )
w.draw_rectangle( 6, 1, 4, 3 ).fill_rectangle( 8, 6, -2, -1 )
show( w )
```

```output
#####.......
....#.#####.
....#.#...#.
....#.#...#.
......#####.
......##....
#...........
```

A circle's outline goes through pixel centres too, so `draw_circle(x, y, r)` spans `2r + 1` pixels;
a filled circle's edge lies on the pixel grid, so `fill_circle(x, y, r)` spans `2r`, about the
corner where four pixels meet.

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "round", {canvas: #hidden, size: (15, 7)} )
w.fill_circle( 3, 3, 3 )
w.draw_circle( 10, 3, 3, 0, pi )
show( w )
```

```output
++++++.........
+####+.........
+####+.........
+####+.++...++.
+####+.++...++.
++++++.+++++++.
........+++++..
```

A line of `linewidth` 0 or 1 is one pixel wide. A wider line of odd width is centred on its pixels'
centres; one of even width is centred on the grid line at their top left, which is where it covers
whole pixels.

### Painting styles

**`linestyle`** is `#solid`, `#dashed` -- dashes and gaps each four line widths long -- or
`#striped`, the same dashes with their gaps in the background colour.

**`fillstyle`** decides what every drawing paints with, the glyphs of
[`draw_string`](#text-and-fonts) included. `#solid` paints the foreground. `#textured` paints the
window's **`pattern`**, its ones in the foreground and its zeros in the background, and `#masked`
paints its ones only, leaving what is under the zeros. A `#striped` line's gaps are painted as X
paints them: in the background under `#solid`, exactly as the dashes under `#textured`, and under
`#masked` the pattern's ones in the background with its zeros left alone. The pattern repeats from
the canvas's corner. A pattern is one of Icon's names -- `"black"`, `"verydark"`, `"darkgray"`,
`"gray"`, `"lightgray"`, `"verylight"`, `"white"`, `"vertical"`, `"diagonal"`, `"horizontal"`,
`"grid"`, `"trellis"`, `"checkers"`, `"grains"`, `"scales"`, `"waves"` -- or a bit pattern: a width
from 1 to 32, a comma, and the rows from the top, either `#` and hexadecimal digits, as many to a row
as the width needs (`"4,#9696"`), or decimal numbers separated by commas (`"4,9,6,6,9"`). The low bit
of a row is its leftmost pixel. Setting `pattern` to anything else fails.

**A foreground colour may start with `transparent` or `opaque`**, Icon's modifiers: `"transparent
red"` paints at half coverage, mixing evenly with what is there, and `"opaque red"` is `"red"`. The
background is always opaque and refuses them; `reverse` hands the background the colour alone.

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "styles", {canvas: #hidden, size: (12, 6)} )
w.set( {linestyle: #dashed} ).draw_line( 0, 0, 11, 0 )
w.set( {fillstyle: #textured, pattern: "4,#9696"} ).fill_rectangle( 0, 2, 8, 4 )
w.set( {fillstyle: #solid, fg: "transparent black"} ).fill_rectangle( 9, 2, 3, 4 )
show( w )
write( w.pixel(10, 3) )
```

```output
####....####
............
#..##..#.+++
.##..##..+++
#..##..#.+++
.##..##..+++
32639,32639,32639
```

The named patterns are Icon's own, bit for bit: each is eight rows of eight, `"gray"` is
`"8,#55aa55aa55aa55aa"`, and `"diagonal"` is `"8,#8040201008040201"`, a line rising to the right.

### Drawing in reverse

**`drawop: #reverse` makes drawing undo itself**, Icon's rubber band: every pixel a shape covers has
the foreground exclusive-ored with the background exclusive-ored into it, so a background pixel turns
the foreground colour, a foreground pixel turns the background colour, and drawing the same shape a
second time puts back exactly what was there. Every drawing method obeys it -- points, lines,
segments, polygons, curves, rectangles, arcs and circles, drawn or filled, and the glyphs of
[`draw_string`](#text-and-fonts) -- inside the clip rectangle, as `#copy` does.

**A shape drawn in reverse is not anti-aliased.** A pixel is in it when the shape covers at least half
of it and out otherwise, since a pixel only partly changed could not be changed back.

**The rule is X's exclusive-or, under the colours Icon gives it in reverse**: wherever a drawing in
`#copy` would paint the foreground, `fg ^ bg` is exclusive-ored in, and wherever it would paint the
background, `bg` is. So under `#solid` every covered pixel takes `fg ^ bg`; under `#textured` the
pattern's ones take `fg ^ bg` and its zeros `bg`; under `#masked` the ones take `fg ^ bg` and the zeros
are left as they were; and a `#striped` line's gaps take what its gaps would be painted with, `bg`
under `#solid`. Since an exclusive-or undoes itself, drawing the same thing twice puts back what was
there under every fill style. With the default black on white, `bg` is white, so a `#textured`
pattern's zeros invert just as its ones do, and a striped line inverts all the way along.

**`erase_area` and `copy_area` are copies under either operation**, as Icon's `EraseArea` and
`CopyArea` are. **So are the terminal's [`write` and `writes`](#the-window-as-a-terminal)**, Icon's
`XDrawImageString`, which ignores the drawing operation and the fill style; but its glyphs are
painted in the pixel Icon sets the foreground to in reverse, `fg ^ bg`. Black on white then writes
white glyphs on white cells, and red on blue magenta on blue, and writing again changes nothing.

```funl
import { open } from funl:graphics

w = open( "xor", {canvas: #hidden, size: (2, 1)} )
w.set( {fg: "red", bg: "blue", fillstyle: #textured, pattern: "2,1", drawop: #reverse} ).fill_rectangle()
write( w.pixel(0, 0), w.pixel(1, 0) )
w.fill_rectangle()
write( w.pixel(0, 0), w.pixel(1, 0) )
```

```output
0,65535,0, 65535,65535,0
65535,65535,65535, 65535,65535,65535
```

The canvas opens white, and the colours are set after. The pattern's one at the left takes red ^
blue, turning white green, and its zero at the right takes
blue, turning white yellow; the second fill turns both back.

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "band", {canvas: #hidden, size: (8, 4)} )
w.fill_rectangle( 0, 0, 3, 4 )
w.set( {drawop: #reverse} ).draw_rectangle( 1, 1, 5, 2 )
show( w )
w.draw_rectangle( 1, 1, 5, 2 )
show( w )
```

```output
###.....
#..####.
#.#...#.
#..####.
###.....
###.....
###.....
###.....
```

### Erasing, copying and clipping

**`erase_area` paints the background colour**, whatever the fill style. **`copy_area` copies
pixels exactly**: each window's `dx` and `dy` move its own coordinates, what lies off `w1`'s canvas
copies as `w1`'s background, and `x2` and `y2` default to `w2`'s corner. `w2` may be `w1`, and the
two rectangles may overlap: the copy is read whole before any of it is written.

**`clip(x, y, wd, ht)` sets the clip rectangle** -- the `clipx`, `clipy`, `clipw` and `cliph`
attributes -- with Icon's rectangle, moved by `dx` and `dy`; drawing, erasing and copying then touch
only the pixels inside it. `clip()` takes it away. A clone has a context of its own, so its clip
rectangle limits only its own drawing.

```funl
import { open } from funl:graphics

def dot( p ) = if p == "0,0,0" then "#" else if p == "65535,65535,65535" then "." else "+"
def show( w ) = for y <- 0 to w.get("height") - 1 do write( join([dot(p) | p <- w.pixel(0, y, w.get("width"), 1)]) )

w = open( "copies", {canvas: #hidden, size: (8, 4)} )
w.fill_rectangle( 0, 0, 2, 2 )
w.copy_area( w, 0, 0, 2, 2, 1, 1 )
w.clip( 5, 0, 3, 4 ).fill_circle( 4, 2, 4 ).clip()
show( w )
```

```output
##...##+
###..##+
.##..##+
.....##+
```

**A drawing method faults** on a count of numbers its groups cannot make (an odd count of
coordinates, say), on an argument that is not a number -- `clip` and `copy_area` want integers -- and
on a closed window.

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
clip rectangle confines them, and the text cursor does not move. The glyphs are painted as any
drawing is: in the [`fillstyle` and `pattern`](#painting-styles), and exclusive-ored under
[`drawop: #reverse`](#drawing-in-reverse). The terminal's `write` and `writes` are not: their text is
always solid, as Icon's is.

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

### Reading from a window

**`w.read()` and `w.reads(n)` read what is typed in the window**, as Icon's `read(W)` and
`reads(W, n)` do, so a procedure that reads from "a stream" reads from a window unchanged. They take
keys off the window's [event queue](#events), oldest first: `read()` up to a newline or carriage
return, answering the line without it, and `reads(n)` exactly `n` characters, line ends included.
Every other event they come to — a button, motion, a symbol key like `#home`, a key's release — is
taken and passed over, and each event taken sets the [event keywords](#events) as `event()` would.
**Backspace and delete rub out the character before them**, when there is one in what is being read.

**While `echo` is on (the default), each key is written at the [cursor](#the-window-as-a-terminal)**
as `writes` would write it, and the line end that finishes `read()` moves the cursor to the next
line. A rub-out moves the cursor back over its character and paints that cell in the background
colour, when the character is still on the cursor's line. With `echo` off, nothing is drawn and the
cursor stays where it was.

**A window [on screen](graphics-screen.md) waits for its user to type**, as Icon's does. **On a
hidden canvas a read the queue cannot finish faults**, as `event()` does on an empty queue —
nothing but `enqueue` fills it, and the program cannot call that while it waits — and the queue and
the canvas are left as they were.

```funl
import { open } from funl:graphics

w = open( "terminal", {canvas: #hidden, size: (60, 26)} )
for k <- ["n", "o", "x", "\b", "!", #lpress, "\r", "4", "2"] do w.enqueue( k )
write( w.read(), w.get("row"), w.get("col") )
write( w.reads(2), w.get("col") )
write( w.read() catch _ -> "nothing left to read" )
```

```output
no!, 2, 1
42, 3
nothing left to read
```

## Images

| method | what it does |
|---|---|
| `w.draw_image(x, y, spec)` | draws Icon's image string `spec` with its corner at `x`, `y` |
| `w.read_image(file, x, y)` | reads an image file onto the canvas with its corner at `x`, `y` |
| `w.write_image(file, x, y, wd, ht)` | writes a rectangle of the canvas to `file` as a PNG |
| `w.image_bytes(x, y, wd, ht)` | answers the PNG `write_image` would write, as a [byte string](../reference/data.md#bytes) |

Each answers `w` (`image_bytes` its bytes) and fails where Icon's does. As in drawing, `x` and `y`
default to the canvas's corner and are moved by `dx` and `dy`, and an image goes only where the clip
rectangle lets it; `write_image` and `image_bytes` read their rectangle as [`pixel`](#reading-pixels)
does and keep the part of it on the canvas. An image is laid on the canvas pixel for pixel, with no
drawing operation and no anti-aliasing.

**An image string is a picture written as text.** `"wd,palette,data"` gives the width in pixels, a
[palette](#palettes), and then a character a pixel, row by row, each a key of the palette. A `~`, and
the character of code 255, are transparent, leaving the canvas as it was, where the palette has no
such key; a space or a comma is punctuation, read as nothing, where the palette has none. `"wd,#hex"`
is bi-level: each row is a run of hex digits read as one number, whose bit `i` is pixel `i`, drawn in
the foreground for a one and the background for a zero -- or left as it was, in `"wd,~hex"`.

```funl
import { open } from funl:graphics

w = open( "icon", {canvas: #hidden, size: (4, 2)} )
w.draw_image( 0, 0, "4,c2,rgb~ kkkk" )
write( [p | p <- w.pixel(0, 0, 4, 1)] )
w.set( {fg: "red"} ).draw_image( 0, 1, "4,~9" )
write( [p | p <- w.pixel(0, 1, 4, 1)] )
write( w.draw_image(0, 0, "3,c2,rgbr") | "not a rectangle" )
```

```output
["65535,0,0", "0,65535,0", "0,0,65535", "65535,65535,65535"]
["65535,0,0", "0,0,0", "0,0,0", "65535,0,0"]
not a rectangle
```

**Reading takes whatever stb_image reads**: PNG, JPEG, BMP, GIF (its first frame), TGA, PSD, HDR,
PIC and PNM. `read_image` fails when the file is not there or is no such image, and lays a pixel
that is partly transparent over what the canvas has. The `image` attribute reads a file onto the
canvas and makes the canvas its size, at `open` or after it.

**Writing is PNG**, whatever the file is called. A name ending in another image format's extension
-- `.jpg`, `.gif`, `.bmp` and the rest, in any case -- faults rather than putting PNG bytes under a
name that says otherwise. `write_image` fails when none of the rectangle is on the canvas or the file
cannot be made.

```funl
import { open } from funl:graphics
import { remove } from funl:fs

w = open( "strip", {canvas: #hidden, size: (3, 1)} )
w.draw_image( 0, 0, "3,c2,rgb" )
w.write_image( "docs/library/strip.png" )
v = open( "copy", {canvas: #hidden, image: "docs/library/strip.png"} )
write( v.get("size"), [p | p <- v.pixel()] )
u = open( "wider", {canvas: #hidden, size: (5, 1), bg: "black"} )
u.read_image( "docs/library/strip.png", 2 )
write( [p | p <- u.pixel()] )
write( w.image_bytes()(0..<4) )
write( u.read_image("docs/library/nothing.png") | "no image" )
remove( "docs/library/strip.png" )
```

```output
(3, 1), ["65535,0,0", "0,65535,0", "0,0,65535"]
["0,0,0", "0,0,0", "65535,0,0", "0,65535,0", "0,0,65535"]
bytes([137, 80, 78, 71])
no image
```

```funl
import { open } from funl:graphics

w = open( "photo", {canvas: #hidden, size: (3, 1)} )
w.write_image( "photo.jpg" )
```

```error
'write_image' writes PNG only, and `photo.jpg` names a .jpg file
```

Icon's `ReadImage` takes a palette to reduce an image to, which exists for colour-mapped displays and
is not adopted.

## Palettes

**A palette is a fixed set of colours, each named by one character, its key**, in which an image
string is written. `"c1"` is Icon's ninety named colours; `"c2"` to `"c6"` are colour cubes, `n`
levels of each of red, green and blue, with the greys between the cube's own added after it; `"g2"`
to `"g256"` are that many greys from black to white.

| function | answers |
|---|---|
| `palette_chars(p)` | the keys of palette `p`, a string, in palette order |
| `palette_color(p, k)` | the colour of key `k`, in the `"r,g,b"` form; fails when `p` has no such key |
| `palette_key(p, c)` | the key of the colour of `p` nearest colour `c` |
| `palette_grays(p)` | the keys of `p`'s greys, from black to white |

Each fails on a string that names no palette, and faults on a palette that is not a string or a key
that is not one character. The keys and colours are Icon's, number for number. **A palette's name is
read as Icon reads it**, with C's `sscanf(s, "%c%d%c")`: the letter, then a number that may follow
white space and carry a sign, and nothing after it, so `"c+2"` and `"c 2"` name `c2` and `"c2 "`
names nothing.

```funl
import { palette_chars, palette_color, palette_key, palette_grays } from funl:graphics

write( palette_chars("c2"), palette_chars("g8") )
write( palette_color("c1", "A"), palette_color("c3", "M"), palette_color("g5", "1") )
write( palette_key("c1", "light blue"), palette_key("c2", "orange"), palette_key("g16", "gray") )
write( palette_grays("c1"), palette_grays("c3") )
write( palette_chars("c7") | "no such palette", palette_color("c2", "z") | "no such key" )
```

```output
kbgcrmywx, 01234567
65535,0,0, 32767,32767,32767, 16383,16383,16383
j, r, 7
0123456, @abMcdZ
no such palette, no such key
```

**In `c1` the comma is a key**, very light yellow, so a `c1` image string has no punctuation but the
space. The keys of `c5`, `c6` and the greys past `g64` are the characters from code 0 up, so those
image strings are built with `char` rather than typed.

**`w.new_color(…)` fails**, as Icon's `NewColor` does on a true-colour display: there is no colour
map to take an entry of, and every colour is already a specification.

**`w.free_color(c, …)` and `w.color(i, …)` are Icon's with no mutable colours.** `free_color`
takes colour specifications and mutable colours' indexes, has nothing to free, and answers `w`;
`color(i)`, which reads mutable colour `i`, and `color(i, c, …)`, which sets it, fail. Each faults
where Icon's gives a run-time error: `free_color` given no colour or something that is neither a
string nor an integer, and `color` given no index, an odd number of arguments past one, or a first
index that is not an integer.

```funl
import { open } from funl:graphics

w = open( "map", {canvas: #hidden, size: (1, 1)} )
write( w.new_color("red") | "no colour map" )
write( w.free_color("red", -1), w.color(-1) | "no mutable colour" )
```

```output
no colour map
<window handle>, no mutable colour
```

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

## The current window

**`&window` is the window `open` made last**, so a procedure can draw on "the current window"
without being handed one, as Icon's do. A program may assign it a window, or `undefined` for none, and
it fails when there is none or the one it holds is closed:

```funl
import { open } from funl:graphics

def title() = &window.get( "label" )

a = open( "first", {canvas: #hidden} )
b = open( "second", {canvas: #hidden} )
write( title() )
&window = a
write( title() )
a.close()
write( &window | "no window" )
```

```output
second
first
no window
```

## Events

**A window's events wait on its queue, and `w.event()` takes the oldest**, answering its code:

- **a key with a character** is a one-character string: `"a"`, `"A"`, `"\r"`, `"\e"`, `"\x03"` for
  control-C;
- **a key with no character** is a symbol named for Icon's key constants, lower-cased: `#home`, `#end`,
  `#pgup`, `#pgdn`, `#left`, `#right`, `#up`, `#down`, `#insert`, `#f1` … `#f12`;
- **a mouse event** is a symbol, and the keywords `&lpress`, `&ldrag`, `&lrelease`, `&mpress`,
  `&mdrag`, `&mrelease`, `&rpress`, `&rdrag` and `&rrelease` answer the same symbols, `#lpress` …
  `#rrelease`; `&resize` is `#resize`. The wheel is `#wheelup`, `#wheeldown`, `#wheelleft` and
  `#wheelright`, and motion with no button held is `#move`;
- **closing the window** from its title bar is `#close`.

**`event()` also sets the event keywords**, Icon's set: `&x` and `&y`, where the pointer was, in the
window's coordinates (`dx` and `dy` taken off); `&row` and `&col`, the text cell under it, counted from
1; `&interval`, the milliseconds since the event before; and `&control`, `&meta` and `&shift`, which
succeed when the modifier was held and fail when it was not, so `if &shift then …` reads as Icon's.

**`w.enqueue(code, x, y, mods, interval)` puts an event on the queue**, which `event()` then reads
exactly as it reads one the screen put there. `x`, `y` and `interval` default to 0, and `mods` is a
string of `"c"`, `"m"` and `"s"` for the modifiers held, and `"r"` for a key let go. A hidden canvas
takes its events only from `enqueue`, which is how a program with no screen — a test — clicks and
types:

```funl
import { open } from funl:graphics

w = open( "events", {canvas: #hidden, size: (100, 60)} )
w.enqueue( #lpress, 10, 20 )
w.enqueue( "a", 15, 20, "s", 120 )
write( w.pending() )

while w.pending() do
  e = w.event()
  write( e, &x, &y, &interval, if &shift then "shift" else "-", if e == &lpress then "a click" else "a key" )
```

```output
2
lpress, 10, 20, 0, -, a click
a, 15, 20, 120, shift, a key
```

| method or function | what it does |
|---|---|
| `w.event()` | takes the oldest event, answers its code and sets the event keywords |
| `w.pending()` | answers how many events are queued; **fails when there are none**, so `if w.pending() then` asks whether any are |
| `w.enqueue(code, x, y, mods, interval)` | puts an event on the queue |
| `w.next_event()` | answers a promise of the oldest event, as a map, setting no keyword ([below](#the-promise-form-next_event)) |
| `active()` | generates each window that has an event waiting, one for each canvas |

**`inputmask` asks for the events not reported by default**: with `"m"` in it, motion with no button
held is reported as `#move`; with `"k"`, a key's release is reported too, as the same code with
`&released` succeeding, so a release is told from a press without a second set of codes. An event the
mask leaves out is not queued, as the screen would not queue it:

```funl
import { open } from funl:graphics

w = open( "keys", {canvas: #hidden} )
w.enqueue( "q", 0, 0, "r" )
write( w.pending() | "nothing queued" )
w.set( {inputmask: "k"} )
w.enqueue( "q" )
w.enqueue( "q", 0, 0, "r" )

while w.pending() do
  write( w.event(), if &released then "up" else "down" )
```

```output
nothing queued
q, down
q, up
```

**The event keywords belong to the machine that called `event()`**: each `async` task has its own,
so one task's events never change what another reads, and a task starts with `&x` and `&y` at 0 and
no modifier held.

**A window on screen is waited on**: `event()` on its empty queue, and `active()` when no window has
an event, wait for its user to press a key or click ([Windows on screen](graphics-screen.md#events-from-the-screen)).
**Nothing can queue an event on a hidden canvas while the program waits for one**, so there they
fault rather than wait for ever:

```funl
import { open } from funl:graphics

w = open( "quiet", {canvas: #hidden} )
w.event()
```

```error
'event': no event is queued, and none can come while the program waits: a hidden canvas takes events only from `enqueue`
```

### The promise form: `next_event`

**`w.next_event()` answers a promise of the next event, as a map**, so a task can await a click while
other tasks run, where `event()` would hold the whole program. The map carries what `event()` would
have set the keywords to:

| key | value |
|---|---|
| `code` | the event's code, as `event()` answers it: a one-character string or a symbol |
| `x`, `y` | where the pointer was, in the window's coordinates (`dx` and `dy` taken off) |
| `row`, `col` | the text cell under it, counted from 1 |
| `interval` | the milliseconds since the event before |
| `control`, `shift`, `meta`, `released` | `true` or `false` |
| `window` | the window `next_event` was called on |

**It sets no keyword.** An awaited value is settled as it stands, and a map read after the `await` is
the task's own, which another task's `event()` cannot overwrite where a keyword could:

```funl
import { open } from funl:graphics

w = open( "promised", {canvas: #hidden, size: (100, 60)} )
w.enqueue( #lpress, 10, 20 )
w.event()
w.enqueue( "a", 15, 25, "s", 120 )
e = await w.next_event()
write( e.code, e.x, e.y, e.interval, e.shift, e.control )
write( &x, &y )
```

```output
a, 15, 25, 120, true, false
10, 20
```

**`event()` and `next_event()` take from the one queue, each event once, in order.** An event already
queued settles the promise at once; otherwise the promise waits for the next event to reach the queue
-- from the window's user while the program waits on the [event loop](async.md), or from `enqueue` --
and promises waiting on one window are settled oldest first. A promise on a hidden canvas that nothing
will `enqueue` onto is one nothing settles, and a program left awaiting only that ends, as with any
such promise. Two tasks may each await their own window:

```funl
import { open } from funl:graphics

left = open( "left", {canvas: #hidden} )
right = open( "right", {canvas: #hidden} )

async def watch( w ) =
  e = await w.next_event()
  write( e.window.get("label"), e.code )

a = watch( left )
b = watch( right )
right.enqueue( #rpress )
left.enqueue( "k" )
await a
await b
```

```output
right, rpress
left, k
```

**A window closed while its event is awaited faults the promise**, as a call on the closed window
would, since a closed canvas is closed for every handle on it:

```funl
import { open } from funl:graphics

w = open( "brief", {canvas: #hidden} )
e = w.next_event()
w.close()
await e
```

```error
'next_event' was awaited on the handle <closed window handle>, which was closed while it waited
```
