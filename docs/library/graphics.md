---
title: funl:graphics
weight: 22
---

# `funl:graphics` — windows, after Icon's

`funl:graphics` gives FunL the graphics facilities of Icon 9.5. A window is a handle, and everything
done with it is a method on that handle. The module exports `open`, which makes a window; `couple`,
which joins two; `color_value`, which reads a colour; and `active`, which finds the windows with
events waiting.

This page covers windows on a **hidden canvas**: a picture drawn in memory and read back, with no
window on screen, [drawing](#drawing) on it, its events, put on its queue by the program, and
[text and fonts](#text-and-fonts). Images come with a later part of the module.

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
{"size": (40, 30), "width": 40, "height": 30, "label": "attrs", "canvas": hidden, "resize": false, "cursor": false, "echo": true, "inputmask": "", "fg": "red", "bg": "white", "reverse": false, "linewidth": 3, "linestyle": dashed, "fillstyle": solid, "pattern": "black", "drawop": copy, "clipx": undefined, "clipy": undefined, "clipw": undefined, "cliph": undefined, "dx": 0, "dy": 0, "depth": 24, "density": 1, "font": "fixed", "leading": 13, "ascent": 10, "descent": 3, "fheight": 13, "fwidth": 6, "x": 0, "y": 10, "row": 1, "col": 1}
```

| attribute | value | default |
|---|---|---|
| `size` | `(width, height)` | 80 columns by 12 lines of the font: `(480, 156)` |
| `width`, `height` | an integer from 1 to 32767 | |
| `label` | the title, a string | `open`'s title |
| `canvas` | `#hidden` (`#normal`, `#maximal` and `#iconic` are on-screen states) | |
| `resize` | whether the user may resize the window | `false` |
| `cursor` | whether the text cursor is shown | `false` |
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
`#striped`, the same dashes over a line in the background colour.

**`fillstyle`** decides what every drawing paints with. `#solid` paints the foreground. `#textured`
paints the window's **`pattern`**, its ones in the foreground and its zeros in the background, and
`#masked` paints its ones only, leaving what is under the zeros. The pattern repeats from the
canvas's corner. A pattern is one of Icon's names -- `"black"`, `"verydark"`, `"darkgray"`,
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
of it and out otherwise, since a pixel only partly changed could not be changed back. Under
`#textured` or `#masked` only the pattern's ones are exclusive-ored, and a `#striped` line changes
only its dashes; a pixel the pattern or the gaps leave alone stays as it was. **`erase_area`, `copy_area`
and the terminal's [`write` and `writes`](#the-window-as-a-terminal) are copies under either
operation**, as Icon's `EraseArea`, `CopyArea` and `WWrite` are.

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

**On a hidden canvas a read the queue cannot finish faults**, as `event()` does on an empty queue —
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

**Nothing can queue an event while the program waits for one** in this build, where every canvas is
hidden, so `event()` on an empty queue, and `active()` when no window has an event, fault rather than
wait for ever:

```funl
import { open } from funl:graphics

w = open( "quiet", {canvas: #hidden} )
w.event()
```

```error
'event': no event is queued, and none can come while the program waits: a hidden canvas takes events only from `enqueue`
```
