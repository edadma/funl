---
title: funl:graphics
weight: 22
---

# `funl:graphics` — windows, after Icon's

`funl:graphics` gives FunL the graphics facilities of Icon 9.5. A window is a handle, and everything
done with it is a method on that handle. The module exports `open`, which makes a window; `couple`,
which joins two; and `color_value`, which reads a colour.

This page covers windows on a **hidden canvas**: a picture drawn in memory and read back, with no
window on screen, and [drawing](#drawing) on it. Images, text and events come with later parts of the
module.

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

A canvas opened with no `size` is Icon's default, 80 columns by 12 lines of the default font. It
starts painted in its background colour.

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
{"size": (40, 30), "width": 40, "height": 30, "label": "attrs", "canvas": hidden, "resize": false, "fg": "red", "bg": "white", "reverse": false, "linewidth": 3, "linestyle": dashed, "fillstyle": solid, "pattern": "black", "drawop": copy, "clipx": undefined, "clipy": undefined, "clipw": undefined, "cliph": undefined, "dx": 0, "dy": 0, "depth": 24, "density": 1}
```

| attribute | value | default |
|---|---|---|
| `size` | `(width, height)` | `(480, 156)` |
| `width`, `height` | an integer from 1 to 32767 | |
| `label` | the title, a string | `open`'s title |
| `canvas` | `#hidden` (`#normal`, `#maximal` and `#iconic` are on-screen states) | |
| `resize` | whether the user may resize the window | `false` |
| `fg`, `bg` | a colour; `fg` may also be [transparent](#painting-styles) | `"black"`, `"white"` |
| `reverse` | `true` swaps `fg` and `bg`, `false` swaps them back | `false` |
| `linewidth` | an integer, 0 or more | `1` |
| `linestyle` | `#solid`, `#dashed` or `#striped` | `#solid` |
| `fillstyle` | `#solid`, `#textured` or `#masked` | `#solid` |
| `pattern` | a [pattern](#painting-styles): a name or a bit pattern, a string | `"black"` |
| `drawop` | `#copy` or `#reverse` | `#copy` |
| `clipx`, `clipy`, `clipw`, `cliph` | the clip rectangle; setting one when there is none starts from the whole canvas, and `undefined` takes it away | `undefined` |
| `dx`, `dy` | the origin, added to every coordinate | `0` |
| `depth` | read-only: bits per pixel | `24` |
| `density` | read-only: device pixels per point, always 1 on a hidden canvas | `1` |

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
