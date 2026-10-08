---
title: funl:time
weight: 20
---

# `funl:time` — clocks, waiting and timestamps

`funl:time` reads the clocks, waits, and turns points in time into ISO 8601 text and back.

**A point in time is a whole number of milliseconds since 1970-01-01T00:00:00Z**, so two of them
compare, subtract and sort as ordinary numbers. An **offset** is whole minutes east of UTC; where one
is left out, UTC is meant.

```funl
import { make, format } from funl:time

t = make( 2026, 3, 8, 14, 30, 0 )
write( t )
write( format( t ) )
```

```output
1772980200000
2026-03-08T14:30:00Z
```

## The clocks

`now()` is the wall clock, in whole milliseconds. It is the clock a person or a time service may set,
so two readings of it may differ by anything, even a negative amount. `monotonic()` is for measuring:
milliseconds as a real, counted from an origin nobody names, that never goes back. A single reading
means nothing; the difference of two is how long something took. `sleep(ms)` waits that many
milliseconds, whole or fractional.

```funl
import { now, monotonic, sleep } from funl:time

start = monotonic()
sleep( 20 )
write( if monotonic() - start >= 20 then "waited" else "early" )
write( if now() > 1700000000000 then "after 2023" else "before" )
```

```output
waited
after 2023
```

## Making a point in time

`make(year, month, day)` is midnight UTC of that day; `make(year, month, day, hour, minute, second)`
adds the time of day; a seventh argument says the reading is on that offset. A date or time that does
not exist **fails**:

```funl
import { make } from funl:time

write( make( 2024, 2, 29 ) )
write( make( 2026, 2, 30 ) | "no such day" )
write( make( 2026, 1, 1, 24, 0, 0 ) | "no such time" )
write( make( 2026, 3, 8, 9, 30, 0, -300 ) )
```

```output
1709164800000
no such day
no such time
1772980200000
```

## Writing one down

`format(t)` writes an RFC 3339 timestamp, in UTC; `format(t, offset)` writes the wall reading on that
offset. A fraction of a second appears only when there is one.

```funl
import { make, format } from funl:time

t = make( 2026, 3, 8, 14, 30, 0 )
write( format( t, -300 ) )
write( format( t, 330 ) )
write( format( t + 250 ) )
write( format( -1 ) )
```

```output
2026-03-08T09:30:00-05:00
2026-03-08T20:00:00+05:30
2026-03-08T14:30:00.250000Z
1969-12-31T23:59:59.999000Z
```

## Reading one

`parse(text)` is the reverse. It takes the `T` or a space between date and time, seconds or none, and
`Z` or an offset. Text that is not a timestamp **fails**, so `|` gives a default.

```funl
import { parse } from funl:time

write( parse( "2026-03-08T09:30:00-05:00" ) )
write( parse( "1970-01-01 00:01Z" ) )
write( parse( "yesterday" ) | "not a timestamp" )
write( parse( "2026-13-01T00:00:00Z" ) | "not a timestamp" )
```

```output
1772980200000
60000
not a timestamp
not a timestamp
```

## The calendar reading

`fields(t)` is an immutable map of the reading: `year`, `month`, `day`, `hour`, `minute`, `second`,
`millisecond` and `weekday`, 1 for Monday through 7 for Sunday. `fields(t, offset)` reads it on an
offset.

```funl
import { make, fields } from funl:time

f = fields( make( 2026, 3, 8, 14, 30, 5 ) + 250 )
write( f("year"), f("month"), f("day"), f("hour"), f("minute"), f("second"), f("millisecond") )
write( f("weekday") )
write( fields( make( 2026, 3, 8, 14, 30, 5 ), -300 )("hour") )
```

```output
2026, 3, 8, 14, 30, 5, 250
7
9
```

## The host's zone

`local_offset(t)` is the offset, in minutes, the host's own time zone is set at time `t`, so
`format(t, local_offset(t))` is the host's wall reading. What it answers depends on the machine.

```funl
import { local_offset } from funl:time

o = local_offset( 0 )
write( if o > -1440 & o < 1440 then "within a day" else "outside" )
```

```output
within a day
```

## Faults

An argument of the wrong kind is a fault that `catch` takes, and an offset past a day is one too.

```funl
import { format } from funl:time

write( format( "x" ) )
```

```error
'format' wants a whole number and was given the string 'x'
```

```funl
import { format } from funl:time

write( format( 0, 5000 ) catch e -> "caught" )
```

```output
caught
```
