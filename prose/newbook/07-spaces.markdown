CS is a bad name. It's not about computers and it's not a science.

The study of computation is about three things:

1. Conciseness - we'd like to solve an infinite class of problems using a small,
   finite set of rules
2. Correctness - we'd like to get a useful answer
3. Abstraction - it should help us solve a class of problems


## Spaces

We begin our journey into computation by way of a series of familiar examples in
the real world, starting from the mundane and moving progressively into deeper
and deeper abstract territory. One of the core tenets of computation is its
focus on highly-abstract ideas, and thus we will need to flex our abstraction
muscles before much else will be possible.

<!-- TODO(sandy): TEMPERATURES are a great simpler example -->


As a first introduction to spaces, consider an atlas of the Earth. With only two
coordinates, latitude and longitude, we can accurately pinpoint any location on
the planet. Latitude and longitude, the *axes* of the coordinate system, are
*orthogonal* to one another, that is to say, moving along one axis doesn't move
you along the other.

The equator is a natural "middle ground" on the planet, and thus we
measure latitude with reference to equator. Another way of saying that is that
latitude is defined such that the equator is always at 0&deg; latitude.

For longitude, there is no natural "zero" line, so it is arbitrarily chosen to
be the prime meridian. Thus, we have the point 0&deg;N, 0&deg;W which lies on
the equator and prime meridian, and is uniquely defined on Earth as being the
zero point, or *origin* of our coordinate system. The origin is always the point
in the space from which we start measuring.

All spaces are defined in a similar way to latitude and longitude. The important
concerns are

1. what are the space's axes, and
2. what is the origin of the space

Another space you're likely familiar with is the coordinate system on a chess
board. A chess board is divided into two axes, ranks and files. Ranks span side
to side, such that all of a player's pawns start in the same rank. Files span
top to bottom, such that both player's queens start in the same file.

Although it might sound silly to point out such an obvious thing, ranks and
files are orthogonal; going left or right in a rank doesn't change which file
you're in (although certainly some pieces will move their rank and file
positions simultaneously!)

Ranks are numbered, starting from `1` at white's side, spanning to `8` on
black's side. Files are ordered alphabetically, with `a` being on white's left,
and `h` being on their right. Although there is no "all zero" point like on the
atlas, the chess board space still has an origin. Recall that the origin is the
point from which begin measuring positions along the axis. Since `1` is the
first number and `a` is the first letter, `a1` (the starting point for white's
left-hand rook) is the origin of the space.

It's important to note that the origin of any space is arbitrarily chosen.
Picking a different origin doesn't change the space itself, it merely changes
the coordinates that we assign to specific places in the space itself. It's not
hard to imagine a globe where the 0&deg; longitude line is chosen to be one
which goes through Singapore rather than Greenwich. Such a decision doesn't
change the planet in any meaningful way, it merely shifts the numbers we assign
to different places on the globe. Though there are often "natural" choices for
origins, the origin can always be chosen arbitrarily. The resulting system is
always meaningful and coherent, though it isn't necessarily convenient.

<!-- TODO(sandy): this stuff might be irrelevant if we just care about state spaces -->


```{design=code/Languages/ColorWheel.hs}
colorWheel 5 10 $ \h v -> sHSV h v 1
```

```{design=code/Languages/Plane.hs}
plane 20 20 $ \h v -> sHSV h 1 v
```


color wheel


