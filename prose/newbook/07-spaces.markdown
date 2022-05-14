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
let f h v = sHSV h v 1 in colorWheel 5 10 f
```

```{design=code/Languages/Plane.hs}
let f h x =  rgb h 0 x in plane 20 20 f
```

```{design=code/Languages/Plane.hs}
linear $ take 25 $ reverse $ sortOn (length . fst) namedColors
```

---

## Try again.

The first step in solving any problem is to figure out what are the salient
features of the thigngs under study. To work out an issue. we need to determine
*what the problem is,* and what a solution would look like. This is known in the
jargon as **describing the problem space.** A space is a generalization of a
physical space, like our everyday 3-dimensional world, coordinates on a map, or
determining whether a new sofa will fit in the living room.

A usually unstated assumption in the land of computation is that all problems
are solvable. This attitude is often interpreted by outsiders as naivety; sure,
maybe technical skills can help build bridges, but they're unlikely to solve
political problems. I agree that this is unlikely, but not because the
techniques are flawed, merely that we are unable to adequately define what the
problem is, or what a solution might look like. It is fair to say that the
problem space involving the messy world of humans and their psychology is
extremely big. By the end of this book, I expect you'll agree with the
assessment that human problems are unsolved primarily because we don't know how
to define the necessary spaces.

It might seems funny to think that all problems can be thought of spatially. As
is common in mathematics (and make no mistake, computation is mathematics,) a
concept was inspired by a real-world phenomenon, but over the decades, has had
all of real-world features taken away.

Let's take a particular problem as an example to be solved. We can consider the
issue to be "how can we darken a color?" That is, we'd like some sort of
mechanism that "moves" red into dark red, or white into gray. Here already we
can see the spatial metaphor sneaking in; if we can describe the *space of
colors,* we can define a solution to our problem as a particular way of *moving
through that space.*

So, what exactly is a color space? As a first approximation, we will say it is a
"coordinate system over colors" --- that is, a mapping from some numbers into a
color. But of course, that only punts on the problem. *Which* numbers should map
to which colors? By virtue of this question not having an immediate answer, we
must acknowledge that there is a decision to be made here. There are infinitely
many ways of mapping a number to a color, yet we have only finite amounts of
brain space and time in this universe; therefore, not only do we *not want* most
of these color spaces, we *can't want* most of them! Nevertheless, a decision
must be made.

Let's start with *bad* color space, to be subsequently contrasted with a good
one; just to get a sense of how the right framing of a problem can dramatically
help solve it. As our bad color space, we will simply assign numbers to colors
in the order we think of them. For example, maybe we pick the following
assignment:

1. Black (`color:black`)
2. Red (`color:red`)
3. Green (`color:green`)
4. Yellow (`color:yellow`)
5. Blue (`color:blue`)
6. Magenta (`color:magenta`)
7. etc.

The biggest problem here is that the sequence is unpredictable. It's unclear
what color will be listed at #7, and it's just as unclear where we should expect
to find orange (`color:orange`). A helpful space should allow us to reliably guess
the answers to both questions. But, there is no meaningful connection between
the numbers we've chosen and the colors they correspond to.

However, for our purposes, there is a related, but more important problem in
this randomly-built space --- it doesn't help us solve the problem darkening
colors. For example, where is dark green (`color:darkgreen`)? It's important to
know if we'd like to move from green (`color:green`) there!

Rather than continuing exploring this poorly-designed space, let's instead turn
our attention to a better one. You are undoubtedly aware that orange is somehow
"between" red and yellow, though it's not necessarily clear what "between" means
here. But, nevertheless, the spatial metaphor returns, since "between" is
related to "distance" which itself is a spacial concept. A well-defined space
should let us deduce the location of orange based on the locations of red and
yellow.

So, how can we do this? When in doubt, look to reality for inspiration. How do
we humans perceive color? Inside of our retinas are photoreceptive cells called
"cones." Cones come in three flavors: those that respond to red, those that
respond to green, and those that respond to blue. Our eyes are literally
incapable of seeing other colors, which should set off warning alarms in your
head --- because clearly we *can* see other colors! The solution to this
apparent paradox is in the choice of words: we can't *see* other colors, but we
can *perceive* them.

The way it works is that our eyes respond to the levels of red, green and blue
that they pick up, and then our brains fill in the missing information by
"reading between the lines." When only the red cones are activated, we
perceive red. When the red and green cones are activated to similar degrees, we
experience yellow. Taking the "average" between these two experiences, that is,
fully active red cones, and half active green cones, results in the perception
of orange.

We can use this information to define a color space, using three coordinates
which we will call `red`, `green`, and `blue`. Each coordinate can range from 0%
to 100%, corresponding to how activated each of those cones "flavors" are when
we look at the given color. Under such a system, we can move one "direction"
along the `red` axis to increase the amount of red in the color, and move in a
perpendicular direction `green` to increase the amount of green in the color.
Likewise, for blue. We will call this the **RGB color space.**

Each of the `red`, `green` and `blue` axes are perpendicular to one another,
that is to say, moving in one coordinate direction doesn't change your position
in any of the others. You can imagine this like a room in your house. As you
move left and right, you change how red you are. As you move forwards and
backwards, you change how green you are. And as you move up and down, you change
how blue you are. In such a room, @fig:plane-rg corresponds to the colors that
lie along the floor (that is to say, with `blue` being held constant at 0%.)

```{design=code/Languages/Plane.hs #fig:plane-rg}
let f r g =  rgb r g 0 in plane 40 40 f
```

When we allow `blue` to also vary, our space becomes a cube, with black in one
corner (no `red`, `green` or `blue` activation), white in the opposite, as well
as green (`color:green`), blue (`color:blue`), yellow (`color:yellow`), cyan
(`color:cyan`) and magenta (`color:magenta`). This is a hard thing to illustrate
in a book, but @fig:cube-rgb presents a "net" of the resulting color space that
could be folded into a cube.

```{design=code/Languages/Plane.hs #fig:cube-rgb}
net 16 16  rgb
```

Remember that this cube is entirely solid, full of less vibrant colors on the
inside. Importantly for this example, all of our everyday intuitions about
navigation hold in the RGB color space. That is, if we're looking for a color
perceptually-between two other colors, we can simply pick a color
spatially-between them. For example, there are no grays in @fig:cube-rgb, but we
know that gray lies somewhere between black and white, thus, all grays must lie
on the inside of the cube, along the diagonal that connects black to white.

Armed with the RGB color space, our original problem of darkening a color has
become more precise. We can now ask quantify the question to "*how dark* do you
want it?" Because black lies in one corner of the cube, and that the RGB color
space preserves spatial notions, darkening a color is simply moving closer to
the black corner.

As you can see, introducing the RBG color space has both clarified and
simplified our original problem. By finding a "natural" way of describing color,
we were able to rephrase the problem.

To a great degree, this is what computation engineering feels like; thinking
deeply about an imprecise question, and in the process, designing spaces which
in some way capture the relevant parts of the problem.

Let's pick some specific numbers. We can define the problem of "darkening a
color" to be, for illustration, to that color 25% of the way towards black. To
look at a few examples, that means:

> TODO(sandy): mapsto element is missing

* `color:green` &mapsto; `color:darken 0.75 green`
* `color:purple` &mapsto; `color:darken 0.75 purple`
* `color:aquamarine` &mapsto; `color:darken 0.75 aquamarine`

While examples like these give us a sense that what we've done probably works,
we'll get a better spatial sense of the problem by looking at these movements in
color space. For example, @fig:some-rgb-darkening shows the results of some
colors being darkened, while @fig:all-rgb-darkening shows the movement at all
points.

```{design=code/Languages/Plane.hs #fig:some-rgb-darkening}
showSome (const $ const $ withOpacity white 1) straightShaft (darken 0.75)
  [ rgb 1 1 0
  , rgb 1 0.5 0
  , rgb 0.25 0.7 0
  , rgb 0.5 0.1 0
  ] $ let f r g = rgb r g 0
       in plane 100 100 f
```

```{design=code/Languages/Plane.hs #fig:all-rgb-darkening}
showAll contrastingArrow    straightShaft (darken 0.75) $
   let f r b = rgb r 0 b
    in plane 20 20 f
```

As you can see in @fig:all-rgb-darkening, our spatial solution to darkening
colors is a succinct solution (move halfway towards black) to an infinite number
of problems (make `color:rgb 1 0 0` darker, make `color:rgb 1 0.5 0` darker,
etc. for every possible color.) This is sign of a job well done; the power of
computation comes from solving a class of problems *once and for all.* Imagine
if we tried to solve the problem of color darkening with our original random
color space. While it might be possible, to do, the solution certainly couldn't
possibly be succinct --- we'd need a rule for every possible color about where
to send it, since we wouldn't have anything systematic that could be exploited
towards our ends.


```{design=code/Languages/Plane.hs #fig:cube-hsv}
let f h = sHSV (h * 360) in net 16 16 f
```





