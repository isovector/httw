## Abstraction

Lots of ideas that look different at first blush are actually different examples
of the same overarching idea. The core skill in computation is the ability to
squint at several ideas, ignore their differences, and see what is common
between them. This technique is called abstraction, and is the most powerful
tool in the computationalist's belt, because finding a solution to the abstract
problem is a solution to all of the other problems. Thus, the practitioner can
multiply their effort, solving several (often infinitely many) problems in the
time it takes to solve only one.

Imagine being a comic artist, needing to draw a variety of characters in a
variety of poses. While it's possible to find source material to use as
inspiration for very pose you'll need, this is akin to solving lots of
slightly-different instances of the same problem. Instead, you can save a huge
amount of effort by learning enough anatomy to not require source material.

Take for example, addition, multiplication, and exponentiation --- what is the
unifying idea underlying all three of these operations? They are all operations
over numbers certainly, but can we say anything more "structural" about them?
Consider the following three observations:

$$
5 + 3 = 5 + 1 + 1 + 1
$$

$$
5 \times 3 = 5 + 5 + 5
$$

$$
5^3 = 5 \times 5 \times 5
$$

There is clearly something going on here; each operation does *an operation* some
number of times corresponding to the given number. Furthermore, the operation
being defined in one case is the operation being used in the next. Addition
builds on succession, multiplication builds on addition, and exponentiation
builds on multiplication. We can express this idea as a syllogism:

> Multiplication is to addition as exponentiation is to multiplication.

Whenever you see a syllogism like this, where the same idea (multiplication) is
on either side, you should start looking for an abstraction. An intriguing
question is what should fill in the following blank:

> ______ is to exponentiation as exponentiation is to multiplication.


The first step is to come up with a name for the thing we're trying to talk
about; let's call it "super-exponentiation," and use the symbol &uarr;&uarr;. As
in:

$$
5 \mathbin{\uparrow\uparrow} 3
$$

The pattern for super-exponentiation actually seems quite straightforward; we
need to construct a "power tower." That is,

$$
5 \mathbin{\uparrow\uparrow} 3 = 5 ^ {(5 ^ 5)} = 5 ^ {3125}
$$

Super-exponentiation is capable of making really big rather quickly. For
example, $5 \mathbin{\uparrow\uparrow} 3$ is already 2185 digits long! You might
wonder why on earth we might want to construct numbers as big as this, but
that's the wrong question to ask. It's not a question of "why," but rather, "why
not?" Finding patterns like this is much like doing a puzzle; it's to be done
for its own sake, not because it's a fundamentally valuable use of time. In
fact, a huge percentage of math is invented half a century before anyone comes
up with a use for it.

Is there "super-super-exponentiation?" Well, why not? We can repeat the same
pattern, instead making &uarr;&uarr;&uarr; expand into $n$-copies of
&uarr;&uarr;. As an illustration:

$$
5 \mathbin{\uparrow\uparrow\uparrow} 3 = 5 \mathbin{\uparrow\uparrow} (5\mathbin{\uparrow\uparrow} 5)
= 5 \mathbin{\uparrow\uparrow} 5^{5^{5^{5^5}}} = 5 \mathbin{\uparrow\uparrow} 5^{5^{5^{3125}}}
$$

which is a very very large number indeed --- it's a tower of exponents that is
"astronomically" (English doesn't have sufficiently big words here) taller than
the huge 2185 digit number we computed from $5 \mathbin{\uparrow\uparrow} 3$.
We're talking orders of magnitude bigger than the number of centimeters in the
diameter of the observable universe.

Again, there's no reason to build numbers as big as this; it's just fun to try.
Of course, we can follow the same technique to build
super-super-super-exponentiation, and super-super-super-super-exponentiation,
and so on ad infinitum. But you get the idea.

Having reasoned through all the various levels of "meta"-exponentiation, we're
left only with building such a thing mechanically. Like all the operations we've
looked at before, meta-exponentiation has both a left- and a right-hand side,
which we will call `a` and `b` respectively. However, meta-exponentiation also
has a third setting, or parameter, which is the level of exponentiation we'd
like. Let's pick the notation `hyper n a b` for this problem.

It's traditional in mathematics to begin counting at zero, so we will choose
level zero (that is, $n = 0$) to be addition, level one to be multiplication,
two to be (usual) exponentiation, and so on and so forth. We can thus describe
the rule that $n = 0$ corresponds to addition via:

```haskell
hyper 0 a b = a + b
```

The trick now is to determine how multiplication cashes out in terms of
addition. That is, we can see the following expression always holds true:

$$
a \times b = a + a \times (b - 1)
$$

and of course, we also know that

$$
a \times 0 = 0
$$


```haskell
(+) a zero = a
(+) a (suc b) = suc ((+) a b)

(*) a zero = 1
(*) a (suc b) = a + (*) a b

(^) a zero = 1
(^) a (suc b) = a * (^) a b

(^^) a zero = 1
(^^) a (suc b) = a ^ (^^) a b

f zero a zero = a
f zero a (suc b) = suc (f zero a b)
f (suc n) a zero = 1
f (suc n) a (suc b) = f n a (f (suc n) a b)
```


### Spotting Structure

The game of squinting and abstracting works all over the place. What does a city
metro have in common with a population's social connections? One interesting
feature is that both can be thought of as interconnected networks. The city
metro is a number of stations joined by trains running between them, while a
population is a number of people joined by social ties between them. Those
social relationships might be "are friends" or "have dated" or really any
distinction you might want to draw between two people.

Of course, some social relationships might only work in one direction; if Joan
is Gerald's best friend, that doesn't mean that Gerald is Joan's! This scenario
doesn't often happen on the subway, but it does in road networks, where some
streets are one-way only.

Transportation systems and people are entirely different things in the real
world, but if we ignore the vast majority of the details, they both look like
interconnected networks. And doing this allows us to use metaphors from one
domain in the other; that is, we can see how many "station hops" are required to
get from one person to another, or we can see if any subway stations are "not
dating anyone right now," or peculiar questions like those.

One question that makes sense on the subway is "which two stations are the
furthest away from one another." That notion of distance is not physical
separation, but instead, which two stations require the most legs to get to
traverse? An intriguing question if you're attempting to determine where to live
after you've already got a job lined up!

But this question is also interesting when transferred to the real of humans.
Which two people are the furthest friendship hops away from one another? How
big is that number of hops?

Incidentally, this idea of an interconnected network is called a *graph.* The
"people" of the graph are called its *nodes,* while the "friendships" are called
its *edges.*Graphs show up everywhere, even if we need to build them for
ourselves.

Graphs don't need to exist physically. We can build a graph whose nodes are
words, where two words share an edge if they differ by only one letter. For
example, the following pairs words would have an edge:

* `apple` - `apples`
* `ear` - `far`
* `crane` - `crate`

but the following pairs would **not* have an edge:

* `chapel` /- `rabbit`
* `berry` /- `betsy`
* `lead` /- `lede`

What's the longest path on this graph? Are there any nodes which aren't
connected to any other nodes?

> TODO(sandy): is this an interesting question?

> TODO(sandy): code it up and get an answer if it is

