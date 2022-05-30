## Embeddings

One common issue, in life as well as in computing, is that we often need to
present multidimensional information into a linear format. This is not unknown
to you, dear reader --- you run across this problem hundreds of time every day.
For example, my thoughts on encoding (the topic of this chapter) are themselves
a big, complicated web of interrelated ideas, that I must somehow encode, one
word after the next, in a way that attempts to convey meaning to you.

Speech and writing are two examples of this process, serializing information
over time and space, respectively. But this problem exists everywhere: from
storing electronic data to visualizing high dimensional geometry on paper.

To be more precise, the problem exists whenever we have information in one
"shape" and need to force the same information into a different, perhaps
less-natural, shape.

> TODO(sandy): image of a cube

Perhaps you'd say that @fig:cube is a handsome cube! But you'd be wrong; it
isn't. It's an encoding of a cube. A cube is a 3-dimensional object, that exists
in length, breadth and depth, capable of being rotated along any of these!
Compare that to @fig:cube, which is a flat projection of its 3-dimensional
counterpart. We are so used to equating encodings with the information they
attempt to communicate that we usually do it without noticing.

When encoding, we sometimes necessarily lose information. Any 2-d shadow of a
3-d object can never present the full story. While a shadow can give us a rough
understanding of the broad-scale geometry, some details must always remain a
mystery.

To illustrate this, an everyday photograph of a person can give you some idea of
what they look like --- indeed, we often define "what a person looks like" to be
exactly that which can be captured by a camera! But there is so much going on
beneath the surface. People age over time, which changes their outward
appearance. And on the inside, trillions of interrelated cells cooperate to
perform the necessary functions of life. Specialized medical technology can
indeed give us pictures of what a person looks like on the inside, often by
looking at a single "slice" of the body!

> TODO(sandy): image of an MRI

These aspects of what a person "is" are attempts to project a complicated,
multidimensional being, down to a 2 dimensional image. There is simply too much
information to fit onto the plane, and thus any such projection must
necessarily, if implicitly, choose what to focus on.

An encoding which necessarily throws away information is called a projection, in
the spirit of the cube above, or perhaps even Plato's shadows on the cave wall.
But not all projections are necessarily onto flat, 2-d surfaces! A census is a
projection from all the people in a country down to a statistical model of
pre-chosen questions. The information available in the census is necessarily
smaller than the information available in the population at large --- in the
latter, for example, we can determine John Smith's favorite color, while it is
unlikely that any census would have this particular information.

Likewise, any prose is necessarily a projection; I cannot (nor can other author)
send information from my brain into yours with perfect reliability.

In some domains, however, we are perfectly capable of reliably encoding
information. Consider, for example, the following grade-school equation:

$$
(2 + (7 - 3)) \times 5 = 28 + 2
$$

By virtue of being written on paper, this is already an encoding of some
information, serialized into a series of ink dots. This encoding, however, is
lossless. The equation itself is more naturally expressed as in
@fig:math-lossless.

```{#fig:math-lossless design=code/Languages/Math.hs}
asMath $ Equals ((2 + (7 - 3)) * 5) (28 + 2)
```

Patterns like these are the natural shapes of all mathematical expressions. The
linear structure of $(2 + (7 - 3)) \times 5 = 28 + 2$ has been turned into a
series of circles and arrows. The circles correspond to the mathematical ideas
we'd like to talk about, while the arrows show relationships between them. To be
more specific, the arrows mean that the pointee "belongs to," in some sense, the
pointer.

Diagrams like @fig:math-lossless are called trees.

An enticing part of the tree representation is that it requires no parentheses,
or explicit rules about "orders of operations." In fact, the parentheses are an
artifact of the serialization process that turns @fig:math-lossless into $(2 +
(7 - 3)) \times 5 = 28 + 2$ --- the parentheses exist merely to show where the
"subtrees" begin and end.

Despite the fact that a tree is a more natural representation for mathematics
than the usual, embedded notation, the tree is clearly less "compact" --- it
physically takes more space to write down! This is, at least in book format, a
compelling reason to use the embedded notation! This isn't true of all encodings
--- some are much, much larger --- but it's not uncommon either. Consider the
number 2000, which is an encoding much more compact than having two thousand of
anything!


### Evaluation

Why is the tree notation of an equation more "natural" than standard
mathematical notation? The best answer here is that the tree more closely
expresses the way we think about equations, and that it perfectly captures the
relationships we value --- namely, how to actually evaluate the answer!

We can get any equation tree into its simplest form by repeatedly performing the
following algorithm:

1. Find the smallest non-number subtree.
2. Replace this subtree with the number it computes.
3. Go back to step 1 if there is more work to do.

This process is illustrated in @fig:math-tree-arith.

```{#fig:math-tree-arith design=code/Languages/Math.hs}
Cons     " " (Equals ((2 + (7 - 3)) * 5) (28 + 2))
  $ Cons " " (Equals ((2 + 4) * 5) (28 + 2))
  $ Cons " " (Equals (6 * 5) (28 + 2))
  $ GoesTo " " (Equals 30 (28 + 2)) (Equals 30 30)
```

Algorithms like this appear in problem domains of every sort, and their wide
applicability is why trees are such interesting objects of study.


### Organizational Charts

Trees are the natural shape of many more things than just mathematics. For
example, we can present a traditional corporate org-chart as a tree. Such a
thing need not correspond  with the way influence *really* flows through the
company --- the org-chart is a projection of the true social influence network.
Nevertheless, the org-chart shows how a given company works in theory, if not
necessarily in practice! An example org-chart is given in @fig:org-chart.


```{#fig:org-chart design=code/Dot.hs}
LRose "Alan"
  [ LRose "Richard"
      [ LRose "Lisa" [LPure "Parson", LPure "Siobhan"]
      , LPure "Raymond"
      ]
  , LRose "Veronica"
      [LPure "Cynthia", LPure "Mark"]
  , LRose "Victor"
      [LPure "Michael", LPure "Tev"]
  , LPure "Wanda"
  ]
```

The organization presented by @fig:org-chart is a lossless encoding of the
org-chart tree, which itself is a *lossy* encoding of the organization of the
whole. It's like making a perfect copy of a document, coffee stain and all.

> TODO(sandy): coffee stained image

Implicit in any projection is the choice of what to project. Usually this choice
is itself a projection of the author's idea of what's important. But,
unfortunately, many projections are given only because they show the only thing
the author could find, or worse, the only thing the author could measure!


### Trees

What exactly is a tree? It's an explicit representation of a hierarchy of some
sort, where the whole is built out of self-similar things.

A tree is a collection of "nodes" --- people in an organization, or mathematical
ideas, or what-have-you --- equipped with a hierarchy relationship of some sort.
In the org-chart, this relationship is "directly reports to," while
mathematically it's something more like "is fully encapsulated by." These
relationships are indicated in our diagrams via arrows. For any arrow, the
pointer is called the "parent" of the pointee, which is known as a "child." This
terminology is likely uncomfortable for anyone in an org-chart, but it's
convenient to have a single word to use in multiple contexts.

Not everything is a tree, however. A tree is required to satisfy two properties:

1. Exactly one node has zero parents.
2. Every other node has exactly one parent.

The unparented node is known as the *root* of the tree. In our org-chart, Alan
is the root, and in our equation, `=` is the root.

Implicit in these two rules is the constraint that every tree is tree shaped ---
that is, like a biological tree that grows out in the real world. The
restrictions are sufficient to prevent any case where a node is an ancestor of
itself. Biologically, it would be challenging for an organism to be its own
mother, and professionally, it would be bad for an organization if someone were
their own boss (and could set their pay accordingly!)

An important property of trees is that they are *self-similar.* That is, that a
tree is made up of smaller trees, which themselves are made up of even smaller
trees. Given a tree, we can get a subtree by picking an arbitrary node and
deciding it will be the root --- ignoring the rest of the tree in the process.
In the business world, this corresponds to the idea that departments can be
thought of as independent entities.


```{#fig:suborg design=code/Dot.hs}
let { sub =
      LRose "Richard"
        [ LRose "Lisa" [LPure "Parson", LPure "Siobhan"]
        , LPure "Raymond"
        ];
    org =
      LRose "Alan"
        [ sub
        , LRose "Veronica"
            [LPure "Cynthia", LPure "Mark"]
        , LRose "Victor"
            [LPure "Michael", LPure "Tev"]
        , LPure "Wanda"
        ] }
 in GoesTo " " (focus "Richard" org) (fmap Unfocused sub)
```



