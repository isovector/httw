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


Obj/Tree

  : fig:math-lossless

  : Equations can be represented as trees.

  : "=" [ "\*" ["+" ["2", "-" ["7", "3"]], "5"]
        , "+"  ["28", "2"]]


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


Obj/Tree

  : fig:org-chart

  : A typical organizational chart.

  : "Alan"
      [ "Richard"
          [ "Lisa" ["Parson", "Siobhan"]
          , "Raymond"
          ]
      , "Veronica"
          ["Cynthia", "Mark"]
      , "Victor"
          ["Michael", "Tev"]
      , "Wanda"
      ]


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


```{#fig:suborg design=code/Languages/Tree.hs}
let { sub = asRose $
        "Richard"
        [ "Lisa" ["Parson", "Siobhan"]
        , "Raymond"
        ];
    org = asRose $ "Alan"
        [ sub
        , "Veronica"
            ["Cynthia", "Mark"]
        , "Victor"
            ["Michael", "Tev"]
        , "Wanda"
        ] }
 in GoesTo " " (focus "Richard" org) (fmap Unfocused sub)
```

A more "constructive" way of saying the same thing is that we can build bigger
trees out of smaller ones. The specific details about doing so depend on the
purpose of the tree --- for example, there are no rules in general about
building org-charts, but in a mathematical equation tree, we'd like there to be
only one `=` node, and we'd like for it to be at the root of the tree! After
all, it's meaningless to build mathematical expressions like $(5 = 8) + 3$.


### Inverses and Isomorphisms

In the next section, we'll discuss how to design an algorithm for encoding
arbitrary trees --- that is, on designing a "writing system" for articulating
ideas about trees in succinct ways. But before getting into the weeds, it's
important to first discuss why we'd want to do such a thing, and the important
considerations to keep in mind.

The purpose of a writing system is to be able to preserve ideas throughout time.
That is, once I have an idea, I am afforded the ability to write it down, forget
about it, and read the idea back later. Since human brains are incapable of
keeping very much in active memory, this system allows me to do some thinking,
write down a partial answer, think about something else, and then come back to
the original problem at a later time --- perhaps after having done some
ancillary thinking relating to the same topic!

It's no accident that philosophy sprung up as a discipline only after writing
systems did; the ideas philosophy tries to tackle are too big to fit inside of
our skulls all at once. The same is true of most of the modern world. Writing
(but not necessarily prose) is an essential extension of our brain.

Implicit in this capability is writing and reading are two halves of the same
coin. A writing system is of no use to us if we are unable to read it back
later. In fact, we'd like to go further, and say that (ideally) after reading,
the contents of our mind should be *exactly* what they were when we did the
writing.

Another way of saying that is:

> The idea we get from reading is the same one we wrote down.

Or even, for any particular thought $i$, we'd like to be able to say:

$$
\text{read} (\text{write } i) = i
$$

A closely related idea to this, perhaps more appropriate for medieval scribes,
is that writing out an idea that you just read should result in the same piece
of text! This exact mechanism is how most books were copied throughout the ages.
Symbolically, for any piece of text $t$, we can write:

$$
\text{write} (\text{read } t) = t
$$

We use this mathematical notation because these statements truly are equations.
They're not equations about *numbers*, but they are equations nevertheless!
Together, the two equations express the sentiment that reading and writing are
"opposites" of one another, and that we can move back and forth between ideas
and text without losing anything in the process.

Of course, this isn't strictly true about human minds; we're complicated
creatures with unpredictable responses to stimuli. But it's not hard to imagine
that these equations are true of a computer: the computer can write data to a
disk, and subsequently read that data back into memory without having lost any
of the information in the process.

Together, we say that `read` and `write` are inverses of one another.


### Encoding Trees

We turn our attention to finding a means of encoding trees.


Obj/Tree

  : fig:life

  : A (partial) tree of life.

  : "Life"
      [ "Archaea"
          [ "Haloarchaea"
          , "Methanosarcina"
          ]
      , "Bacteria"
          [ "Aquifex"
          , "Cyanobacteria"
          ]
      , "Eukaryota"
          [ "Animalia"
            [ "Arthropoda"
            , "Annelid"
            , "Mollusca"
              [ "Cephalopod"
              ]
            ]
          , "Fungi"
          , "Plantae"
          ]
      ]


We'd like to find a way to embed @fig:life into a series of words and symbols,
such that we can "read" it back into the same tree. In essence, the problem here
is to find a way of flattening out the tree, without losing any information. One
obvious idea is to split the tree into its tiers --- `Life` in one tier, and
`Archaea`, `Bacteria` and `Eukaryota` into the next --- and then embed those,
one after another. The result is:

> Life
> Archaea Bacteria Eukaryota
> Haloarchaea Methanosarcina Aquifex Cyanobacteria Animalia Fungi Plantae
> Arthropoda Annelid Mollusca
> Cephalopod

This is certainly a projection of our tree down into text --- but is it a proper
embedding? Can we subsequently "read" back the tree into its original form?
Unfortunately, this approach doesn't seem to work, because we don't know where
to form the subtrees. If I were ultimately antagonistic, I could read back this
encoding as a flat tree like in @fig:bad-flat-life.


Obj/Tree

  : fig:bad-flat-life

  : A bad decoding.

  : "Life"
      [ "Archaea"
      , "Bacteria"
      , "Eukaryota"
      , "Haloarchaea"
      , "Methanosarcina"
      , "Aquafex"
      , "..."
      ]


In order to prevent some sort of adversarially, intentionally
maximally-misunderstanding entity from getting the wrong idea, we clearly need
to encode more of the tree "structure." We can try arbitrary ideas all day long
until the cows come home, but befitting our need to encode more structure,
perhaps we should take a more structured approach.

A tree is made out of two things; a root node, and its sub-trees. We can encode
the root node as we were before, and then encode each of its sub-trees
immediately, one after another. It's important to keep track of where each
sub-tree begins and ends, so we can take a hint from mathematics and use
parentheses to "scope" each sub-tree.

We've said we can embed a tree by embedding its sub-trees, but isn't that just
passing the buck? How do we encode a sub-tree? Here's the beautiful part.
Remember that all trees are constructed out of smaller trees, thus, we can embed
a sub-tree in the same way --- write down its "root," and followed by each of
its sub-trees! We can follow this strategy all the way through the tree, working
on smaller and smaller sub-trees until there is no more work to be done.

When this algorithm is performed over the above tree, we get the following:

```
Life
  (Archaea
    (Haloarchaea)
    (Methanosarcina))
  (Bacteria
    (Aquifex)
    (Cyanobacteria))
  (Eukaryota
    (Animalia
      (Arthropoda)
      (Annelid)
      (Mollusca
        (Cephalopod)))
    (Fungi)
    (Plantae))
```

The white space here is merely formatting for our benefit. The human eye is well
attuned to visual organization, but this is superfluous to the problem. The tree
structure we'd like to encode is entirely captured by the parentheses:

```
Life (Archaea (Haloarchaea) (Methanosarcina)) (Bacteria (Aquifex)
(Cyanobacteria)) (Eukaryota (Animalia (Arthropoda) (Annelid) (Mollusca
(Cephalopod))) (Fungi) (Plantae))
```

Encoding trees like this brings about an interesting question: if a node has
multiple sub-trees, in which order should we encode them? Thus far we have
always written the sub-trees out left-to-right in the order that they appear on
the page. But recall that the presentation of a tree, as ink on paper, is merely
a projection of the ideal, platonic tree that we have in mind.

Nowhere in our discussion of trees have we mentioned that the children of a node
are in any particular order! We have mistaken the map for the territory: our
implicit assumptions about trees have been formed via analogy, and those
assumptions were misleading.

Of course, when we present a tree, either in the form of a diagram, or in the
nested parenthetical form above, we must make concessions; by virtue of being
spatial (and legible), a node's children must be in different places. And one of
those must be in a place that feels more natural to think of as being "first"
--- but this is merely an illusion. It's like being surprised that the name of a
street isn't magically floating in front of you as walk around a city. Just
because floating street names are a property of the map doesn't mean they
necessarily exist in the territory itself!

We are thus left with an uncomfortable decision; we can decide that the order of
our nodes' children is in someway meaningful, or we can decide that it isn't. If
we choose the latter, we are forced to concede that a tree does not have a
unique embedding! For example, @fig:bst has the nice property that every node in
the left sub-tree is smaller than the root, while every node in the right is
bigger than the root:


Obj/Tree

  : fig:bst

  : A sorted tree.

  : "5"
      [ "2" [ "1", "4" ]
      , "10"
        [ "9"
        , "13" [ "11", "14" ]
        ]
      ]


Are we really willing to stomach the fact that the above tree is exactly the
same as @fig:shuffle-bst?


Obj/Tree

  : fig:shuffle-bst

  : A unsorted tree.

  : "5"
      [ "10"
        [ "9"
        , "13" [ "14", "11" ]
        ]
      , "2" [ "4", "1" ]
      ]


Our choice here doesn't particularly matter; what's important is to recognize
that this is indeed a choice. It might seem like a matter of trivial semantics,
but decisions like these are of profound significance when you playing the role
of designer god, as we are.

Computation requires this sort of precision, not because we are fans of
precision, but because we humans are exceptionally good at fooling ourselves
into thinking we know what we're talking about. This precision of thought is our
only tool against false beliefs. It's often annoying and tedious, but it's a
necessary part of the job.

