## Languages

An extremely important idea in computer science is that of the tree. A tree
look something like this:

```{#fig:tree-ex design=code/Dot.hs}
Rose [Rose [Pure Club, Pure Club], Pure Heart, Pure Diamond, Rose [Rose [Pure Spade]]]
```

It looks more like a conventional, real-world tree if you imagine it
upside-down. But, for whatever historical reason, these trees are drawn
top down. Trees are made up of *nodes,* which are those circles with arrows
between them. A node may or may not have *children,* which are other nodes that
it points to. For example, the node at the top of @fig:tree-ex has four
children: &otimes;, &hearts;, &diams;, and &otimes;.

If &hearts; is a child of &otimes;, then, as you might expect, &otimes; is a
*parent* of &hearts;. Likewise, we have the notions of *ancestors* and
*descendants* in a tree.

We don't allow any "incestuous" relationships between nodes of a
tree. Every node (except the "root" of the tree) must have exactly one parent,
and it is not allowed to be one of its own descendants --- that is to say, no
loops are allowed.

To illustrate these two rules, *neither of @fig:not-a-tree-loop nor
@fig:not-a-tree-parents are trees,* even though they look superficially similar
to @fig:a-tree --- which is.

```{#fig:not-a-tree-loop design=code/Dot.hs label="Not trees"}
Beside
  [ do
      a <- newNode "A"
      b <- newNode "B"
      c <- newNode "C"
      addEdge a b
      addEdge a c
      addEdge c a
      pure a

  , do
      a <- newNode "A"
      b <- newNode "B"
      c <- newNode "C"
      addEdge a b
      addEdge a c
      addEdge b c
      pure a
  , do
    a <- newNode "A"
    b <- newNode "B"
    c <- newNode "C"
    addEdge a c
    addEdge b c
    pure a
  ]
```

```{#fig:a-tree design=code/Dot.hs label="Trees"}
Beside
  [ invisNode
  , newNode "&hearts;"
  , do
      a <- newNode "A"
      b <- newNode "B"
      c <- newNode "C"
      addEdge a b
      addEdge b c
      pure a
  , do
      a <- newNode "X"
      b <- newNode "X"
      c <- newNode "X"
      d <- newNode "X"
      e <- newNode "X"
      addEdge a b
      addEdge a c
      addEdge a d
      addEdge a e
      pure a
  ]
```

That's enough theory for now. Let's play a little tree game. I will give you a
tree, like @fig:yanay. The goal of the game is change the tree into the smallest
one you can find.  But there's a trick --- you're only allowed to "reduce" the
tree via a few, very specific rules. The tree I give you will only have nodes
labeled `Y`, `N`, and `A`.

```{#fig:yanay design=code/Languages/And.hs}
A Y (A N Y)
```

Ready for the rules?

```{#fig:yna-rule1 design=code/Languages/And.hs label="Rule 1"}
GoesTo "Rule 1" (AnyContext $ A N N) $ AnyContext N
```

Rule 1 says that whenever you see a "sub-tree" that is an `A` node with two `N`
children, you can replace it with a single node labeled `N`. The &hellip; node
means that this rule can be used anywhere in the tree.

To illustrate this, we could use rule 1 to make the following transformation:

```{#fig:yna-rule1-ex design=code/Languages/And.hs}
let f x = A (A Y Y) (A x (A Y Y)) in GoesTo "Rule 1" (f $ A N N) $ f N
```

In @fig:yna-rule1-ex, we used rule 1 to *rewrite* the `NAN` sub-tree as just a
single `N` node. Crucially, in doing so, the remainder of the tree stays exactly
the same.

Sometimes a single rule applies to multiple parts of the tree at once, like in
@fig:yna-rule1-ex2, where we have two `NAN` sub-trees.

```{#fig:yna-rule1-ex2 design=code/Languages/And.hs}
A (A N N) (A N N)
```

When a situation like this arises, we can apply the rule at any place it
matches. That is, both the trees in @fig:yna-rule1-ex2-results are appropriate
"transitions," and you can go in whichever direction seems more promising to
you.

```{#fig:yna-rule1-ex2-results design=code/Languages/And.hs label="Possible transitions after @fig:yna-rule1-ex2"}
Beside
  [ A N (A N N)
  , A (A N N) N
  ]
```

Of course, nothing stops you from using the rule again immediately afterwards,
if the other possibility is still a match.

Let's move on to rule 2:

```{design=code/Languages/And.hs label="Rule 2"}
GoesTo "Rule 2" (AnyContext $ A Y (MV Club)) $ AnyContext $ MV Club
```

What's this &clubs; doing here? Didn't I promise you that the only nodes in this
game were `Y`, `N` and `A`? This &clubs; symbol is not a part of the real tree,
it's a "placeholder" for *any sub-tree you want.* Whenever you apply rule 2, you
can pick &clubs; to be anything at all.

In words, then, rule 2 says "we can remove the `A` and `Y` nodes if `Y` is on
the left side." As an example, we could make transformation shown in
@fig:yna-rule2-ex by letting &clubs; "fill in" for `NAY`:

```{#fig:yna-rule2-ex design=code/Languages/And.hs}
let f x = x (A N Y) in GoesTo "Rule 2" (f (A Y)) $ f id
```

Like rule 1, rule 2 can be applied to any sub-tree, anywhere in the tree.

But rule 2 isn't magical. It can't reduce a `Y` on the *right* side. For
example, the following transformation is **illegal** via rule 2:

```{#fig:yna-rule2-nex design=code/Languages/And.hs label="An **illegal** move!"}
let f x = x (A N N) in GoesTo "Rule 2" (f (flip A Y)) $ f id
```

If we find ourselves in a situation like @fig:yna-rule2-nex, not all is lost. Rule 3
can sometimes be helpful:

```{design=code/Languages/And.hs label="Rule 3"}
GoesTo "Rule 3" (AnyContext $ A (MV Club) (MV Diamond)) $ AnyContext $ A (MV Diamond) (MV Club)
```

In rule 3, we now have two placeholders --- &clubs; and &diams; --- each of
which can fill in for any sub-tree that would be convenient. In effect, rule 3
says we can swap what's on the left and the right of an arbitrary `A` node.
This is illustrated by @fig:yna-rule3-ex.

```{#fig:yna-rule3-ex design=code/Languages/And.hs}
let f x = x A (A Y N) N in GoesTo "Rule 3" (f id) $ f flip
```

Note that rule 3 only moves the left sub-tree to the right, and vice versa. It
doesn't "mirror" the left and the right sides!

In addition to these three "rules of play", there are two additional ones, which
broadly state "you are not allowed to cheat."


RULE 4.

:  Rules 1-3 cannot be run backwards. That is to say, you can't replace an
   `N` with `NAN` by running rule 1 in the opposite direction.


RULE 5.

:  You are not allowed to change the term in any way other than via rules 1-3.


Let's look again at @fig:yanay (the "`YA(NAY)` tree,) and play with these rules.
Remember, the goal is to produce as small a tree as possible. At any point,
multiple rules might apply, and we can decide to follow any one of them. Because
our tree does not contain `NAN`, rule 1 does not apply. But both rule 2 and 3
are available to us.

By choosing to follow different rules, we will get to different trees. Each of
the three trees in @fig:yanay-next is possible.

```{#fig:yanay-next design=code/Languages/And.hs label="Possible transitions after @fig:yanay"}
Beside
  [ A N Y
  , A (A Y N) Y
  , A Y (A Y N)
  ]
```

We can get to these trees in order by via the different routes:

1. `NAY`, by following rule 2
2. `(NAY)AY`, by following rule 3 and pivoting around the root `A`
2. `YA(YAN)`, by following rule 3 and pivoting around the child `A` instead.

Of these three, #1 is the shortest, so we might choose to explore rules
accessible to us from that string. Picking the direction that moves us closest
to the goal is called a *greedy* strategy, and it works better in same games
than it does in others.

From `NAY`, our only option is rule 3, to transform the tree into `YAN`:

```{#fig:nay-swap design=code/Languages/And.hs label="NAY to YAN"}
GoesTo "Rule 3" (A N Y) (A Y N)
```

And from `YAN`, we can either apply rule 3 to swap into `NAY`, or we can apply
rule 2 to get `N`. The former is clearly an unhelpful move, so we will decide to
use rule 2 instead.

Looking at `N`, no more rules apply, so we say that this string is in *normal
form.* It can't go anywhere else, so if we ever get to `N`, we're stuck. But a
tree with a single node is a very small tree indeed!

We can write out the *derivation* of our result by drawing out the intermediate
trees, and the rules that we took along the way, like in @fig:yanay-deriv.

```{#fig:yanay-deriv design=code/Languages/And.hs label="Derivation of `N`"}
Cons "Rule 2" (A Y (A N Y))
  $ Cons "Rule 3" (A N Y)
  $ GoesTo "Rule 2" (A Y N) N
```

Now that the rules of the game are clear, try finding derivations to normal
forms for the trees in @fig:and-more-starts on your own. While you work through
them, pay close attention to the process.

```{#fig:and-more-starts design=code/Languages/And.hs label="More starting trees"}
Beside
  [ A (A (A N N) Y) (A N N)
  , A (A (A N N) Y) Y
  , A Y (A (A Y N) N)
  , A (A Y (A Y Y)) (A (A Y Y) Y)
  ]
```

If you've diligently worked through these four trees, you've probably noticed
some things about the derivation process. For example, there never seems to be
any `A` nodes left over when you get to a normal form. And that regardless of in
what order you apply the rules, you always get to the same normal form from a
given starting tree. Perhaps you even came up with some shortcuts for finding
the normal form faster than by doing all the rules.

<!--

Let's now discuss the form of the starting strings. You probably already have
an intuition for what the rules of its "grammar" are, but we will write them
here explicitly; they are likely phrased differently than you expect.

1. `Y` is grammatical.
2. `N` is grammatical.
3. If `j` and `k` are both grammatical, then `jAk` is a grammatical.

Rule 3 might need an illustration. We are asked to choose strings `j` and `k`
which are grammatical, and having done so, we can stick them together with an
`A` in between. For example, we might choose `Y` for `j` and `NAN` for `k`. Both
of these is grammatical, so we can put them together into `YANAN`.

Note that rule 3 doesn't mean you can put the *letters* `j` or `k` into a term!
These are merely symbolic "placeholders" for us, the humans, to talk about how
to build grammatical strings. If you ever write a string that has a literal `j`
or `k` in it, you've gone wrong somewhere!

Also note that nothing says `j` and `k` must be different strings. They *can* be
different, but need not be. Choosing `j = k = Y` is just fine!

By the rules of grammar above, the following strings are all grammatical: `Y`,
`N`, `YANANAY`. And the following terms are **not**: `X`, `YYY`, `NANN`, `jAk`,
`AYA`.

Given these rules of grammar, come up with a few grammatical terms on your own,
and try to reduce them to normal forms. Are you always successful? Does your
shortcut from earlier continue to work?

Interestingly, the rules of our game all take grammatical strings to grammatical
strings. If you start with a grammatical string, there is no way to produce an
ungrammatical term by following the rules of the game.

-->

For example, because `NAN` gets replaced with `N`, it's not too hard to show
that any tree consisting only of `N` and `A` nodes will always reduce to a
single `N`. Thus, we can reduce @fig:big-nan-gate in one step.

```{#fig:big-nan-gate design=code/Languages/And.hs}
GoesTo " " (A (A N (A N N)) N) N
```

Compound moves of this sort are called *combinators.* Combinators are not rules
per se, but they are a sequence of rules combined. You'll get the same answer
whether you replace all `NAN`s at once a la @fig:big-nan-gate, or if you do them
one at a time --- though the first approach saves some time.

<!--

A more interesting combinator is that we can eliminate a `YA` from anywhere, not
just at the beginning of the string. How? By being strategic with our pivoting.
There are two places that a `YA` might be --- at the front, at the end, or
somewhere in the middle.

If it's at the front, the `YA` is extremely easy to eliminate --- just use rule
2!  But maybe the `Y` is somewhere in the middle. But this must mean there are
two `A`s surrounding it. Thus, the situation must look like this (recall our
placeholder notation):

```text
jAYAk
```

If we pivot around the first `A`, we produce

```text
YAkAj
```

Now `YA` is at the front, so we can eliminate it, producing `kAj`. All that's
left is to pivot again on the `A`, to get back where we started (minus the
`YA`.) It might be tempting to stop at `kAj`, without swapping back, but such a
combinator wouldn't do what we claim. The claim is that we can "eliminate a `YA`
from anywhere," which implicitly also says "without changing anything else." We
will look at more complicated systems later on, where our only hope of
understanding is dependent on our combinators working *precisely how we say.*

There is another, very similar combinator, that says we can eliminate a `AY`
from anywhere in the string. Try to derive this combinator for yourself.

Between these three combinators, it's extremely easy to immediately jump to the
normal form for any grammatical string. We can eliminate all `YA` and `AY`s,
which will result in either a single remaining `Y`, or a string of `NAN`s, which
can immediately collapsed into a single `N`.

-->

We can describe the behavior of this game thusly: a term's normal form is `N`
if --- and only if --- the term contains one or more `N`s . Otherwise, the
normal form is `Y`.

Perhaps you will be surprised to learn that this little game is played by
executive boards and governmental committees all around the world. It's true!
But in those contexts, this game is called "a unanimous vote." You can interpret
a `Y` to be a "yea" vote, an `N` to be a "nay", and an `A` to mean "and". Thus,
we can decipher `(YAN)AY` as "yea and nay and yea."

Under this interpretation, the starting term corresponds to the votes of each
member, and the normal form is whether or not the motion passed!

This is our first taste of a *model* for a problem --- where "problem" is to be
understood very broadly; it might better be called as a "situation." A model of
a problem is a transformation from the real-world details to a mathematical
system like our game. In order to be a model, the system must adequately
"capture" the salient parts of the decision. For example, our game makes no
mention of the hairstyles of the voters; it is tacitly understood that, while
the voters do have hairstyles (probably,) such details are irrelevant to the
problem at hand. Of course, if we were modeling a hair salon, the hairstyles
might become extremely relevant, while the political leanings of the patrons
would diminish in importance.

Problem modeling, as a skill, is the most important skill of the
computationalist. A model corresponds to a worldview --- a way of understanding
the problem. Thus, your solution to the problem can only ever be as good as your
model. The most beautiful, elegant, economical solution in the world is of no
use if it's for a problem you don't have.


### Conceptualizing Trees

Trees are often used to show hierarchies. You've likely seen the
phylogenetic tree, used in biological circles to show how different lifeforms
are related. In linguistics, parse trees are used to show which parts of a
sentence belong to which part of the language's grammar. In mathematics,
trees can be used to show in what order to perform arithmetic. An example of
this is shown in @fig:tree-arith-ex, which corresponds to the expression
$5 \times 7 + 5 - (10 - 4 \times 2)$.

> TODO(sandy): add an image of a phylogenetic tree

```{#fig:tree-arith-ex design=code/Languages/Math.hs}
asMath $ 6 * 7 + 5 - (10 - 4 * 2)
```

When arithmetic is expressed as a tree, it is no longer necessary to remember
the "order of operations" drilled into school children. There is no ambiguity in
how to perform the arithmetic --- practitioners can simply start at the leaves
of the tree and work upwards, like in @fig:tree-arith-eval.

```{#fig:tree-arith-eval design=code/Languages/Math.hs}
Cons     " " (asMath $ 6 * 7 + 5 - (10 - 4 * 2))
  $ Cons " " (42 + 5 - (10 - 4 * 2))
  $ Cons " " (42 + 5 - (10 - 8))
  $ GoesTo "etc." (47 - (10 - 8)) 45
```

Since $5 \times 7 + 5 - (10 - 4 \times 2) = 45$, in some sense, we are justified
in saying that @fig:tree-arith-eval also is *equal to* 45. Of course, this is
fully dependent on what we mean by "equal!" Under the usual rules of arithmetic,
there is clearly an equivalence between the two, but it would be wrong to say
that @fig:tree-arith-eval is *exactly the same thing* as 45 --- after all, one
is a drawing with circles and arrows, while the other is a number. As you can
see, the idea of "equality" is actually quite tricky to pin down!

> TODO(sandy): does this stuff re: equality belong here?

Let's revisit the `YNA` game and do a little renaming. Instead of `Y` and `N`
nodes, we'll write `1` and `0`. And instead of `A`, we'll write $times;. Let's
call this variant the `10X` game. Then, the `(YAN)AY` derivation from
@fig:yanay-deriv can be rewritten in the flavor of `10X` as
@fig:yanay-math-deriv.

```{#fig:yanay-math-deriv design=code/Languages/Math.hs label="Derivation of `0`"}
Cons "Rule 2" (asMath $ 1 * (0 * 1))
  $ Cons "Rule 3" (0 * 1)
  $ GoesTo "Rule 2" (1 * 0) 0
```

What's interesting about `10X` is that all the rules correspond to simple
algebraic truths. Rule 1 says $0 \times 0 = 0$, rule 2 that $1 \times x = x$,
and rule 3 that $x \times y = y \times x$. So what does this mean? Is this just
a coincidence? Was `YNA` just ciphered arithmetic this whole time? Neither ---
it's not a coincidence, and `YNA` isn't just arithmetic. Rather, both `YNA` and
arithmetic are just two different shadows on the cave wall. We will return to
this discussion, to see just what is producing those shadows, in @sec:lattices.

The important takeaway here is that different-looking systems are capable of
solving similar problems. While `YNA` trees describe real-world unanimous votes,
we can *model* that same process as statements of mathematics in `10X` trees.
This process of modeling is of the utmost importance --- by being careful, we
can design systems which describe real-world situations, but are manipulated
symbolically. That's the key; it's easy to manipulate symbols, while it's much
harder to keep lots of real-world details in mind simultaneously. Instead, the
solution is to transform the real-world problem into a different language where
we can reason about it formally, solve the problem in the model, and then
transform the solution back into the real world.

In any case, we can think about this operation *abstractly.* The exact
implementation details here are unimportant. Whether we are modeling
multiplication, voting, or circuitry, what matters is that the system
consistently behaves in the way we expect it to. "The purpose of abstraction, is
not to be vague, but to create a new semantic level in which one can be
absolutely precise," says Dijkstra. We don't need to care about exactly how the
`YNA` game works, but merely that it does.

To further drive this home, consider @gunji_robust_2012, in which researchers
implemented the `YNA` game in the behavior of soldier crabs. Crabs walk
sideways, rather famously. But when two of these crabs meet at an intersection,
they will "swarm" together, and move in a predictable direction --- a direction
different than either of the crabs' previous target. This behavior is shown in
@fig:crabs-and.

![Crabs meeting at an intersection will travel together](images/and-3.png){#fig:crabs-and}

However, if a crab doesn't intersect with another, it will continue in it's
original direction, like in @fig:crabs-id.

![A crab will continue along its path if it doesn't encounter another.](images/and-4.png){#fig:crabs-id}


In this sense, we can model an `N` as the absence of crabs, a `Y` as the
presence of crabs, and an `A` node as one of these intersections. For example,
in @fig:crabs-ex some crabs will be at &clubs; if and only if some started at
&hearts;, moving in the direction of the arrow.

![A crab will continue along its path if it doesn't encounter another.](images/and-5.png){#fig:crabs-ex}

Why do we know this to be true? Because if there are not crabs starting at
&hearts;, then the crabs on the left side of @fig:crabs-ex will have no crabs to
swarm with, and will instead continue in their current direction, like in
@fig:crabs-ex2.

![A crab will continue along its path if it doesn't encounter another.](images/and-6.png){#fig:crabs-ex2}

But it's rather messy to draw these diagrams of crab movement. So, perhaps you
will agree that @fig:crabs-ex is equivalent to the tree in @fig:crab-tree-ex,
where &clubs; corresponds to its normal form.

```{#fig:crab-tree-ex design=code/Languages/And.hs}
A (A Y Y) (MV Heart)
```









### Revisiting YNA

> TODO(sandy): there is great opportunity for a pun here, if we replace yea with
> "da"
>
> but then it's hard to make ANY work for the dual?

An interesting fact about systems is that we can get the same results from
completely different rules. For example, the following two rules "give rise" to
the same YNA game:


RULE 1.

:  If your string is of the form `YAj`, it will reduce to `j`.


RULE 2.

:  If your string is of the form `NAj`, it reduces to `N`.


There is no rule 3 (but we keep rules 4-5 around implicitly, since they are more
"rules about how to use the rules" than *actual* rules.)

Let's call this game YNA' (read: "YNA-prime".) Given the same starting
string, YNA and YNA' will reduce to the same normal form. It's not too hard to
show why[^how-to-derive]. That we can formulate the same system in two different
ways is particularly thought provoking, as it suggests there is something "real"
about the problem that we are capturing, albeit indirectly.

[^how-to-derive]: If you're inclined, try to derive these rules as combinators
  of YNA, and then try to derive the rules of YNA as combinators of YNA'.

To illustrate this idea more poetically, consider @fig:carving-joints, which is
a sketch of the sun and the moon. If we'd like to partition this image,
separating the sun from the moon, we will need to draw a boundary between the
two. But there are infinitely many different boundaries we can draw --- as shown
in @fig:carving-joints --- and each is as good as the next for separating the
celestial bodies. Of course, we might have other desiderata to consider: such as
simplicity, or level, or elegance, or ease of drawing.

![Different ways of carving](images/sketches-0.png){#fig:carving-joints}

```{design=code/Languages/And.hs}
A (A N (A (MV Spade) (MV Diamond))) (A Y (A (MV Club) (MV Heart)))
```

