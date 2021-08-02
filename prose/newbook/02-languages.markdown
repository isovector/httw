## Languages

This is a book primarily about designing big systems --- ones which, in some
sense, are in cahoots with reality. Eventually we will build our way up to a
computer, which, in an odd turn of fate, we will see is nothing more than a
machine for evaluating the systems we've been building all along.

<!-- TODO(sandy): terrible sentence, but hey, gotta start somewhere -->

Let's play a little game. I will give you a string of letters, like `YAYAN`.
These strings are also called *terms,* and I will use both words. The goal of
the game is to "reduce" the term into the shortest possible string that you can.
But there's a trick --- you're only allowed to reduce a string via a few, very
specific rules. The string I give you will consist only of the letters `Y`, `N`
and `A`, and have a very particular form, which I will reveal to you later.

Ready for the rules?


RULE 1.

:  If your string contains a `NAN`, you can replace it with `N`.


![Rule 1](images/and-1.png)


For example, you can transform `YYNANN` into `YYN` via rule 1. If you're given
`YNANAN`, you can produce `YNNAN`, or `YNANN`.


RULE 2.

:  If your string begins with `YA`, you can remove the prefix.


![Rule 2](images/and-0.png)


For example, rule 2 lets us change `YAY` into `Y`. But it **doesn't** allow us
to change `NYA` into `N`, because `YA` *must come at the beginning.*


RULE 3.

:  If your string contains an `A` in the middle, you can swap what's on the
   left side of the `A` with what's on the right side.


![Rule 3](images/and-2.png)


To illustrate rule 3, consider the term `YYANAN`. By rule 3, we can replace it
with either `NANAYY`, or with `NAYYAN`, depending on which `A` we "pivot"
around.


RULE 4.

:  Rules 1-3 cannot be run backwards. That is to say, you can't replace an
   `N` with `NAN` by running rule 1 in the opposite direction.


RULE 5.

:  You are not allowed to change the term in any way other than via rules 1-3.


Let's take our example string, `YAYAN`, and play with these rules. Remember, the
goal is to produce as short a term as possible. At any point, multiple rules
might apply, and we can decide to follow any one of them. Because our string
does not contain `NAN`, rule 1 does not apply. But both rule 2 and 3 are
available to us.

By choosing to follow different rules, we will get to different strings. Each of
the three following strings can be produced:

1. `YAN`, by following rule 2, and chopping off the initial `YA`.
2. `YANAY`, by following rule 3 and pivoting around the *first* `A`.
2. `NAYAY`, by following rule 3 and pivoting around the second `A` instead.

Of these three, #1 is the shortest, so we might choose to explore rules
accessible to us from that string. Picking the direction that moves us closest
to the goal is called a *greedy* strategy, and it works better in same games
than it does in others.

From `YAN`, we only have two options:

1. `N` (rule 2)
2. `NAY` (rule 3)

Because there is only one `A` in the string, we only have one possibility for
rule 3.

Looking at `N`, no more rules apply, so we say that this string is in *normal
form.*
It can't go anywhere else, so if we ever get to `N`, we're stuck. There are some
interesting questions here to ponder. Would all terminating sequences of rules
have gotten us to this same place? Are there other normal forms? If so, how
many are there? And perhaps most interestingly, *is there a faster way to
get to a normal form?*

See if you can come up with some answers to these questions. You will probably
develop strong convictions after playing with a few more examples. However, not
every combination of `A`, `N` and `Y` is allowed as a starting term; as I said
earlier, these terms have a very particular form they must follow. We will
discuss the form in a moment, but before then, try playing around with the valid
starting strings `YAYAY`, `NANANAYAN` and `YANAYAYAYAYAYAYAN`.

If you've diligently worked through the these three terms, you probably came
upon some answers to our earlier questions. It seems that every sequence of
rules results in the same normal form, and that, at least in these examples,
that normal form is always either `Y` or `N`. Perhaps you have even come up with
a shortcut for getting to the normal form directly!

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

In playing with the examples, you might have noticed how some rules can be
combined in order to save some work. For example, because `NAN` gets replaced
with `N`, by taking a birds-eye view of the system, we can notice that *any*
length of alternating `N`s and `A`s will reduce to a single `N`. Moves of this
kind are called *combinators.* Combinators are not rules per se, but they are
a sequence of rules combined. You'll get the same answer whether you replace all
`NAN`s at once, or if you do them one at a time, but the first approach saves
some time.

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

We can describe the behavior of this game thusly: a term's normal form is `N`
if --- and only if --- the term contains one or more `N`s . Otherwise, the
normal form is `Y`.

Perhaps you will be surprised to learn that this little game is played by
executive boards and governmental committees all around the world. It's true!
But in those contexts, this game is called "a unanimous vote." You can interpret
a `Y` to be a "yea" vote, an `N` to be a "nay", and an `A` to mean "and". Thus,
we can decipher `YANAY` as "yea and nay and yea."

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

> TODO(sandy): next steps:
> tie this to boolean algebra
> show OR via duality
> what's a game we can play with heyting algebra? 3sat?


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
A (A Y (A N N)) (A Y (A Y Y))
```

