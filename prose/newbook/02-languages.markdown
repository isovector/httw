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


For example, you can transform `YYNANN` into `YYN` via rule 1. If you're given
`YNANAN`, you can produce `YNNAN`, or `YNANN`.


RULE 2.

:  If your string begins with `YA`, you can remove the prefix.


For example, rule 2 lets us change `YAY` into `Y`. But it **doesn't** allow us
to change `NYA` into `N`, because `YA` *must come at the beginning.*


RULE 3.

:  If your string contains an `A` in the middle, you can swap what's on the
   left side of the `A` with what's on the right side.


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
starting strings `YAYAY`, `NANAYAN` and `YANAYAYAYAYAYAYAN`.

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

It's time to reveal the shortcut for "jumping ahead" to the normal form of the
starting string. The rules of our game were carefully constructed so that the
normal form is `N` *if, and only if,* there was at least one `N` in the starting
term. If there wasn't, the normal form is `Y`.



