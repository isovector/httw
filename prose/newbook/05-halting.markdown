## Decidability and Constructivism

Is every postulate about the world necessarily either *true* or *false?* It
seems like it must be so; after all, true and false form a natural,
mutually-exclusive dichotomy. Of course, it's possible that there are answers
which we don't yet know whether they are true or false, but it seems like *in
principle* we should be able to do so.

However, while intuitive, this argument doesn't actually hold water. Consider
the following claim, which we will call `P`:

> This sentence is false.

Is `P` true or false? It seems as though we've run into a problem. If `P` is
true, then it must be false. But if is indeed false, then it was telling the
truth the whole time, and is actually true! What a conundrum. Thus we are left
with two bad alternatives; we can either claim that `P` is somehow
*logically ungrammatical* (and thus meaningless,) or we can bite the bullet and
accept that `P` is neither true nor false. Because there is no good argument to
be made for how `P` is ungrammatical, we are left biting the bullet.

In classical logic, the claim that every postulate is either true or false is
known as the **law of the excluded middle.** It is alternatively stated as "if
we know it is *not the case* that `Q` is *not true*, then `Q` must be true."
More formally, we can write

$$
\neg \neg Q \iff Q
$$

But, as we have seen in the case of `P`, this isn't so! `P` isn't false, but
that doesn't mean it's true. Thus, whenever you see double-negation like this
being canceled out in an argument, you should immediately be on guard; probably
someone is trying to fool you, or themselves.

Perhaps you have heard of Hempel's raven paradox. It states that by observing a
non-black non-raven, we can deduce that all ravens must be black. This is
clearly bogus, but the logic seems (classically) impeccable. The claim goes like
this:

1. All ravens are black, therefore
2. If something is not black, it is not a raven

If we accept the premise (1) that all ravens are black, then (2) clearly
follows, in a mathematical move called the **contrapositive.** More formally, we
can say

$$
(P \to Q) \to (\neg Q \to \neg P)
$$

That is, if `P` implies `Q`, then not `Q` surely implies not `P`. The
trouble comes in when we do this move again:

$$
(\neg Q \to \neg P) \to (\neg\neg P \to \neg\neg Q)
$$

Classical logic now stealthily applies the law of the excluded middle, replacing
$\neg\neg P$ with $P$, and likewise for $\neg\neg Q$. But this brings us back to
the problem of ravens, namely that it leads to the paradoxical result that
observing a non-black non-raven lets us deduce that all ravens are black.
Nonsense.

For a long time paradoxes like Hempel's, and others, have cast a dark shadow
over logic. If it can't even get the right answer for stupid toy problems like
ravens, how much can we actually trust it for "real world" problems? And people
were right to have doubted, because classical logic *was getting the wrong
answers.* Logic is useful inasmuch as it leads to truth; it's not a game that we
play for its own sake.

Of course, none of this is to say that *some* particular claims can't be said to
be true or false. Merely, that there exists claims which are neither true nor
false. Furthermore, it's not saying "we don't know," or even "we can't know,"
but "no answer exists."

Perhaps this feels like a mathematical digression, but there is an important
philosophical takeaway: some questions just can't be answered.


### The Entscheidungsproblem

While computers have been around only since the 1940s, computer programming is,
amazingly, an older discipline. In 1928, David Hilbert posed the
**Entscheidungsproblem,** asking for an automatic means of checking whether a
particular mathematical proposition is true or false. That is, it should say
"yes" when given the propositions

$$
\forall a b.\; a + b = b + a
$$

and

$$
\exists a.\; a + 2 \ge 7
$$

but should say "no" to

$$
\exists a.\; a = a + 1
$$

and

$$
5 = 10
$$

While these examples seem easy enough, the Entscheidungsproblem asks for an
automatic means of checking *any* mathematical proposition. That it, it should
be able to handle trickier problems too, like the following:

$$
\exists a b c d e x y z.\\
(x \times a \times b) + (y \times b \times d) + (a \times b \times e)
=
((1 - x) \times a \times b) + (b \times y \times c) + (c \times d \times (1 - z))
$$

or even propositions that are too long to fit in this book. For problems of this
size, it's clearly impossible to just "eyeball" it.

Consider the importance of a solution to the Entscheidungsproblem.




### The Halting Problem

Let's consider an analogous problem in computing. Is it necessarily so that
every computer program must either stop or run forever? A different way of
phrasing this is:

> Is there a computer program which can definitively say whether some other
> program will stop or run forever?

```python
def P(input):
  if halts(P, input):
    loop forever
  else:
    stop
```

This construction is analogous to the liar's paradox in the earlier section; `P`
stops if and only if it doesn't stop. Thus, we are unable to give a definitive
yes or no to whether or not an arbitrary program will halt. This result is known
affectionately as **the halting problem** and is the most fundamental result in
all of computing.

Why does it matter? As it happens, we can shortcut a lot of work in showing
other problems are impossible simply by proving a solution to them would be a
solution to the halting problem. But of course, the halting problem is
impossible, so any solution which would let us tackle the halting problem must
itself be impossible.

