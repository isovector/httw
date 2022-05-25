## Equality

In this chapter we will investigate how challenging exact thought really can be,
see what happens when it goes wrong, and become acquainted with some fundamental
building blocks of thought.

Consider the simple problem of when are two things "the same?" In the abstract,
this question feels easy to answer, but things get hairier when we look at
particular examples: is a chair the same as a stool? is Simon Idlewild the same
as Simon Beets? is the largest continent the same as Asia? is the cat the same
as a dog? is two the same as 2? is a car the same as a Volkswagen? is the author
of this book the same as Sandy Maguire? is 10 + 4 the same as 2? if two people
have the same DNA, are they the same person? if knows a secret that only you
know, is that person the same as you? is the best strategy in investing the same
as buying low and selling high? is pi the same as 3.14? are two voters in a
democracy the same?

Some of these things clearly feel like they are indeed the same, and others do
not. Some even, might be "it depends!" While we can attempt to answer each of
these questions individually, a truly pragmatic answer is "why do you want to
know?" Cats are not the same as dogs to a veterinarian, but probably are under
an apartment rental agreement! I, the author of this book, am indeed Sandy
Maguire, but I am also *more* than just the author of this book! The answer you
get about sameness really seems to depend on the context in which you're asking!

Indeed, not even mathematics are safe from this context-dependency! It seems
like there is no world in which $10 + 4$ is the same as 2, except when you think
about a clock, in which 4 hours past 10 certainly is 2! What's going on here?
Have we somehow cheated? Maybe that operation shouldn't be allowed to be called
$+$?

What a mess this all is! And unfortunately, it is not relegated merely to
pedantic quibbling --- problems of sameness occur every day. If the previous
owner of my phone number opted into receiving political fund-raising calls, does
that mean I want to give money? The staffers certainly seem to think so, and
no effort on my part seems able to convince them otherwise! To the managers of
the donation-seekers, two people are the same if they have the same phone
number!

Most systems consider two people to be the same if they both possess something
only the owner is expected to have. This might be a physical asset like a key at
the mailbox, or a secret piece of information like a password. The assumption is
that only the intended person has the possession, and thus, if someone has the
possession, they must be the intended! The possession becomes a *proxy* for the
authorized person. Such an access-control system works until the possession is
stolen or copied, at which point, all bets are off.

Due to the importance of this question, we'd like to study what it means for two
things to be the same, at least in some context. That is to say, what is the
meaning of "sameness?" This is a challenge, since the contexts can vary so
widely, but we can make at least a few claims about *all* natures of sameness.
You will not find the following three properties hard to believe:

* Everything is the same as itself everything `(R)`
* If X is the same as Y, then Y is the same as X `(C)`
* If X is the same as Y, and Y is the same as Z, then X is the same as Z `(T)`

It's hard to think of any measure of "sameness" that wouldn't satisfy all of
`(R)`, `(C)` and `(T)`! We will call any relationship that respects these three
rules an *equivalence relation.* Of course, this is still in a context, of what
sorts of things X, Y and Z are allowed to be, but we will explore this idea
later.

The simplest equivalence relation we can come up with is unsatisfyingly trivial.
If our context allows only one sort of thing, call it &bullet;, does such a
thing "give rise" to an equivalence? Let's see!

* Is &bullet; the same as &bullet;? We'd better hope so! Thus, `(R)` is
  satisfied.
* If X is the same as Y, is Y the same as X? Well it must be, since X and Y can
  must both be &bullet;. Therefore, `(C)` holds.
* If X is the same as Y, and Y is the same as Z, is X the same as Z? Yes, and so
  `(T)` must hold, for the same reason.

I told you it was unsatisfying!

So let's instead look at a slightly more interesting example; a world in which
X, Y, and Z might be either **YES** or **NO**. We can run the analysis again,
but we will still be disappointed. In some sense, this disappointment is
necessary of all equivalence relations, because the idea of equivalence is so
deeply familiar to the human experience.

Equivalence relationships of this sort --- that is, pick some number of distinct
symbols and say that each is the same only as itself --- are known as
*propositional equalities.* Under propositional equality, two things are the
same if and only if they are *completely indistinguishable.* Under propositional
equality, 2 and two are not the same, but 2 is the same as 2 and two is the same
as two. Propositional equality is fundamental building block of all logic,
mathematics, and as far as we can tell, physics. Propositional equality would be
a marvelous thing to use to restrict access to our bank accounts in that it
would be perfectly secure; except that it would require smashing us into our
constituent atoms in order to be certain we are truly indistinguishable at every
level. Which might make it significantly more challenging to enjoy the money
we've just withdrawn!

We can construct new relations by choosing specific properties of an object, and
comparing those with an existing equivalence relationship. Perhaps we'd like to
determine whether a group of people is in danger for sun burns. A person is made
up of lots of interesting properties that make them who they are. They have a
name, an age, a country of origin, a life story, childhood traumas, moments of
triumph, and a rich and complicated social network. But rather than looking at
any of these things, we can instead ask the question "is this person wearing
sunscreen?" and project a complicated question of equality down to a simple
**YES**/**NO** binary. Everyone wearing sunscreen (that is, everyone in the
**YES** category) belongs in one group, and everyone who isn't belongs in the
other.

Of course, discriminating people based on whether or not they are wearing
sunscreen is a gross simplification of what it means to be human, but this sort
of thing happens all the time. Folk psychology constantly projects complicated
people down onto smaller equivalence classes --- for example, we sort people
into whether they are **CAT** or **DOG** people, which political party they vote
for, which of twelve astrological signs they have, or their Myers-Briggs types.

It's easy to come up with some mutually exclusive set of labels (propositional
equality), and project a complicated thing down into one of them. And this is
often necessary --- but it's important to never confuse equivalence with
equality! A particular equivalence class might be adequate for whatever use-case
you have in mind, but you mustn't conflate equivalence in one domain with the
same equivalence in another.


### Other Equivalences

Equality-like equivalences are not the only equivalences! One of the joys of
mathematics is that once we have a definition in place, we are often pleasantly
surprised to find other things that fit the definition.


### Definitional vs Propositional Equality

> TODO(sandy): get a real life example here

As you can see, X isn't literally identical to Y. But every prediction we can
make about X holds true in Y as well.


### Properties of



