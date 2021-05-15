## Case Study: Games

```{.haskell law="choose gate"}
∀ (ef :: EventFilter) (g :: Game).
  choose [(ef, g)] = gate ef g
```


Exercise

: Design a game which allows players to choose exactly three rewards from a
  choice of five (duplicate selections are allowed.)


### The First Model

> The first principle is that you must not fool yourself, and you are the
> easiest person to fool.
>
> --Richard P. Feynman

Now that we've done the analysis of *what to build,* it's time to get started
actually building. But we are not going to dive into the final implementation,
complete with clever performance optimizations. This is the path towards madness
--- that is to say, ad-hoc changes that are likely unmaintainable and will
almost certainly not follow the semantics we reasoned through so carefully.
Writing fast code is hard enough, without the complexity of simultaneously
juggling proofs that our optimizations work in all cases.

Instead, we are going to build our system twice. The first time will be a model
implementation; one that prioritizes being *obviously correct* over all other
concerns. The clarity afforded to us by this model will be used to test the
composition of our semantics in anger, and to automatically generate a suite of
regression tests that we can use to ensure we don't break anything when we move
to our fast implementation. In addition, the additional things we learn about
our algebra while working through the model will prove invaluable towards
finding performance optimizations, but let's not get ahead of ourselves just
yet.

Our first implementation will be an *initial algebra* of the design; that is, we
will take the operations we've discovered, and turn them immediately into data
constructors. In this form, it's extremely easy to pattern match on operations,
and the smart constructors we laid out can use pattern matching to ensure that
their laws hold. Let's start by lifting our operations to data constructors for
`Game`.

[code/workflow/initial_algebra.hs:Game](Snip)

You'll notice there are not corresponding data constructors present for
`bottom`, `gate` or `comeback`. We'll address why shortly. But with the
definition of `Game` above, we can give implementations for our smart
constructors:

[code/workflow/initial_algebra.hs:win](Snip)

[code/workflow/initial_algebra.hs:lose](Snip)

[code/workflow/initial_algebra.hs:choose](Snip)

et cetera.

For constructors that have associated laws, such as the semantics for `subgame`:

```{.haskell law="subgame win"}
∀ (g1 :: Game) (g2 :: Game).
  subgame win g1 g2 = g1
```

```{.haskell law="subgame lose"}
∀ (g1 :: Game) (g2 :: Game).
  subgame lose g1 g2 = g2
```

we can implement these directly as pattern matches when giving the
implementation for `subgame`:

[code/workflow/initial_algebra.hs:subgame](Snip)

Similarly, for constructors that have laws stating they are completely
equivalent to other syntactic forms --- for example, `bottom` --- we don't
introduce a data constructor in the first place, and simply define one smart
constructor as another.

[code/workflow/initial_algebra.hs:gate](Snip)

[code/workflow/initial_algebra.hs:bottom](Snip)

[code/workflow/initial_algebra.hs:comeback](Snip)

**It's very important that we exclusively use the smart constructors in these
definitions.** The data constructors should be considered sacred, and *only ever
constructed* in one single place. Doing so ensures that our semantics (and thus
our invariants) hold, and that there is only ever a single place to update if we
want to make changes. Once the smart constructors are in place, they are the
contract. You're still allowed to pattern match on the data constructors, but
*never ever* use them to construct a value. This unfortunately requires a great
deal of self-discipline, and at time of writing, there are no tools to help on
this front. Constant vigilance is essential.

For completeness, the rest of the algebra looks like this:

[code/workflow/initial_algebra.hs:both](Snip)

[code/workflow/initial_algebra.hs:eitherG](Snip)

[code/workflow/initial_algebra.hs:race](Snip)

[code/workflow/initial_algebra.hs:andThen](Snip)

We also need to give definitions for `Result`, `Event`, `EventFilter` and
`Reward`. We will again use an initial algebra, supplying data constructors
where necessary. In order to simulate possible *inequalities*, we make the
arbitrary decision that `Event` and `Reward` are implemented by 8-bit integers.
This isn't observable by our public interface, and thus doesn't change any
semantics, but serves merely as an artificial source of difference. We choose
8-bit integers not for any deep reason, but because they work well in practice.
They have enough values that we aren't guaranteed a collision when generating
two randomly, but do allow for collisions sufficiently often. We will explore
why this is a desirable property in a moment. But first, the model
implementations:

[code/workflow/initial_algebra.hs:Result](Snip)

[code/workflow/initial_algebra.hs:victory](Snip)

[code/workflow/initial_algebra.hs:defeat](Snip)

[code/workflow/initial_algebra.hs:Event](Snip)

[code/workflow/initial_algebra.hs:EventFilter](Snip)

The `Exactly` constructor at [1](Ann) exists so that we can filter events,
looking for one whose internal `Word8` is exactly equal to this constructor's
parameter. Again, this is a tool only for playing around with our algebra;
because it's not part of the public signature, it remains a temporary,
invisible, implementation detail.

[code/workflow/initial_algebra.hs:always](Snip)

[code/workflow/initial_algebra.hs:never](Snip)

[code/workflow/initial_algebra.hs:Reward](Snip)

Lastly, we need to implement our two observations, `matches` and `runGame`. The
first is the easier of the two:

[code/workflow/initial_algebra.hs:matches](Snip)

Implementing `runGame` is less straightforward. First we will introduce a helper
function, `_stepGame`[^underscore-impl], whose purpose is to implement our
single-step semantics for games. Somewhat curiously, `_stepGame` takes a `Maybe
Event` as an argument, because many terms in our algebra aren't gated, and thus
can be stepped without an explicit event to drive them. We use the functionality
of the `Writer` monad to accumulate the discovered calls to `reward`, which
alleviates the tedium of needing to explicitly thread those values ourselves.

[^underscore-impl]: This book uses the convention that function names which
  begin with underscores, like `_stepGame`, are *implementation details,* and
  not to be confused as a part of the public API.

[code/workflow/initial_algebra.hs:_stepGame](Snip)

Notice the pattern evident in this code; it commonly looks at a data
constructor, and then calls `_stepGame` over its arguments, reconstructing the
resulting game via the corresponding smart constructor. For example, at
[1](Ann), we unpack a `Subgame`, and then at [2](Ann), we pack it right back up
again with the result of stepping each argument. The simplification laws that we
wrote into the smart constructors are responsible for turning, eg., `subgame win
g1 g2` into `g1`.

Now that `_stepGame` is complete, we can provide another helper function, which
feeds a list of `Event`s into `_runGame`. Special care needs to be taken to
completely reduce the game after all events have been run through it, as
indicated by [1](Ann) below. It is this use case that motivates the use of our
`Maybe Event` parameter in `_stepGame`.

[code/workflow/initial_algebra.hs:_runGame](Snip)

One final helper function is necessary before we can implement `runGame`. We
need to look at the final `Game` after it's been stepped on every `Event`, and
then determine if we are in a definite win or loss state.

[code/workflow/initial_algebra.hs:_toResult](Snip)

With all of our helper functions out of the way, we're now ready to define
`runGame`. Unsurprisingly, it's not very complicated, since we've offloaded all
of the actual work elsewhere:

[code/workflow/initial_algebra.hs:runGame](Snip)

Let's do a quick sanity check to ensure all of the above works. Our `bingo` game
from earlier is probably sufficiently complicated to hit most of the cases in
`_stepGame`. We can define a quick 3x3 game of `bingo`:

[code/workflow/initial_algebra.hs:bingo_game](Snip)

Here the first number of an `Event` is the $y$ coordinate of the square, and the
second is the $x$. For example, `Event 12` is the rightmost square in the middle
row. Let's run a few events through the system to see if it works:

```{ghci=code/workflow/initial_algebra.hs}
runGame [] bingo_game
runGame [Event 0, Event 1] bingo_game
runGame [Event 0, Event 1, Event 2] bingo_game
runGame [Event 2, Event 0, Event 1] bingo_game
runGame [Event 1, Event 11, Event 21] bingo_game
```

The first two examples ensure nothing goes haywire if we don't complete any rows
or columns in the game. The third example is that we can complete a row (in this
case, the top one), and the fourth example shows that the ordering doesn't
matter. Our fifth example here shows that we can complete the middle column as
well. Everything appears to be in order.


### Exploring the Model

Now that we have a working (if naive) implementation of our library, we finally
have something we can play with. Here is where the true value of the
Algebra-Driven Design approach comes into action.

Our goals now are twofold; first we will build *generators* --- pieces of code
capable of constructing random, arbitrarily complicated games. Generators are
useful for covering code-paths, and for programmatically constructing unit
tests. Our other goal is to use our generators to help discover more laws about
our algebra, properties that hold true of the implementation but which might
have escaped our attention for one reason or another. We will eventually turn
these discovered properties into *semantic regression tests* --- ensuring any
future implementation we come up with is semantically equivalent to our current
implementation. The discovered properties are also interesting in their own
right, as each simultaneously provides one fewer concern for our users, and one
more possible optimization route for our implementation.

Let's first tackle our generator for `Game`s. We will use the generator to create
thousands of unit tests, by filling in the universally quantified variables in
our laws, that is to say the `g` in

```{.haskell law="bottom"}
∀ (g :: Game). bottom = gate never g
```

If we generate thousands of random `Game`s, and see that the above law is true
for all of them, we can be quite confident that this law holds. Of course,
function equality is undecidable in general, but after ten thousand tests, if
the two haven't been shown yet to be unequal, they probably never will.

Generators are implemented by giving an instance of the
`Test.QuickCheck.Arbitrary` class for the relevant types involved. In this
section we will present only the bare minimum in order to get our generators up
and running, but @sec:quickcheck in the reference section of this book contains
everything you could possibly want to know about writing generators --- and why
the ones presented in this section are what they are.  Although it is likely to
break flow, the motivated reader is encouraged to consult the reference for any
questions that arise when going through the generators present here.

The simplest generator is that for `Result`, which simply chooses one of two
values:

[code/workflow/initial_algebra.hs:ArbitraryResult](Snip)

The generators for `Reward` and `Event` are also very simple; they simply use
the underlying `Word8` generator, and wrap its result in the respective data
constructor:

[code/workflow/initial_algebra.hs:ArbitraryReward](Snip)

[code/workflow/initial_algebra.hs:ArbitraryEvent](Snip)

When we get to `EventFilter` however, we would like to weight the output of our
generator; it's not very interesting to usually generate the `never` filter. By
using the `frequency` combinator, we can assign weights to how often a
sub-generator is used. In the `Arbitrary` instance below, we will produce a
`never` term at odds $1/(3+1+5) = 1/9$, while we will produce `Exactly` five
times more often.

[code/workflow/initial_algebra.hs:ArbitraryEventFilter](Snip)

Finally, the generator for `Game` steps up *its* game. Because most of the
`Game` constructors are themselves made up of `Game`s, we must be careful to
ensure each sub-game is smaller than its parent. We use the `decayArbitrary n`
generator to make a term that is $1/n$ the size of its parent; therefore,
because the `subgame` term has three smaller games, each is generated via
`decayArbitrary 3`. When the size gets to 1, we force the generator to produce
either `win` or `lose`, ensuring the generator terminates.

[code/workflow/initial_algebra.hs:ArbitraryGame](Snip)

It's important that our generator can produce every possible constructor in our
API --- not just the *data constructors* of our type. We'd like to hit every
publicly-facing method, regardless of its actual implementation; this ensures
that our randomly generated data is realistic in testing the public API.

With our generator implemented, we can test its output:

```{ghci=code/workflow/initial_algebra.hs}
generate $ arbitrary @Game
generate $ arbitrary @Game
generate $ arbitrary @Game
generate $ arbitrary @Game
generate $ arbitrary @Game
```

As you can see, our new `arbitrary` generator creates a different, but valid,
`Game` every time it runs. Unfortunately, Haskell's default `Show` function
shows us the data constructors that make up our data, rather than the
publicly-facing API we'd prefer, but doing better would require significantly
more work than we'd like to invest right now.

The last step we need to perform is to give an instance of `QuickSpec.Observe`
for `Game`. The `Observe` type class is parameterized by three types, and
instances are of the form `Observe input result type`. In our case, the `type`
is `Game`, and we will show two `Game`s are observationally equal by producing
two equal `result`s, given a random `input`. The observation we'd like to
consider is `runGame`, which means our `input` is a list of `Event`s, and our
`output` is `([Reward], Maybe Result)`.

[code/workflow/initial_algebra.hs:ObserveGame](Snip)

QuickSpec's workflow is based around *signatures* --- consisting of little
descriptors of the surface of our API, and configuration for the tool's
behavior. In this section we will present just enough to get by, but the
interested reader is encouraged to read @sec:quickspec for better understanding,
and for using QuickSpec in his own projects.

We begin by listing which types exist in our algebra (the `monoType` and
`monoTypeObserve` functions,) and optionally, how to name variables of these
types (given by `vars`):

[code/workflow/initial_algebra.hs:sig_types](Snip)

Signatures form a monoid, so by grouping them into conceptual chunks, we give
ourselves ourselves flexibility in the future for exploring different parts of
the algebra. The constructors of the `Result` type form such a conceptual chunk,
and so we can give a signature for them:

[code/workflow/initial_algebra.hs:sig_results](Snip)

Here, the `con` function describes a constructor in our algebra. Although it's a
bit tedious to describe each and every relevant piece of functionality to
QuickSpec, the benefits quickly outweigh the pain. Similarly, we will give
signatures for the distinguished `EventFilter`s, as well as for the "core"
`Game` constructors:

[code/workflow/initial_algebra.hs:sig_filters](Snip)

[code/workflow/initial_algebra.hs:sig_games_core](Snip)

The `sig_games_core` signature corresponds to the constructors in our algebra
which are fundamental --- those which, as far as we
know[^maybe-quickspec-knows], can't be reduced to other constructors. Contrast
this against the extensions of the algebra, which by definition are the
constructors which can be expressed in terms of other combinators:

[^maybe-quickspec-knows]: But perhaps QuickSpec will notice something we didn't.

[code/workflow/initial_algebra.hs:sig_games_ext](Snip)

By running `quickSpec` with our signatures, our hard work is rewarded by a big
list of equations that are *true* of our implementation:

```{#initial_laws quickspec=code/workflow/initial_algebra.hs}
quickSpec $ withMaxTermSize 5 <> sig_types <> sig_games_core <> sig_filters
```

QuickSpec begins by printing out which constructors it knows about, under the
`Functions` banner, and then gets down to business. Under the hood, QuickSpec is
generating every possible syntactically-valid `Game`, and then using our
`Observe` instance and the `Game` generator to compare every pair for
observational equality. If equality of the two expressions can't be falsified
after a large number of tests, QuickSpec assumes the equation is true and prints
it out.

The list of equations above is rather spectacular; it continues to amaze me that
a computer can tell me so much about my implementation. Now that we have this
enumerated set of laws, we can roll up our sleeves and get to work. The strategy
here is to go through these laws and think deeply about each. Do they make
sense? The computer can only tell us what's true, not *what should be true.*
Incomprehensible laws in this list correspond to bugs in our semantics, or in
our implementation, and in either case are worth knowing about.

Remember that bug in `andThen` that we intentionally left in, that `andThen` can
never evaluate its second argument? This is exactly what law
[CiteLaw:initial_laws](andThen g g2 = g) states --- that `andThen g g2` is
equivalent to `g`. QuickSpec immediately the bug in our semantics! Let's now
implement the fix:

[code/workflow/bugfree_algebra.hs:andThen](Snip)

Don't forget to remove the `AndThen` data constructor from the `Game` type. But
be careful not to remove `andThen` from our generator, however! It's still
important to generate these terms.

Skimming through the remainder of the laws, most of them seem good. Though it's
curious we don't see commutativity laws, e.g., there is no law stating `both g
g2 = both g2 g`. There is a good reason for this, and QuickSpec is trying to
tell us something through its absence, but we will not worry about this for now.

When we come across law [CiteLaw:initial_laws](race (reward r) lose = lose),
it's clear there's another problem. This law shows that a race in which both
sides finish simultaneously will steal our just rewards. This is a *genuine bug*
in our implementation, and in fact, one that I didn't notice until writing this
paragraph. One possible fix is to explicitly check for the `GiveReward`
constructor, and then return that if it happens.

Getting these semantics right is trickier than it appears. The principle of
least surprise says that a player should receive both rewards if the two
sub-games simultaneously terminate in rewards. Seems easy, but unfortunately our
only way to give two rewards is by the syntactic form `andThen (reward r1)
(reward r2)`, *which is guaranteed to lose a race against `win` or `lose`!*

Stepping away from this particular problem, we notice that it is a symptom
rather than the cause. The underlying issue is that semantically we have two
terminal cases --- `win` and `lose`, but syntactically `reward` also acts as a
terminal case, one whose semantics result in a win. The solution is to instead
give a new syntactic form:

```haskell
rewardThen :: Reward -> Game -> Game
rewardThen = RewardThen
```

with the semantics

```{.haskell law="rewardThen"}
∀ (r :: Reward) (g :: Game).
  rewardThen r g = andThen (reward r) g
```

```{.haskell law="reward"}
∀ (r :: Reward). reward r = rewardThen r win
```

This change allows us to remove the `GiveReward` constructor entirely, solving
the problem of having three terminal forms. Furthermore, and most importantly,
we can now express games in a canonical form, where rewards are all given before
any other work is done. This is accomplished by a host of distribution laws for
`rewardThen`:

```{.haskell law="rewardThen distrib both L"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  both (rewardThen r g1) g2 = rewardThen r (both g1 g2)
```

```{.haskell law="rewardThen distrib both R"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  both g1 (rewardThen r g2) = rewardThen r (both g1 g2)
```

```{.haskell law="rewardThen distrib either L"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  eitherG (rewardThen r g1) g2 =
    rewardThen (r :: Reward) (eitherG g1 g2)
```

```{.haskell law="rewardThen distrib either R"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  eitherG g1 (rewardThen r g2) =
    rewardThen (r :: Reward) (eitherG g1 g2)
```

```{.haskell law="rewardThen distrib race L"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  race (rewardThen r g1) g2 = rewardThen r (race g1 g2)
```

```{.haskell law="rewardThen distrib race R"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  race g1 (rewardThen r g2) = rewardThen r (race g1 g2)
```

```{.haskell law="rewardThen distrib subgame"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game) (g3 :: Game).
  subgame (rewardThen r g1) g2 g3 =
    rewardThen r (subgame g1 g2 g3)
```

```{.haskell law="rewardThen distrib andThen"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game) .
  andThen (rewardThen r g1) g2 =
    rewardThen r (andThen g1 g2)
```

If you noticed a slight inconsistency in these definitions, don't fret; we'll
resolve it soon enough.

The laws above allow us to pull a `rewardThen` through any of the multi-game
combinators, meaning we will definitely encounter them before any of the
short-circuiting games witness something that allows them to terminate early.
For anyone keeping score, this change requires us to weaken some of our earlier
laws, for example, the following is no longer unconditionally true:

```{.haskell law="both win L"}
∀ (g :: Game). both win g = g
```

but instead needs a predicate:

```{.haskell law="both win L"}
∀ (r :: Reward) (g1 :: Game) (g :: Game).
  g /= rewardThen r g1 =>
    both win g = g
```

Such changes are tedious and uninteresting, and so we will not enumerate them
further. When in doubt, the `rewardThen` distributive laws take precedence. As
annoying as this change is, think about what would happen if we already had a
*real* implementation of `Game`s; there is almost no chance that such a sweeping
change would get made. Almost certainly the inconsistency would go unnoticed,
and if it came to light, at best a comment would be written about the behavior.

We will not go through all of these changes together, but the big ones are:

[code/workflow/bugfree_algebra.hs:Game](Snip)

[code/workflow/bugfree_algebra.hs:rewardThen](Snip)

[code/workflow/bugfree_algebra.hs:reward](Snip)

The `race` constructor gets its `RewardThen` distributive law inlined, and this
change is characteristic of how `subgame`, `both` and `eitherG` also need to be
updated:

[code/workflow/bugfree_algebra.hs:race](Snip)

Finally, don't forget to update the `Arbitrary` instance for `Game` so it can
also produce `rewardThen` cases.

Careful readers will have noticed a problem in our distributive laws. To
illustrate, the following laws are inconsistent:

```{.haskell law="rewardThen distrib both L"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  both (rewardThen r g1) g2 = rewardThen r (both g1 g2)
```

```{.haskell law="rewardThen distrib both R"}
∀ (r :: Reward) (g1 :: Game) (g2 :: Game).
  both g1 (rewardThen r g2) = rewardThen r (both g1 g2)
```

when given the input `both (rewardThen r1 g1) (rewardThen r2 g2)`. We can run
the laws in either order, giving us the rewards in a nondeterministic order. But
of course, when looked at like this, it's clear that the order in which we
receive rewards isn't important. And if you recall, QuickSpec was conspicuously
missing the equation that `both g g2 = both g2 g` --- this is why! The two
expressions would result in a different order of rewards.

We come to the conclusion that the type of `runGame` is wrong; that instead it
should return a `Set Reward`, rather than a `[Reward]`. The change is
noninvasive, but as a side effect, requires that we change our `Observe`
instance for `Game`s:

[code/workflow/bugfree_algebra.hs:ObserveGame](Snip)

As you can see; getting the semantics right is quite tricky! The inconsistencies
we found in our last run of QuickSpec have been sorted out, and we're ready for
a new run.

```{#bugfree_laws quickspec=code/workflow/bugfree_algebra.hs}
quickSpec $ withMaxTermSize 5 <> sig_types <> sig_games_core <> sig_games_ext <> sig_filters
```

Everything here looks good to me. Because we have nothing more to add to our
design, and the semantics are consistent, I am now comfortable describing the
API as finalized. This doesn't mean we are resigned to using our model
implementation, but finalizing the API means adhering to its semantics now and
forevermore. As far as public-facing behavior goes, this is all that our users
are going to get, barring the possible extensions mentioned earlier. At this
point, we could happily ship version 1.0 of the library, and people would be
able to get real work done, while we work on revamping the implementation. Any
work our users do on this version will continue to work in our new
implementation, but will run significantly faster.

An elegant design with consistent semantics is no small accomplishment. The
standard of software we've developed here is undoubtedly in the 95th percentile,
and probably significantly higher. But our job isn't yet complete; elegance
isn't sufficient --- still, it needs to be fast.


Exercise

: Add `timeout` and `seqG` from @sec:game_analysis to your QuickSpec signature.
  What laws does QuickSpec find for them? Do they need to be added to the
  QuickCheck generator as well?


### The Final Implementation

Before we get started in our final implementation proper, we can first use our
model to generate regression tests --- useful for ensuring our new
implementation corresponds exactly with the model. By enabling the option
`withPrintStyle ForQuickCheck`, we can convince QuickSpec to emit the laws it
discovers as QuickCheck properties. Each law becomes a property-based test,
which QuickCheck can use to generate an arbitrary number of unit tests. The
result is hundreds of thousands of tests, each checking the semantics we've
so-carefully worked out.

Because there is nothing worse than starting from faulty assumptions, I like to
aggressively ratchet up the max number of successful tests that QuickSpec needs
in order to think a law passes. For designs like ours that have lots of
primitive operations, it's disconcertingly likely for QuickSpec to suggest
false-positives. In the design phase, such superfluous tests aren't a stumbling
block, but for the sake of our sanities, we will ask QuickSpec for a much higher
burden of proof. By setting the `withMaxTests` signature to something extremely
large --- I often set it to 100,000 and leave it running over night --- we can
get a bullet-proof set of regression tests.

For the sake of brevity, I've only asked QuickSpec for laws with a maximum term
size of three here, but you should set it to somewhere in the range seven to
nine when generating your regression tests.

```{quickspec=code/workflow/bugfree_algebra.hs}
quickSpec $ withMaxTermSize 3 <> withPrintStyle ForQuickCheck <> withMaxTests 100 <> sig_types <> sig_games_core <> sig_games_ext <> sig_filters
```

These property tests now form the acceptance criteria for our new
implementation; we're golden when all the tests go green.

Sometimes the semantic equations are exceptionally useful in finding an
efficient implementation, but unfortunately, this is not one of those cases. For
a particularly compelling example in which they are, see @hughes_design_1995.
Instead we must make do with our wits, and take solace in the fact that we now
know as much as we ever will about the design space.

It's helpful to ask ourselves where are the inefficiencies? How much work must
we do in the asymptotically optimal case? Clearly in the worst case we will need
to visit every node of the entire game tree --- but importantly, there is no
reason to need to visit any node more than one time. Our model implementation
requires traversing the entire tree to find the incomplete nodes, propagating
events to them, and rippling their results backwards. For an extreme example,
consider a large bingo board with side length $n$; in this case, every `gate`
exists only at the leaves, and multiple games have duplicate squares. For every
event we'd like to send it needs to be propagated to the $O(n^2)$ gates, and
requires traversing an additional $O(n^2)$ nodes in order to get there.
Furthermore, the work of determining if an event matches a filter will be
duplicated, because, for example, the top-left square occurs in three different
games: a row, a column, and the diagonal.

Clearly we could shave off a good amount of this work by keeping track of which
event filters are blocking which nodes. By matching each event against every
currently waiting filter, we can eliminate the work of traversing the game tree,
and simply unblock gates when a matching event comes in. So this is the insight;
actually making it happen is quite another thing.

Let's take tally of what needs to be accounted for at any point in the game. We
need to know if we've already won or lost, what rewards have been granted, and
which gates are currently blocked. Each of our "simultaneous" constructors ---
that is `both`, `eitherG` and `race` --- forms a monoid, suggesting that there
is some connection between monoids and simultaneity in this domain. Because
combining the results of simultaneous games is certainly going to be a
challenge, let's constrain ourselves to looking only for monoidal structures in
order to accumulate our necessary data.

The collection of rewards was already explicitly chosen in order to be an
order-independent monoid. If we were to collect the currently accessible
`EventFilter`s and map them to the actions that should be taken after their
corresponding gates are passed, that thing would be a monoid if its codomain
were. Let's table that for a second, and consider the semantics of `race`, which
takes its left-most argument's result if both arguments end at the same time.
Correspondingly, the `First` monoid from `Data.Monoid` gives us
"first-one-seen-wins" semantics, which are what is required for `race`. We've
almost found the desired monoid for our game data; all that's left is to find a
suitable monoid to which we can map our waiting `EventFilter`s.

At a high level, the idea is that when an event arrives, we can immediately
check it against every currently waiting gate, and then update our current state
according to what should happen next. That state change should remove the
just-matched event filters from the list of waiting filters. If there was a
newly-discovered `gate`, its accepting filters should be added to the list of
waiting filters. This suggests the thing we map `EventFilter`s to should itself
be game data. Our analysis above required such a thing to be a monoid, and by
choosing the game data here, we neatly tie the knot --- the game data is a
monoid if and only if it is a monoid. Deliciously devilish!

[code/workflow/cps_algebra.hs:EarlyGameData](Snip)

[code/workflow/cps_algebra.hs:EarlyGameDataSemigroup](Snip)

[code/workflow/cps_algebra.hs:EarlyGameDataMonoid](Snip)

A little care needs to be taken at [1](Ann), because the semigroup instance for
`Map` simply ignores duplicate keys. By using `unionWith` instead, we can ask it
to use the provided function in order to merge keys --- which in this case is
why we use `(<>)` to append the gated `GameData`s.

When considering how to implement the `both` constructor, it's clearly not the
whole story. We've successfully captured the intuition behind `gate` and
`choose`s, but now have no means of talking about, for example, how `both`
should "win" if and only if both of its sub-games win. The "win" here is
equivalent to setting the `_result` field of `GameData` only if the `both` game
is the root node. If instead, the `both` node is a child of `andThen`, winning
the `both` should result in evaluating the second argument of the `andThen`
node.

This threading of execution flow sounds like a perfect use-case for a
continuation-based implementation (see @sec:cps for a more complete discussion
of CPS.) The top-level `runGame` evaluator can pass in actions for what to do
when winning and losing, which can then be manipulated and sent to games'
children. The different ways in which constructors use and manipulate these
continuations are what allow them to encode our desired semantics. To
illustrate, we can implement a `Game` as something which uses two `GameData`s
--- representing what to do on victory or defeat, respectively --- in order to
produce a third `GameData`:

[code/workflow/cps_algebra.hs:EarlyGame](Snip)

The definitions for `win` and `lose` are straightforward, and simply select the
corresponding argument:

[code/workflow/cps_algebra.hs:win](Snip)

[code/workflow/cps_algebra.hs:lose](Snip)

Under this scheme, `comeback` has a cute implementation that just swaps its win
and lose arguments:

[code/workflow/cps_algebra.hs:comeback](Snip)

In order to encode `andThen`, whose semantics, recall, are to switch to the
second game if and only if its first game wins, can be implemented as follows:

[code/workflow/cps_algebra.hs:andThen](Snip)

Here, we manipulate the win action given to `g1` to be the result of `g2` using
the original win action. Both games are given the lose action to perform in the
case that they end in defeat.

Similarly, `subgame` distributes the win and lose actions to its last two
arguments, and gives those to its first argument. In doing so, it ensures the
first game branches to the correct sub-game, before continuing on to the next
action after its sub-games are finished.

[code/workflow/cps_algebra.hs:subgame](Snip)

The `bottom` constructor's implementation is interesting; because `bottom` can
never be completed, it simply ignores the actions it was supposed to do after
completion.

[code/workflow/cps_algebra.hs:bottom](Snip)

We can implement `reward` as usual in terms of `rewardThen`:

[code/workflow/cps_algebra.hs:reward](Snip)

In order to give rewards, `rewardThen` constructs an empty `GameData`, consiting
solely of the reward to give, and appends this directly to the result of
evaluating the sub-game. Here we first get a taste of why we worked so hard to
ensure `GameData` formed a monoid; by doing so, it gives us the ability to
easily combine partial data into one:

[code/workflow/cps_algebra.hs:earlyRewardThen](Snip)

The `gate` constructor works similarly, in that it doesn't immediately execute
its sub-game; instead it returns a `GameData` which is waiting on the desired
event filter, and which will run the sub-game afterwards.

[code/workflow/cps_algebra.hs:earlyGate](Snip)

Unfortunately, here we find ourselves stuck. Implementing any of `choose`,
`both`, `eitherG` or `race` requires communication between the sub-games. For
example, `choose` only allows a single choice to be made, meaning its
sub-games need some way of signaling that one has already proceeded, and thus
that other may not run. Rather than scraping our work to this point, we will
take encouragement that this continuation-passing style has gotten us as far as
it has. The technique is clearly correct; it's just that we need a little extra
functionality in order to facilitate this communication.

While it's tempting to attempt to define an algebraic data type corresponding to
the states that a sub-game can be in, this presents a great many difficulties.
For one, there is no obvious place to store this metadata --- games do not have
any means of being uniquely identified, and there is no denotational property
that can distinguish one sub-game from the same one in a different part of the
game tree. They might be in different states! Instead what we'd like is to be
able to introduce a piece of local state at each of the simultaneous nodes.
Rather than futzing about attempting to do such a thing with the `State` monad,
we jump immediately to `ST`, which gives us the ability to allocate new pieces
of state at will.

Making these changes is a trivial task. First we change the definition of `Game`
and nudge its instances into a state that typechecks:

[code/workflow/cps_algebra.hs:Game](Snip)

The syntax at [1](Ann) states that `s` is an existential type variable; it's
present inside of the `Game` constructor, but can never come out. For those
readers less familiar with the `ST` monad, it provides the ability to run "pure"
stateful code. That is to say, it allows us to create, read and change mutable
variables, but only in such a way that the result is still a function. In our
case, the `Game` interface is entirely pure and denotational, and so it's OK to
be implemented with mutability --- so long as nobody ever finds out. The `ST`
monad enforces this mutable-but-pure invariant through the type-system, by
requiring that the result of an `ST s` computation can't depend on the `s`
parameter. In short: the existential `s` variable at [1](Ann) needs to be there,
but it's an implementation detail that is safe to hide from the user.

[code/workflow/cps_algebra.hs:SemigroupGame](Snip)

[code/workflow/cps_algebra.hs:MonoidGame](Snip)

We need some similar changes to `GameData`:

[code/workflow/cps_algebra.hs:GameData](Snip)

and a couple of minor changes to `rewardThen` and `gate` so that they are in the
`ST` monad:

[code/workflow/cps_algebra.hs:rewardThen](Snip)

[code/workflow/cps_algebra.hs:gate](Snip)

The `race` constructor is simplest, so we can tackle it first. The idea is to
race each sub-game with the monoid append operation, but to use a lock
(implemented via `_oneshot` below)  to ensure that the continuation gets called
only once.

[code/workflow/cps_algebra.hs:race](Snip)

At [1](Ann), `race` creates a new `STRef` which tracks whether one of the
branches has finished. At [2](Ann) we instrument the win and lose continuations
with a call to `_oneshot` --- who performs the given `ST` action only if the
`filled` reference hasn't yet been set. The implementation is useful, and can be
repurposed in all of our other simultaneous games:

[code/workflow/cps_algebra.hs:_oneshot](Snip)

Likewise, in a `choose` we'd like to instrument each game with `_oneshot`, so
that at most one of them can run:

[code/workflow/cps_algebra.hs:choose](Snip)

The implementation for `both` is less straightforward, as it shouldn't win until
both of its sub-games have resulted in wins. Here not only do we need a
`_oneshot`, but also we will need to track the number of wins we're still
waiting for:

[code/workflow/cps_algebra.hs:both](Snip)

There is quite a lot going on here. At [1](Ann) we create an `STRef` for the
number of wins we still need to see before calling the win continuation. At
[2](Ann) we create a local function that instruments our sub-games' wins --- it
is responsible for updating the `remaining_wins` counter, and calling the
winning continuation once there are no more wins. At [3](Ann), the `runWin`
function wraps the sub-game's continuations. In addition, every possible call to
the continuations is guarded by `_oneshot`. As expected, the implementation for
`eitherG` is dual to this.

[code/workflow/cps_algebra.hs:eitherG](Snip)

That about tears it for our constructors. All that is left is to implement
`runGame`, which again we will build up to. For some inscrutable reason, there
is no `foldMapM` combinator in the Haskell standard library, so we will begin
with that:

[code/workflow/cps_algebra.hs:foldMapM](Snip)

Given a monadic function from some type `a` to a monoid, `foldMapM` monadically
reduces a collection of `a`s into that monoid. We will use this function to
implement `_stepGame`, which handles updating the game in response to a single
incoming `Event`. Unlike in our model implementation, there is no `Maybe Event`
this time around, as the continuation passing takes care of the nasty "final
update" that was required before.

[code/workflow/cps_algebra.hs:_stepGame](Snip)

This is the most involved function in the entire project, so we will take our
time going through it. At [1](Ann) we collect all of the event filters and their
associated actions, and iterate through them at [2](Ann) using the `foldMapM`
combinator we just wrote. The index of this iteration is `ef :: EventFilter` and
`res :: ST s (GameData s)`. For each event filter, at [3](Ann) we check if the
incoming event matches it (remember that multiple filters can simultaneously
match the same event), and if it does, at [4](Ann) we perform the actions in
`res`. As the last step in this iteration, we construct a pair of an
*endomorphism* --- a function `GameData s -> GameData s` which removes the
current event filter from the list of waiting filters --- and the `GameData` we
acquired by evaluating `res`.

Here we are leaning heavily on the fact that `GameData` is a monoid. Because
endomorphisms also form a monoid, and that the pair of any two monoids is itself
a monoid, we can efficiently and non-destructively go through every pending
filter, try them all, evaluate and remove the ones that match, and while doing
this, accumulate all of the resulting state changes. Finally at [6](Ann) we use
`appEndo` to run the series of filter deletions that we built up at [5](Ann),
and then at [7](Ann) add in whatever new state was generated. Impressively, this
new state might contain the same event filters that were just removed, but we
need not concern ourselves with this because our monoid operations just do the
right thing.

The `_stepGame` function does the vast majority of the work, but we need a few
wrappers around it yet. In order to process multiple events, we add `_pumpGame`,
which folds a list of `Event`s by stepping each one. There is nothing much of
value here, other than [1](Ann) which checks to see if the game has been
completed, and if so, refuses to process any more events.

[code/workflow/cps_algebra.hs:_pumpGame](Snip)

Finally, we can implement `runGame`, which calls `_pumpGame`, finagles the
resulting `GameData` into the correct return type, and then evaluates the `ST`
monad.

[code/workflow/cps_algebra.hs:runGame](Snip)

Of particular interest here is that we finally need to "tie off" the win and
lose continuations. At [1](Ann), we pass in the `giveVictory` and `giveDefeat`
actions (defined immediately below,) which do nothing but assign the `_result`
field of `GameData`.

[code/workflow/cps_algebra.hs:giveVictory](Snip)

[code/workflow/cps_algebra.hs:giveDefeat](Snip)

There is one final step necessary, and that is to give an instance of `Show` for
`Game`. QuickCheck requires such an instance before it will run our tests, and
so we can do the same trick of evaluating a `Game` using `giveVictory` and
`giveDefeat`:

[code/workflow/cps_algebra.hs:ShowGame](Snip)


Exercise

: Give implementations for `timeout` and `seqG`.


Exercise

: Change this implementation to also have a new `action :: Action -> Game`
  constructor. The `runGame` observation should now also return a `Set Action`.
  How much of our implementation needs to change? Do our monoid instances help?


Exercise

: After making the above change: does the addition of `action` change any of our
  semantic laws? Make a prediction. Add `action` to the generator and see if
  your tests still pass.


### Discussion

This concludes our implementation. The tests can now be run, and all of our
semantic regression tests are green. Though the details of working through our
implementation were mostly elided --- as this book is more about *design* and
less about *implementation* --- the tests proved extremely useful. The
implementation as presented wasn't my first attempt, and required many
iterations to get everything to pass.

Of particular note towards the usefulness of these tests, my original
implementation of `both` would fail the `both g g2 = both g2 g` test whenever
run by the test suite --- but not when I tested it by hand. As it happened, this
implementation didn't use `_oneshot` to ensure its win continuation wasn't
called multiple times. It seemed to me that because the `GameData` is monoidally
idempotent --- that is to say, appending an element twice is the same as
appending it only once --- that it shouldn't matter if I locked my continuations
or not.

But this is not true of the constructed continuations in general. Consider the
following term:

```haskell
both (eitherG win win) lose
```

Such a thing should be equivalent to `lose`, but in my implementation,
confusingly it would `win`! Why? Because the win continuation given to the
`eitherG` wins after it sees two wins. But the `eitherG` calls that continuation
twice on each of its wins! In effect, the wins seen by `eitherG` were leaking
upwards, and my implementation considered them to be wins seen by `both`! Such a
bug might not have been caught by a traditional set of unit tests, because it
was triggered only by a particular interaction between `both` and `eitherG`, and
even then, only when `eitherG` had both of its branches win before the `both`
saw its own loss.

I was an insufficiently clever programmer to find this failing test case, even
when I knew there was a bug here. Every case I was capable of generating with my
limited human creativity gave the correct result. While it's true that this bug
probably would have been quickly triggered "in the wild," it's unlikely that my
old company would have noticed. Think of just how much monitoring, reporting and
dashboard systems would have needed to be implemented in order to see this sort
of bug happening live. By taking a few extra days of thinking about our design
before diving into the implementation, we have saved weeks of
otherwise-necessary infrastructure for tracking the inevitable bugs. Of course,
convincing the business people of this truth is a task left for another book.

![Relative performance of game
implementations](images/game-cps-perf.png){#fig:game_perf}

Having done all of this CPS implementation work, it's natural to wonder whether
it actually made anything faster. @fig:game_perf shows the results, and they're
very pleasing; we see asymptotic improvement from $O(n^2)$ in the initial
term-based algebra to $O(n)$ in the CPS implementation. These graphs show the
results of running our semantic test suite, requiring 10,000 successes for each
of the 93 tests in the suite. The x-axis shows the size of the QuickCheck `size`
parameter, and the y-axis is the time the suite took to run, in seconds.

In our term representation, nodes that contain `gate` or `choose` descendants
are stuck, and need to be traversed on each and every event in order to propagate
the event to where it needs to be. For a game with $n$ waiting `gates`, there
are $O(n)$ nodes that need to be traversed before we can ever do any useful
work. At $n$ event filters to evaluate, and $O(n)$ nodes to traverse, it's no
wonder the term implementation is quadratic.

Contrast this to the CPS implementation, where a node is only ever expanded
once, and we a close eye on the gated parts of the tree. For Any given event, we
now only need to check against the $n$ active filters, but no traversing is
required. This shaves off a cool factor of $n$ in the resulting asymptotics, and
is asymptotically-hierarchically better than the exponential amount of states
necessary to represent our game with the "obvious" nondeterministic finite
automaton.

However good these results, it's natural to wonder if we might do better yet.
One rather immediate inefficiency with the CPS implementation as given is that
event filters which are guarded by a tripped `_oneshot` are never removed. Such
filters must be checked against every incoming `Event`, but can't do any work
even if they match. As such, this is entirely wasted computation. A particularly
antagonistic example is a 10x10 bingo board, which is used as the first argument
to `andThen`. If we clear this game of bingo in our first ten events, the 90
remaining event filters need to be tested for every incoming event for the
remainder of the game. Perhaps we could prune these obviously useless filters.

And indeed, I implemented this, by giving a unique ID to every node in the game
tree, and then attaching a piece of metadata to `_waitingOn`, describing which
nodes had contributed continuations to each filter. As games ran their eventaul
win or lose continuations, they would instrument those by removing their IDs
from any attached filters. Filters that eventually had no associated IDs would
be removed. In retrospect, perhaps this is a naive implementation, and the
runtime agreed with me. The benchmarks I ran were over 2x slower than the
original *term-based implementation,* suggesting insanely large constant factors
in order to perform this bookkeeping. Furthermore, antagonistic games like these
are atypical, as most games will have a small, constant number of `gate`s. And
until evaluation of `matches` becomes a performance bottleneck, we shouldn't
expect this micro-optimization to buy us much.

Additionally we can expect us to find more potential optimizations inside of the
structure of `EventFilters` themselves. Although we've left them completely
unspecified, it's reasonable to assume they will eventually acquire at least the
following filter combinators:

```haskell
and :: EventFilter -> EventFilter -> EventFilter
or  :: EventFilter -> EventFilter -> EventFilter
not :: EventFilter -> EventFilter
```

In our current implementation, we evaluate every pending `EventFilter` exactly
once for each incoming event. When the combinators above are present, we will
likely duplicate the work of evaluating the terminal filters. Indeed, there are
optimizations to be made here, but such a change can be made without touching
any of the other `Game` machinery, and the work is straightforward. Furthermore,
it seems unlikely that boolean predicates over `Events` are likely to be our
next performance bottleneck. Thus that this optimization is probably not worth
the effort now, but we can rest assured that we have not painted ourselves into
a corner if later work on this front is deemed necessary.


