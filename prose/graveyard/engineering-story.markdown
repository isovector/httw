

We begin with an overview of what it is we'd like to build. This example is
based heavily on some commercial work I did several years ago. Our company's
main offering was a real world, social game-platform, whose purpose was to
incentivize users to try things they'd never done before. Think of an
augmented-reality scavenger hunt --- the game might ask you to choose between
climbing a tree in the park, or visiting a cafe you'd never gone to before and
ordering something bizarre. Every week you'd be given a scenario with multiple,
possibly-branching objectives, and be rewarded for both partial and total
success.

Though the data-collection aspect of this problem is interesting in its own
right, we will focus our attention only on the management of the game-state.
When I joined the team, the company was on its second total-rewrite of the
project, and by the time I left, it was on its fourth. Each had slightly
different semantics as we learned more and more things about what this platform
we were building *actually was.* Whispered mentions suggest that they're
currently working on a fifth rewrite.

We'd like to build a domain-specific language for constructing these games.

What doesn't seem like a terribly difficult problem on the surface is clearly
sufficiently hard to require several extremely costly re-imaginings of the
solution. The first solution solved the problem, but required every game
scenario to be painstakingly hand-coded, relegating engineering time directly
into content creation. The rewrites of the system were primarily motivated to
let the product people to build arbitrary scenarios without the assistance of
any engineers.

The scenarios available in the first version of the game were linear sequences;
do A, then B, and then finally C. In some *particularly exotic* scenarios, you
could complete them in any order. The number of required tasks would vary.
Sometimes you'd get small rewards for partial completion, but some scenarios
would only give you one large reward at the end. As you can imagine, the
initial scenarios weren't particularly fun.

Much of the subsequent explorations of the scenario design-space were based
around supporting games similar to the initial release. There was product talk
of games that might be similar to a scratch-and-win card, with lots of possible
actions, but rewards that were few and far between. The mythical
über-implementation would be capable of supporting *bingo-like games:* ones with
multiple overlapping sub-games, any of which could be won on its own. Nobody was
sure how to make a system that supported bingo without simply hard-coding
support for it. But recall, this is antithetical to our goal --- avoiding
bespoke implementations of certain game-types was the entire point of the
rewrites.

Inevitably, the product people would come up with an idea that simply couldn't
be implemented in the scenario engine. They'd complain that they were being
constrained creatively; the engineers would complain that nothing like this was
ever in the specification. As you can imagine, there were sour feelings on both
sides.

As the engine was written and rewritten, we learned more and more about the
system. Each rewrite was certainly better than the previous incarnation, but in
the time I worked there, we were continually blindsided by what the product
people came up with. Our solutions were never good enough.


None of this is to suggest that I'm without blame in this story. I'm guilty of
having written an over-constrained implementation of the game engine. What I
built was one that met the specifications as they were given, but which didn't
anticipate the specifications coming down the pipe. There are two personal
stories intertwined in this case study: on the object level, it's a chance to
finally solve the problem "that got away." On the meta level, it's an
opportunity in designing systems that are robust against changing
specifications. I suspect I'm not the first engineer who has implemented the
system my client *asked for,* rather than the thing they *wanted.*

While as engineers we can stand firm on providing a solution to the stated
problem, human nature isn't going to change, and the world would be better off
if we instead learned to build systems capable of solving problems for people
not even born yet. Some people instinctively recoil from such an idea,
conflating flexibility with slow, complicated, impossible to configure, and full
of enterprise-strength (read: useless) abstractions. This is not what I'm
talking about. Let's tackle the flexibility issue first, and deal with the
others separately.

Remember: there is no law of physics that requires software to become unwieldy
and complicated. It just feels like it sometimes.
