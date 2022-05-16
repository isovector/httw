## Computation

<!-- ### What is Computation -->

<!-- #### Nature of Reality -->

The universe proceeds steadily --- from one momentary configuration of subatomic
particles to the next. Every picosecond, the state of the universe evolves,
slowly, resolutely, unwaveringly. Somehow, the universe "knows" the true laws of
physics --- much better than we humans could possibly do --- and unerringly
calculates the applications of these rules. If the laws say an electron should
zig, neither the electron nor reality itself can subvert that expectation.

Of course, none of that is to say that we humans necessarily comprehend how the
universe works; thankfully, the universe works whether we understand it or not.

Long before our modern understanding of friction, some unknown and unsung hero
realized that rolling objects require significantly less energy expenditure to
push than their sliding counterparts, and the wheel was born.

Archimedes didn't have a deep understanding of gravity, but he nevertheless
realized that water can be raised from a well by virtue of rotating a corkscrew.

We humans have a long history of achieving amazing feats via a clever
application of the mechanisms of reality that we do understand. The entire
project that we call infrastructure is manifest only by humanity's ingenious
exploitation of the laws of nature. The ancients realized that bricks can hold
loads much heavier than their own weights, and thus they stacked them to build
castles. Water was learned to be uncompressable, which lead to the elevation of
water towers to use gravity to provide running water to towns below, to say
nothing of pneumatic tools.

Indeed, we humans have been blessed with a curiosity about the world, and an
impressive knack for channeling the resulting knowledge into means of bending
reality to our will. If the universe's unceasing calculation of the future is a
river, we humans are the canal diggers, transforming the unstoppable flow of
time into fruits --- of our labor, yes, but more importantly, of our minds. The
human mind is a machine which transforms the universe's raw force into desirable
states of existence.

For most of human existence, this process has been slow and accidental. The
human brain simply wasn't designed for the sorts of work with which we task it.
We get bored easily. We can keep only a handful of things in our heads at once.
We're often distracted by hunger, thirst, status, sex, and perceived threats.
Our brains haven't changed physiologically for hundreds of thousands of years,
and are much better at keeping us alive in an ancient savannah than they are at
methodical tasks like reading, engineering, reasoning, designing or
experimenting. And yet it is exactly these sorts of activities which most
influence modern life! What a state of affairs.

Modern systems are simply too big for any one person to keep in mind. Imagine
the impossibility trying to keep a millimeter-accurate model of a city --- or
the entirety of a country's tax code --- in your head. It's a preposterous idea.
And yet, still, cities function and taxes get paid. How can this possibly be?

The human mind has one, and exactly one, strategy for dealing with complex
systems: break them down into simpler, smaller systems, and to do one's
reasoning there, returning to the full-size only when a solution has been worked
out. Plumbers don't need to understand a city's entire waterworks, they need
only understand how the house they're currently looking at work, and how the
house is connected to the city. The house can be considered in isolation.
Humans, when working on big systems, are capable only of subdividing problems
and combining solutions. Due to the small number of things we can simultaneously
keep in mind, we are left with no choice but to look at local pieces of the
problem, and keep the wider system in mind only as a black box.

This ability to subdivide problems is known as *decomposition,* and to recombine
solutions as *composition.* They are two sides to the same coin, and thus we
will henceforth refer to these abilities collectively as *composition.* A
necessary prerequisite of composition is for the simple synthesis of solutions;
the ability to split up problems is of no use if it's impossibly challenging to
put the resultant pieces back together again.

Humans solve challenging problems via composition. They pull big problems apart
into smaller ones, and solve those smaller ones --- possibly by decomposing the
subproblems again. At some point, the hope is that the subproblems become easy
enough to solve that we can work our way back up, combining the results until we
have a solution to the original question.

Because we bore easily, a huge chunk of human ingenuity has been spent on
constructing ways of automating menial tasks. Consider mathematics, which was
originally designed to save the effort involved in accounting. Or the excavator,
which dramatically improves upon the shovel, itself a dramatic improvement on
digging with shells or rocks, themselves an improvement on digging by hand.
Factories now automate many tasks that were done by hand, to the degree that
we often forget that such things were done manually in the first place!

All of this leads to computation: a human invention that helps to automate away
the doldrums of menial thought and mental effort. You are certainly familiar
with this sort of thought. Recall back to grade-school mathematics, where you
are forced to memorize a table of addition and of multiplication, as well as
several rules of arithmetic. Afterwards, you are subjected to uncountably many
hours of dull calculation --- things like "evaluate $(14 + (8 - 2))^2$," which
aren't hard, merely tedious and requiring exact work. But the answer you get at
the end (assuming you've been a good robot and followed the steps correctly) is
always the same. Grade-school "mathematics" is not an activity fit for humans;
it is a task for machines, and asking humans to do it is akin to transforming
them into automatons.

Computation is the clever exploitation of the universe's tendency to evolve in
order to automate deliberate thinking in service of a goal. The
computationalist's task is to shape nature such that the universe will provide
us with the answers we seek, in the same way that a river has no choice but to
deliver water to its mouth.

It is in this context that we will consider the importance of computation
design; that is, building the systems that tame the universe's raw computational
flow into something more desirable. And it is by means of composition that we
will develop this skill. The book currently in your hands is a study of the art
of composition, for the purpose of applying it to computation design and
automating tedious tasks.


### Some Misunderstandings

Before diving into the main thrust of this book, we would be remiss to not first
clear up some extremely common misunderstandings --- namely those around
computers and software. You likely believe that computation is somehow
intrinsically tied to *computers;* it is not. A great pioneer of the field once
said:

> [Computation] is no more about computers than astronomy is about telescopes.
>
> --Edsger Dijkstra

The space of human sensory perception is very small, compared to the huge number
of possible phenomena in the universe. Our eyes, unassisted, are capable of
seeing resolutions down to about 0.1mm, but this is much smaller than the size
of things we can precisely manipulate by hand. Most of us are unable to lift
more than 100kg --- especially not for extended periods of time. We can neither
see nor hear radio waves. Most of what the universe has to offer is profoundly
outside of our human limits.

And yet, none of these are insurmountable challenges, because humans are good at
building machines that extend their physical capabilities. Microscopes let us
see smaller things, power tools help us work with bigger objects, and modern
electronics are capable of detecting all but the most extreme electromagnetic
wavelengths.

Computation should be similarly considered as a phenomenon outside of our
everyday experience. Computers are tools --- like telescopes --- that help us
interact with the latent phenomenon, but they are not themselves the object of
study. Confusing the map for the territory here is especially troublesome, in
that modern computers impose severe *cognitive restrictions* on their
practitioners. By studying *computers,* the student trains themself to think
thoughts that are easy for the computer to handle. But this shouldn't be the
goal; the student of computation instead looks to build machines capable of
expressing the thoughts they care about. Studying computers is the equivalent of
willfully ignoring all of those cosmos which your current telescope cannot see;
rather than being driven by the burning curiosity to see what else is out there.

The other misunderstanding we need to clear is that computation is equivalent to
software. Software is indeed a special case of the more general idea of
computation, but it is limiting to consider the two to be equivalent.
Computation is a physical process; software is a specific encoding of ideas
meant to be executed by specific computers. When you think of software you
probably think about consumer-facing computer programs, things which find
information for us, connect us with one another, help with our taxes, and
orchestrate transportation.

Computation is much more general than this. Computation occurs everywhere in the
universe, always. That's not to say that we humans *care* about the vast
majority of it, but every time an electron interacts with a magnetic field, the
universe is performing computation. Every time gas molecules bounce in a
chamber, computation is occurring. It would be very odd to call this software.

> TODO(sandy): computation:running programs :: ? : software

It would be very interesting to learn where the universe gets its computation
power from.

Much closer to home, computation also occurs inside of our brains when we
think. When we find our bearings on a map, or follow a difficult argument, or
ponder on who we are as people, make no mistake, we are performing mental
computation.

Thus a study of computation design is important not only for participating in
the modern economy, it's fundamentally the study of *thought itself.*
Philosophers will disagree with this statement, but they have much less to show
than the computationalists do. And as we will see in @sec:philosophy, a solid
understanding of computation immediately clears up many philosophical
misconceptions.

Finally, computation is not about "zeroes and ones." These, again, are a
practical engineering decision in the machinery of the computer, but are neither
deep nor fundamental to the idea of computation itself. In fact, the earliest
computers worked on analog signals, which had an infinite number of possible
values, rather than just the zeroes and ones of the present day. This book is
not particularly concerned with engineering concessions. Instead we will focus
on building good computational arguments, and learning how to *embed* their
functioning into any physical system we desire --- whether that thing be a
computer, a series of water valves, or a plethora of common rock crabs.

Computation happens everywhere; the universe can't help itself.


### Designing Computation


