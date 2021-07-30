## What is Computing?

Different disciplines bring with them not only different tools, but different
mindsets. The lawyer approaches the world in a way very apart from that of the
doctor, teacher, engineer, scientist, or mathematician. A scientist vies the
world through the lens of hypotheses and falsifiability. She makes and tests
predictions, and you can see this behavior in her everyday life. The engineer
builds things on the Pareto frontier, making trade-offs between relevant
desiderata. The lawyer is fundamentally a storyteller, weaving together
plausible narratives, but also a shrewd analyst with a keen eye for
inconsistency. Once you know what to look for, it's often possible to correctly
guess a new acquaintance's profession, based only on the way their discipline's
mindset manifests itself.

What is the mindset of the computationalist? In this book you will get a healthy
immersion into it, but let us first set the stage. The primary distinction
between the computationalist and the other disciplines is in the *treatment of
information.* In computing, information is a very real substance, not unlike
clay. It has an exact, measurable quantity, and it can be shaped and sculpted to
suit different purposes. Information has a natural tendency to evaporate away; a
significant portion of the job is coming up with clever ways to maintain and
preserve it.

In computing, not only do you know a fact, you necessarily must also know *why*
a fact is true. The computer is an idiot device. It knows nothing for itself,
and can follow only the simplest of instructions. Successfully working in
computing means understanding problems with such clarity and precision that you
can explain them to the intellectual equivalent of a goldfish. An instruction
like "go to the pantry" is billions and billions of times too complex for the
idiot computer machine; one first must describe how to actuate every muscle
involved in walking to the pantry, explain what is a pantry, and how to identify
one given millions of pieces information about light intensity.

At the same time, the computationalist mindset requires a rapid ability to zoom
out, and work at higher-levels. Complicated systems, whether they are technical
or not, are often amalgamations of smaller, self-contained pieces.
Multinational corporations have a head branch in each country, each of which
oversees smaller offices, made up of many teams of individuals --- perhaps all
of whom share an IT desk and accounting system. The computationalist is an
expert at designing overarching systems by constantly switching between levels,
and building the necessary infrastructure at each layer. This is a consequence
of the idiot machine; it is simply too hard to solve hard problems "close to the
ground," and as a defense mechanism, computationalists prefer to build "upwards"
rather than "outwards."

Much like the mathematician, the computer scientist loves simplicity, and has a
great distrust of exceptions to the rule. Computationalists are extremely good
at simulating computers in their brains, and general principles are ones which
are easy to compute[^not-learn]. A software system which contains lots of
special exceptions is a system that cannot be maintained. For the most part, in
the mind of the computer scientist, *special cases don't exist.* Their presence
in a system is indicative of having captured the wrong essence of the problem,
and is *shameful* to its architect.

[^not-learn]: Though they not necessarily easy to learn in the first place.

Relatedly, to the computationalist, the presentation of a problem is often as
important as the problem itself. Systems can be deconstructed along many axes,
and each brings with it an understanding and mental model. Like the comedian
knows: it's not what you say, but how you say it. For example, imagine needing
to move a large amount of freight across the ocean. One way to do it would be
via a convoy of ships --- big, and lots of work to orchestrate, but gives you a
lot of flexibility in where all of the freight ends up. Alternatively, you could
just put everything in one giant container ship --- more compact, but impossible
to shuffle the contents without unloading everything first. Both systems get
your freight where it needs to go, but the logistics between the two are
extraordinarily different, and you will expect different sorts problems to arise
in the process. As such, the *structure of your solution* --- not just the
solution itself! --- is an important thing, deserving of study in its own right.

Throughout this book, the themes above will recur time and time again. The
material is organized in the same order it would be approached "in the field."
That is to say, we will start with small, simple concepts, and progressively
build bigger and bigger pieces out of them.

If this is your first time reading a technical book, welcome! Though be warned
that this material builds on itself rapidly, and all of it is necessary. Much
like missing a few math classes in school, you will quickly fall behind if you
don't have a strong understanding of each step along the way. This is a book to
read slowly, while taking high-level notes to help you remember what each piece
does. It's not necessary to memorize the inner workings of every piece; indeed,
to the computationalist, these building blocks are "black boxes." We needn't
know how they work unless they aren't working in the way we expect. The trick,
then, is pay close attention to the *narrative* --- what are we building, what
is it supposed to do, and why do we need it?

> TODO(sandy): maybe give a quick summary of each big piece as they're
> introduced, perhaps in an explicit admonition box.

