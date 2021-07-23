## Machines

All of computation is based upon extremely precise logical pieces of reasoning.
Mind-boggling amounts of tiny, predictable machines are laid out in complicated
patterns. These machines push a small amount of electricity under some
circumstances, and not under others. At every layer of the system, the entire
process of moving electrons is deterministic and prescribed. There is no
absolutely no intelligence, intuition, or randomness whatsoever in the system,
and yet it still behaves in ways that are meaningful and surprising to humans.
How can this be? How can meaning arise from millions of tiny, mindless machines
shuffling electrons around based on predetermined rules?

The answer to this question is the core of the book you're currently reading,
and it comes in many different flavors. In one sense, this is a question of
engineering --- what procedure can be followed to build a system with these
properties? What are the governing dynamics underlying this natural phenomenon?

But in another sense, this question is highly philosophical. Where does meaning
come from, and why should we expect any relationship between the world of
electron flow and human understanding? Why should these symbolic systems map to
reality?

Why do these systems map to reality? The answer is rather tongue-in-cheek, but
entirely true: *because we can't build anything else.* There are astronomically
large numbers of possible machines that could exist. More than every star in the
sky; more than every *atom in the universe.* There are so many possible machines
that there are no words that give any sense of understanding of the sheer
number. We must rely on analogy.

Imagine if you were tasked with finding one particular grain of sand on Earth.
There are (very) approximately 7,500,000,000,000,000,000 grains of sand on the
planet, so even if you could inspect one grain every second, and you did nothing
else --- not even sleeping or eating --- for your entire life of 100 years,
you'd still need, on average, 2.5 *billion* lifetimes to find the one you're
looking for.

While that sounds hard, it's still nowhere near the difficulty of finding a
particular machine in the space of all possible computations. Let me explain.

Two and a half billion lifetimes to find a grain of sand doesn't sound like it
will be happening anytime soon, but it's still a tractable problem. Seeing as
there's about seven billion people, if you could enlist them all, you'd have
more than the requisite number of lifetimes to dedicate to the problem.

This is what's known as a *linear* system. If you throw twice as many resources
at the problem, you get the result in half as much time. Most systems that
humans encounter in their day-to-day life are linear. The big outliers are
population growth and investments --- your population grows faster the more
people you have, which begets faster growth again, ad infinitum. Interest from
investments is similar, as you gain interest, you have more money, of which you
can now gain more interest off of. Systems that grow like this are said to grow
*exponentially.*

Computer scientists are very interested in how quickly their systems (and
problems) grow --- so much so that they've invented an entire taxonomy of
classification for growth rates.

Let's return to our problem of finding any particular grain of sand. Even as a
linear problem it's already quite hard, and would require the coordination of
the entire human race in order to reliably solve. But imagine now if the number
of grains of sand weren't just 7.5 quintillion. Imagine if the grains of sand
were doubling *every second.* The problem that before required every human to
give up their entire life to help solve has now doubled in difficulty 3 million
times. That's not to say it's 3 million times harder, but that it's become twice
as hard, and then twice as hard again, and then twice as hard *again...* three
million times.

At this scale, it's inconceivable to think that humans can ever tackle this
problem. It's not an issue of speed, but of *growth.* Even if humans colonized
every planet in the solar system, and each one could check a billion grains of
sand every second, we'd still never get close to having a chance.

As computations grow in size (that is to say, in *potential interest* to
humans,) the number of programs that could possibly exist grow roughly as
quickly as our hypothetical dividing grains of sand. But unlike sand, we can't
just cast our gaze on the beach in order to locate arbitrary grains. The only
way to get your hands on a complicated machine is to get your hands on a
slightly less complicated machine and make it a litter bigger.

This argument starts to suggest why human-relevant computation is possible ---
because computers need to be made by humans, and the only way we can make them
is by slowly extending machines we already know and understand. That is to say,
the human mind is *literally blind* to the existence of the crushing majority of
possibilities. In a sense, most programs are so meaningless that they're
impossible for humans to think about, and thus, the only ones we *can* think
about are the ones relevant to us. That's not to say that these things don't
exist, only that we can never experience them.

It's not that we *don't* want meaningless machines. It's that we *can't* want
them.

Rather humbling and mystical, isn't it? There are fundamentally unknowable
things in the universe. Not because of non-existence, but because, in a sense,
of *too much* existence. It's a lot like being a fish in the ocean; the water
around you is so ubiquitous that it's not even noticeable. We're all just
swimming in computation, oblivious to all but those that are so special that
they stand out from the rest by being *meaningful.*

Returning from the philosophical detour to the question of how can we engineer
these sorts of systems, we find that we've already got an answer. Big,
comprehensible systems are made by combining smaller comprehensible systems. To
engineer a meaningful machine, we must start from the exceptionally simple ---
where there are so few in number that we *can* differentiate between them ---
and then intentionally build up from there.

The remainder of this chapter is dedicated to looking at these fundamental
building blocks. Literally everything else that can be analytically studied by
humans is constructed by combining these parts in different and interesting
patterns.


### Diagrams

In electronics, we often reason by way of diagram. Diagrams are fantastically
expressive tools of thought, which allow us to see at a glance the salient
aspects of concepts. Here's an example of a diagram for a simple circuit:

```{#fig:not design=code/Design.hs label="A Diagram"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = unsafeReinterpret >>> notGate >>> unsafeReinterpret } in c
```

This diagram shows a very simple computation; it's the equivalent of flipping a
light-switch. Either the light was on before and now it's off, or it was off
before, and now it's on.

Diagrams are read from left to right. The funny five-sided shapes labeled
`Before` and `After` are called *ports.* `Before` corresponds to an *input,* in
this case, it corresponds to the state of the light bulb *before* we flip the
switch. We don't differentiate between whether the light is on or off; both
states are possible in the input. At first blush, it might seem feckless to
disregard whether the light is on or off, but suspend your disbelief for now. If
you will excuse the pun, it is exactly this disregard for the exact scenario
that lends our diagrams their power.

The port labeled `After` is the *output* of the system, and in this case, it
represents the state of the light bulb *after* flipping the switch.

Diagrams are read by following the lines which connect the individual pieces of
the circuit. Rather suggestively, these lines are called *wires,* and correspond
to physical wires when building physical circuits. Wires serve only to connect
ports and other components to one another. A wire's connectivity is its *only*
property of importance. We don't care about the length of the wire, or about
whether it winds through the diagram. All that matters is what's on either end.

This simplification is justified, because in real circuits, electricity
moves through wire at millions of kilometers per hour; it's so fast that for all
intents and purposes, it's instantaneous.

> TODO(sandy): need to call it a not gate

All that remains in the diagram is the triangle with a circle on its tip.
Anything in a diagram that isn't a port or a wire is called a *component,* and
components are where the interesting things happen. In our example, this
component corresponds to the action of flipping the switch. On the state of the
world represented by the wire on the left side the component, the switch hasn't
yet been flipped, and on its right wire, the switch has been flipped.

These findings are summarized in the following *function* table, which describes
how the state of the light bulb changes after the circuit.

```{#fig:not_truth design=code/Design.hs fn=truth label="Function Table"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = unsafeReinterpret >>> notGate >>> unsafeReinterpret } in c
```

So far so good? Try this next diagram on for size, and attempt to decipher its
meaning before continuing.

```{#fig:notnot design=code/Design.hs label="Flipping Twice"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = unsafeReinterpret >>> notGate >>> notGate >>> unsafeReinterpret } in c
```

Once you've understood the first diagram, the meaning of this one should be
easy. Now, rather than flipping the light switch once, it's being flipped twice.
The resulting state of the light is the same as if the switch hadn't been
flipped at all, as shown in the equivalent function table:

```{#fig:notnot_truth design=code/Design.hs fn=truth label="Function Table"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = unsafeReinterpret >>> notGate >>> notGate >>> unsafeReinterpret } in c
```

As system designers, we are afforded the liberty of deciding when exactly two
systems are the same, or "equal." By fiat, we say that the only important
property of a circuit is its function table. Thus, two circuits are the same *if
and only if* they have the same function table --- that is, if they agree on the
output for every input.

Because of this fact, despite looking different, @fig:notnot is equivalent to @fig:id:

```{#fig:id design=code/Design.hs label="No Flips"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = id >>> unsafeReinterpret } in c
```

This equivalence is also justified physically. Remember, the length of the wires
between components is inconsequential, and the interpretation in @fig:notnot should be
that the switch is flipped twice instantaneously. There is no moment in which
the light turns on before turning back off. In essence, the two flips
immediately cancel one another out.

While @fig:notnot shows that circuits can be combined sequentially, it is also
possible to combine circuits in *parallel.* Make a guess as to the
interpretation of @fig:notnot_par:

```{#fig:notnot_par design=code/Design.hs label="Parallel Composition"}
both notGate
```

In the spirit of our running example, @fig:notnot_par is two independent light
switches and bulbs. They can be toggled separately, and exert no influence on
one another. The function table for this diagram is:

```{#fig:notnot_par_truth design=code/Design.hs fn=truth label="Parallel Composition"}
both notGate
```

which is just the table for @fig:not repeated twice. Some care need be taken
when working out function tables for parallel composition. It is of critical
importance that every possible combination of inputs be listed and given an
output. It is **worse than meaningless** to give a function table which doesn't
contain a row for every possible input state. In general, if you are combining a
circuit with $m$ inputs with one with $n$ inputs, the resulting table will have
$m \times n$ inputs, **not**  $m + n$ as might be expected.

My apartment doesn't have overhead lighting; instead, the light switches control
wall outlets directly, and finding the right outlet to plug a lamp into is
always an exercise in patience. It seems that most lamps were not designed with
this use-case in mind, because they all have their own switch. Thus, the lamp in
my living room is only turned on when both the wall switch and the lamp switch
are turned on:

> TODO(sandy): cheating here. this is no longer flipping a switch; now it
> describes the state of the system

```{#fig:and design=code/Design.hs label="Wall and Lamp Switches"}
unsafeReinterpret @(Named "Wall" Bool, Named "Switch" Bool) >>> andGate
```

The function table for this system is:

```{#fig:and_truth design=code/Design.hs fn=truth label="Wall and Lamp Switches"}
unsafeReinterpret @(Named "Wall" Bool, Named "Switch" Bool) >>> andGate
```

For the first time, this circuit has a different number of inputs and outputs.
There is no law stating they need be the same! Our new component is called an
*and gate* because it outputs `on` only when input 1 **and** input 2 are both
`on`. To help remember its symbol, notice that it looks like the capital "D" in
the word "AND."

Diagrams can be arbitrarily complicated by connecting the outputs of components
to the inputs of others. For example, we could stick a `not` gate immediately
after an `and`, like in @fig:andnot.

```{#fig:andnot design=code/Design.hs label="Not after And"}
andGate >>> notGate
```

By visual analysis of @fig:andnot, it feels safe to say that the machine is
`off` whenever the `and` of the two inputs is `on`, and vice versa. Intuitively,
this provides an easy way to derive the corresponding function table --- just
take the function table for `and` and flip its output values.

```{#fig:notand_truth design=code/Design.hs fn=truth label="Not after And"}
andGate >>> notGate
```

It's also possible to build a new machine composed by putting a component in
front of an input. For example, @fig:notandnot puts a `not` gate before the
`and` from @fig:andnot.

```{#fig:notandnot design=code/Design.hs label="Precomposing a not gate"}
first' notGate >>> andGate >>> notGate
```

> TODO(sandy): write precomposition prose; using explicitly back mapped tables
> like in httw would be a dramatic help

By precomposing another `not` gate, this time into the other output, like in
@fig:notnotandnot, an interesting result manifests in the function table.

```{#fig:notnotandnot design=code/Design.hs label="Not/not and not"}
both notGate >>> andGate >>> notGate
```

```{#fig:notnotandnot_truth fn=truth design=code/Design.hs label="Not/not and not"}
both notGate >>> andGate >>> notGate
```

While the `and` gate returns `off` if any of its inputs is `off`, this machine
we've built does the opposite. The circuit in @fig:notnotandnot evaluates to
`on` if any of its inputs is `on`. Such functionality is extremely useful in
practice, and so this machine is so popular it too gets a special symbol:

```{#fig:or design=code/Design.hs label="Or gate"}
orGate
```

This is called an *or* gate, so-named because it produces `on` if one **or** the
other of its inputs are `on` (**or** if both are!)

Rather curiously, another strange thing occurs if we pull the same trick,
attaching a `not` gate to every wire connected to an `or` gate, like so:

```{#fig:notnotornot design=code/Design.hs label="Not/not or not"}
both notGate >>> orGate >>> notGate
```


Exercise

: Construct the function table for @fig:notnotornot, and compare it to
  machines you've seen. What is it equivalent to? Why?


Hint

: Recall the equivalency from @fig:notnot.


Believe it or not, these three gates: `and`, `or`, and `not`, constitute the
fundamental building blocks of all digital computers. When people say that
computers are all just ones and zeroes, what they really mean is that computers
are just really big circuits that determine which outputs should be `on` or
`off`. We will encounter one more necessary construction --- the *tri-state
buffer* --- in @sec:buffers, but we'll cross that bridge when we come to it.


### Associativity

Let's return to my lamp, with its wall switch and pull chain switch. We
represented whether or not the light was on by taking the `and` of the two
switches. But these are not the only relevant factors; there is also a fuse or
breaker in the house which dictates whether electricity is even getting to the
outlet. Another `and` gate is necessary:

```{#fig:breaker design=code/Design.hs label="A fused system"}
unsafeReinterpret @((Named "Breaker" Bool, Named "Wall" Bool), Named "Switch" Bool) >>> first' andGate >>> andGate
```

The wire connecting the two `and` gates corresponds to whether the outlet has
power to give to the lamp. If it doesn't, the switch on the lamp itself is moot
--- even if the lamp wanted power, it's not going to get any, and thus the lamp
is off regardless. This is a perfectly reasonable interpretation of the system.
But equally reasonable is an alternative diagram:

```{#fig:breaker2 design=code/Design.hs label="Another fused system"}
unsafeReinterpret @(Named "Breaker" Bool, (Named "Wall" Bool, Named "Switch" Bool)) >>> second' andGate >>> andGate
```

Here, the order of the `and` gates has been swapped. While @fig:breaker better
describes the physical flow of electricity, @fig:breaker2 more closely tracks
how we'd go about troubleshooting the lamp being off. If the lamp is off, we'd
first check that both switches are on, and only then would we check the breaker
box.

We are left with a dilemma: both @fig:breaker and @fig:breaker2 are reasonable
descriptions of the problem --- but which one is *correct*? Careful construction
of the function tables for each shows that it *doesn't matter* which we choose.
Both function tables look like this:

```{#fig:breaker_truth design=code/Design.hs fn=truth label="A fused system"}
unsafeReinterpret @((Named "Breaker" Bool, Named "Wall" Bool), Named "Switch" Bool) >>> first' andGate >>> andGate
```

Appealing to the function tables is certainly a strong *logical argument,* but
machines quickly become too complicated to reason about by building their
tables. Instead, we can also reason about this circuit *intuitively.* Remember
that machines are made in order to be meaningful to humans, so it is desirable
for us to understand systems like these without relying on rote analysis. Asking
whether my lamp currently emits light is the same as asking whether every
relevant switch is `on`.  It doesn't matter in which order you look, or how you
divvy up the work; either all switches are `on`, or at least one is `off`.

This idea that the exact divvying-up doesn't matter should be familiar to you
from primary school. We are all fine tallying up our restaurant receipt as $5.50
+ 12.12 + 8.47$ without worrying about the minutiae of where exactly the
parentheses should go (is it $(5.50 + 12.12) + 8.47$ or $5.50 + (12.12 +
8.47)$?)

Similarly, we could write out our circuit as `switch & wall & breaker` without
worrying exactly where the parentheses should go. This property is called
*associativity.*

But this technique doesn't work for all machines. The vast majority we encounter
will *not* be associative, so care must be taken. There is an analogous
situation in arithmetic; while addition is associative, subtraction is most
certainly not. Try it for yourself!

Because `and` gates are associative, we can play a little fast-and-loose with
our notation. Often, it's convenient to assume an `and` gate can take an
arbitrary number of input wires. This isn't true when you're physically
assembling a circuit, but because of associativity, the exact details of the
layout can't matter, and so the meaning is unambiguous to any technician in the
field.

```{#fig:bigand design=code/Design.hs label="Notational convenience"}
unsafeReinterpret @((Named "Breaker" Bool, Named "Wall" Bool), Named "Switch" Bool) >>> bigAndGate
```

The machine in @fig:bigand is completely identical to that in @fig:breaker (and
@fig:breaker2.) The darker wire labeled `/3/` indicates that it is actually just
three wires drawn together (but remaining separate in reality.) As you will see,
it is often very convenient to move large collections of wires around, because
our machines are quickly going to become very, *very* large.

