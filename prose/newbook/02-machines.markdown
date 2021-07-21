## Machines

To start off our journey into the depths of abstraction, we first need to learn
a few fundamentals. *Literally* everything we build in the remainder of the book
will be nothing more than interesting combinations of these few building blocks.
It will probably feel slow and obvious, but it's an unfortunate fact of
difficult things (and of life) that we must walk before we can run.


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

which is just the table for #fig:not repeated twice. Some care need be taken
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

