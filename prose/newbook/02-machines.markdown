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

```{design=code/Design.hs label="A Diagram"}
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

All that remains in the diagram is the triangle with a circle on its tip.
Anything in a diagram that isn't a port or a wire is called a *component,* and
components are where the interesting things happen. In our example, this
component corresponds to the action of flipping the switch. On the state of the
world represented by the wire on the left side the component, the switch hasn't
yet been flipped, and on its right wire, the switch has been flipped.

Take a moment to consider this next diagram, and attempt to decipher its
meaning.

```{#fig:notnot design=code/Design.hs label="Flipping Twice"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = unsafeReinterpret >>> notGate >>> notGate >>> unsafeReinterpret } in c
```

Once you've understood the first diagram, the meaning of this one should be
easy. Now, rather than flipping the light switch once, it's being flipped twice.
The resulting state of the light is the same as if the switch hadn't been
flipped at all. Remember, the length of the wires between components is
inconsequential, and the interpretation here should be that the switch is
flipped twice instantaneously. There is moment in which the light turns on
before turning back off.

Interestingly, because of this fact, we are justified in saying that flipping
the switch twice is equivalent to not having flipped it at all. And thus the
circuit described by @fig:notnot is equivalent to @fig:id:

> TODO(sandy): maps state of the world into equivalent states

```{#fig:id design=code/Design.hs label="No Flips"}
let { c :: Circuit (Named "Before" Bool) (Named "After" Bool); c = id >>> unsafeReinterpret } in c
```


