In the previous chapter, we introduced the general idea of machines, and a
specific example of one: the not gate. Recall that machines operate
deterministically on their inputs to produce an output, as described by their
*function table*.

Today we will take a look at some more complicated machines, how we can combine
them to create more sophisticated machinery, and what happens if we look inside
of them.



## The Formidable "And" Gate

The first machine we will look at today is the **and gate**: a machine of two
inputs and one output. It's function table looks like this:

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 0       | 0       | 0      |
| 0       | 1       | 0      |
| 1       | 1       | 1      |
| 1       | 0       | 0      |

Studying this table for a second should give you a hint about why it's called
the "and" gate: it is on only when input `A` **and** input `B` are *both* on.

You might be wondering about the seemingly strange order I've listed my inputs
in the function table above (most people would think the 1/1/1 row would be
last!). We'll dive into the details later, but for now you'll have to trust me
when I say it makes patterns come out more easily.

So what does an `and` gate look like? Well, it looks like this:

``` {#and_gate}
circuit = hsep 0.1 [ wireLabel "A" === svspacer === wireLabel "B"
                   , andGate undefined ||| inputWire ||| wireLabel "out"]
```

This shape seems somewhat arbitrary, but it's pretty easy to remember because it
sort of looks likes like the "D" in the word "AND".


## Connecting Machines

Remember when I said that wires were only good for connecting machines together?
That implies that connecting machines together is important, but we haven't done
that yet! Let's see what happens when we stick a `not` gate after an `and` gate.

``` {#and_not}
circuit = labeled "" $ runCircuit $ void $ do
  and <- liftDia andGate
  not <- liftDia notGate
  spaceH 0.5 and not
  arr (and, (Out 0)) (not, (In 0))
  afterwards (||| inputWire)
```

We've drawn the big square box around our combination of gates to highlight the
fact that this thing we've built out of machines *is itself a machine*. Imagine
that box were solid black, and we couldn't look at what's inside of it. All we'd
know is that it took two inputs on the left, and produced one output on the
right. That's exactly what we said a machine was!

This is indeed a remarkable fact -- that we can combine any two machines
together in any way we want, and no matter what, we're guaranteed to get a new
machine for our efforts. The fancy word for connecting machines like this is
**to compose them**. Because composing any two machines itself leads to a
new machine, we say that **machines are closed under composition**.

> Takeaway: Machines are closed under composition. That means, whenever you have
> two machines, you can compose them together and get a new machine out of the
> deal.

We've now composed our `and` gate with our `not` gate, and because machines are
closed under composition, we must have a new machine to play with. It will be
informative to look at its function table.

But how do we know what its function table must be? Well, because we know what
the function table for an `and` gate is, let's start with that. I'll repeat it
here, if you've forgotten:

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 0       | 0       | 0      |
| 0       | 1       | 0      |
| 1       | 1       | 1      |
| 1       | 0       | 0      |

But, our new machine isn't an `and` gate! It's an `and` gate with a `not` gate
after it. So we need to somehow compose (combine) our function tables together,
too! This turns out to be really simple.

For every *output* in our `and` gate, we use that as the *input* to our `not`
gate, and look up the corresponding transformation in the function table. In
this case, we already know that `not` simply flips a wire's value, so all of our
1s become 0s, and vice versa.

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 0       | 0       | ~~0~~ &rarr; 1 |
| 0       | 1       | ~~0~~ &rarr; 1 |
| 1       | 1       | ~~1~~ &rarr; 0 |
| 1       | 0       | ~~0~~ &rarr; 1 |

Great! So we've determined the function table for our new machine. What happens
if we add another not gate -- this time right before input `A`?

``` {#not_and_not}
circuit = labeled "" $ runCircuit $ void $ do
  notA <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) notGate
  and <- liftDia andGate
  and1i <- getPort and (In 1)
  not <- liftDia $ fmap (||| inputWire) notGate
  notAi <- getPort notA (In 0)
  assertSame and (Out 0) not (In 0)
  assertSame notA (Out 0) and (In 0)
  c <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) bend
  cp <- getPort c Split

  leftOf cp and1i
  above notAi cp
  arr (c, Split) (and, In 1)
```

We must now work backwards to determine the new function table for this machine,
but otherwise we apply the same method as we did before.

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| ~~0~~  &rarr; 1 | 0       | 1 |
| ~~0~~  &rarr; 1 | 1       | 1 |
| ~~1~~  &rarr; 0 | 1       | 0 |
| ~~1~~  &rarr; 0 | 0       | 1 |

Cool! We can compose machines to the left of others, just like we could to the
right of them. But be careful when composing to the left, it's easier to get
yourself into a sticky situation. We'll discuss this more in the future when we
talk about positive and negative function positions.

But back to the task at hand. Let's do this composition stuff one more time,
this time adding a `not` gate before input `B`.

``` {#not_not_and_not}
circuit = labeled "" $ runCircuit $ void $ do
  notA <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) notGate
  notB <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) notGate
  and <- liftDia andGate
  not <- liftDia $ fmap (||| inputWire) notGate
  assertSame and (Out 0) not (In 0)
  assertSame notA (Out 0) and (In 0)
  assertSame notB (Out 0) and (In 1)
```

As before, we update our function table:

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 1 | ~~0~~  &rarr; 1 | 1 |
| 1 | ~~1~~  &rarr; 0 | 1 |
| 0 | ~~1~~  &rarr; 0 | 0 |
| 0 | ~~0~~  &rarr; 1 | 1 |

After reshuffling back into the same order of inputs as we used for our `and`
gate, we get this particularly interesting function table:

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 0       | 0       | 0      |
| 0       | 1       | 1      |
| 1       | 1       | 1      |
| 1       | 0       | 1      |

Take a moment to look at the relationship between inputs and outputs for this
machine. This machine produces an output of 1 when either input `A` **or** input
`B` is 1 (or both!).

As you might guess, this new machine of ours is the reverential `or` gate. It
too, is so popular, that it gets a special symbol:

``` {#or_gate}
circuit = orGate undefined ||| inputWire
```


## De Morgan's Laws

Most books on this stuff present the `or` gate as being "fundamental", in the
sense that it is a necessary tool in our toolbox for constructing more
heavy-duty machines. But as we have just seen, `or` isn't fundamental at all! We
just built it out of `and` and `not`, so clearly those must be "more
fundamental".

We'll investigate that claim further in just a second. But first, a puzzle. What
happens if we put `not` gates all around our `or` gate, like we did earlier with
`and`? Let's take a look at the function table for this diagram:

``` {#not_not_or_not}
circuit = labeled "" $ runCircuit $ void $ do
  notA <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) notGate
  notB <- liftDia $ fmap (\x -> (inputWire ||| x) # scale 0.5) notGate
  or <- liftDia orGate
  not <- liftDia $ fmap (||| inputWire) notGate
  assertSame or (Out 0) not (In 0)
  assertSame notA (Out 0) or (In 0)
  assertSame notB (Out 0) or (In 1)
```

If we do some work to compute our new function table, we get this:

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| ~~1~~ &rarr; 0       | ~~1~~ &rarr; 0       | ~~1~~ &rarr; 0      |
| ~~1~~ &rarr; 0       | ~~0~~ &rarr; 1       | ~~1~~ &rarr; 0      |
| ~~0~~ &rarr; 1       | ~~0~~ &rarr; 1       | ~~0~~ &rarr; 1      |
| ~~0~~ &rarr; 1       | ~~1~~ &rarr; 0       | ~~1~~ &rarr; 0      |

... which looks oddly familiar. Hey! This is just the function for `and`! What
gives? We just managed to build an `and` gate by composing some `not`s and an
`or` gate -- just like how we originally built the `or` gate.

Spooky.

This fact was originally discovered by a man named August de Morgan in the
mid-1800s. In his honor, we call these negation-of-and-is-just-or and its
partner **De Morgan's laws**.

De Morgan's laws state that whenever we have an `and` gate, we can turn it into
an `or` gate by wrapping it up in `not` gates. And vice versa. Performing this
transformation is sometimes described as doing it "by de Morgan", as in "we can
get this diagram out of that one *by de Morgan*."



## Universality

Returning to my original point about `and` being more "fundamental" than `or`,
well, we know now that that is simply untrue! `and` and `or` are somehow
"equally fundamental", in the sense that we can always produce one out of the
other *by de Morgan*. Assuming we have some `not` gates lying around, obviously.

So if it isn't `and` that's fundamental, maybe it's `not`? A few minutes of
pondering on this implies it can't be so -- because `not` gates only have one
input and one output, the only thing we can do is chain them together, and
whenever we chain two together, they cancel one another out.

It must be some combination of `and` and `not` (or, equivalently, `or` and
`not`) that result in this "universality" we're looking for. And indeed, it is.
We've actually already looked at it in this chapter.

``` {#and_not_labeled}
circuit = labeled "Nand" $ runCircuit $ void $ do
  and <- liftDia andGate
  not <- liftDia notGate
  spaceH 0.5 and not
  arr (and, (Out 0)) (not, (In 0))
  afterwards (||| inputWire)
```

You're probably not convinced that this is indeed the one, true, fundamental
machine, from which all others can be built, but it really is! It's called the
`nand` gate, which, as Wikipedia would call it, is a *portmandeau* of `not` and
`and`.

Of course, since this gate is *extra* popular, it too has a special symbol.

``` {#nand_gate}
circuit = nandGate undefined ||| inputWire
```

If you squint, it's kind of like a combination of the `and` and `not` symbols.

But is this thing truly fundamental? It really is; I'm not pulling your leg.
Here, let's use it to construct a `not` gate:

``` {#not_from_nand}
circuit = labeled "Not" $ runCircuit $ void $ do
  c <- liftDia con
  nand1 <- liftDia $ fmap (||| inputWire) nandGate
  arr (nand1, (In 0)) (nand1, (In 1))
  liftCircuit $ constrainWith hcat [ c, nand1 ]
  afterwards (inputWire |||)
```

"What is this sorcery?" you might be asking? Well, we're connecting the *one*
input wire to our machine to *both* inputs of the `nand` gate. Remember our rule
about wires -- they have the same value all the way across them. So we know that
both inputs to our `nand` gate must have the same value.

Looking at the function table for our `nand` gate, that means we can cross off
any rows in which input `A` and input `B` are not the same.

| Input A | Input B | Output |
|:-------:|:-------:|:------:|
| 0       | 0       | 1      |
| ~~0~~   | ~~1~~   | ~~1~~  |
| 1       | 1       | 0      |
| ~~1~~   | ~~0~~   | ~~1~~  |

And furthermore, since we know that `A` and `B` must be the same always, we
don't need two columns for them.

| Input | Output |
|:-----:|:------:|
| 0     | 1      |
| 1     | 0      |

And voila! We've created a `not` gate out of `nand`s. Since we know that a
`nand` is just an `and` with a `not` after it, and that two `not`s in a row
cancel, getting from here to an `and` gate isn't actually very hard at all:

``` {#and_from_nand}
circuit = labeled "" $ runCircuit $ void $ do
  c <- liftDia con
  nand1 <- liftDia $ fmap (||| inputWire) nandGate
  nand2 <- liftDia nandGate
  arr (nand2, (In 0)) (nand2, (In 1))
  liftCircuit $ constrainWith hcat [ nand1, c, nand2 ]
  afterwards (||| inputWire)
```

If you're still feeling dubious, feel free to derive the function table for this
diagram. But the reason (and arguably, the beauty) behind why we use diagrams is
that they're easy to visualize, and with some practice, you can begin to see how
values must flow through these machines. It takes a little bit of work, but it's
going to save you a huge amount of time to not need to derive a function table
for every diagram you see.

There is a question yet that remains unanswered: *why* is a fundamental `nand`
gate "better" than just using `and`s and `not`s? It's a good one, and one that
we'll explore further next chapter.

---

## Exercises

1) Build an `or` gate entirely out of `nand` gates. Derive a function table for
   it, to really convince yourself that you you got it right.
2) What would a `nor` gate look like? Write its function table, and take a guess
   at what its symbol would be.
3) Would a `nor` gate *also* be universal? Why or why not? If so, build a `nand`
   gate out of them. If not, explain why.

