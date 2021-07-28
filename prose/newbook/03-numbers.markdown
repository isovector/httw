## Numbers

In the previous chapters, we discussed how to break down what we had into more
fundamental parts. Such an approach has an obvious due-date attached to it; once
you get to the fundamental building blocks, there's nowhere left to go in that
direction.

And so in this chapter, we turn our attention around, and look at how we can
build more complicated things out of our simpler pieces.

Wires, as we've defined them, allow us to carry a single piece of information --
whether they're on or whether they're off. While that's great and all, lots of
things we as humans care about are more complicated than yes-or-no. The good
news, is that yes-and-no is all we need in order to build arbitrarily
interesting things, if we're clever in how we go about it.

But we must walk before we can run. And so we will turn our attention to
construct the second-most-simple thing we can -- numbers. We do this not only
because it will be useful, but also as an exercise in deconstructing things into
their constituent parts.


### A Short History of Numbers

It might not seem like it, today, but numbers were quite a radical invention. Of
course, the prehistoric idea of counting is of great importance, but that's not
exactly what we mean by the invention of numbers. The subtle marvel at work here
is in the way we *represent* numbers.

Consider the number 16. We have many different but equivalent ways of
representing it; as a word: "sixteen;" as you might when tallying points in a
card game:  [16](tally); how you might engrave it in Braille: [16](braille); and how we could
write it in Roman numerals: XVI. These are all competing pieces of technology,
much in the way that consumer electronics still come with a multitude of
cables.

Depending on the use, certain representations of numbers are better than others.
While any grade-school child can add $537+341$ in a matter of seconds, it's much
harder to add "five hundred and thirty seven" to "three hundred and forty one"
--- let alone DXXXVII plus CCCXLI. But it's much faster to change [4](tally) to
[5](tally) than it is to change 4 to 5. MCMXCIX is much less scrutable (and thus
carries more prestige) than 1999. And it's much easier to read "eighty" aloud
than [80](braille), while the latter is much more compact and easy to emboss.

In short, different representations of numbers are useful for different things.

Of these systems, it will be particularly informative to contrast tallying,
Roman numerals, and our everyday Arabic numerals.

Tallying was likely the first form of counting; every time you count another
one, you add another tally. From this lens, tallying is a lot like representing
the number 4 as $1+1+1+1$, except tat the plus signs are invisible. Indeed, the
fact that both the Roman and Arabic systems represent one vertical scratches (I
and 1 respectively,) suggests that they both have tallying as a common ancestor.
Because there's only one symbol, tallying is called a *unary* number system.

Under a tally representation, the only thing that's important is the number of
tallies. There is no terse notation for bigger numbers: writing out 50 is ten
times more work than writing out 5 --- and requires ten times as much space.  An
interesting property of tallying is it makes basic arithmetic easy to perform.
For example that two numbers can be added by putting their respective tallies
beside one another: $[5](tally) + [5](tally) = [10](tally)$. And multiplication
can be performed by copying a number once for every tally in a second number.

The system of Roman numerals addresses the problem of difficult-to-represent
large numbers by adding new symbols. Rather than requiring its scribes to write
out [10](tally), the Roman system just added a new symbol [10](roman). And
instead of writing out five of those, you could instead write [50](roman). The
biggest symbol they had was [1000](roman), which you must admit is significantly
terser --- both to read and write --- than one thousand tally marks.

Where the Roman system faltered, however, was that it made arithmetic very
difficult. The new symbols were not evenly spaced along the number line, which
lead to similar numbers having dramatically different representations.
[1999](roman) is only one less than [2000](roman). The rules for deciphering
Roman numerals into the number they represent is rather complicated, and
interestingly, this is not due to a lack of familiarity on the part of modern
people. The Romans themselves had just as hard of a time!  There's a lot of
computation required to figure out what's hidden in that jumble of letters.

Also, by the modern rules of Roman numerals, there is no commonly-accepted way
to express numbers bigger than [3999](roman).

As a result of these notational inconsistencies, doing arithmetic with Roman
numerals is extremely challenging. There is no equivalent system to grade-school
"long addition" for Roman numerals. It's likely that Roman accountants and other
numerically-inclined persons had long tables for addition, subtraction and
multiplication, similar to modern day "times tables," but much, much larger.
Doing the math by hand was simply too difficult.

Having looked at alternative systems, it is now much easier to appreciate the
technological improvements made by our familiar Arabic numerals. Like the Roman
system, Arabic numerals have different symbols for different numbers. But
unlike the Roman system, these numbers are evenly spaced.

Arabic numerals are a base-ten, or *decimal,* system. This means that we have ten
distinct digits: 0, 1, 2, 3, 4, 5, 6, 7, 8, and 9. Counting (implicitly) begins
at 0, and for each tally, we change our current digit to the next one. When we
run out of unique digits, we go back to 0, incrementing the digit to the left
instead.

Rather interestingly, even though it's a base-ten system, there is no distinct
digit for ten. The symbol for ten is in fact a composition of the digits 1 and
0. This is the primary technological invention of Arabic numerals, that, unlike
in tallying, we can use the relative positions of digits to encode information.
For example, the number 20 is ten times bigger than the number 02, even though
they consist of the same digits.

This system makes arithmetic much simpler than under Roman numerals. A school
child need only memorize their one-digit addition and multiplication tables. The
rules they learn work for not only the ones' column, but also the tens',
hundreds', and even trillions'.

It's important to recognize that this is a *human invention,* not some intrinsic
properly of numbers themselves. There is no deep reason to prefer base-10 over
base-9, or base-60 --- except maybe as a limit of the human brain's ability to
memorize big multiplication tables. In short, basing our numbers around ten is
an *arbitrary decision.*

The addition and multiplication tables that are drilled into grade-school
students are *exercises in symbol manipulation,* not in *mathematics.* There is
a distinction to be drawn between the fact that adding two apples to three
applies results in five apples, and the fact that $2+3=5$. When someone asks us
to compute $6 \times 7$ in our heads, we're more likely to be *looking up the
answer* in our mental multiplication table, than we are to be thinking about the
area of a rectangle with side lengths 6 and 7, or whatever. Nobody is *actually
multiplying* in their head, they just memorized what the answer should be.

While arithmetic is based on deep mathematical ideas, we (thankfully) do not
engage with those deep ideas in our day to day lives. Instead, we rely primarily
on pattern matching and symbol manipulation.

The important takeaway here is that when we do arithmetic, we do it via
manipulating symbols according to some rules we memorized a long time ago. It's
all just moving symbols around according to rules; there is no deep
understanding required.

*No deep understanding required* seems like the sort of thing we could teach to
a machine.


### Encoding Binary Numbers

While in our everyday decimal system we have ten distinct digits, in our machine
diagrams we have only two: `off` and `on`. Thus, we will build a *binary* number
system. Before we considered `on` to correspond to the flow of electricity, but
this isn't an important property of our machines. What's important is that `off`
and `on` are distinct from one another, in exactly the same way that 0 and 1 are
distinct. Accordingly, we can represent the number zero via `off`, and the
number one via `on`.

Recall that any decimal number, like 6258, can be represented as a sum of its
decimal places:

$$
6258 = 6000 + 200 + 50 + 8
$$

which we can then write out in an alternative form:

$$
6258 = (6 \times 10^3) + (2 \times 10^2) + (5 \times 10^1) + (8 \times 10^0)
$$

If you don't like math and the exponents make you unhappy, don't fret, we're not
going to do much more with them. Intuitively, this says that the rightmost
symbol describes how many *ones* your number has; the next-to-rightmost
describes how many *tens* your number has. And so on, until you run out of
symbols.

This same technique works in binary. As a shorthand, we can write [13](binary)
to correspond to the sequence `on on off on`, and decompose this number in the
same way:

$$
[13](binary) = (1 \times 2^3) + (1 \times 2^2) + (0 \times 2^1) + (1 \times 2^0)
$$

which can be simplified to:

$$
[13](binary) = 8 + 4 + 1 = 13
$$

> TODO(sandy): describe how we will use WORDS to talk about numbers themselves,
> and the digits for the encoding in this section

Thus, the binary number [13](binary) is an encoding of the number thirteen,
just like the decimal number 13 is. It's exactly the same number, just in
different pants.

Going the other direction --- encoding a decimal number as a binary one ---
requires a little more calculation, but is fundamentally the same process. The
idea is to find the largest power-of-two that is smaller than your current
number, and subtract it. Add a 1 to the beginning of the binary encoding. Then,
for each subsequently smaller power of two, see if it can be subtracted from
your running total. If so, subtract it and push a 1 onto your binary encoding;
if not, just attach a 0. Repeat until you've run out of powers of two.

To illustrate this, consider the binary encoding of 49. The largest power of two
smaller than this is 32 --- thus there should be a 1 in the "thirty-seconds'
place" of our binary-encoded number. We subtract 32 from 49, and continue:

$$
49 - 32 = 17
$$

The greatest power of two less than 32 is 16, which can also be subtracted from
17. Thus, there is also a 1 in the "sixteenths' place:"

$$
17 - 16 = 1
$$

Iterating through the remaining powers of two, we find 8, then 4, then 2. Each
of these is bigger than our running total of 1, so nothing happens, and we put a
0 in the eights', fourths' and twos' place of our binary number.

Finally, we get to the ones' column[^power-of-two], and we can subtract it from
the total, so our last place is a 1.

[^power-of-two]: One, perhaps surprisingly, is a power of two: $2^0 = 1$

There is no more work to do. Our binary-encoded number can now be read off as
[49](binary).


### The Symbol Shuffling of Addition

As mentioned in the previous section, the importance of the Arabic numeral
system is how easy it makes arithmetic. Consider the calculation of
$2477+3426$. There is a grade-school algorithm for performing this sum which you
are certainly familiar with. Starting from the rightmost column, add the two
digits, and write down the result in the bottom row. If the number was bigger
than ten, write only its ones' column in the bottom row, and "carry" the
tens' digit to the next column. Move left a column, and repeat until there are
no more columns.

The table resulting from such a calculation might look like this, where the row
labeled `C` is the "carry" row.

|   |   |   |   |   |
|---|---|---|---|---|
| C |   | 1 | 1 |   |
|   | 2 | 4 | 7 | 7 |
| + | 3 | 4 | 2 | 6 |
| = | 5 | 9 | 0 | 3 |

The power of this technique is that breaks down a hard problem (add two four
digit numbers together) into several easier problems (add two one digit numbers
together $\times$ 4.) By compelling children to memorize their one-digit
addition tables, each of the easier sub-problems becomes trivial. The result:
given sufficient time and motivation, arbitrarily large numbers can be added "by
hand" with little probability of error.

Addition in this manner is an example of a *divide and conquer* algorithm.
Rather than solve a hard problem directly, instead, break it down into easier
problems, solve those, and then combine their answers. We will see divide and
conquer solutions time and time again, as they are a fantastic way of dealing
with highly-complicated problems.

As you might expect, the grade-school addition technique works just fine for
binary numbers as well as it does for decimal numbers. The one-digit addition
table looks like this:

|   |   |   |
|---|---|---|
| + | 0 | 1 |
| 0 | 0 | 1 |
| 1 | 1 | 10 |

where the only case in which we have a carry is $[1](binary) + [1](binary) =
[2](binary)$. For example:

|   |   |   |   |    |
|---|---|---|---|----|
| C |   | 1 | 1 |    |
|   | 1 | 0 | 0 | [1](binary) |
| + | 0 | 0 | 1 | [1](binary) |
| = | 1 | 1 | 0 | [0](binary) |

Thus, $[9](binary) + [3](binary) = [12](binary)$. Take a moment to decode all
three numbers into their decimal form to confirm that the sum is correct. This
result should be particularly amazing --- you just added two numbers *purely
symbolically,* without having any idea what numbers they *actually were!* And
so we can see that this algorithm correctly adds two numbers, without having any
understanding of "what" "numbers" "are." We merely shuffled symbols around
according to some fixed rules.

Pay close attention to the second and third column in the above table --- the
columns with "carries." In those columns, we actually needed to add *three*
one-digit numbers --- the two digits from the numbers we're adding, and one
digit from the carry of the previous column. In columns one and four, the carry
was implicitly 0.

Having worked through the addition algorithm thoroughly, we're now ready to
build a machine capable of performing sums.


### The Full Adder

We'd like to build a machine that can perform automatic addition, by mindlessly
shuffling symbols like we did in the previous section. Constructing such a
machine is equivalent to "translating" the grade-school addition algorithm into
the language of `and`, `or` and `not` gates.

Rather than tackle the full problem of addition in one go, let's first solve a
simpler problem and ignore the carry-in, assuming it will always be 0. This is a
circuit with a special name, the so-called "half adder." At a high level, it
looks like @fig:bb_halfadder.

```{#fig:bb_halfadder design=code/Design.hs label="Half Adder"}
let { c :: Circuit (Named "A" Bool, Named "B" Bool) (Named "A+B" Bool, Named "Cout" Bool); c = copy >>> (component "sum1" (fst' >>> unsafeReinterpret) *** component "carry1" (fst' >>> unsafeReinterpret)) } in c
```

All that's now is to determine what's inside the `sum1` and `carry1` boxes.
Carrying is easy. We are adding two one-digit numbers, and need to carry
whenever the twos' column has been filled. This is only the case when `A` and
`B` are both [1](binary), which is to say `on`. Thus, `carry1` is just an `and`
gate.

```{#fig:carry1 design=code/Design.hs label="carry1"}
andGate
```

Building `sum1` is a little harder. The ones' digit should be set when exactly
one of the inputs is `on`. Take a moment to convince yourself of the truth of
this; if both are `off`, then the result should also be off. When they're both
`on`, the result is 2, which in binary is [2](binary), and again the ones'
column is 0.

So `sum1` is `on` when exactly one of the inputs is `on`. Which means it's on
whenever (`A` is on *and* `B` is not) *or* whenever (`A` is off *and* `B` is
on). Expressed in this way, constructing the circuit is straightforward as in
@fig:sum1.

```{#fig:sum1 design=code/Design.hs label="sum1"}
copy >>> first' notGate *** second' notGate >>> both andGate >>> orGate
```

A different way of thinking about `sum1` is that it's `on` whenever its inputs
are different from one another. This happens to be a common problem in
circuitry, and as such, `sum1` has its own name and symbol. It's more commonly
called a `xor` gate, which is short for "exclusive or" --- meaning one, or the
other, *but not both.* It is this final condition that differentiates `xor` from
`or`, which will happily accept both inputs being `on`. We draw `xor` with the
unique symbol shown in @fig:xor.

```{#fig:xor design=code/Design.hs label="xor"}
xorGate
```

Putting everything together, we can now give the full circuit for `half adder`
as in @fig:halfadder.

```{#fig:halfadder design=code/Design.hs label="Half Adder"}
let { c :: Circuit (Named "A" Bool, Named "B" Bool) (Named "A+B" Bool, Named "Cout" Bool); c = unsafeReinterpret >>> copy >>> xorGate *** andGate >>> unsafeReinterpret} in c
```

As you can see, circuits which feel overwhelming can always be broken into
smaller parts, solved independently, and recombined. The result is often
surprisingly elegant, like in @fig:halfadder. It's the *divide and conquer*
principle at work again.

Of course, what we really want is a *full adder* --- one capable of dealing with
carry-in inputs as well. At a high-level, the design looks like
@fig:bb_fulladder, which is analogous to the half adder, but has an additional
carry-in input.

```{#fig:bb_fulladder design=code/Design.hs label="Full Adder"}
let { c :: Circuit (Named "A" Bool, (Named "B" Bool, Named "Cin" Bool)) (Named "A+B" Bool, Named "Cout" Bool); c = copy >>> (component "sum" (serial >>> unconsC >>> fst' >>> unsafeReinterpret) *** component "carry" (serial >>> unconsC >>> fst' >>> unsafeReinterpret)) } in c
```

In the full adder, `sum` is an easier sub-circuit to build, so let's start
there. Like in the half adder, we want to output a 1 whenever an odd number of
inputs are `on`. But this is equivalent to adding two of them via `sum1`, and
then adding the third to the result of that. And so `sum` is just two `xor`
gates to combine the three inputs.

```{#fig:sum design=code/Design.hs label="carry"}
sum
```

Determining when to carry is a little harder. We need to carry whenever two or
three of the inputs are `on`. If we take every pair of input wires (`A/B`,
`A/Cin` and `B/Cin`), we can run each pair through an `and` --- this will tell
us if both were `on`. If any one of these `and` gates was `on`, the result of
`cout` should also be `on` --- so we can combine them together with `or` gates.

```{#fig:cout design=code/Design.hs label="carry"}
cout
```


### Endianness and the Ripple Carry Adder  {#sec:adder}

> TODO(sandy): no discussion of endianness

The full adder machine corresponds to adding together a single column of binary
digits. We can build a fully-operational, multiple-digit adding machine simply
by connecting several full adders together. The trick is to chain one machine's
`Cout` output to the next's `Cin` input. For the first `Cin` in the chain, we
connect a wire that is always `off`.

In the resulting diagram, this leads to a cascade of full adders, where the
carry "ripples" from one machine to the next. Thus, the machine in @fig:ripple4
is called the *ripple carry adder.*

```{#fig:ripple4 design=code/Design.hs depth=1 label="Ripple Carry Adder"}
unsafeReinterpret @(Named "A" Word4, Named "B" Word4) >>> addN @Word4 >>> unsafeReinterpret @_ @(Named "A+B" Word4, Named "Cout" Bool)
```

Recall from @fig:bigand the notational convenience of the thick vertical bar,
called a *split.* In the `A` case, to the left of this split is a wire labeled
`/4/`, and to the right are four wires, numbered 0 through 3. The `/4/` wire
called a *ribbon* or a *bundle* of wires, and it is a short-hand form of saying
"here are four wires, all moving together."

The split exists to separate this bundle for when we eventually do want to
connect the individual wires of the ribbon to different places. To the right of
the split are four wires, each individually labeled.  In the case of
@fig:ripple4, the wire labeled `0` is connected to the `A` input of the first
`full adder` machine, while the wire labeled `1` is connected to the second
`full adder`.

Similarly, there is a thick vertical bar on the output `A+B`. This one is called a
*join,* because it takes separate wires and re-bundles them.

In @fig:ripple4, the inputs `A`, `B` and the output `A+B` are all ribbons of
four wires. The size of this ribbon is known as a computer's *word size,* and
thus in @fig:ripple4 we have a word size of four. Why this number? No reason ---
it was chosen arbitrarily. It's big enough to see the "carry cascade," but still
small enough to comfortably fit on a page.  However, in a real machine, there
are pragmatic considerations for the size of your wire bundles.

While there is an infinite amount of numbers, there is a biggest number that you
can write on any particular sheet of paper --- namely, as many 9s as can fit in
your smallest, most-precise handwriting. If you want to a bigger number, you'll
need to go buy a bigger sheet of paper.

The ribbon sizes of the full adder analogously limit the biggest number that can
be represented. Recall that each wire is either `on` or `off`, which we
interpret as [1](binary) or [0](binary). By placing these wires beside one
another, it is as if we are using each as a digits' column. So, with a ribbon of
four wires, the biggest number we can encode is $[15](binary) = 15 = 2^4 - 1$.
Why is that $- 1$ there? Because we need to be able to encode 0 as well! So
there are indeed $2^4$ different numbers we can encode, with the largest being
$2^4 - 1$.

We can choose a word size arbitrarily, but it comes with a trade-off. By
choosing a larger word size, we are able to represent significantly bigger
numbers, at the cost of, well, cost. Because we need to build a `full adder` for
each wire in the word, the cost of manufacturing this circuit will go up as the
word size does, as will the physical size of the circuit. Today's computers have
a word size of 64, which is why they are called "64-bit computers."

> TODO(sandy): what's a bit?

The split and join bars are essential for understanding any machine with a large
word size. Conceptually, the word is a single conceptual entity, made up of
several wires --- much like how we usually think of our hand as a single whole,
rather than as a thumb and four fingers. That's not to say we *never* think
about our fingers, but rather that we need only do it when the concept of "hand"
isn't sufficiently precise.

To *sum* up this section, we can introduce a new symbol for working with ripple
carry adders of any word size. You will find yourself familiar with the
iconography in @fig:ripple4_sym, which is a desirable feature when designing
large systems.

```{#fig:ripple4_sym design=code/Design.hs label="Ripple Carry Adder"}
unsafeReinterpret @(Named "A" Word4, Named "B" Word4) >>> addN @Word4 >>> unsafeReinterpret @_ @(Named "A+B" Word4, Named "Cout" Bool)
```


### Negative Numbers and Subtraction

A large portion of computer science is learning how to *represent* data out of
bits. In this chapter we have looked at how to represent positive integers by
treating a sequence of bits as digits of a binary number. As we have seen, we
can consider each bit of `on` or `off` as a 1 or a 0.

But another way of thinking about bits is as a choice between two options. In
the case of numbers, that choice was between 0 or 1. But perhaps in the context
of a Robert Frost poem, `off` might mean to follow one path, while `on` would be
to take the road less traveled by. Of particular importance to this section, we can
choose `off` to mean *a positive number[^or-zero],* and `on` to mean *a negative
number.*

[^or-zero]: Or zero.

The most obvious way to be able to encode negative numbers alongside positive
ones is to attach an extra wire, whose job it is to track whether or not the
number is negative. For example, [19](1s) would encode 19, while [-19](1s) would
encode $-19$. This system works, except that it has a fatal flaw --- namely,
that there are two different ways of encoding one particular number (what is
it?) As a result, not only do we "waste" one of the precious numbers that are
representable by our word size, we also run into mathematical difficulties.

A good encoding of negative numbers would observe the grade-school equality that
$a - b = a + (-b)$ --- namely, that we can perform subtraction by adding a
negative. This is desirable, because we went through all the effort of building
an adding machine in @sec:adder. It would be nice to be able to reuse it.

The trick here is to realize that, given a word size of $n$, the largest number
we can represent is $2^{n - 1}$. For example, with a word length of 4, the
biggest number we can represent is $[15](binary) = 2^4 - 1 = 15$. If we add
one to this, we get $[16](binary)$, but there is no wire to put the 1 digit on!
And so it silently gets carried away, leaving us with [0](binary-4). This is
known as an *overflow,* and occurs when the result of a sum is bigger than our
word size can hold.

Overflowing is how time works on an analog clock. If the clock reads 11, an hour
later it will read 12, and an hour after it will read 1. The clock has
overflowed!

We can exploit overflow in order to make subtraction work. Think about what
happens when you add 9 to a one digit number:

| | +9 | After Overflow |
|:-:|:-:|:--|
| 0 | 9 | 9 |
| 1 | 10 | 0 |
| 2 | 11 | 1 |
| 3 | 12 | 2 |
| 4 | 13 | 3 |
| 5 | 14 | 4 |
| 6 | 15 | 5 |
| 7 | 16 | 6 |
| 8 | 17 | 7 |
| 9 | 18 | 8 |

Comparing the first column to the third, it's clear that, after overflow, adding
9 is equivalent to subtracting 1. A similar pattern occurs when we add 8, except
that this time it's the same as subtracting 2:

| | +8 | After Overflow |
|:-:|:-:|:--|
| 0 | 8 | 8 |
| 1 | 9 | 9 |
| 2 | 10 | 0 |
| 3 | 11 | 1 |
| 4 | 12 | 2 |
| 5 | 13 | 3 |
| 6 | 14 | 4 |
| 7 | 15 | 5 |
| 8 | 16 | 6 |
| 9 | 17 | 7 |

If we only had ten digits to play with, we could assign a mapping between our
digits and numbers that looks like this:

| A | B |
|:-:|:-:|
| 0 | 0 |
| 1 | 1 |
| 2 | 2 |
| 3 | 3 |
| 4 | 4 |
| 5 | -5 |
| 6 | -4 |
| 7 | -3 |
| 8 | -2 |
| 9 | -1 |

Here we've split half our digits into positive numbers, and half into negative
numbers. The positive digits map to the same numbers, but 5 maps to $-5$ and
they start counting back up from there. This system is peculiar at first blush,
but as we will see, corresponds closely with our notions of how addition and
subtraction should work.

The trick is now think about the B column, but do arithmetic in the A column.
For example, if we wanted to add $4 + (-2)$ we would consider those both to be
numbers in the B column. We could rewrite them as their numbers in the A column
($4 + 8$) and then add as usual. This gives us $12$, which then overflows to
$2$. This is still in the A column, so we move it back to the B column (although
it stays 2.) And voila, we've calculated that $4 + (-2) = 2$.

Of course, this doesn't come for free. There are a few nonsensical results in
the above system --- for example, $4 + 4 = -2$ and $(-5) + (-4) = 1$. These are
limitations of the technique known as *signed integer overflows,* and happen
when the result of an addition has the opposite sign of what you'd expect. The
bogus equations arise at the extremities of the numbers we can represent, and
are caused by numbers getting "too positive" or "too negative."

> TODO(sandy): something interesting to be said here about the infinitude of
> numbers and the finiteness of the universe. these constraints exist
> EVERYWHERE, but we aren't bothered by them because we never actually deal with
> anything big enough to care

Unfortunately, there is no solution to this problem. Because circuits are
physical devices with physical constraints, there will always be numbers so
extreme that they can't fit. However, this isn't a huge problem in practice;
numbers that humans care about are rarely bigger than 10,000, which fit
comfortably inside word sizes used by everyday computers.

We have simplified the problem of subtraction into a problem of addition of a
negative value. The high-level circuit is described by @fig:sub_bb --- all that
remains is to implement `negate`.

```{#fig:sub_bb design=code/Design.hs label="Computing A-B"}
unsafeReinterpret @(Named "A" Word4, Named "B" Word4) >>> second' (component "negate" id) >>> addN @Word4 >>> unsafeReinterpret @_ @(Named "A-B" Word4, Named "Cout" Bool)
```

So, what does a negative binary number look like? By convention, a binary number
with its largest bit set to 1 is considered negative. This forces --- like in
the base-10 example --- that the positive numbers come before negative ones.
And, analogous to how 9 acts as $-1$ in the base-10 example, the largest number
we can represent in our word length should also correspond to $-1$.

Thus, we must solve the following set of equations:

1. $negate [0](binary-4) = [0](binary-4)$
2. $negate [1](binary-4) = [15](binary-4)$

Equation 1 comes from the fact that $-0 = 0.

> TODO(sandy): interlude on bit negation here?

After some fiddling, we can see that "negate all the bits, and then add one" is
a satisfactory implementation of `negate`.

```{#fig:negate_naive design=code/Design.hs label="negate"}
bigNotGate >>> unsafeParse >>> intro 1 >>> addN @Word4 >>> fst'
```

While @fig:negate_naive certainly works, it's rather wasteful, requiring a
entire second `add` circuit simply to add one. But recall that all `full adder`
circuits have a "carry-in" input which corresponds to whether or not a one
should be added to the result. In @fig:ripple4_sym we tied the lowest `Cin` to
`off` --- but now we've discovered a use for it. Rather than using a second
`add` circuit inside of `negate`, we can repurpose the `Cin` input.

```{#fig:ripple2sub design=code/Design.hs depth=1 label="Ripple Carry Adder Subtractor"}
unsafeReinterpret @(Named "Sub" Bool, (Named "A" Word2, Named "B" Word2)) >>> addsubN @Word2 >>> unsafeReinterpret @_ @(Named "A+B" Word2, Named "Cout" Bool)
```

> TODO(sandy): tribuf appears here for the first time

```{#fig:ripple4sub_bb design=code/Design.hs label="Ripple Carry Adder Subtractor"}
unsafeReinterpret @(Named "Sub" Bool, (Named "A" Word4, Named "B" Word4)) >>> addsubN @Word4 >>> unsafeReinterpret @_ @(Named "A+B" Word4, Named "Cout" Bool)
```


