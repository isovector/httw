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


### The Symbol Shuffling of Addition

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

This same technique works in binary. As a shorthand, we can write [1101](binary)
to correspond to the sequence `on on off on`, and decompose this number in the
same way:

$$
[1101](binary) = (1 \times 2^3) + (1 \times 2^2) + (0 \times 2^1) + (1 \times 2^0)
$$

which can be simplified to:

$$
[1101](binary) = 8 + 4 + 1 = 13
$$

> TODO(sandy): describe how we will use WORDS to talk about numbers themselves,
> and the digits for the encoding in this section

Thus, the binary number [1101](binary) is an encoding of the number thirteen,
just like the decimal number 13 is. It's exactly the same number, just in
different pants.





## The Binary System

I'll bet you can see where we're going with all of this. Since a wire can be
either hot or cold, which can think of it as having two different symbols. If we
stack a bunch of wires beside one another, we can decide on a convention for
which one is the biggest, and then replace all of the "tens" in our argument
above with "twos".

The resulting system we get out of all of this work is known as the **binary
system**. I'm sure you've heard of it.

To really whet your whistle for these things, let's go through a few examples
for deciphering a number *represented in the binary system* into one you'd be
more familiar with, *represented in the decimal system*.

Remember that in the decimal system, even though we had ten symbols, we had no
symbol *for* the number ten. Similarly, in the binary system, we have two
symbols, but no symbol *for* two.

So as to not confuse anyone, we'll end any binary numbers we're working with
with a final `b`. So the number [10](bin) should be considered a binary number,
while the number $10$ is the usual ten that we know and love.

Starting slowly, we'll look at the number [0](bin), which is obviously the same
as our regular decimal $0$. Likewise, we still have a symbol to shuffle, so
[1](bin)=1.

If we increase one to both sides, our decimal side easily goes up to $2$, but in
binary, we've now run out of symbols, and so we need to steal a new one.
Therefore [10](bin)=2. In the same way that a decimal number full of `9`s
switches to a `1` followed by the same number of `0`s when its increased, a
binary number full of `1`s switches too!

That decomposition of decimal numbers into their powers-of-ten trick that we saw
earlier works equally well in binary land. For example:

[101101](bin) $$= 1\times 2^5+0\times 2^4 + 1\times 2^3 + 1\times 2^2 + 0\times
2^1 + 1\times 2^0 = 45$$

Your powers-of-two likely aren't up to the same calibre as your powers-of-ten,
but you have to admit, this method is much easier than trying to count all the
way up from 0.

And so that's it! We've come up with a scheme for representing numbers in terms
of the building blocks we had, namely wires! This is the beginning of a trend.
As we'll see more and more, wires are, in fact, useful. We use them to describe
*things that are*, while we use machines to describe *how things change*, by way
of function tables. In this sense, our wires are "nouns", and our machines are
"verbs".

In the next chapter we'll look at putting these numbers directly into our
diagrams, and reinterpret some of our existing machines in terms of how they act
on numbers. Additionally, we'll build some new machines for doing useful things
*with* numbers.

---

## Exercises

1) Convert the number [11111111](bin) into its decimal representation.
   Recalling what you know about how numbers "roll-over", is there an easier way
   of computing this?
2) Change the decimal number $17$ into its binary representation.
3) Come up with a general strategy for changing decimal numbers into their
   binary equivalents. Hint: Start from the left, and try to find the biggest
   power-of-two that you can pull out of it.

