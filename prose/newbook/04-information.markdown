## Information

With the advent of the telephone came the need to pin down exactly what it is
we're talking about when we talk about communication. Early telephones were
extremely expensive and unreliable, and thus were much more akin to a traffic
light than they are to modern telecommunications. A traffic light communicates
very little, but very important information to motorists, namely whether they
should "go," "stop," or "slow down." Early telephones were similar; per-minute
rates were extraordinarily expensive, so calls were kept short and sweet,
relaying only the most important of news.

The combination of low bandwidth, unreliable transmission, and high cost meant
there was both a need and a profit to be made for anyone who could improve the
situation. That person was Claude Shannon.

Shannon's insight is that information is a measure of *surprise.* Imagine
sending a daily message to a friend in a later timezone, with the text "SUN ROSE
AGAIN TODAY." We already expect the sun to rise every day, and thus receiving
this message doesn't surprise us. But contrast that to receiving "JOHN
PROPOSED" or "WAR ERUPTS" --- events which are decidedly *not* everyday
occurrences.

The takeaway is clear: surprising messages contain more information than
unsurprising ones. And the more surprised you are, the more information was
sent. And receiving the same message twice doesn't surprise you any more than
receiving it once.

In true abstract fashion, none of these ideas about surprise require
telecommunications. We don't think of it as such, but every conversation is
nothing but a series of messages being sent back and forth --- whether those
messages be verbal or non-verbal. We can discover surprising things in the daily
newspaper, in games of chess, in people, really just about anywhere.

But how do we quantify this surprise, or information? Let's consider a visual
example first. Imagine you have a treasure map that leads to buried gold. The
amount of information present in the treasure map is the smallest number of
yes-no questions you'd need to ask in order to reliably find the gold.

Before we've asked any questions, we don't know anything. If we were to shade in
the region of the map of where the treasure *could* be, we'd have to shade in
the whole thing:

```{design=code/Languages/Information.hs}
([] :: Division)
```

However, now we ask "is the treasure in the top-half of the shaded area?
Assuming we trust our interlocutor, his answer "yes" cuts our *search space* in
half:

```{design=code/Languages/Information.hs}
[N]
```

We can now ask if the treasure is in the left-half of the remaining shaded area.
He says no, which refines our search space thusly:

```{design=code/Languages/Information.hs}
[N, E]
```

If we are clever about how we ask questions, each yes-no answer can be exploited
to cut our remaining search space in half. It takes another question to learn
that the treasure is in the left-half of the remaining area:

```{design=code/Languages/Information.hs}
[N, E, W]
```

and then another to learn it's in the lower half:

```{design=code/Languages/Information.hs}
[N, E, W, S]
```

and then in the upper half:

```{design=code/Languages/Information.hs}
[N, E, W, S, N]
```

and then in the eastern half:

```{design=code/Languages/Information.hs}
[N, E, W, S, N, E]
```

At this point, we realize the remaining area of possible treasure locations is
the same size as any actual treasure chest we might find. And thus we don't need
any more information --- if we dig anywhere in the shaded region, we are
guaranteed to find our treasure. It took us six "halvings" to find what we were
looking for, so the amount of information we received is said to be **6 bits.**
A bit is the unit of measurement for information, and it's interpretation is
exactly this --- the number of times a search space needs to be cut in half in
order to reliably find what you're looking for.

The attentive reader might have noticed that the number of halvings necessary to
find something is determined only by two sizes: the total space to be explored,
and the size of the thing we're looking for. The former size relates to how many
possible messages could be received, and the latter is how much uncertainty we
are OK with.

For example, if you're looking for a specific person in Canada, you require
about 25 bits of information, because this is the number of times you'd need to
cut the population of 38 million people in half to find someone in particular.
Perhaps you're looking for me. If I tell you my height is 180cm, this is about
5 bits of information, because only 4% of Canadians are 180cm tall. In order to
positively identify me, you now need only $25 - 5 = 20$ more bits.

In general, the way to compute how many bits are contained in some search space,
we need take only the logarithm (in base two) of the size of the space:

$$
\log_2(|\text{space}|)
$$

This is how I computed the number of bits necessary earlier:

$$
\log_2(38,000,000) \approx 25.17 \quad \text{(population of Canada)}
$$

and

$$
\log_2(4\%) \approx 4.64 \quad \text{(percentage of people who are 180cm tall)}
$$

Let's return to the telegraph. We'd like to transmit a message to a friend
overseas, perhaps again we'll use *John proposed.* How much information is in
this message? If we restrict ourselves to messages that consist of one person
and one verb, we could choose probably one of 150 people that the recipient
knows personally, and probably 500 more people that the recipient is aware of
(think extended social circles, celebrities, etc.) Let's call it an even 1000
people for simplicity. And of those 1000 people, there are maybe 100 possible
things we could say about them, things like *proposed,* *died,* *had a child,*
*got a new job,* etc. thus, there are

$$
1,000 \times 100 = 100,000
$$

possible messages, and we'd need to send $\log_2(100,000) \approx 17$ bits of
information. Of course, this is a massive simplification, since you might also
want to send messages about *two* people, or about the state of the world, or
whatever. But the simplification is fine for our purposes.

Now consider the literal message "JOHN PROPOSED," complete with quotation marks.
"JOHN PROPOSED" is an *encoding* of the message *John proposed.* In order to
send this encoded message, we need to send each individual character over the
line. If we'd like to be able to encode all 26 letters, 10 numbers, and a
handful of punctuation characters (things like spaces, periods and commas), each
character over the wire requires

$$
\log_2(26 + 10 + 10) \approx 6
$$

bits. Thus, to transmit "JOHN PROPOSED" (13 characters long) we need to transmit

$$
6 * 13 = 78
$$

bits of information over the wire --- significantly more than the 17 bits
required in theory. Telegrams were priced by the number of characters sent,
so this is not a trivial problem. @loscher gives us a price point of \$3.11 per
word send over telegram, which means sending this paragraph over the wire would
cost \$171.

> TODO(sandy): CITE
> The Bolsheviks: Twilight of the Romanov Dynasty
> By John D. Loscher

Reducing the costs here are clearly valuable goals for society. One way of doing
that would be to build more telegram infrastructure, but that is possibly
prohibitively expensive. A better way is to instead get by with sending the same
message in less information. This introduces us to the problem of compression:

> How can I say what I want to say, using as little bandwidth as possible?

> TODO(sandy): How do we actually signal these things over the wire?


Earlier, we pegged sending a character across the wire at 6 bits. But, can we do
better? As it happens, we can. The key insight is that the letter "E" comes up
significantly more often than does the letter "Q". Thus, if we want to shorten
average message lengths, we'd like to assign a short code to "E", at the expense
of assigning a long code to "Q." For concreteness, here are the letters of the
Latin alphabet, with their relative percentages in English text:


| Letter | Likelihood | Letter | Likelihood |
|:-:|:-:|:-:|:-:|
| E |11.2% | M | 3.0% |
| A | 8.5% | H | 3.0% |
| R | 7.6% | G | 2.5% |
| I | 7.5% | B | 2.0% |
| O | 7.2% | F | 1.8% |
| T | 7.0% | Y | 1.8% |
| N | 6.7% | W | 1.3% |
| S | 5.8% | K | 1.1% |
| L | 5.5% | V | 1.0% |
| C | 4.5% | X | 0.3% |
| U | 3.6% | Z | 0.3% |
| D | 3.4% | J | 0.2% |
| P | 3.2% | Q | 0.2% |

Looking at this table makes it clear that we really want to assign short codes
to "EARIOT," which together make up half of all letters in English. We can build
up the desired short codes *inductively,* by looking at the two smallest
likelihoods, and building a tree with one on the left, and the other on the
right:

```{design=code/Dot.hs #jq}
makeTree "0.4%" [ Pure 'J', Pure 'Q' ]
```

We label the root of this tree with the combined likelihoods of all children.
Our probability chart now looks like this:

| Letter | Likelihood | Letter | Likelihood |
|:-:|:-:|:-:|:-:|
| E |11.2% | M | 3.0% |
| A | 8.5% | H | 3.0% |
| R | 7.6% | G | 2.5% |
| I | 7.5% | B | 2.0% |
| O | 7.2% | F | 1.8% |
| T | 7.0% | Y | 1.8% |
| N | 6.7% | W | 1.3% |
| S | 5.8% | K | 1.1% |
| L | 5.5% | V | 1.0% |
| C | 4.5% | JQ | 0.4% |
| U | 3.6% | X | 0.3% |
| D | 3.4% | Z | 0.3% |
| P | 3.2% | | |

We can repeat the procedure, this time using "X" and "Z" as the least likely
letters:

```{design=code/Dot.hs #xz}
makeTree "0.6%" [ Pure 'X', Pure 'Z' ]
```

| Letter | Likelihood | Letter | Likelihood |
|:-:|:-:|:-:|:-:|
| E |11.2% | M | 3.0% |
| A | 8.5% | H | 3.0% |
| R | 7.6% | G | 2.5% |
| I | 7.5% | B | 2.0% |
| O | 7.2% | F | 1.8% |
| T | 7.0% | Y | 1.8% |
| N | 6.7% | W | 1.3% |
| S | 5.8% | K | 1.1% |
| L | 5.5% | V | 1.0% |
| C | 4.5% | XZ | 0.6% |
| U | 3.6% | JQ | 0.4% |
| D | 3.4% | | |
| P | 3.2% | | |

Still, XZ and JQ are the least likely, so we now combine their *trees*:

```{design=code/Dot.hs #jqxz}
let jq = makeTree "0.4%" [ Pure 'J', Pure 'Q' ]; xz = makeTree "0.6%" [ Pure 'X', Pure 'Z' ] in makeTree "1.0%" [ jq, xz ]
```

| Letter | Likelihood | Letter | Likelihood |
|:-:|:-:|:-:|:-:|
| E |11.2% | M | 3.0% |
| A | 8.5% | H | 3.0% |
| R | 7.6% | G | 2.5% |
| I | 7.5% | B | 2.0% |
| O | 7.2% | F | 1.8% |
| T | 7.0% | Y | 1.8% |
| N | 6.7% | W | 1.3% |
| S | 5.8% | K | 1.1% |
| L | 5.5% | V | 1.0% |
| C | 4.5% | JQXZ | 1.0% |
| U | 3.6% | | |
| D | 3.4% | | |
| P | 3.2% | | |

We can continue this pattern, building a tree by always joining the lowest two
likelihoods. @fig:huffman gives a final version of this tree.

```{design=code/Languages/Huffman.hs #fig:huffman}
huffman [(Pure 'E', 11.2), (Pure 'A', 8.5), (Pure 'R', 7.6), (Pure 'I', 7.5) , (Pure 'O', 7.2) , (Pure 'T', 7.0) , (Pure 'N', 6.7) , (Pure 'S', 5.8) , (Pure 'L', 5.5) , (Pure 'C', 4.5) , (Pure 'U', 3.6) , (Pure 'D', 3.4) , (Pure 'P', 3.2) , (Pure 'M', 3.0) , (Pure 'H', 3.0) , (Pure 'G', 2.5) , (Pure 'B', 2.0) , (Pure 'F', 1.8) , (Pure 'Y', 1.8) , (Pure 'W', 1.3) , (Pure 'K', 1.1) , (Pure 'V', 1.0), (Pure 'X', 0.3) , (Pure 'Z', 0.3), (Pure 'J', 0.2), (Pure 'Q', 0.2) ]
```

@fig:huffman now gives a tree weighted by likelihood. More frequent letters
appear closer to the root of the tree than less frequent ones do. All that's
left is to pull an encoding for each letter out of this tree. If we interpret a
left branch as a `0`, and a right branch as a `1`, then we can encode the letter
"E" (right, right, left in the tree) as `110`. The letter "Q", our least
frequent character, has an encoding of `011100001`. We can now again try
encoding our original message, "JOHN PROPOSED":


| Letter | Code |
|:-:|:-:|
| J | 011100000 |
| O | 0100 |
| H | 11111 |
| N | 0001 |
| P | 00000 |
| R | 0110 |
| O | 0100 |
| P | 00000 |
| O | 0100 |
| S | 1110 |
| E | 110 |
| D | 00001 |

which in total is the message `011100000 0100 11111 0001 00000 0110 0100 00000
0100 1110 110 00001`, and is only 57 bits long. This simple compression
strategy, of giving the common letters shorter codes managed to shave off 21
bits, compared to our original, naive encoding.

It is interesting to note that, as a special case, the above algorithm will give
the same naive encoding if every letter in the alphabet is equally frequent.
This is an encouraging result; it means we have found a *more general* solution
to the problem than we started with.

What has happened here? Where did this optimization power come from? The old
adage "knowledge is power" is a very cogent truth, both in human relations and
computation. Here it was our understanding of the problem and its context that
we were able to exploit in order to pull optimization out of.

## Vector Spaces

