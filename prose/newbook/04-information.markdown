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



