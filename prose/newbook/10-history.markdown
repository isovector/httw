## History

Algorithms are a series of mechanical steps, which encoding understanding of a
problem into rules which, when precisely followed, reliably get a result.
Following an algorithm requires no understanding, creativity, imagination, or
even free will.

Take me for example; I don't know anything about nutrition or cooking or
biology, nevertheless, I can follow a recipe (algorithm) for French onion soup,
and reliably create a delicious, nutritious, meal for my friends. The recipe, in
some sense, is a manifest representation of humanity's understanding of taste,
chemical change in food, and human nutrition. Importantly, I can act as a robot
as I follow the recipe, and so long as my tools and ingredients are fine, I can
reliably make a pot of soup without understanding anything for myself.

Producing an algorithm for a problem is thus having solved it once and for all;
once the algorithm exists, no thought needs to be further expended on the
problem; we can just mechanically turn the crank until the answer is spit out
--- and bonus points, that cranking can be delegated to an automaton, like a
computer or hapless grad-student.

Once we have an algorithm for a problem, it's reasonably straightforward to
arrange the universe in order to turn this crank for us. Most commonly this is
done with an electrical computer, but more obscure computation devices exist,
and are equally powerful in their capacities. It's the algorithm that matters,
not the automaton that executes them!

Algorithmic work is distinguished from intuitive or creative work, because, as
of yet, we don't know how to automate those things, and can only "get lucky" on
tasks requiring those inputs. Of course, some people are better at this kind of
work than others, but at the end of the day, it still requires some sort of
"stroke of brilliance" to make progress; either you have a good idea, or you
don't!

Computation is the study of algorithms. It is the technique of solving problems
once and for all.

Algorithms have been known since antiquity, with cooking, accounting, and
mathematics as the primary fields in which algorithms were used. But the idea of
an algorithm didn't come along until the early 800s, in Persia. The Qur'an had
appeared some two hundred years earlier, introducing complex rules for Islamic
inheritance. Estates were to be divided up and shared between many people, in
varying ratios. Evidently the rules were complicated enough that new techniques
needed to be invented to correctly carry out the inheritance.

Around 820, Muhammad ibn Musa al-Khwarizmi published his seminal book *The
Compendious Book on Calculation by Completion and Balancing.* In the Compendious
Book, al-Khwarizmi introduced the idea of the mathematical equation,
simultaneously gave the first modern treatment of algebra, and introduced
techniques for mechanically solving algebraic equations. This book was so
influential that both algebra and the rules for solving them, the algorithms,
are today named after al-Khwarizmi.

Mathematics carried on steadily until 1928, when David Hilbert put forth the
Entscheidungsproblem, asking whether it was possible to automate determining the
truth content of an arbitrary mathematical statement. That is to say, is there
some sort of "oracle" that can look at a mathematical problem and give back an
answer, without appeal to insight or creativity. In other words, Hilbert was
looking for an algorithm. An algorithm that solved the Entscheidungsproblem
would effectively automate away mathematicians, reducing the field into what
most people (incorrectly) think of as math: mindless computation of meaningless
problems with definite right or wrong answers.

Formal systems like mathematics have two desirable properties:

1. every true statement can be articulated `(completeness)`
2. no false statement can be articulated `(consistency)`

Implicit in the formulation of the Entscheidungsproblem is the idea that
mathematics is complete. This seems like a very reasonable assumption, after
all, who has seen an unproven mathematical truth? How would you ever know if
you'd spotted one? Such a thing would necessarily be a mathematical claim
without a proof; but it is exactly proof that allows us to verify that a
statement is in fact true! The whole idea of "true, but unprovable" seems like a
very funny business indeed.

However, in 1931, Kurt Gödel published his Incompleteness Theorem, showing once
and for all that mathematics is not, in fact, complete. His trick was to find a
clever way of turning numbers into mathematical statements, and subsequently
discovering a number which encoded

> This statement cannot be proven.

Uh oh! Once you can write this statement down mathematical, you're in an awful
lot of trouble. There are two possibilities:

1. the statement is false, in which we have proven it, and we've proven
   something that's false!
2. the statement is true, and thus we cannot prove it.

The first case here is completely destructive to mathematics; as soon as
you've got a proof of false in your tool-belt, you can prove anything
whatsoever --- absurdities included. Mathematics only works in the first place
because of its consistency: that is, the only things it can prove are true.

Thus, Gödel has backed us into an unfortunate corner; we must choose between
consistency or completeness, Without consistency, mathematics is useless, and so
we must keep it at any cost and throw away completeness instead.

A mechanical solution to the Entscheidungsproblem has no choice but to work
symbolically, that is to say, without any "understanding" of the actual problem.
In order to do so, it must work within the rules of mathematics, that is to say,
in the realm of proof. But without completeness, there are truths without
proofs, and thus the Entscheidungsproblem cannot be solved in general.

Of course, there are mathematical truths that can be mechnically shown to be
true; for example, you can instantly tell me that $4 \times 5 = 20$ is true, but
that $1 + 1 = 3$ is false. And furthermore, neither of these snap judgments
seems to require any creative effort! Statements like these seem like we could
automate them. So, what's the difference? How do we know which statements we can
automate, and which we can't? Computation as a field arose directly from
attempts to answer this exact question.


### Three Accounts of Computation

Between May 1935 and May 1936, three separate theories were raised as to which
mathematical proofs were computable. The most famous of the three was put forth
by Alan Turing; his system is known today as the Turing machine.

Turing imagined a machine, equipped with two read/write heads on two infinite
spools of tape. The machine could spin either spool to effectively move the
head, it could change the symbol under the head, and it could read which symbol
was under the head of either tape. Using one tape for what we'd today call the
program, and the other for the data, instruction symbols on one head would
dictate to the machine how to manipulate the symbols on the other in order to
produce the desired result. The computable mathematical statements were exactly
the same as the ones the Turing machine could build an answer for.

Although Turing machines are not the focus of this book, it will be informative
to see just how complicated they are to reason about.


Let's imagine a simple Turing machine. For our purposes, we will assume the
symbols on the data head are the natural numbers (0, 1, 2 ...), though this
isn't necessary for the operation of the Turing machine; it's merely to help us
reason about the system. Our machine will have three pairs of instructions:

* `<`  and `>` --- move the data head left and right, respectively
* `-` and `+` --- change the symbol at the data head one previous and one next,
  respectively
* `[` --- if the symbol at the data head is empty, move the instruction head
  forward to just after the matching `]`
* `]` --- move the instruction head back to the matching `[`

> TODO(sandy): can just give a recipe here instead of introducing all these
> symbols

We can write a program to add the number on the data tape in position 1
to the number in position 2 like this:

```
[>+<-]>
```

The data head starts at position 1, with the instruction head looking at the
first `[` instruction. If the data at position 1 is a zero, we skip to the last
`]`, and are finished (because zero added anything doesn't change the result.)
If the data is not a zero, the instruction head now encounters the `>`
instruction, and moves the data head to position 2.

The `+` instruction tells the data head to increment the number at position 2 by
one, and then `<` moves the data head back to 1, which is decremented via `-`.
Thus, after the first time around, we have subtracted one from the data in
position 1, and added one to the data in position 2.

The instruction head now reads the `]` symbol, and rewinds itself to the `[`,
from where the whole program runs again, proceeding as above. After the second
time through, we have subtracted two from data position 1, and added two to data
position 2. This process will continue, subtracting one from data position 1
and adding one to data position 2, until the first position is empty.

In effect, this has added the number that started in data position 1 to the
number that started in data position 2. It gets the job done, but it isn't an
easy thing to reason about! And this is a simple program on a simple Turing
machine. If you'd like a challenge, try deciphering what this one does:

```
[>[->+>+<<]>[-<+>]<<-]
```

Understanding programs designed for Turing machines is no easy task, even for
seasoned, professional programmers. The biggest issue is that the data tape (and
its position) gets changed over time, meaning we humans need to mentally add up
innumerable tiny changes to simulate a Turing machine in our heads. And
afterwards, assuming we haven't made any mistakes in computation, we need to
infer what *meaning* all those tiny changes add up to. That is to say, we need
to deduce why someone wrote this program in the first place, based only upon how
it changes the data tape!

Ease of use was never Turing's goal; the idea was simply to show for which
problems we could approximate the Entscheidungsproblem.

Turing's idea caught on, probably because it was first and immediately framed as
a mechanical device. Huge insight wasn't necessary to consider *actually
building* such a machine; and thus the modern computer era was born.
Unfortunately, to this day, the Turing machine is the conventional way of
thinking about computation, and all of its challenges and complexities have come
along for the ride.

Of chief concern with the Turing machine is that the meaning of a program is
implicit in its context. Recall that composition is the only strategy humans
have for reasoning about complicated tasks. The Turing machine is antithetical
to composition; implicit in every instruction is "where am I on the data tape,"
the answer to which changes like the weather. We cannot take an arbitrary piece
of a Turing program, move it elsewhere, and expect the meaning to remain the
same.

> TODO(sandy): what is meaning


### Church

The year prior to Turing's work, Alonzo Church published the Lambda calculus,
his answer to the Entscheidungsproblem. The Lambda calculus is a system of
computation based on the idea of substitution; programs are written with
explicit holes that should be filled in, to be decided later. The newly filled
holes might themselves contain holes, and the system continues filling holes
until there is nothing left to do.

We will come back to explore this idea more thoroughly in @sec:lambda, but to
first get a taste of it, the following is a program in the Lambda calculus:

```
(λx y. f x y) (g f)
```

The `λx y` notation means "inside of these parentheses, `x` and `y` are two
holes to be filled". The things we'd like to fill it with comes outside of the
parentheses, in this case `(g f)`. Holes are filled one at a time, and a hole is
always filled as soon as it is able. Thus, the above program is equal to the
following, having replaced `x` with `(g f)`:

```
(λ y. f (g f) y)
```

Since we don't have anything to fill the `y` hole, it remains unfilled.

The Lambda calculus takes nothing for granted, not even the existence of numbers
(or equivalently, some set of symbols like in the Turing machine.) Instead, we
can build numbers out of a pair of holes, `s` and `z`, which correspond
mathematically to $1+$ and 0. For example, we can write the number 4 as $1+ (1+
1+ (1+ 0))$, or, in the Lambda calculus, as `(λs z. s (s (s (s z))))`.

For comparison to the Turing machine, we can look at the same addition simple
program in Lambda calculus:

```
(λa b. (λs z. a s (b s z)))
```

This program cleverly replaces the meaning of 0 in the number `a` with the
number `b`. Essentially, if we'd like to add 2 to 3, we encode 2 as normal via

```
(λs z. s (s z))
```

and replace its 0 with the number 3:


```
  (λs z. s (s 3))
=
  (λs z. s (s (s (s (s z)))))
=
  5
```

For now, it's not critical to deeply understand the mechanism behind the Lambda
calculus. What's been presented here is merely a flavor of how computation can
work differently than the "recipe" approach that is perhaps more natural to our
everyday experience. The critical part of the Lambda calculus is that it is
"movable" in a way that Turing machines are not. The meaning of any
parenthesized expression of the Lambda calculus depends only on itself, not on
the implicit position of some data tape. As a result, Lambda expressions are
highly compositional; we can solve a problem in parts, and trivially glue the
results together.

> TODO(sandy): give an example of how we need to manipulate a turing program
> when composing solutions

> TODO(sandy): the construction of numbers here could use much more explanation.
> start with that, replace s with 1+ and z with 0


### Try Again

Like I said, there were three solutions to which problems the
Entscheidungsproblem could answer. Turing's approach is one of them, but it's
not the most elegant of the problems. That award goes to Alonzo Church's work,
called the λ-calculus (pronounced "lambda".) λ-calculus has nothing to do with
the calculus you learned in grade school, it's name comes from the fact that it
is useful is "calculating" (or "computing") things.

The elegance of the λ-calculus comes from how little it asks for. A Turing
machine needs a collection of symbols, and two effectively-infinite tapes to
store its data and its instructions --- no small ask! In comparison, all that is
required in the λ-calculus are functions.

Perhaps you remember from grade-school math the idea of a function, that is, a
generator of arithmetic expressions. You probably spent countless hours
evaluating and graphing functions like

$$
f(x) = x^2 - 9
$$

The idea is that $f$ transforms some number $x$ into the number $x^2 - 9$,
simply by replacing $x$ with the number you pick. For example, we might pick $x
= 5$, in which case $f$ sends $5$ to $5^2 - 9 = 16$.

In the above, $f$ is merely the name of this transformation from $x$ to $x^2 -
9$. It's often convenient to build nameless, anonymous functions, for which we
use the funny notation:

$$
\lambda{}x \to x^2 - 9
$$

which means exactly the same thing, except didn't require us to have to come up
with an arbitrary name like $f$.

> TODO(sandy): function composition

Functions in math class are very tied to numbers: they transform numbers into
other numbers. But what would happen if we forgot about numbers, and instead
made our functions work over other functions? Behold, the glorious idea behind
λ-calculus. It's functions all the way down.

In the λ-calculus, everything is built out of nothing but functions --- even
numbers and other functions. It's a weird idea, but turns out to be immensely
powerful because of it's extreme capabilities for composition. In a Turing
machine, the programs are made out of a completely different thing than the data
that the machine can manipulate. The only things we can manipulate in a Turing
machine is the predefined collection of symbols on the data tape; and
unfortunately, things we'd like to build on top of these symbols will always be
"second-class" citizens. This seriously hampers any attempt to build big
programs out of smaller ones, because we will never be able to extend our
vocabulary.

Programming on a Turing machine is a lot like trying to live in the modern
world, restricting yourself only to words that exist in Latin. All of a sudden,
we're unable to take a train, we must instead ride the self-propelled iron horse
which travels only along two parallel lines. Certainly, you can get the job of
communicating done, but it sure takes a lot of effort. Better just to add
"train" to your vocabulary, and go from there.


### Numbers

Let's wet our whistle for the λ-calculus by seeing how to build numbers out of
functions. Our journey will be rather roundabout, taking some philosophical
detours, and really diving into what exactly is the essence of "numberness."

The trick for constructing things out of functions is to ponder deeply about how
those things can be made in the first place. In grade school we learn (perhaps
implicitly) that exponentiation is simply repeated multiplication, while
multiplication is just repeated addition. And addition, when you think about is,
is just repeated counting. For example, you will agree that the following
equation is true:

$$
5 + 3 = 5 + 1 + 1 + 1
$$

and, you will also not be surprised to learn that we can cancel the fives:

$$
3 = 1 + 1 + 1
$$

or that we can always add zero:

$$
3 = 1 + 1 + 1 + 0
$$

or even that we can introduce parentheses at will:

$$
3 = 1 + (1 + (1 + (0)))
$$

In fact, if we are patient enough, we can construct every non-negative,
non-decimal number in this way. It's unbelievably tedious to do so, but we can
indeed write 15 as

$$
1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + (1 + 0))))))))))))))
$$

but for the sake of sanity, let's go back to using three as the example. The
last trick is to make two functions:

$$
s(x) = 1 + x
$$

$$
z = 0
$$

It's rather odd to write three this way, but there's nothing wrong with it:

$$
3 = s(s(s(z)))
$$

Notice that under this scheme, we can encode any number $x$ as a series of
nested $s$ functions ($x$-many of them), terminated finally by an inner-most
$z$.

> TODO(sandy): this might be nice with a history lesson on mr peano

Have you ever wondered what exactly *is* a number? There is clearly something in
common between having three coffees, writing three papers, and staying awake for
three days. But what is that commonality? How can we get our hands on it? In
fact, our above encoding of numbers as chains of $s$ and $z$ gives us hint.
Notice that $s$ and $z$ are merely *symbols,* they have no meaning apart from
that which we ascribe to them. I intentionally chose $s$ to suggest *succession*
and $z$ to suggest *zero,* but those choices are not set in stone.

What if we instead reinterpret $s$ as a transformation that takes a stomach to a
stomach with another tomato in it, and $z$ as my stomach. Then $s(s(s(z)))$
would mean "put three tomatoes inside my stomach." The three-ness of
$s(s(s(z)))$ is thus *completely independent* of our choices of meaning for $s$
and $z$.

Whenever we see this pattern --- a choice needs to be made, but we don't care
*which choice* --- the technique is to add a function that allows us to make
that choice later. This leads us directly to our means of constructing numbers
out of functions:

$$
0 = \lambda{}s z \to z
$$

$$
1 = \lambda{}s z \to s z
$$

$$
2 = \lambda{}s z \to s (s z)
$$

$$
3 = \lambda{}s z \to s (s (s z))
$$

Under this scheme, there is a lovely little way to perform addition --- we
merely replace the zero of the first number with the second:

$$
\lambda{}a b \to (\lambda s z \to a s (b s z))
$$

Just to convince ourselves that this works, we can look at the derivation that
$3 + 1 = 4$:

```{.haskell .proof}
  plus 3 1
= -- .via definition of plus
  (λa b. (λs z. a s (b s z))) 3 1
= -- .via applying 3
  (λb. (λs z. 3 s (b s z))) 1
= -- .via applying 1
  (λs z. 3 s (1 s z))
= -- .via definition of 3
  (λs z. s (s (s (1 s z)))
= -- .via definition of 1
  (λs z. s (s (s (s z)))
= -- .via definition of 4
  4
```

We won't belabor the point any further, since we'll return to this idea in
@sec:lambda. The takeaway here is that we can indeed do computation in a way
that is very unlike the traditional recipe-like programming that makes up the
vast majority of computation today.


## Logicism

At the turn of the 20th century, mathematics was undergoing a dramatic
transformation. The idea was simple, that all of mathematics could be reduced to
mere logic. The idea was that logic and mathematics were, for all intents and
purposes, equivalent, and thus that logic could be used to give a *foundation*
mathematics.

The desire was much akin to having an understanding of chemistry --- that we
have different elements with different (but regular) properties --- but not
having the physics to understand why it all fits together. This is the state
mathematics was in around 1900, but with a burning desire to figure out why it
all worked.

The demand for a foundation for mathematics came, in part, from Bertrand
Russell's paradox:

> Pete is the barber who shaves the head of everyone who doesn't shave
> themselves. Does Pete shave himself?

Mathematics had gotten powerful enough to the point where it could express
situations like the above, which spelled trouble. Either way you slice it, the


