## Data Structures

One of the central problems in computing is the storage and subsequent
organization of information. The situation is analogous to that of organizing a
kitchen. Some kitchen utensils get used significantly more often than others
(eg. knives vs colanders), while it's convenient for certain things to be
grouped together (eg. cutlery). You probably know the feeling of a poorly
organized kitchen; it's not the biggest problem in your life, but it's certainly
frustrating every time you're trying to cook. To make matters worse, there is no
"one size fits all" kitchen design. Different physical spaces call for different
considerations when arranging dishes, and the constraints of a home kitchen are
different than those of a neighborhood noodle shop, which are different again
from those of a five-star French restaurant.

Computer systems have analogous problems. Consider how files are kept on the
physical disk. Hard drives are made up of thousands of square little chips, each
of which is made up of a matrix of thousands of memory cells -- each cell
capable of storing a single bit. That is, it looks a bit like @fig:physical-ram.

quad of quads of quads here here

However, for historical reasons, computer systems squint their eyes and pretend
that each of these memory cells exists in one, extremely long, line. To get a
sense of the scale here, if each one of these memory cells were 1 centimeter
long, arranging the average computer's disk space would wrap around the entire
earth twelve thousand times. And somehow, in those half-billion kilometers, the
computer is expected to keep track of which 400 kilometers it stored your photo
in.

Dictionaries have a similar problem. The Oxford English Dictionary lists about
300,000 words, and it's somehow feasible for a reader to *find* any particular
word they're looking for. Although it might not sound like a particularly
exciting piece of technology, the secret to making the OED work is that the
words are organized by their alphabetical ordering. And make no mistake,
alphabetical ordering *is* a piece of technology.

Different ways of organizing data are useful for different tasks. The phone book
is ordered by name, helping us find anyone we might be looking for. A different,
*equally valid* way of ordering the phone book would be instead by phone number.
It would be hard to argue that such a layout is not in fact organized, but it
wouldn't be very useful for getting in touch with an old colleague. However, the
phone company likely maintains such a list --- after all, they need to assign
phone numbers to customers, and need a quick means of determining if a
particular phone number is already in use.


### Lists

Let's start with the simplest data structure: the list. Lists are exactly what
you'd expect --- a sequence of one item after another, like a grocery list or a
laundry list. We can construct a list in one of two ways:

1. an empty list
2. by putting a new item in front of an existing list

There is special jargon associated with these two cases; the first is known as
the `Nil` list, and the second is a `cons`tructed list. For example, if we
wanted to make a list of banana, pineapple, and waffle (in that order), we
would build it *backwards.* This is probably not how you write lists, but there
is a very good reason for doing it this way that we will explore momentarily.

In order to build a list, we must start from the empty list `Nil`:

```{design=code/Dot.hs #fig:list_ex1}
id @[String] []
```

We may then prepend the last element of the list, in this case "waffle":

```{design=code/Dot.hs #fig:list_ex2}
["waffle"]

```

Working backwards from our desired ordering, the next thing we'd like to prepend
to our list is "pineapple":

```{design=code/Dot.hs #fig:list_ex3}
["pineapple", "waffle"]

```

and finally, "banana":

```{design=code/Dot.hs #fig:list_ex4}
["banana", "pineapple", "waffle"]

```

Notice that even though we inserted our desired elements in reverse order, the
eventual list ended up how we wanted it, with "banana" first and "waffle" last.
Because new elements must always be added to the beginning of the list, it's
necessary to work backwards like this.

The reason we work backwards is that the computer is a stupid machine, and can't
keep track of entire lists. All it can do is remember a single element in the
list, and follow the arrows. We can represent this singular *focus* with a
rectangle and an arrow pointing at the current focus, like in @fig:list_ex5.

```{design=code/Dot.hs #fig:list_ex5}
focus "banana" ["banana", "pineapple", "waffle"]
```

The computer is able to follow arrows, thus we can shift the focus downwards:

```{design=code/Dot.hs #fig:list_ex6}
focus "pineapple" ["banana", "pineapple", "waffle"]
```

but, from @fig:list_ex6, we are unable to return focus to `banana`, since there
is no upwards pointing arrow. If we'd like to shift focus, we can travel only
downwards, or, along its *spine.*

```{design=code/Dot.hs #fig:list_ex7}
focus "waffle" ["banana", "pineapple", "waffle"]
```

Since there are no back arrows, changing the focus of the list is equivalent to
forgetting the old focus ever existed at all. It's the computational equivalent
of crossing out an item on your shopping list; once it's in your basket, you no
longer need to remember it. Thus, @fig:list_ex7 and @fig:list_ex8 are equivalent
diagrams:

```{design=code/Dot.hs #fig:list_ex8}
focus "waffle" ["waffle"]
```

For this reason, if the focus isn't clearly labeled, it can be unambiguously
inferred to be the top-most item in the list.


### Searching Lists

Imagine we have a list containing the names of people in our workplace, like in
@fig:people1.

```{design=code/Dot.hs #fig:people1}
["Gem", "Mika", "Brandon", "Vavilov", "Sierra", "Pokey", "Foggy"]
```

We'd like to ask the computer whether "Vavilov" is one of our coworkers. As a
human, you can easily answer this question, but the computer is not so smart.
Recall that it can only inspect one element of the list at a time, and thus it
must *traverse* the structure, checking each person in order to determine if
they're the one were looking for. This process is shown in
@fig:people-search-ok.

```{design=code/Dot.hs #fig:people-search-ok}
let peeps = ["Gem", "Mika", "Brandon", "Vavilov", "Sierra", "Pokey", "Foggy"]
 in Beside
      [ focus "Gem" peeps
      , focus "Mika" peeps
      , focus "Brandon" peeps
      , focus "Vavilov" peeps
      ]
```

In order to determine whether Vavilov works with us, the computer must change
focus four times. You can see how answering this question for different people
will require different numbers of focus changes, for example, Mika requires only
two, but finding Foggy needs seven. However, consider the case where the person
we're looking for isn't, in fact, a member the list. In that case, the computer
needs to change focus eight times until it gets to `Nil`, when it is required to
stop because there are no further arrows to follow.

Why does it matter to us how many times the computer needs to switch focus?
Because computation is a mechanical process, and it requires both energy and
time to physically run this computation. If we wanted to scale this problem up
from "determine if we have a coworker" to "determine if this person exists" we
need to go from the 7 people in this example to the 8 billion people on Earth.
The earliest computers could run about 2000 operations per second, which means
that computing this answer could take as long as 46 days, to say nothing of the
energy costs.


Thus, it's imperative that we find cleverer ways about laying out our data than
just sticking it one-after-another in a list. This, in true essence, is what the
study of data structures is all about.


### Binary Trees

The problem with lists when we are attempting to determine membership (does this
person exist in the data structure?) is that lists require us to look through
too much data. Is there a way we can more quickly hone-in on the relevant part
of the data?

Thinking about how we lookup words in the dictionary provides some insight into
this problem. We don't find a word in the dictionary by starting at the front,
and moving through one-at-a-time. Instead, we start roughly in the middle, and
check whether we've gone too far. If we have, we look in the first half,
otherwise, in the second. We can repeat this process, splitting the unexplored
pages in half and in half again, looking if we've gone too far or not far
enough, and will eventually end up on the page with the word on it.

We can do the same approach with a data structure. While lists only ever had one
arrow flowing out of each item, the *binary tree* has two arrows out. If we
stick the middle-most item into the first focus of this tree, we can check if it
comes before or after the word we're looking for. If it comes after, we will
refocus down the left-arrow, and if it comes before, down the right-arrow. Each
time we move left or right, we necessarily split the remaining work in half.

Our original list of coworkers can be reorganized as a binary tree like in
@fig:people-tree.

```{design=code/Dot.hs #fig:people-tree}
Split "Mika"
  (Split "Foggy"
    (leaf "Brandon")
    (leaf "Gem")
  )
  (Split "Sierra"
    (leaf "Pokey")
    (leaf "Vavilov")
  )
```

To find `Pokey` in this binary tree, we see if it comes before or after the
focus (`Mika`.) Since `Pokey` comes after `Mika` in an alphabetical ordering, we
refocus down the right side of the tree:

```{design=code/Dot.hs #fig:people-tree2}
focus "Sierra" $ Split "Mika"
  (Split "Foggy"
    (leaf "Brandon")
    (leaf "Gem")
  )
  (Split "Sierra"
    (leaf "Pokey")
    (leaf "Vavilov")
  )
```

`Pokey` now comes alphabetically *before* `Sierra`, so we move left:

```{design=code/Dot.hs #fig:people-tree3}
focus "Pokey" $ Split "Mika"
  (Split "Foggy"
    (leaf "Brandon")
    (leaf "Gem")
  )
  (Split "Sierra"
    (leaf "Pokey")
    (leaf "Vavilov")
  )
```

and we're done. We've found `Pokey`!

Notice the layout of the binary tree: each layer of the tree is twice as wide as
the one above it. Because the amount of time we require to search the whole tree
is proportional to the number of steps we need to take before running out of
arrows, this means that doubling the number of people to search requires only
one extra refocusing. While looking up a single human out of the global
population would take 46 days when organized as a list, it would take less than
one second then organized as a binary tree. This is a dramatic improvement.


### Containers

While there is a lot to say about the infinitude of ways for structuring data,
such things are beyond the scope of this book. For our purposes, we want to
consider any sort of structure parameterized by the things it can contain.

The important considerations of any data structure are how it can be built. As
we saw for lists, the empty list is "already built" and we can lengthen a list
by prepending a single element onto it. We can describe this "schema"
pictorially like in @fig:schema_list:

```{design=code/Dot.hs #fig:schema_list}
compile "List &clubs;" $ SPlus
  [ STimes (Ctor "Nil") []
  , SList $ Right Club :| [ Left "List &clubs;"]
  ]

```

This picture represents exactly the schema of a list --- a list is either the
empty list `Nil`, or it's some &clubs; item pointing to some other list (itself
built in the same way.)

We can give a schema for the binary tree as well:

```{design=code/Dot.hs #fig:schema_search}
compile "Search &clubs;" $ SPlus
  [ STimes (Ctor "Empty") []
  , STimes (FakeCtor "&clubs;") [Left "Search &clubs;", Left "Search &clubs;"]
  ]

```

As you can see, the only structural difference between lists and binary trees is
that lists have one arrow coming out of their &clubs; node, while binary trees
have two.

Schema diagrams like @fig:schema_list and @fig:schema_search are extremely
helpful for getting quickly acquainted with new data structures. For example,
let's consider a variant of the binary tree, as shown in @fig:schema_bin.

```{design=code/Dot.hs #fig:schema_bin}
compile "Bin &clubs;" $ SPlus
  [ STimes (FakeCtor "&clubs;") []
  , STimes (Ctor "Br") [Left "Bin &clubs;", Left "Bin &clubs;"]
  ]

```

One way we might lay out our coworkers under the `Bin` &clubs; data structure is
shown in @fig:peeps-bin.

```{design=code/Dot.hs #fig:peeps-bin}
Br
  (Br (L "Brandon") (Br (L "Foggy") $ L "Gem"))
  (Br (Br (L "Mika") (L "Pokey"))
      (Br (L "Sierra") $ L "Vavilov" ))
```

Of course, @fig:peeps-bin is in no way unique; we could build an equivalent tree
with all the same people shuffled around. Or, we could change the shape of the
tree, like in @fig:peeps-bin2.

```{design=code/Dot.hs #fig:peeps-bin2}
Br
  (Br (L "Gem") (Br (Br (L "Mika") $ L "Brandon") $ L "Vavilov"))
  (Br (Br (L "Sierra") $ L "Pokey") (L "Foggy"))
```

@fig:peeps-bin2 contains all the same people as @fig:peeps-bin, but whether or
not they have the same *informational content* depends on the interpretation of
these diagrams. We will discuss interpretations more in @sec:interpretation.


### Understanding Schemas

You may have noticed &clubs; symbols littering all of our schema diagrams. What
are those? Along with the other suites (&hearts;, &diamonds;, &spades;), these
are called *metavariables.* Metavariables are placeholders for *anything at
all.* We can replace metavariables with anything we like, so long as we replace
*all the same metavariables* with the same thing. That is, we can replace
&clubs; with 5 in "&clubs; + (2 + &clubs;)" resulting in `5 + (2 + 5)`. However,
if more than one metavariable is present, we are free to replace the
different metavariables with different things (though we can make the same
choice for every metavariable!)

What's really going on when we insert our coworkers into `List` &clubs; is we
are making the choice to replace &clubs; with `Coworker`, and thus we've really
made a `List Coworker` schema as in @fig:schema-list-coworker.

```{design=code/Dot.hs #fig:schema-list-coworker}
compile "List Coworker" $ SPlus
  [ STimes (Ctor "Nil") []
  , SList $ Left "Coworker" :| [ Left "List Coworker"]
  ]

```


```{design=code/Dot.hs #fig:list-people-names}
let peeps = ["Gem", "Mika", "Brandon", "Vavilov", "Sierra", "Pokey", "Foggy"]
 in
  GoesTo  "length of name" peeps $ fmap length peeps
```







containers
functors
applicatives

```{design=code/Dot.hs}
compile $ SPlus
  [ STimes (Ctor "Nil") []
  , STimes (Ctor "Cons") [Right Heart, Left "List"]
  ]
```

