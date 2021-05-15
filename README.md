<p align="center"><img src="https://raw.githubusercontent.com/isovector/structure-and-decomposition-of-computer-programs/master/sdcp.png"></p>

## Focus

Teach JDLL how to design and structure functional computer programs. "How to do
functional programming in the large." The focus will be on designing and
engineering algebras to solve our problems. An excellent algebra lends itself to
composition, and builds most of its behavior for free.

We will also use this technique for designing an algebra for our programs
themselves, in the form of effects systems. By closely guarding which effects
are available, we can implement most of our logic in modules that are reusable
*between projects,* and make it statically impossible for the implementation of
business logic to have bugs.

## Topics

- Against against planning
  - Other engineering disciplines are not "agile" --- for good reason
  - Do this on some side projects first to get a feel for it
  - HughesPJ has a great quote on redoing it algebraically
      > This choice of combinators was made quite early on in the development of
      > the library, and the first implementation was written from a description
      > more or less like the one just given. But the description is far from
      > satisfactory: although the intention of the design is fairly clear, the
      > precise behaviour of the combinators is certainly not. Not surprisingly,
      > this led to a number of difficulties and strange behaviors.

      > Studying the algebra led to the correction of a subtle error in the
      > combinators' behavior, and to the development of much more efficient
      > implementations. The pretty-printing algebra is just too intricate to
      > rely on intuition alone; working informally I could not see how to
      > implement the optimization considered in section 9, nor could I invent
      > the representation used there. The formal approach has been invaluable.
- Denotational design
  - Several long-form worked examples of actually designing systems
    - IAM system
    - TBA
  - Checklist of things to look for
    - Monoidal structures
    - All the instances you would expect
    - If a type only exists at the leafs, good chance it's a functor
- Effects design
  - Decomposing effects
    - MTL is not enough
    - Tight effects are better than loose ones
      - Constraints liberate, liberties constrain
  - Effect synergy
    - Undo/redo over state
    - Cut/nondet
  - Auditing surfaces
- Testing
  - Correctness composes
- Static verification of invariants
  - GDP
- Dealing with people who don't care about the algebra

## Table of Contents

- Denotational Design
  - Working algebraically
  - Meaning functors
  - Testing algebraically
  - Correctness composes
    - Everything composes!

- Polysemy
  - Micro-transformations
  - Effect systems
  - Implementation details as library code
  - Modeling problems with effects


## What do I need to do to get this ready for HsX?

- Preface describing "hey so what topics are you interested in? Would you buy
    this book?"
- Follow up on TODOs
- Introduction describing what this book is, how it's structured, and what
    you're going to get out of it
- Effects as a nanopass compiler
- Finish the denotational design example ?


## When to Quit

There will be no quitting, unless another book comes out before this, also
targeted explicitly at application-level programming in Haskell, which is judged
by me to be insurmountably superior.

