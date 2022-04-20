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


TODO

containers
functors
applicatives
