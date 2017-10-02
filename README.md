# CS316 "Functional Programming"

Welcome to the webpage for The University of Strathclyde CS316
"Functional Programming"!

This course has a [Twitter account](https://twitter.com/StrathCS316).

*Assessment:* this course is entirely assessed by coursework. There
are four exercises that you will complete (details below). For the
2nd, 3rd, and 4th exercises, you will do two thirds of the exercise at
home or in the labs, and the final third is done in exam conditions in
the lab.

See the [schedule](schedule.txt).

## Contact

**Bob Atkey** LT1305 [robert.atkey@strath.ac.uk](mailto:robert.atkey@strath.ac.uk)

**Fredrik Nordvall Forsberg** LT1310 [fredrik.nordvall-forsberg@strath.ac.uk](mailto:fredrik.nordvall-forsberg@strath.ac.uk)

## Lectures

Lectures are at **11am Mondays** in
[GH816](http://www.learningservices.strath.ac.uk/avfacilities/roomresults.asp?&menu1=Graham%20Hills&roomField=GH816&findRoom=Show+room+details)
and **11am Fridays** in
[LT210](http://www.learningservices.strath.ac.uk/avfacilities/roomresults.asp?&menu1=Graham%20Hills&roomField=GH816&findRoom=Show+room+details). In
week one, there was an additional lecture on **Thursday at 4pm** in
[LT210](http://www.learningservices.strath.ac.uk/avfacilities/roomresults.asp?&menu1=Graham%20Hills&roomField=GH816&findRoom=Show+room+details)
in place of the tutorial normally in that slot.

See the [schedule](schedule.txt) for more details.

Most of the lectures will involve us doing live coding. We will place
the code from each lecture in this repository after each lecture,
interspersed with commentary covering what we talked about.

- [Lecture 1](lectures/Lec01.hs) (with notes) : Data and Pattern Matching
- [Lecture 2](lectures/Lec02.md) (notes in progress) : Standard types and classes. [The GHCi trace of the lecture](lectures/Lec02-trace.txt).
- [Lecture 3](lectures/Lec03.hs) (notes in progress) : Defining functions
- [Lecture 4](lectures/Lec04.hs) (with notes) : List comprehensions
- [Lecture 5](lectures/Lec05.hs) (notes in progress) : Recursive functions
- Lecture 6 : Higher-order functions
- Lecture 7 : Declaring types and classes
- Lecture 8 : QuickCheck

### Tutorials

In addition to the lectures, there are weekly tutorials at **4pm on
Thursdays** in
[LT210](http://www.learningservices.strath.ac.uk/avfacilities/roomresults.asp?&menu1=Graham%20Hills&roomField=GH816&findRoom=Show+room+details). These
are intended for going through some unassessed homework questions that
we will set after the lectures, or for you to ask questions about the
assessed exercises.

### One minute papers

At every lecture and tutorial, we will hand out "One minute papers"
(OMPs) for you to provide us with feedback on the lecture -- what you
have learned in this lecture and what we could have explained
better. At the start of the next lecture, we will go through the OMPs
from last time and try to address the feedback you give us.

Students registered on the course can see their OMPs on the [Marx
system](https://personal.cis.strath.ac.uk/conor.mcbride/Marx/?page=CS316).

## Exercises

As detailed above, this course is entirely assessed by coursework. The
split between the four exercises is shown below:

- Exercise 1 (5%) : [The evaluation
  game](https://personal.cis.strath.ac.uk/robert.atkey/terms.html). Once
  you have finished, enter your username and you will get a
  password. Email this to one of us (email addresses below) by the
  deadline (Thursday 28th September, 4pm).

- Exercise 2 (30%) : [First-order Programming (FOP)](exercises/Ex2.hs). This will be
  released on Thursday 28th September (week 2), and the final deadline
  and test are on Monday 16th October (week 5).

- Exercise 3 (30%) : Higher-order Programming (HOP). This will be
  released on Thursday 12th October (week 4), and the final deadline
  and test are on Monday 6th November (week 8).

- Exercise 4 (35%) : GHOUL. This will be released on Thursday 2nd
  November (week 7), and the final deadline and test are on Monday
  27th November (week 11).

After each of the exercises has been marked, we will email you your
marks, and also put them on the [Marx
system](https://personal.cis.strath.ac.uk/conor.mcbride/Marx/?page=CS316)
for you to see.

### Git commands

To clone a local copy of this git repository, execute

```
git clone https://github.com/bobatkey/CS316-17/
```

Then execute

```
./set-up-gitlab.sh
```

to set up syncing with your personal gitlab account. (This script only
works from your Strathclyde account. Ask us if you want help setting
things up e.g. on your personal computer.)

## Helpful Links

### Other Lecture Courses

These links are to lecture courses by other Universities and
companies. You might find them useful as alternative presentations of
the material in our course.

- Glasgow uni (free!) <abbr title="Massive open online course">MOOC</abbr> on [Functional programming in Haskell](https://www.futurelearn.com/courses/functional-programming-haskell).

- Video lectures by Erik Meijer on [Functional Programming Fundamentals](https://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals).

- Material from [CIS 194: Introduction to Haskell](http://www.seas.upenn.edu/~cis194/fall16/) at the University of Pennsylvania.

### Books

There are now many books written about Haskell. Here are links to some
that we have found useful.

- [Programming in Haskell](http://www.cs.nott.ac.uk/~pszgmh/pih.html)
  is the book that we have based the first half of this course on. You
  do not need to buy the book.

- The [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell). This
  is a free online book that starts very gently, but also includes
  some very advanced material.

- [Learn You a Haskell for Great
  Good!](http://learnyouahaskell.com/). The full text of this book is
  available online for free.

