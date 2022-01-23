# adventofcode.com

This repo holds my solutions to [Advent of Code](https://adventofcode.com), an annual global coding competition every
December 1-25. I have participated in 2020 and 2021, and been gradually backfilling past years.

This repo is in scala, as of this writing updated to [Scala 3](https://docs.scala-lang.org/scala3/). Why scala? Well,
Scala is the last programming language I programmed professional in, and I happen to love the language. For me, it hits
a sweet spot with powerful libraries and expressiveness. Of course,
[not everyone loves scala](https://www.google.com/search?q=scala+sucks), but that ok.

Now, while I am and have tried to compete, I've never made the leader board, probably not even that close. I'm not
really trying to compete; instead, I find the quality of these problems extremely high, and they cover a wide-range of
computer science. So working through these, you learn a lot. I'd also say working through them would be a good way to
get up to speed in a new programming language.

## How to build

You'll need a working scala development environment, which basically means you need the
[Scala SDK](https://www.scala-lang.org/download/scala3.html) and [sbt](https://www.scala-sbt.org/download.html). Getting
that running is an exercise for the reader.

Once that's probably working, you can clone this repo, cd into it, and run `sbt`. Doing so will bring up the sbt shell;
there if you type `test` and hit enter, it will compile all the code and run the tests.

There is a pattern for each day of each challenge: the hard work is generally done
in `src/main/scala/y[year]/Day[day].scala`, and the code that exercises the solution and confirms that the correct
answer remains correct is a parallel [ScalaTest](https://www.scalatest.org) test
suite, `src/test/scala/y[year]/Day[day]Spec.scala`. Often there is also a file `src/main/resources/y[year]/day[day].txt`
holding the test input from the contests.

Some folks out there have amazing rigs for integrating with the test site, and are optimized for getting the answer in
an automated way and as quickly as possible -- I haven't headed down that road.

Rather, my focus has been on trying to create the tightest possible code. This makes it basically unreadable in many
cases, and quite elegant (IMO) in others. I'm not really writing this to be consumed by anyone, though if someone does
find it helpful, it would be good to hear from you.

[![CircleCI](https://circleci.com/gh/ebowman/adventofcode.com.svg?style=svg)](https://circleci.com/gh/ebowman/adventofcode.com)
