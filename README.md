# adventofcode.com

This repo holds my solutions to [Advent of Code](https://adventofcode.com), an annual global coding competition every
December 1-25. I occasionally compete and occasionally back-fill old solutions.

This repo is in Scala, now updated to [Scala 3](https://docs.scala-lang.org/scala3/). Why Scala? Well,
Scala is the last programming language I programmed professionally in, and I love it. It hits
a sweet spot with powerful libraries and expressiveness. Of course,
[not everyone loves scala](https://www.google.com/search?q=scala+sucks), but that ok.

## How to build

You'll need a working Scala development environment, which means you need the
[Scala SDK](https://www.scala-lang.org/download/scala3.html) and [sbt](https://www.scala-sbt.org/download.html). Getting
that running is an exercise for the reader. Maybe `brew install scala` and `brew install sbt` are what you need.

Once that's probably working, you can clone this repo, `cd` into it, and run `sbt`. Doing so will bring up the sbt shell;
If you type `test` and hit enter, it will compile all the code and run the tests.

Over time, I evolved a pattern for each day of each challenge: the hard work is generally done
in `src/main/scala/y[year]/Day[day].scala` and the code that exercises the solution and confirms that the correct
answer remains correct is a parallel [ScalaTest](https://www.scalatest.org) test
suite, `src/test/scala/y[year]/Day[day]Spec.scala`.

To take part in the challenge, either during a contest or during the rest of the year, you create an account. The Advent of Code
website generates custom input data for you and asks you to compute the solution based on that data. Thus, each
user has a custom (or near-custom) output for their program that depends on their input data. Usually, there is a smaller
test data set and a bigger "real" one. By convention, I put the test data in `src/main/resources/y[year]/day[day].test.txt`
and the real data set in `src/main/resource/y[year]/day[day].txt`.

If you log in to https://adventofcode.com, capture the session cookie, and put the value in a file `.session` within the
project (notice I've put `.session` in `.gitignore`), there is a little feature that makes it quicker to compete. If you type:

`sbt "run 2017 08"`

for example, it will create the necessary boilerplate to quickly start working on Day 8 for the year 2017, including downloading
the test data and put it in the file.

[![CircleCI](https://circleci.com/gh/ebowman/adventofcode.com.svg?style=svg)](https://circleci.com/gh/ebowman/adventofcode.com)
