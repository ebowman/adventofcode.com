package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {
  lazy val testInput = Loader(this, "day04.test.txt").toSeq
  lazy val input = Loader(this, "day04.txt").toSeq

  "Day04" should "pass the part 1 tests" in {
    val (plays, boards) = loadBoard(testInput)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      val next = playIter.next()
      for (b <- boards) {
        if (b.play(next)) {
          done = true
          score = b.sumUnmarked * next
        }
      }
    }
    score shouldBe 4512
  }

  it should "pass part 1" in {
    val (plays, boards) = loadBoard(input)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      val next = playIter.next()
      for (b <- boards) {
        if (b.play(next)) {
          done = true
          score = b.sumUnmarked * next
        }
      }
    }
    score shouldBe 5685
  }

  it should "pass part 2 test" in {
    var (plays, boards) = loadBoard(testInput)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      var next = playIter.next()
      if (boards.size == 1) {
        val b = boards.head
        while (!done) {
          if (b.play(next)) done = true else next = playIter.next()
        }
        score = b.sumUnmarked * next
      } else {
        for (b <- boards) {
          if (b.play(next)) {
            boards = boards - b
          }
        }
      }
    }
    score shouldBe 1924
  }

  it should "pass part 2" in {
    var (plays, boards) = loadBoard(input)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      var next = playIter.next()
      if (boards.size == 1) {
        val b = boards.head
        while (!done) {
          if (b.play(next)) done = true else next = playIter.next()
        }
        score = b.sumUnmarked * next
      } else {
        for (b <- boards) {
          if (b.play(next)) {
            boards = boards - b
          }
        }
      }
    }
    score shouldBe 21070
  }
}
