package y2020

import scala.annotation.tailrec

trait Day23 {

  def part1(input: Int): Int = {
    @tailrec def goCrabby(cups: Seq[Int], n: Int): Seq[Int] = {
      if (n == 0) cups
      else {
        val remaining = cups.drop(4)

        @tailrec def nextDestIdx(cup: Int): Int =
          if (remaining.contains(cup)) remaining.indexOf(cup)
          else if (cup - 1 < cups.min) nextDestIdx(cups.max)
          else nextDestIdx(cup - 1)

        val leftCount = nextDestIdx(cups.head) + 1
        goCrabby(remaining.take(leftCount) ++ cups.slice(1, 4) ++ remaining.drop(leftCount) :+ cups.head, n - 1)
      }
    }

    @tailrec def rotate1toHead(seq: Seq[Int]): Seq[Int] =
      if (seq.head == 1) seq
      else rotate1toHead(seq.tail :+ seq.head)

    rotate1toHead(goCrabby(input.toString.map(_ - '0'), 100)).tail.mkString.toInt
  }

  def part2(input: Int): Long = {
    val cups = {
      val nums = input.toString.map(_ - '0')
      val tmp = LazyList.from(1).take(1000001).toArray
      tmp(0) = nums.head
      tmp(1000000) = nums.head
      nums.zip(nums.tail).foreach(curnext => tmp(curnext._1) = curnext._2)
      tmp(nums.last) = 10
      tmp
    }

    @tailrec def gocrabby(current: Int, n: Int = 10000000): Unit = {
      if (n == 0) ()
      else {
        val (first, middle, last) = (cups(current), cups(cups(current)), cups(cups(cups(current))))

        @tailrec def nextLabel(label: Int): Int = {
          val maybe = if (label == 1) 1000000 else label - 1
          if (maybe == first || maybe == middle || maybe == last) nextLabel(maybe) else maybe
        }

        val dest = nextLabel(current)
        cups(current) = cups(last)
        cups(last) = cups(dest)
        cups(dest) = first
        gocrabby(cups(current), n - 1)
      }
    }

    gocrabby(cups(0))
    cups(cups(0)).toLong * cups(cups(cups(0)))
  }
}
