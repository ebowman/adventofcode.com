package y2021

import scala.annotation.tailrec
import scala.util.Try

trait Day18 {

  case class Num(num: Int, depth: Int)

  object NumOps {
    @tailrec def reduceImpl(seq: Seq[Num]): Seq[Num] = {
      val tmp = if seq.canExplode then seq.explode
      else if seq.canSplit then seq.split
      else seq
      if tmp.canReduce then reduceImpl(tmp) else tmp
    }
  }

  implicit class NumOps(seq: Seq[Num]) {

    def canExplode: Boolean = seq.indexWhere(_.depth == 5) != -1

    def canSplit: Boolean = seq.indexWhere(_.num >= 10) != -1

    def canReduce: Boolean = canExplode || canSplit

    def explode: Seq[Num] = {
      val i = seq.indexWhere(_.depth == 5)
      var copy = seq.toArray
      val (left, right) = (copy(i), copy(i + 1))
      Try(copy(i - 1) = copy(i - 1).copy(num = copy(i - 1).num + left.num))
      Try(copy(i + 2) = copy(i + 2).copy(num = copy(i + 2).num + right.num))
      copy = copy.take(i) ++ copy.drop(i + 1)
      copy(i) = Num(0, left.depth - 1)
      copy.toSeq
    }

    def split: Seq[Num] = {
      val i = seq.indexWhere(_.num >= 10)
      var copy = seq.toArray
      copy = copy.take(i + 1) ++ copy.drop(i)
      copy(i + 1) = Num(math.round(copy(i).num / 2f), copy(i).depth + 1)
      copy(i) = Num(copy(i).num / 2, copy(i).depth + 1)
      copy.toSeq
    }

    def |+|(that: Seq[Num]): Seq[Num] =
      NumOps.reduceImpl(seq.map(num => num.copy(depth = num.depth + 1)) ++
        that.map(num => num.copy(depth = num.depth + 1)))

    def magnitude: Int = {
      var copy = seq.toArray
      var maxDepth = copy.maxBy(_.depth).depth
      while maxDepth > 0 do {
        val i = copy.indexWhere(_.depth == maxDepth)
        copy(i) = Num(3 * copy(i).num + Try(2 * copy(i + 1).num).getOrElse(0), copy(i).depth - 1)
        copy = copy.take(i + 1) ++ copy.drop(i + 2)
        maxDepth = copy.maxBy(_.depth).depth
      }
      copy(0).num
    }
  }

  def parse(str: String): Seq[Num] = {
    @tailrec def recurse(in: String, depth: Int = 0, accum: List[Num] = Nil): Seq[Num] = {
      if in.isEmpty then accum.reverse
      else in.head match {
        case '[' => recurse(in.tail, depth + 1, accum)
        case ']' => recurse(in.tail, depth - 1, accum)
        case ',' => recurse(in.tail, depth, accum)
        case n =>
          val digits = in.takeWhile(_.isDigit)
          recurse(in.drop(digits.length), depth, Num(s"$digits".toInt, depth) :: accum)
      }
    }

    recurse(str)
  }

  def solve1(input: Seq[String]): Int = input.map(parse).reduce(_ |+| _).magnitude

  def solve2(input: Seq[String]): Int =
    input.map(parse).combinations(2).flatMap(a => Seq(a, a.reverse)).map(_.reduce(_ |+| _).magnitude).max
}
