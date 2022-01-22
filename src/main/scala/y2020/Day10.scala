package y2020

import scala.annotation.tailrec

trait Day10 {

  def solve(myAdapters: IndexedSeq[Int]): Int = {
    val sorted = 0 +: (myAdapters.sorted :+ (myAdapters.max + 3))
    val zipped = sorted.zip(sorted.tail)
    val d1 = zipped.count(ab => ab._2 - ab._1 == 1)
    val d3 = zipped.count(ab => ab._2 - ab._1 == 3)
    d1 * d3
  }

  def solve2(myAdapters: IndexedSeq[Int]): Long = {
    val sorted = (0 +: myAdapters.sorted).map(_.toLong)
    val deltas = sorted.zip(sorted.tail).map(x => x._2 - x._1).mkString
    lazy val tribSeq = {
      lazy val seq: LazyList[Long] = 0L #:: 0L #:: 1L #:: seq.zip(seq.tail.zip(seq.tail.tail)).map {
        case (a, (b, c)) => a + b + c
      }
      seq.drop(4)
    }

    @tailrec
    def recurse(accum: Seq[Long] = Seq.empty, trib: Seq[Long] = tribSeq, count: Int = 0): Long = {
      def countReps(in: String, rep: String): Int =
        s"""${rep(0)}+""".r.findAllMatchIn(in).count(_.toString.length == rep.length)

      if count == deltas.length then accum.product
      else {
        val reps = countReps(deltas, "1" * (count + 2))
        if reps == 0 then recurse(accum, trib.tail, count + 1)
        else recurse(accum :+ math.pow(trib.head.toDouble, reps).toLong, trib.tail, count + 1)
      }
    }

    recurse()
  }
}
