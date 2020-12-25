package y2020

import scala.annotation.tailrec

trait Day13 {

  case class Solver1(input: Iterable[String]) {
    val time: Int = input.head.toInt
    val buses: Array[Bus] = input.tail.head.split(",").filterNot(_ == "x").map(_.toInt).map(Bus.apply)

    case class Bus(n: Int) {
      def solve(time: Int): Int = n - (time % n)
    }

    def solve: Int = {
      val solution = buses.map(b => (b, b.solve(time))).minBy(_._2)
      solution._1.n * solution._2
    }
  }

  object Solver2 {
    def solve(str: String): BigInt = {
      new Solver2(("0\n" + str).linesIterator.toSeq).solve2
    }
  }

  case class Solver2(input: Iterable[String]) {
    val buses: IndexedSeq[Bus] = input.tail.head.split(",").zipWithIndex.filterNot(_._1 == "x").map(
      ab => Bus(ab._1.toInt, ab._2)).toIndexedSeq

    case class Bus(n: Int, i: Int)

    def solve2: BigInt = {
      val na = buses.map { bus =>
        var a = -bus.i
        while (a < 0) {
          a += bus.n
        }
        (bus.n, a)
      }
      crt(n = na.map(_._1).map(BigInt.apply), a = na.map(_._2).map(BigInt.apply))
    }
  }

  // chinese remainder theorem, see
  // https://medium.com/free-code-camp/how-to-implement-the-chinese-remainder-theorem-in-java-db88a3f1ffe0
  def crt(n: Seq[BigInt], a: Seq[BigInt]): BigInt = {
    val prod = n.product

    @tailrec
    def recurse(n: Seq[BigInt], a: Seq[BigInt], sum: BigInt = 0): BigInt = {

      def inverse(a: BigInt, b: BigInt): BigInt = {
        @tailrec
        def euclid(a: BigInt, b: BigInt, x0: BigInt = 0, x1: BigInt = 1): BigInt =
          if (a > 1) euclid(b, a % b, x1 - (a / b) * x0, x0) else x1

        val x1 = euclid(a, b)
        if (x1 < 0) x1 + b else x1
      }

      if (n.isEmpty) sum
      else {
        val p = prod / n.head

        recurse(n.tail, a.tail, sum + a.head * inverse(p, n.head) * p)
      }
    }

    recurse(n, a) % n.product
  }
}