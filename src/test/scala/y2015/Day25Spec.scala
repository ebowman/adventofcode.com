package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day25Spec extends AnyFlatSpec with Matchers {
  "Cursor" should "behave as expected" in {

    // Infinite stream of cursors, not really efficient enough for solving the problem,
    // but it makes the tests easier
    lazy val cursors: Stream[Cursor] =
    Cursor.origin #:: Cursor.origin.next #:: cursors.zip(cursors.tail).map {
      _._2.next
    }
    cursors.take(6) shouldEqual Seq(
      Cursor(1, 1), Cursor(2, 1), Cursor(1, 2), Cursor(3, 1), Cursor(2, 2), Cursor(1, 3))
  }

  /*
     |    1         2         3         4         5         6
---+---------+---------+---------+---------+---------+---------+
 1 | 20151125  18749137  17289845  30943339  10071777  33511524
 2 | 31916031  21629792  16929656   7726640  15514188   4041754
 3 | 16080970   8057251   1601130   7981243  11661866  16474243
 4 | 24592653  32451966  21345942   9380097  10600672  31527494
 5 |    77061  17552253  28094349   6899651   9250759  31663883
 6 | 33071741   6796745  25397450  24659492   1534922  27995004
   */
  "Generator" should "behave as prescribed" in {
    val values = Seq(
      Generator(Cursor(1, 1), 20151125),
      Generator(Cursor(2, 1), 31916031),
      Generator(Cursor(3, 1), 16080970),
      Generator(Cursor(4, 1), 24592653),
      Generator(Cursor(5, 1), 77061),
      Generator(Cursor(6, 1), 33071741),
      Generator(Cursor(1, 2), 18749137),
      Generator(Cursor(2, 2), 21629792),
      Generator(Cursor(3, 2), 8057251),
      Generator(Cursor(4, 2), 32451966),
      Generator(Cursor(5, 2), 17552253),
      Generator(Cursor(6, 2), 6796745),
      Generator(Cursor(1, 3), 17289845),
      Generator(Cursor(2, 3), 16929656),
      Generator(Cursor(3, 3), 1601130),
      Generator(Cursor(4, 3), 21345942),
      Generator(Cursor(5, 3), 28094349),
      Generator(Cursor(6, 3), 25397450),
      Generator(Cursor(1, 4), 30943339),
      Generator(Cursor(2, 4), 7726640),
      Generator(Cursor(3, 4), 7981243),
      Generator(Cursor(4, 4), 9380097),
      Generator(Cursor(5, 4), 6899651),
      Generator(Cursor(6, 4), 24659492),
      Generator(Cursor(1, 5), 10071777),
      Generator(Cursor(2, 5), 15514188),
      Generator(Cursor(3, 5), 11661866),
      Generator(Cursor(4, 5), 10600672),
      Generator(Cursor(5, 5), 9250759),
      Generator(Cursor(6, 5), 1534922),
      Generator(Cursor(1, 6), 33511524),
      Generator(Cursor(2, 6), 4041754),
      Generator(Cursor(3, 6), 16474243),
      Generator(Cursor(4, 6), 31527494),
      Generator(Cursor(5, 6), 31663883),
      Generator(Cursor(6, 6), 27995004)
    )

    // this is inefficient but helpful to confirm correctness
    lazy val cursors: Stream[Generator] =
      Generator.origin #:: Generator.origin.next #:: cursors.zip(cursors.tail).map {
        _._2.next
      }

    for (v <- values) {
      cursors.contains(v) shouldBe true
    }
  }

  it should "continue to solve for our specific input" in {
    val dest = Cursor(3010, 3019)

    @scala.annotation.tailrec
    def recurse(gen: Generator = Generator.origin): Generator = {
      if (gen.cursor == dest) gen
      else recurse(gen.next)
    }

    recurse().value shouldBe 8997277
  }
}
