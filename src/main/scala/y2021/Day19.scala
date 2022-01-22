package y2021

import scala.annotation.tailrec
import collection.mutable

trait Day19 {
  type Coord = (Int, Int, Int)

  class CoordOps(c: Coord) {
    def i(i: Int): Int = if i == 0 then c._1 else if i == 1 then c._2 else c._3

    def manhattan(d: Coord): Int = (0 until 3).map(i => math.abs(c.i(i) - d.i(i))).sum
  }

  given Conversion[Coord, CoordOps] with
    def apply(coord: Coord): CoordOps = new CoordOps(coord)

  val orientations =
    Seq((-1, -1, -1), (-1, -1, 1), (-1, 1, -1), (-1, 1, 1), (1, -1, -1), (1, -1, 1), (1, 1, -1), (1, 1, 1))

  class Counter[A] {
    private val map = new mutable.HashMap[A, Int]().withDefaultValue(0)

    def addAll(items: Iterable[A]): Unit = items.foreach(item => map(item) += 1)

    def find(f: Int => Boolean): Option[A] = map.find(x => f(x._2)).map(_._1)
  }

  case class Scanner(i: Int, beacons: Seq[Coord], position: Option[Coord] = None) {
    def orient(that: Scanner): Option[Scanner] = {
      (for
        orientation <- orientations.iterator
        perm <- IndexedSeq(0, 1, 2).permutations
      yield {
        val counter = new Counter[Coord]()
        counter.addAll(for our <- beacons; their <- that.beacons yield
          (our(0) + orientation(0) * their.i(perm(0)),
            our(1) + orientation(1) * their.i(perm(1)),
            our(2) + orientation(2) * their.i(perm(2))))
        counter.find(_ >= 3).map { candidate => that.reorient(candidate, orientation, perm) } // 12 is overkill
      }).dropWhile(_.isEmpty).nextOption().flatten
    }

    def reorient(pos: Coord, orientation: Coord, perm: IndexedSeq[Int]): Scanner =
      copy(position = Some(pos), beacons = for beacon <- beacons yield
        (pos(0) - orientation(0) * beacon.i(perm(0)),
          pos(1) - orientation(1) * beacon.i(perm(1)),
          pos(2) - orientation(2) * beacon.i(perm(2))))
  }

  def orientScanners(scanners: Seq[Scanner]): Seq[Scanner] = {
    @tailrec def recurse(s: Seq[Scanner]): Seq[Scanner] = {
      if !s.exists(_.position.isEmpty) then s
      else {
        val (known, unknown) = s.partition(_.position.nonEmpty)
        recurse(known.flatMap(k => unknown.flatMap(k.orient)).foldLeft(s) { case (s, u) => s.updated(u.i, u) })
      }
    }

    recurse(scanners)
  }

  def load(input: Seq[String]): Seq[Scanner] = {
    input.mkString("\n").split("\n{2}").zipWithIndex.toIndexedSeq.map { case (scanner, idx) =>
      Scanner(idx, scanner.linesIterator.drop(1).toSeq.map { coord =>
        val Seq(x, y, z) = """-?\d+""".r.findAllIn(coord).toSeq.map(_.toInt)
        (x, y, z)
      }, Some((0, 0, 0)).filter(_ => idx == 0))
    }
  }

  def scanner(input: Seq[String]): Seq[Scanner]

  def solve1(input: Seq[String]): Int = scanner(input).flatMap(_.beacons).distinct.size

  def solve2(input: Seq[String]): Int =
    scanner(input).flatMap(_.position).combinations(2).map(p => p.head.manhattan(p(1))).max
}
