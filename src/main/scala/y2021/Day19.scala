package y2021

trait Day19 {

  type Coord = (Int, Int, Int)
  type Beacon = Coord
  type Position = Coord


  object Ops {
    implicit class CoordOps(c: (Int, Int, Int)) {
      def apply(i: Int): Int = if (i == 0) c._1 else if (i == 1) c._2 else c._3

      def manhattan(d: (Int, Int, Int)): Int = (0 until 3).map(i => math.abs(c(i) - d(i))).sum
    }
  }

  import Ops._

  object Scanner {
    val orientations =
      Seq((-1, -1, -1), (-1, -1, 1), (-1, 1, -1), (-1, 1, 1), (1, -1, -1), (1, -1, 1), (1, 1, -1), (1, 1, 1))
  }

  case class Scanner(i: Int, beacons: Seq[Beacon], position: Option[Position] = None) {

    import Scanner.orientations

    def orient(scanner: Scanner): Option[Scanner] = {
      var that = scanner
      var positions = Map[(Coord, Seq[Int]), Map[Coord, Int]]()
      var done = false
      for {
        orientation <- orientations if !done
        permutations <- Seq(0, 1, 2).permutations if !done
      } {
        val perm = permutations.toIndexedSeq
        var working = Map[(Int, Int, Int), 0]().withDefaultValue(0)
        for (our <- beacons; their <- that.beacons) {
          val key = (
            our(0) + orientation(0) * their(perm(0)),
            our(1) + orientation(1) * their(perm(1)),
            our(2) + orientation(2) * their(perm(2)))
          working = working + (key -> (working(key) + 1))
        }
        positions = positions + ((orientation, perm) -> working)
        for (candidate <- working if !done) {
          if (candidate._2 >= 12) {
            that = that.reorient(candidate._1, orientation, perm)
            done = true
          }
        }
      }
      if (done) Some(that) else None
    }

    def reorient(pos: Coord, orientation: Coord, perm: IndexedSeq[Int]): Scanner = {
      val newBeacons = for (beacon <- beacons) yield
        (pos(0) - orientation(0) * beacon(perm(0)),
          pos(1) - orientation(1) * beacon(perm(1)),
          pos(2) - orientation(2) * beacon(perm(2)))

      copy(position = Some(pos), beacons = newBeacons)
    }
  }

  def orientScanners(scanners: Seq[Scanner]): Seq[Scanner] = {
    var scannerSet = scanners
    while (scannerSet.count(_.position.isEmpty) > 0) {
      val (knowns, unknowns) = scannerSet.partition(_.position.nonEmpty)
      val updates = knowns.flatMap(known => unknowns.flatMap(known.orient))
      scannerSet = updates.foldLeft(scannerSet) {
        case (s, u) => s.updated(u.i, u)
      }
    }
    scannerSet.sortBy(_.i)
  }


  def load(input: Seq[String]): Seq[Scanner] = {
    var running = input
    var i: Int = 0
    var beacons: List[Beacon] = Nil
    var scanners: List[Scanner] = Nil
    while (running.nonEmpty) {
      if (running.head.startsWith("---")) {
        i = """\d+""".r.findFirstIn(running.head).get.toInt
        running = running.tail
      } else if (running.head.trim.isEmpty) {
        if (i == 0) scanners ::= Scanner(i, beacons.reverse, Some((0, 0, 0)))
        else scanners ::= Scanner(i, beacons.reverse)
        beacons = Nil
        running = running.tail
      } else {
        val Array(x, y, z) = """-?\d+""".r.findAllIn(running.head).toArray
        beacons ::= (x.toInt, y.toInt, z.toInt)
        running = running.tail
      }
    }
    if (beacons != Nil) {
      scanners ::= Scanner(i, beacons.reverse)
    }
    scanners.reverse
  }

  def scanner(input: Seq[String]): Seq[Scanner]

  def solve1(input: Seq[String]): Int = scanner(input).map(_.beacons.toSet).reduce(_ union _).size

  def solve2(input: Seq[String]): Int =
    scanner(input).flatMap(_.position).combinations(2).map(p => p.head.manhattan(p(1))).max
}

