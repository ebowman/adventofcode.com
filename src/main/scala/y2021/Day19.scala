package y2021

import scala.annotation.tailrec
import scala.language.implicitConversions

trait Day19:
  type Coord = (Int, Int, Int)

  extension (c: Coord)
    def i(i: Int): Int =
      if i == 0 then c._1 else if i == 1 then c._2 else c._3

    def manhattan(d: Coord): Int =
      (0 until 3).map(i => math.abs(c.i(i) - d.i(i))).sum

  private val orientations =
    Seq(
      (-1, -1, -1), (-1, -1,  1), (-1,  1, -1), (-1,  1,  1),
      ( 1, -1, -1), ( 1, -1,  1), ( 1,  1, -1), ( 1,  1,  1)
    )

  case class Scanner(
                      idx: Int,
                      beacons: Seq[Coord],
                      position: Option[Coord] = None
                    ):
    def orient(that: Scanner): Option[Scanner] =
      orientations.iterator.flatMap { orientation =>
        IndexedSeq(0, 1, 2).permutations.flatMap { perm =>
          val translationCounts =
            (for
              our   <- beacons
              their <- that.beacons
            yield (
              our(0) + orientation(0) * their.i(perm(0)),
              our(1) + orientation(1) * their.i(perm(1)),
              our(2) + orientation(2) * their.i(perm(2))
            ))
              .groupMapReduce(identity)(_ => 1)(_ + _)

          // e.g. if we need 12 or more overlaps
          translationCounts.collectFirst {
            case (candidate, count) if count >= 12 =>
              that.reorient(candidate, orientation, perm)
          }
        }
      }.nextOption()

    def reorient(pos: Coord, orientation: Coord, perm: IndexedSeq[Int]): Scanner =
      copy(
        position = Some(pos),
        beacons = beacons.map { beacon =>
          (
            pos(0) - orientation(0) * beacon.i(perm(0)),
            pos(1) - orientation(1) * beacon.i(perm(1)),
            pos(2) - orientation(2) * beacon.i(perm(2))
          )
        }
      )

  def orientScanners(scanners: Seq[Scanner]): Seq[Scanner] =
    @tailrec
    def recurse(current: Seq[Scanner]): Seq[Scanner] =
      val (known, unknown) = current.partition(_.position.isDefined)
      if unknown.isEmpty then current
      else
        val newlyOriented = for
          k <- known
          u <- unknown
          oriented <- k.orient(u)
        yield oriented

        if newlyOriented.isEmpty then current
        else
          val updated = newlyOriented.foldLeft(current) {
            case (acc, sc) => acc.updated(sc.idx, sc)
          }
          recurse(updated)

    recurse(scanners)

  def load(input: Seq[String]): Seq[Scanner] =
    input.mkString("\n").split("\n{2}").zipWithIndex.toIndexedSeq.map {
      case (scannerBlock, idx) =>
        val coords = scannerBlock
          .linesIterator
          .drop(1)
          .map { line =>
            val Seq(x, y, z) = """-?\d+""".r.findAllIn(line).toSeq.map(_.toInt)
            (x, y, z)
          }
          .toSeq
        Scanner(idx, coords, position = if idx == 0 then Some((0,0,0)) else None)
    }

  def scanner(input: Seq[String]): Seq[Scanner] =
    orientScanners(load(input))

  def solve1(input: Seq[String]): Int =
    scanner(input).flatMap(_.beacons).distinct.size

  def solve2(input: Seq[String]): Int =
    val positions = scanner(input).flatMap(_.position)
    positions.combinations(2).map {
      case Seq(a, b) => a.manhattan(b)
    }.max
