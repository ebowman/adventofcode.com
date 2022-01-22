package y2020

import scala.annotation.tailrec
import scala.util.Try


trait Day20 {

  case class Tile(id: Int, tf: String = "", bits: Array[Array[Boolean]]) {
    def countPatterns(pattern: IndexedSeq[(Int, Int)]): Int = {
      (for
        perm <- allSymmetries.toSeq
        row <- perm.bits.indices
        col <- perm.bits(row).indices if pattern.forall { case (y, x) => Try(perm.bits(row + y)(col + x)).getOrElse(false) }
      yield 1).sum
    }

    override def equals(any: Any): Boolean = {
      any match {
        case tile: Tile => id == tile.id && tf == tile.tf
      }
    }

    def transform(f: (Int, Int) => Boolean, t: String): Tile = {
      val buffer = Array.ofDim[Boolean](bits.length, bits.length)
      for row <- bits.indices; col <- bits.indices do {
        buffer(col)(row) = f(row, col)
      }
      copy(bits = buffer, tf = tf + t)
    }

    def rotate: Tile = transform((row, col) => bits(bits.length - 1 - row)(col), "R")

    def flipVert: Tile = transform((row, col) => bits(col)(bits.length - 1 - row), "V")

    private def shift(b: Boolean, i: Int): Int = (if b then 1 else 0) << i

    def rotations = Set(this, this.rotate, this.rotate.rotate, this.rotate.rotate.rotate)

    lazy val allSymmetries: Set[Tile] = rotations.flatMap(r => IndexedSeq(r, r.flipVert))

    lazy val left: Int = (for row <- bits.indices yield shift(bits(row)(0), row)).sum
    lazy val right: Int = (for row <- bits.indices yield shift(bits(row)(bits.length - 1), row)).sum
    lazy val top: Int = (for col <- bits.indices yield shift(bits(0)(col), col)).sum
    lazy val bottom: Int = (for col <- bits.indices yield shift(bits(bits.length - 1)(col), col)).sum

    lazy val lefts: Set[Int] = allSymmetries.map(_.left)

    lazy val rights: Set[Int] = allSymmetries.map(_.right)

    lazy val tops: Set[Int] = allSymmetries.map(_.top)

    lazy val bottoms: Set[Int] = allSymmetries.map(_.bottom)

    def isLeftOf(tile: Tile): Boolean = right == tile.left

    def isAbove(tile: Tile): Boolean = bottom == tile.top
  }

  case class Row(tiles: IndexedSeq[Tile]) {

    def containsId(id: Int): Boolean = tiles.exists(_.id == id)

    def last: Tile = tiles.last

    def prune(allTiles: IndexedSeq[Tile]): IndexedSeq[Tile] = allTiles.filterNot(tile => tiles.exists(_.id == tile.id))

    def toTile: Tile = {
      Tile(-1, "", for i <- (1 until tiles.head.bits.length - 1).toArray yield {
        tiles.toArray.flatMap { tile => tile.bits(i).tail.init }
      })
    }
  }

  case class Grid(rows: IndexedSeq[Row] = IndexedSeq.empty) {
    def canAdd(toAdd: Row): Boolean =
      toAdd.tiles.forall(tile => rows.forall(row => !row.containsId(tile.id))) &&
        rows.last.tiles.zip(toAdd.tiles).forall(x => x._1.isAbove(x._2))

    def add(row: Row): Grid = copy(rows = rows :+ row)

    def cornerProduct: Long = Seq(
      rows.head.tiles.head.id,
      rows.head.tiles.last.id,
      rows.last.tiles.head.id,
      rows.last.tiles.last.id).map(_.toLong).product

    def toTile: Tile = {
      rows.map(_.toTile).tail.foldLeft(rows.head.toTile) {
        case (accum, row) => accum.copy(bits = accum.bits ++ row.bits)
      }
    }
  }

  def parse(input: IndexedSeq[String]): IndexedSeq[Tile] = {
    val IdRe = """Tile (\d+):""".r

    @tailrec
    def recurse(i: IndexedSeq[String], tiles: IndexedSeq[Tile] = IndexedSeq.empty): IndexedSeq[Tile] = {
      if i.isEmpty then tiles
      else i.head match {
        case IdRe(id) =>
          recurse(i.drop(12),
            tiles :+ Tile(id.toInt, "", i.tail.take(10).map(line => line.map(c => if c == '.' then false else true)).map(_.toArray).toArray))
      }
    }

    recurse(input)
  }

  @tailrec
  final def recurseAcross(allTiles: IndexedSeq[Tile], accum: IndexedSeq[Row] = IndexedSeq.empty): IndexedSeq[Row] = {
    val goal = math.sqrt(allTiles.size).toInt
    if accum.nonEmpty && accum.head.tiles.size == goal then accum
    else if accum.isEmpty then {
      // find all pairs that can join horizontally, to see the algorithm
      // which then goes
      val next = allTiles.combinations(2).flatMap { comb =>
        for
          IndexedSeq(left, right) <- comb.permutations
          p1 <- left.allSymmetries;
          p2 <- right.allSymmetries if p1.isLeftOf(p2)
        yield Row(IndexedSeq(p1, p2))
      }.toIndexedSeq
      recurseAcross(allTiles, next)
    }
    else {
      // keep adding to row until we have all rows of length dim
      // that stitch together
      val next = for
        row <- accum
        tile <- row.prune(allTiles)
        p <- tile.allSymmetries if row.last.isLeftOf(p)
      yield row.copy(row.tiles :+ p)
      recurseAcross(allTiles, next)
    }
  }

  @tailrec
  final def recurseDown(allRows: IndexedSeq[Row],
                        accum: IndexedSeq[Grid] = IndexedSeq.empty): IndexedSeq[Grid] = {
    if accum.nonEmpty && accum.head.rows.length == allRows.head.tiles.size then accum
    else if accum.isEmpty then {
      val nextGrid: IndexedSeq[Grid] = for
        firstRow <- allRows
        grid = Grid(IndexedSeq(firstRow))
        nextRow <- allRows if grid.canAdd(nextRow)
      yield grid.add(nextRow)
      recurseDown(allRows, nextGrid)
    }
    else {
      val nextGrid = for
        grid <- accum
        nextRow <- allRows if grid.canAdd(nextRow)
      yield grid.add(nextRow)
      recurseDown(allRows, nextGrid)
    }
  }

  def solve(tiles: IndexedSeq[Tile]): IndexedSeq[Grid] = recurseDown(recurseAcross(tiles))

  def part1(input: IndexedSeq[String]): Long = solve(parse(input)).head.cornerProduct

  def part2(input: IndexedSeq[String]): Int = {
    val composite = solve(parse(input)).head.toTile

    val pattern = {
      val seaMonster: IndexedSeq[String] = IndexedSeq(
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   "
      )
      for
        row <- seaMonster.indices
        col <- seaMonster(row).indices if seaMonster(row).charAt(col) == '#'
      yield (row, col)
    }

    val patterns = composite.countPatterns(pattern)
    composite.bits.map(row => row.count(_ == true)).sum - patterns * pattern.size
  }
}
