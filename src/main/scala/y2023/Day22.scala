package y2023

// see https://adventofcode.com/2023/day/22
trait Day22:
  case class Point(x: Int, y: Int, z: Int)
  case class Brick(start: Point, end: Point):
    val (minX, maxX) = (math.min(start.x, end.x), math.max(start.x, end.x))
    val (minY, maxY) = (math.min(start.y, end.y), math.max(start.y, end.y))
    val (minZ, maxZ) = (math.min(start.z, end.z), math.max(start.z, end.z))

    def moveDown(n: Int): Brick = Brick(start.copy(z = start.z - n), end.copy(z = end.z - n))

  def simulateFall(bricks: Seq[Brick]): (Array[Brick], Array[Set[Int]], Array[Set[Int]]) =
    val sortedBricks = bricks.sortBy(_.minZ).toArray
    val n = sortedBricks.length
    val supportedBy = Array.fill(n)(Set.empty[Int])
    val supports = Array.fill(n)(Set.empty[Int])

    val heightMap = collection.mutable.HashMap[(Int, Int), (Int, Int)]()

    sortedBricks.indices.foreach { idx =>
      val brick = sortedBricks(idx)
      var maxHeight = 0
      var supporters = Set.empty[Int]

      for
        x <- brick.minX to brick.maxX
        y <- brick.minY to brick.maxY
      do
        heightMap.get((x, y)).foreach { case (height, supportIdx) =>
          if height > maxHeight then
            maxHeight = height
            supporters = Set(supportIdx)
          else if height == maxHeight then
            supporters += supportIdx
        }

      val settledBrick = brick.moveDown(brick.minZ - (maxHeight + 1))
      sortedBricks(idx) = settledBrick
      supportedBy(idx) = supporters

      supporters.foreach { i =>
        supports(i) += idx
      }

      val newHeight = settledBrick.maxZ
      for
        x <- settledBrick.minX to settledBrick.maxX
        y <- settledBrick.minY to settledBrick.maxY
      do
        heightMap((x, y)) = (newHeight, idx)
    }

    (sortedBricks, supportedBy, supports)

  def countFallingBricks(idx: Int, supportedBy: Array[Set[Int]], supports: Array[Set[Int]]): Int =
    val n = supportedBy.length
    val falling = new Array[Boolean](n)
    falling(idx) = true
    var count = 0
    var changed = true

    while changed do
      changed = false
      var i = 0
      while i < n do
        if !falling(i) && supportedBy(i).nonEmpty && supportedBy(i).forall(falling(_)) then
          falling(i) = true
          count += 1
          changed = true
        i += 1

    count

  def setup(input: Seq[String]): (Array[Brick], Array[Set[Int]], Array[Set[Int]]) =
    val bricks = input.map { line =>
      val Array(start, end) = line.split("~")
      val Array(x1, y1, z1) = start.split(",").map(_.toInt)
      val Array(x2, y2, z2) = end.split(",").map(_.toInt)
      Brick(Point(x1, y1, z1), Point(x2, y2, z2))
    }

    simulateFall(bricks)

  def solvePart1(input: Seq[String]): Int =
    val (settled, supportedBy, supports) = setup(input)
    settled.indices.count { idx =>
      supports(idx).forall { supported =>
        supportedBy(supported).size > 1
      }
    }

  def solvePart2(input: Seq[String]): Int =
    val (settled, supportedBy, supports) = setup(input)
    settled.indices.map { idx =>
      countFallingBricks(idx, supportedBy, supports)
    }.sum