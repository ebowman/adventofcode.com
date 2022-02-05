package y2017

trait Day03:
  type Coord = (Int, Int)

  case class Ring(r: Int):
    def iterator: Iterator[Coord] =
      if (r == 0) Seq((0, 0)).iterator
      else {
        new Iterator[Coord] {
          var cursor = (r, 1 - r)
          var done = false
          var state = 'U'

          override def hasNext: Boolean = !done

          override def next(): Coord =
            val n = cursor
            state match {
              case 'U' =>
                cursor = (cursor._1, cursor._2 + 1)
                if cursor == (r, r) then state = 'L'
              case 'L' =>
                cursor = (cursor._1 - 1, cursor._2)
                if cursor == (-r, r) then state = 'D'
              case 'D' =>
                cursor = (cursor._1, cursor._2 - 1)
                if cursor == (-r, -r) then state = 'R'
              case 'R' =>
                cursor = (cursor._1 + 1, cursor._2)
                if cursor == (r + 1, -r) then done = true
            }
            n
        }
      }

  private def ringIterator = new Iterator[Coord] {
    private var ring = 0
    private var iter = Ring(ring).iterator

    def hasNext: Boolean = true

    def next(): Coord =
      if iter.hasNext then iter.next
      else
        ring += 1
        iter = Ring(ring).iterator
        iter.next
  }

  def solve1(input: Int): Int =
    val coord = ringIterator.drop(input - 1).next()
    math.abs(coord._1) + math.abs(coord._2)

  def solve2(input: Int): Int = {
    import collection.mutable
    val grid = mutable.Map[Coord, Int]().withDefaultValue(0)
    grid((0, 0)) = 1
    ringIterator.drop(1).map { coord =>
      val newValue =
        (for
          i <- -1 to 1
          j <- -1 to 1 if (i, j) != (0, 0)
        yield grid(coord._1 + i, coord._2 + j)).sum
      grid(coord) = newValue
      newValue
    }.dropWhile(_ <= input).next()
  }
end Day03

