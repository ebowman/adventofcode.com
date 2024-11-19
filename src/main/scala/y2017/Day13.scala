package y2017

trait Day13:
  case class Layer(depth: Int, range: Int):
    def scannerPosition(time: Int): Int =
      if range == 0 then 0
      else
        val period = 2 * (range - 1)
        val pos = time % period
        if pos < range then pos else period - pos

    def isScannerAtTop(time: Int): Boolean = scannerPosition(time) == 0

    def severity: Int = depth * range
  end Layer

  def parseInput(input: Seq[String]): Seq[Layer] =
    input.map: line =>
      val parts = line.split(": ").map(_.trim.toInt)
      Layer(parts(0), parts(1))
    .sortBy(_.depth)

  def isCaught(layer: Layer, delay: Int): Boolean =
    layer.isScannerAtTop(delay + layer.depth)

  def solvePart1(input: Seq[String]): Int =
    val layers = parseInput(input)
    layers.filter(l => isCaught(l, 0)).map(_.severity).sum

  def solvePart2(input: Seq[String]): Int =
    val layers = parseInput(input)

    def canPassAt(delay: Int): Boolean =
      !layers.exists(l => isCaught(l, delay))

    Iterator.from(0).find(canPassAt).getOrElse(0)
end Day13