package y2017

trait Day21:
  case class Pattern(grid: Vector[String]):
    def size: Int = grid.size

    def allVariations: Set[Pattern] =
      val rotations = LazyList.iterate(this)(_.rotate).take(4).toSet
      rotations.flatMap(p => Set(p, p.flip))

    def rotate: Pattern =
      val n = size
      val newGrid = (0 until n).map: i =>
        (0 until n).map(j => grid(n - 1 - j)(i)).mkString
      .toVector
      Pattern(newGrid)

    def flip: Pattern =
      Pattern(grid.map(_.reverse))

    def split: Vector[Pattern] =
      val subSize = if size % 2 == 0 then 2 else 3
      val chunks = grid.map(_.grouped(subSize).toVector)
        .grouped(subSize)
        .toVector

      for
        row <- chunks
        col <- row.transpose
      yield Pattern(col)

    def merge(parts: Vector[Pattern], totalSize: Int): Pattern =
      val subSize = parts(0).size
      val rowSize = totalSize / subSize
      val rows = parts.grouped(rowSize).toVector
      val mergedRows = rows.flatMap: rowPatterns =>
        (0 until subSize).map: i =>
          rowPatterns.map(_.grid(i)).mkString
      Pattern(mergedRows)

  object Pattern:
    def parse(s: String): Pattern =
      Pattern(s.split("/").toVector)

  def solve(input: Seq[String], iterations: Int): Int =
    val rules = parseRules(input)
    val initial = Pattern(Vector(".#.", "..#", "###"))

    val result = (1 to iterations).foldLeft(initial): (pattern, _) =>
      enhance(pattern, rules)

    countPixels(result)

  private def parseRules(input: Seq[String]): Map[Pattern, Pattern] =
    input.flatMap: line =>
      val Array(from, to) = line.split(" => ")
      val fromPattern = Pattern.parse(from)
      val toPattern = Pattern.parse(to)
      fromPattern.allVariations.map(_ -> toPattern)
    .toMap

  private def enhance(pattern: Pattern, rules: Map[Pattern, Pattern]): Pattern =
    val parts = pattern.split
    val enhanced = parts.map(p => rules(p))
    val newSize = if pattern.size % 2 == 0 then
      pattern.size / 2 * 3
    else
      pattern.size / 3 * 4
    pattern.merge(enhanced, newSize)

  private def countPixels(pattern: Pattern): Int =
    pattern.grid.map(_.count(_ == '#')).sum
end Day21