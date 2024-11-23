package y2022

trait Day03:
  def priority(c: Char): Int =
    if c.isLower then c - 'a' + 1
    else c - 'A' + 27

  def findCommonItem(rucksack: String): Char =
    val (comp1, comp2) = rucksack.splitAt(rucksack.length / 2)
    comp1.toSet.intersect(comp2.toSet).head

  def findBadge(group: Seq[String]): Char =
    group
      .map(_.toSet)
      .reduce(_ intersect _)
      .head

  def solvePart1(input: Seq[String]): Int =
    input.map(findCommonItem).map(priority).sum

  def solvePart2(input: Seq[String]): Int =
    input
      .grouped(3)
      .map(findBadge)
      .map(priority)
      .sum
end Day03
