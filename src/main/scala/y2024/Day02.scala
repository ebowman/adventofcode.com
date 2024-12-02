package y2024

class Day02 extends util.Day(2):
  private def isValidSequence(nums: Seq[Int]): Boolean =
    if nums.length < 2 then true
    else
      val pairs = nums.sliding(2).toSeq

      val differences = pairs.map(p => p(1) - p.head)
      val allIncreasing = differences.forall(_ > 0)
      val allDecreasing = differences.forall(_ < 0)

      val validDifferences = differences.forall: d =>
        d.abs >= 1 && d.abs <= 3

      (allIncreasing || allDecreasing) && validDifferences

  private def isValidWithDampener(nums: Seq[Int]): Boolean =
    if isValidSequence(nums) then true
    else
      nums.indices.exists: i =>
        val withoutOne = nums.patch(i, Seq(), 1)
        isValidSequence(withoutOne)

  def solvePart1(input: IndexedSeq[String]): Int =
    input
      .map(_.split(" ").map(_.toInt).toSeq)
      .count(isValidSequence)

  def solvePart2(input: IndexedSeq[String]): Int =
    input
      .map(_.split(" ").map(_.toInt).toSeq)
      .count(isValidWithDampener)
    
end Day02