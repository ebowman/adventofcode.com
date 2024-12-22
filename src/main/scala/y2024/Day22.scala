package y2024

// see https://adventofcode.com/2024/day/22
class Day22 extends util.Day(22):

  override def solvePart1(input: IndexedSeq[String]): Long =
    input
      .map(line => processBuyer(line.toLong, 2000)._1)
      .sum

  override def solvePart2(input: IndexedSeq[String]): Long =
    val globalMap = input.foldLeft(Map[(Long, Long, Long, Long), Long]().withDefaultValue(0L)):
      case (acc, line) =>
        val (_, localMap) = processBuyer(line.toLong, 2000)
        localMap.foldLeft(acc):
          case (innerAcc, (deltas, firstPrice)) =>
            innerAcc + (deltas -> (innerAcc(deltas) + firstPrice))

    globalMap.values.max

  private def processBuyer(initialSecret: Long, steps: Int): (Long, Map[(Long, Long, Long, Long), Long]) =
    val secrets = generateSecrets(initialSecret, steps)
    val prices = secrets.map(_ % 10)

    // part 1
    val lastSecret = secrets.last

    // part 2
    val localMap = (4 until secrets.length).foldLeft(Map[(Long, Long, Long, Long), Long]()):
      case (acc, i) =>
        val deltas = (
          prices(i - 3) - prices(i - 4),
          prices(i - 2) - prices(i - 3),
          prices(i - 1) - prices(i - 2),
          prices(i) - prices(i - 1)
        )
        if !acc.contains(deltas) then acc + (deltas -> prices(i))
        else acc

    (lastSecret, localMap)

  private def generateSecrets(initialSecret: Long, steps: Int): Vector[Long] =
    (1 to steps).foldLeft(Vector(initialSecret)):
      case (acc, _) =>
        acc :+ nextSecret(acc.last)

  private inline def nextSecret(num: Long): Long =
    val step1 = prune(mix(num, num * 64))
    val step2 = prune(mix(step1, step1 / 32))
    val step3 = prune(mix(step2, step2 * 2048))
    step3

  private inline def mix(a: Long, b: Long): Long = a ^ b

  private inline def prune(x: Long): Long = x % 16777216L

end Day22
