package y2017

trait Day15:
  // Constants for the generators
  private val factorA = 16807L
  private val factorB = 48271L
  private val divisor = 2147483647L
  private val mask16 = (1L << 16) - 1  // Mask for lowest 16 bits

  private def parseInput(input: Seq[String]): (Long, Long) =
    val numbers = input.map(_.split(" ").last.toLong)
    (numbers(0), numbers(1))

  private def generateValues(start: Long, factor: Long): LazyList[Long] =
    LazyList.iterate(start)(prev => (prev * factor) % divisor)

  private def matchesLow16Bits(a: Long, b: Long): Boolean =
    (a & mask16) == (b & mask16)

  def solvePart1(input: Seq[String]): Int =
    val (startA, startB) = parseInput(input)
    val genA = generateValues(startA, factorA)
    val genB = generateValues(startB, factorB)

    // Take 40 million pairs and count matches
    genA.zip(genB)
      .take(40_000_000)
      .count((a, b) => matchesLow16Bits(a, b))

  def solvePart2(input: Seq[String]): Int =
    val (startA, startB) = parseInput(input)

    // Filter generators according to part 2 rules
    val genA = generateValues(startA, factorA).filter(_ % 4 == 0)
    val genB = generateValues(startB, factorB).filter(_ % 8 == 0)

    // Take 5 million pairs and count matches
    genA.zip(genB)
      .take(5_000_000)
      .count((a, b) => matchesLow16Bits(a, b))
end Day15