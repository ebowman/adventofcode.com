package y2024

class Day11Spec extends util.DaySpec(new Day11):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 55312

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 211306

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 250783680217283L
