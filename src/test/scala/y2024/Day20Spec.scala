package y2024

class Day20Spec extends util.DaySpec(new Day20):

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1355

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1007335
