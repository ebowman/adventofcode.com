package y2024

class Day04Spec extends util.DaySpec(new Day04):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 18

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 2603

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 9

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1965
