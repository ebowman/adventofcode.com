package y2024

class Day14Spec extends util.DaySpec(new Day14):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 12

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 230435667

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 7709
