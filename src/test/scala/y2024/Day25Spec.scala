package y2024

class Day25Spec extends util.DaySpec(new Day25):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 3

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 2854

