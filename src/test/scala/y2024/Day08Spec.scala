package y2024

class Day08Spec extends util.DaySpec(new Day08):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 14

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 364

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 34

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1231
