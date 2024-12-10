package y2024

class Day10Spec extends util.DaySpec(new Day10):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 36

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 652

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 81

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1432
