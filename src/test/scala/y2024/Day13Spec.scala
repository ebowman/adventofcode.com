package y2024

class Day13Spec extends util.DaySpec(new Day13):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 480

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 35729

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 88584689879723L
