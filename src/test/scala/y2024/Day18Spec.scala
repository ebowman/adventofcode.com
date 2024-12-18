package y2024

class Day18Spec extends util.DaySpec(new Day18):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 22

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 336

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe (6, 1)

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe (24,30)
