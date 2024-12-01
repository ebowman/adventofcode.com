package y2024

class Day01Spec extends util.DaySpec(new Day01):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 11

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1941353

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 31

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 22539317
