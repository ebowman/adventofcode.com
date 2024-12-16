package y2024

class Day16Spec extends util.DaySpec(new Day16):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 7036
    solution.solvePart1(testInput2) shouldBe 11048

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 95476

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 45
    solution.solvePart2(testInput2) shouldBe 64

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 511
