package y2024

class Day03Spec extends util.DaySpec(new Day03):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 161

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 183669043

  it should "solve part 2 test" in:
    solution.solvePart2(testInput2) shouldBe 48

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 59097164
