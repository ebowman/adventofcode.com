package y2024

class Day12Spec extends util.DaySpec(new Day12):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 1930

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1359028

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 1206

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 839780
