package y2024

class Day21Spec extends util.DaySpec(new Day21):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 126384

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 132532

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 154115708116294L

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 165644591859332L
