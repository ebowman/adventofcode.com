package y2024

class Day06Spec extends util.DaySpec(new Day06):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 41

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 4883

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 6

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1655
