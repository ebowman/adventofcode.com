package y2024

class Day22Spec extends util.DaySpec(new Day22):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 37327623

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 15006633487L

  it should "solve part 2 test" in:
    solution.solvePart2(testInput2) shouldBe 23

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1710
