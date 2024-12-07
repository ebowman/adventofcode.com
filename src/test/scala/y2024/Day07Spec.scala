package y2024

class Day07Spec extends util.DaySpec(new Day07):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 3749

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1_620_690_235_709L

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 11387

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 145_397_611_075_341L
