
package y2023

class Day02Spec extends util.DaySpec(new Day02):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 8

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 2879

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 2286

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 65122
