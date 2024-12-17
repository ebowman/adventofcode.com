package y2024

class Day17Spec extends util.DaySpec(new Day17):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe "4,6,3,5,6,3,5,2,1,0"

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe "1,4,6,1,6,4,3,0,3"

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 265_061_364_597_659L
