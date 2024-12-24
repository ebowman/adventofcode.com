package y2024

class Day24Spec extends util.DaySpec(new Day24):

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 55730288838374L

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe "fvw,grf,mdb,nwq,wpq,z18,z22,z36"
