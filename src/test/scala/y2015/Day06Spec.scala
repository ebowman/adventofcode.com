package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import y2015.Lights.{BitGrid, InstructionParser, IntGrid, Rect, Toggle, TurnOff, TurnOn}

class Day06Spec extends AnyFlatSpec with Matchers with InstructionParser {
  "Parser" should "pass basic tests" in {
    parseAll(num, "45").get shouldBe 45
    parseAll(pair, "37,1020").get shouldBe(37, 1020)
    parseAll(range, "0,0 through 1000,1001").get shouldBe Rect(0, 0, 1001, 1002)
    parseAll(command, "turn on 887,9 through 959,629").get shouldBe TurnOn(Rect.mkRect(887, 9, 959, 629))
    parseAll(command, "turn off 539,243 through 559,965").get shouldBe TurnOff(Rect.mkRect(539, 243, 559, 965))
    parseAll(command, "toggle 720,196 through 897,994").get shouldBe Toggle(Rect.mkRect(720, 196, 897, 994))
  }

  it should "pass the test case" in {
    val grid = new BitGrid(1000, 1000)
    val instructions = Seq(
      ("turn on 0,0 through 999,999", 1000000),
      ("toggle 0,0 through 999,0", 1000000 - 1000),
      ("turn off 499,499 through 500,500", 1000000 - 1000 - 4)
    )
    instructions.foreach { case (instruction, count) =>
      parseAll(command, instruction).get.operate(grid)
      grid.countTurnedOn() shouldBe count
    }
  }

  it should "pass the final test" in {
    val grid = new BitGrid(1000,1000)
    util.Loader(this, "day06.txt").foreach { instruction =>
      parseAll(command, instruction).get.operate(grid)
    }
    grid.countTurnedOn() shouldBe 377891
  }

  "test2" should "pass the test case" in {
    val grid = new IntGrid(1000, 1000)
    parseAll(command, "turn on 0,0 through 0,0").get.operate(grid).countTurnedOn() shouldBe 1
    parseAll(command, "toggle 0,0 through 999,999").get.operate(grid).countTurnedOn() shouldBe 2000001
    parseAll(command, "turn off 0,0 through 999,999").get.operate(grid).countTurnedOn() shouldBe 1000001
  }

  it should "pass the final test" in {
    val grid = new IntGrid(1000,1000)
    util.Loader(this, "day06.txt").foreach { instruction =>
      parseAll(command, instruction).get.operate(grid)
    }
    grid.countTurnedOn() shouldBe 14110788
  }
}
