package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {

  lazy val code: String = Loader.is(this, "day11.txt").head

  it should "solve part 1" in {
    val robot = new Robot(code)
    robot.run() shouldBe 1681
  }

  it should "solve part 2" in {
    val robot = new Robot(code, initColor = Robot._w)
    robot.run()
    robot.dumpGrid() shouldBe
      "" +
        " XXXX  XX  XXXX  XX  XXX  X  X  XX  X  X   " + "\n" +
        " X    X  X    X X  X X  X X X  X  X X X    " + "\n" +
        " XXX  X      X  X    X  X XX   X    XX     " + "\n" +
        " X    X XX  X   X    XXX  X X  X XX X X    " + "\n" +
        " X    X  X X    X  X X X  X X  X  X X X    " + "\n" +
        " XXXX  XXX XXXX  XX  X  X X  X  XXX X  X   "
  }
}
