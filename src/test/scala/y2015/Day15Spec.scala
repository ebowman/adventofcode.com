package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {
  lazy val input =
    """
      |Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
      |Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
      |Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
      |Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1""".stripMargin.trim.linesIterator.toSeq

  "Basic parsing" should "work" in {
    input.map(parse).head shouldBe Ingredient("Frosting", 4, -2, 0, 0, 5)
  }
  "Cursor" should "fulfill basic properties" in {
    var iter = Cursor(3, 10)
    iter.values.sum shouldBe 10

    @scala.annotation.tailrec
    def recur(i: Cursor): Cursor = {
      i.next match {
        case Some(n) =>
          i.values.sum shouldBe 10
          recur(n)
        case None => i
      }
    }

    recur(iter)
  }
  "Ingredients" should "sum together as specified" in {
    val input =
      """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
        |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin.trim.linesIterator.toSeq
    val ingredients = input.map(parse)

    solve(ingredients, 100) shouldBe 62842880
  }

  it should "solve the first test problem" in {
    solve(input.map(parse), 100) shouldBe 18965440
  }

  it should "solve part 2" in {
    solveCalories(input.map(parse), 100, 500) shouldBe 15862900
  }
}
