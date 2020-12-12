package y2015

import scala.annotation.tailrec

/*
Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1
 */
trait Day15 {

  val Parser = """([a-zA-Z]+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  def parse(str: String): Ingredient = {
    str match {
      case Parser(name, cap, dur, flav, text, cal) =>
        Ingredient(name, cap.toInt, dur.toInt, flav.toInt, text.toInt, cal.toInt)
    }
  }

  def solve(ingredients: Seq[Ingredient], totalVolume: Int): Int = {
    @tailrec
    def recurse(c: Option[Cursor], max: Int = 0): Int = {
      c match {
        case None => max
        case Some(crs) =>
          val newMax = Math.max(max, combine(ingredients, crs)) // compiler bug! temp var required!
          recurse(crs.next, newMax)
      }
    }

    recurse(Some(Cursor(ingredients.size, totalVolume)))
  }

  def combine(ings: Seq[Ingredient], cursor: Cursor): Int = sumUp(ings.zip(cursor.values))

  def solveCalories(ingredients: Seq[Ingredient], totalVolume: Int, calories: Int): Int = {
    @tailrec
    def recurse(c: Option[Cursor], max: Int = 0): Int = {
      c match {
        case None => max
        case Some(crs) =>
          val newMax = Math.max(max, combineCalories(ingredients, crs, calories))
          recurse(crs.next, newMax)
      }
    }

    recurse(Some(Cursor(ingredients.size, totalVolume)))
  }

  def combineCalories(ings: Seq[Ingredient], cursor: Cursor, exactCalories: Int): Int = {
    val tmp = ings.zip(cursor.values)
    val calories = (tmp.map { case (ing, amt) => ing.calories * amt }).sum
    if (calories == exactCalories) sumUp(tmp)
    else 0
  }

  private def sumUp(ingredients: Seq[(Ingredient, Int)]) = {
    def ck(x: Int) = Math.max(x, 0)

    ck((ingredients.map { case (ing, amt) => ing.capacity * amt }).sum) *
      ck((ingredients.map { case (ing, amt) => ing.durability * amt }).sum) *
      ck((ingredients.map { case (ing, amt) => ing.flavor * amt }).sum) *
      ck((ingredients.map { case (ing, amt) => ing.texture * amt }).sum)
  }

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  case class Cursor(sum: Int, values: Array[Int]) {

    def next: Option[Cursor] = {
      @scala.annotation.tailrec
      def recur(n: Cursor): Cursor = {
        if (n.isDone || n.isValid) n
        else recur(n.incr)
      }

      recur(this.incr) match {
        case r if r.isValid => Some(r)
        case _ => None
      }
    }

    private def isValid: Boolean = values.sum == sum

    private def isDone: Boolean = values.forall(_ == sum)

    private def incr: Cursor = {
      @scala.annotation.tailrec
      def helper(arr: Array[Int], cur: Int = 0): Array[Int] = {
        if (arr(cur) == sum) {
          arr(cur) = 0
          helper(arr, cur + 1)
        } else {
          arr(cur) = arr(cur) + 1
          arr
        }
      }

      Cursor(sum, helper(values))
    }
  }

  object Cursor {
    def apply(n: Int, sum: Int): Cursor =
      Cursor(sum, new Array[Int](n)).next.get
  }
}