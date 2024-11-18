package y2015

trait Day15:
  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  private val Parser =
  """([a-zA-Z]+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  def parse(str: String): Ingredient = str match
      case Parser(name, cap, dur, flav, text, cal) =>
        Ingredient(name, cap.toInt, dur.toInt, flav.toInt, text.toInt, cal.toInt)

  def solve(ingredients: Seq[Ingredient], totalVolume: Int): Int =
    // Generate only valid combinations that sum to totalVolume
    def generateValidCombinations(remaining: Int, numIngredients: Int): Iterator[List[Int]] =
      if (numIngredients == 1) Iterator(List(remaining))
      else for
        i <- Iterator.range(0, remaining + 1)
        rest <- generateValidCombinations(remaining - i, numIngredients - 1)
      yield i :: rest

    def scoreRecipe(amounts: List[Int]): Int =
      val properties = List(
        ingredients.zip(amounts).map { case (ing, amt) => ing.capacity * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.durability * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.flavor * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.texture * amt }.sum
      )
      if (properties.exists(_ <= 0)) 0 else properties.product

    generateValidCombinations(totalVolume, ingredients.size)
      .map(scoreRecipe)
      .max
  end solve

  def solveCalories(ingredients: Seq[Ingredient], totalVolume: Int, targetCalories: Int): Int =
    def generateValidCombinations(remaining: Int, numIngredients: Int): Iterator[List[Int]] =
      if (numIngredients == 1) Iterator(List(remaining))
      else for
        i <- Iterator.range(0, remaining + 1)
        rest <- generateValidCombinations(remaining - i, numIngredients - 1)
      yield i :: rest

    def scoreRecipeWithCalories(amounts: List[Int]): Int =
      val calories = ingredients.zip(amounts).map { case (ing, amt) => ing.calories * amt }.sum
      if (calories != targetCalories) return 0
      val properties = List(
        ingredients.zip(amounts).map { case (ing, amt) => ing.capacity * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.durability * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.flavor * amt }.sum,
        ingredients.zip(amounts).map { case (ing, amt) => ing.texture * amt }.sum
      )
      if (properties.exists(_ <= 0)) 0 else properties.product

    generateValidCombinations(totalVolume, ingredients.size)
      .map(scoreRecipeWithCalories)
      .max
  end solveCalories
end Day15