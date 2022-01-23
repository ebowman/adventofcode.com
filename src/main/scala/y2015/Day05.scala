package y2015

trait Day05 {
  lazy val disallowed = Set("ab", "cd", "pq", "xy")

  @inline private def isVowel(char: Char): Boolean =
    char == 'a' || char == 'e' || char == 'i' || char == 'o' || char == 'u'

  def isNice(str: String): Boolean =
    str.sliding(2).exists(ab => ab.head == ab(1)) &&
      str.count(isVowel) >= 3 &&
      disallowed.forall(d => !str.contains(d))

  def isNice2(str: String): Boolean =
    (0 until str.length - 3).exists(i => str.substring(i + 2).contains(str.substring(i, i + 2))) &&
      (0 until str.length - 2).exists(i => str(i) == str(i + 2))
}
