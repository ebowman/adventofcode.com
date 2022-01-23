package y2015

import scala.annotation.tailrec

trait Day11 {
  def threeInARow(a: Array[Char]): Boolean = (a(1) == a(0) + 1) && (a(2) == a(1) + 1)

  def has3(password: Array[Char]): Boolean = password.sliding(3).exists(threeInARow)

  def noIllegalChars(password: Array[Char]): Boolean = !password.exists(c => c == 'i' || c == 'l' || c == 'o')

  def twoInARow(x: Array[Char]): Boolean = x(0) == x(1)

  def repeatingPair(password: Array[Char]): Boolean =
    password.length < 2 ||
      password.sliding(2).filter(twoInARow).sliding(2).exists(x => x.size > 1 && !(x.head sameElements x(1)))

  def valid(password: Array[Char]): Boolean =
    noIllegalChars(password) && has3(password) && repeatingPair(password)

  @tailrec final def nextPassword(password: Array[Char]): Array[Char] =
    nextRotation(password.reverse).reverse match {
      case next if valid(next) => next
      case next => nextPassword(next)
    }

  @tailrec final def nextRotation(arr: Array[Char], cur: Int = 0): Array[Char] = {
    if cur == arr.length then arr :+ 'b'
    else if arr(cur) == 'z' then
      arr(cur) = 'a'
      nextRotation(arr, cur + 1)
    else
      arr(cur) = (arr(cur) + 1).toChar
      arr
  }

  def solve(input: String): String = new String(nextPassword(input.toCharArray))
}
