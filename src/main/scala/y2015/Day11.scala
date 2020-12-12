package y2015

trait Day11 {
  case class Password(password: String) {
    lazy val has3: Boolean = {
      def check(a: String): Boolean = (a(1) == a(0) + 1) && (a(2) == a(1) + 1)
      password.sliding(3).exists(check)
    }
    lazy val noIllegalChars: Boolean = {
      !password.contains("i") && !password.contains("l") && !password.contains("o")
    }
    lazy val repeatingPair: Boolean = {
      if (password.length > 1) {
        def pair(x: String) = x(0) == x(1)
        val pairs = password.sliding(2).filter(pair).zipWithIndex.toSeq
        pairs.size > 1 && pairs.zip(pairs.tail).filterNot(x => x._1._1 == x._2._1).map(_._1._1).nonEmpty
      } else false
    }
    lazy val valid: Boolean = has3 && noIllegalChars && repeatingPair

    def safeIncr: Password = {
      @scala.annotation.tailrec
      def recurse(pw: Password): Password = {
        if (pw.valid) pw
        else recurse(pw.incr)
      }
      recurse(incr)
    }
    def incr: Password = {
      @scala.annotation.tailrec
      def helper(arr: Array[Char], cur: Int = 0): Array[Char] = {
        if (cur == arr.length) {
         // rolled over
         arr :+ 'b'
        } else {
          if (arr(cur) == 'z') {
            arr(cur) = 'a'
            helper(arr, cur + 1)
          } else {
            arr(cur) = (arr(cur) + 1).toChar
            arr
          }
        }
      }
      Password(new String(helper(password.reverse.toCharArray)).reverse)
    }
  }
}
