package y2020

import scala.util.parsing.combinator.RegexParsers

trait Day02 {

  val parser: InputParser


  /** @return the number of lines that pass the password check */
  def count(inputs: Iterable[String]): Int = {
    inputs.count { input => parser.parseAll(parser.line, input).get }
  }

  trait Rule {
    def check(password: String): Boolean
  }

  // Parser that takes a password rule factory to adapt to the different rules
  class InputParser(mkRule: (String, Int, Int) => Rule) extends RegexParsers {
    override def skipWhitespace = false

    def line: Parser[Boolean] = (rule <~ ": ") ~ "[a-z]+".r ^^ { case rule ~ password => rule.check(password) }

    def rule: Parser[Rule] = (num <~ "-") ~ (num <~ " ") ~ ".".r ^^ { case a ~ b ~ c => mkRule(c, a, b) }

    def num: Parser[Int] =
      """\d+""".r ^^ {
        _.toInt
      }
  }

  object Passwords {


    lazy val parser = new InputParser(Checker.apply)
    lazy val result: Int = count(util.Loader(this, "day02.txt"))

    case class Checker(c: String, min: Int, max: Int) extends Rule {
      def check(password: String): Boolean = {
        val count = password.count(_ == c(0))
        count >= min && count <= max
      }
    }

  }

  /*

Your puzzle answer was 591.

The first half of this puzzle is complete! It provides one gold star: *

--- Part Two ---

While it appears you validated the passwords correctly, they don't seem to be what the
Official Toboggan Corporate Authentication System is expecting.

The shopkeeper suddenly realizes that he just accidentally explained the password policy rules from his old job at
the sled rental place down the street! The Official Toboggan Corporate Policy actually works a little differently.

Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second
character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!)
Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant
for the purposes of policy enforcement.

Given the same example list from above:

1-3 a: abcde is valid: position 1 contains a and position 3 does not.
1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
How many passwords are valid according to the new interpretation of the policies?
 */
  object Passwords2 {

    lazy val result: Int = count(util.Loader(this, "day02.txt"))
    lazy val parser = new InputParser(Checker.apply)

    private def xor(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)

    case class Checker(c: String, min: Int, max: Int) extends Rule {
      def check(password: String): Boolean = {
        xor(password(min - 1) == c(0), password(max - 1) == c(0))
      }
    }

  }

}
