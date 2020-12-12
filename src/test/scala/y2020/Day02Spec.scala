package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class ParserSpec extends AnyFlatSpec with Matchers with y2020.Day02 {

  val parser = new InputParser(AllCapsChecker.apply)

  case class AllCapsChecker(c: String, a: Int, b: Int) extends Rule {
    def check(password: String): Boolean = password.capitalize == password
  }

  "Parsing" should "pass basic tests" in {
    parser.parseAll(parser.num, "2").get shouldBe 2
    parser.parseAll(parser.rule, "1-3 a").get.check("HELLO") shouldBe true
  }
}

class PasswordSpec extends AnyFlatSpec with Matchers with y2020.Day02 {

  lazy val input: Seq[String] =
    """
      |1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin.split("\n").filter(_.nonEmpty).toSeq
  val parser = Passwords.parser.asInstanceOf[InputParser]

  "Checker" should "pass basic tests" in {
    count(input) shouldBe 2
  }

  it should "solve the final puzzle" in {
    Passwords.result shouldBe 591
  }
}

class Password2Spec extends AnyFlatSpec with Matchers with Day02 {

  lazy val input: Seq[String] =
    """
      |1-3 a: abcde
      |1-3 b: cdefg
      |2-9 c: ccccccccc
      |""".stripMargin.split("\n").filter(_.nonEmpty).toSeq

  val parser = Passwords2.parser.asInstanceOf[InputParser]

  "Checker2" should "pass the examples" in {
    count(input) shouldBe 1
  }

  it should "solve the final puzzle" in {
    Passwords2.result shouldBe 335
  }
}
