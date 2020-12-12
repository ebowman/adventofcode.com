package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {
  "Increments" should "work in basic tests" in {
    Password("a").incr shouldBe Password("b")
    Password("z").incr shouldBe Password("ba")
    Password("ab").incr shouldBe Password("ac")
    Password("abc").incr shouldBe Password("abd")
    Password("abz").incr shouldBe Password("aca")
    Password("zzzzz").incr shouldBe Password("baaaaa")
  }

  "has3" should "meet the basic requirements" in {
    Password("abc").has3 shouldBe true
    Password("abd").has3 shouldBe false
    Password("xyz").has3 shouldBe true
    Password("hijklmmn").has3 shouldBe true
    Password("abbceffg").has3 shouldBe false
  }
  "basic tests" should "pass" in {
    Password("abbceffg").repeatingPair shouldBe true
    Password("abbcegjk").repeatingPair shouldBe false
    Password("abbbcegjk").repeatingPair shouldBe false
    Password("abbbceggjk").repeatingPair shouldBe true
    Password("aaabbbcccddd").repeatingPair shouldBe true

    Password("abcdefgh").safeIncr shouldBe Password("abcdffaa")
    Password("ghijklmn").safeIncr shouldBe Password("ghjaabcc")
  }
  it should "solve the first part" in {
    Password("hxbxwxba").safeIncr shouldBe Password("hxbxxyzz")
    Password("hxbxxyzz").safeIncr shouldBe Password("hxcaabcc")
  }
}
