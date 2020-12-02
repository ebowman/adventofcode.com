import org.scalatest.funsuite._

class P05Test extends AnyFunSuite {
  import P05._
  test("known inputs") {
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(!isNice("jchzalrnumimnmhp"))
    assert(!isNice("haegwjzuvuyypxyu"))
    assert(!isNice("dvszwmarrgswjxmb"))
  }
}
