import org.scalatest.FunSuite

class P05Test extends FunSuite {
  import P05._
  test("known inputs") {
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(!isNice("jchzalrnumimnmhp"))
    assert(!isNice("haegwjzuvuyypxyu"))
    assert(!isNice("dvszwmarrgswjxmb"))
  }
}
