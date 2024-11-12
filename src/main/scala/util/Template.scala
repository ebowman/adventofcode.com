package util

def mainSrc(year: String, day: String): String =
  s"""
     |package ${year}
     |
     |trait Day${day} {
     |
     |}
     |""".stripMargin

def mainTest(year: String, day: String): String =
  s"""
     |package ${year}
     |
     |import org.scalatest.flatspec.AnyFlatSpec
     |import org.scalatest.matchers.should._
     |import util.Loader
     |
     |class Day${day}Spec extends AnyFlatSpec with Matchers with Day${day} {
     |
     |  lazy val input: IndexedSeq[String] = Loader(this, "day${day}.txt").toIndexedSeq
     |
     |  it should "solve part 1 test" in {
     |
     |  }
     |
     |  it should "solve part 1" in {
     |
     |  }
     |
     |  it should "solve part 2 test" in {
     |
     |  }
     |
     |  it should "solve part 2" in {
     |
     |  }
     |}
     |""".stripMargin

@main def template(args: String*): Unit =
  val year = s"y${args(0)}"
  val day = args(1)
  val srcPath = s"src/main/scala/$year/Day$day.scala"
  val testPath = s"src/test/scala/$year/Day${day}Spec.scala"
  val rezPath = s"src/main/resources/$year/day$day.txt"
  val mainSrcContent = mainSrc(year, args(1))
  new java.io.PrintWriter(srcPath) { write(mainSrcContent); close() }
  new java.io.PrintWriter(testPath) { write(mainTest(year, args(1))); close() }
  new java.io.PrintWriter(rezPath) { write(""); close() }