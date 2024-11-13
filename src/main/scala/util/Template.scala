package util

import java.io.{BufferedReader, PrintWriter}
import java.nio.file.{Files, Paths}

def mainSrc(year: String, day: String): String =
  s"""package y${year}
     |
     |// see https://adventofcode.com/$year/day/${day.toInt}
     |trait Day${day} {
     |
     |}
     |""".stripMargin

def mainTest(year: String, day: String): String =
  s"""package y${year}
     |
     |import org.scalatest.flatspec.AnyFlatSpec
     |import org.scalatest.matchers.should._
     |import util.Loader
     |
     |class Day${day}Spec extends AnyFlatSpec with Matchers with Day${day} {
     |
     |  lazy val input: IndexedSeq[String] = Loader(this, "day${day}.txt").toIndexedSeq
     |  lazy val testInput: IndexedSeq[String] = Loader(this, "day${day}.test.txt").toIndexedSeq
     |  lazy val testInput2: IndexedSeq[String] = Loader(this, "day${day}.test2.txt").toIndexedSeq
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


def downloadInput(year: String, day: String, dest: String): Unit =
  import scala.sys.process.*
  val url = s"https://adventofcode.com/$year/day/${day.toInt}/input"
  val cookies = "cat .session".!!.trim
  val conn = new java.net.URI(url).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
  conn.setRequestMethod("GET")
  conn.setRequestProperty("Cookie", s"session=$cookies")
  val in = new BufferedReader(new java.io.InputStreamReader(conn.getInputStream))
  val out = new PrintWriter(dest)
  var input = in.readLine()
  while input != null do
    out.println(input)
    input = in.readLine()
  in.close()
  out.close()

@main def template(args: String*): Unit =
  val year = args(0)
  val day = args(1)
  val srcPath = s"src/main/scala/y$year/Day$day.scala"
  val testPath = s"src/test/scala/y$year/Day${day}Spec.scala"
  val rezPath = s"src/main/resources/y$year/day$day.txt"
  val testRezPath = s"src/main/resources/y$year/day$day.test.txt"
  val testRezPath2 = s"src/main/resources/y$year/day$day.test2.txt"
  val mainSrcContent = mainSrc(year, day)
  if !Files.exists(Paths.get(srcPath)) then new PrintWriter(srcPath) {
    write(mainSrcContent); close()
  }
  if !Files.exists(Paths.get(testPath)) then new PrintWriter(testPath) {
    write(mainTest(year, day)); close()
  }
  if !Files.exists(Paths.get(rezPath)) then downloadInput(year, day, rezPath)
  if !Files.exists(Paths.get(testRezPath)) then new PrintWriter(testRezPath) {
    write(""); close()
  }
  if !Files.exists(Paths.get(testRezPath2)) then new PrintWriter(testRezPath2) {
    write("");
    close()
  }
