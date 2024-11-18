package util

import java.io.{BufferedReader, PrintWriter, StringWriter}
import java.nio.file.{Files, Paths}

def mainSrc(year: String, day: String): String =
  s"""package y${year}
     |
     |// see https://adventofcode.com/$year/day/${day.toInt}
     |trait Day${day}:
     |  def solvePart1(input: Seq[String]): Int = 0
     |  def solvePart2(input: Seq[String]): Int = 0
     |end Day${day}
     |""".stripMargin

def mainTest(year: String, day: String): String =
  s"""package y${year}
     |
     |import org.scalatest.flatspec.AnyFlatSpec
     |import org.scalatest.matchers.should._
     |import util.Loader
     |
     |class Day${day}Spec extends AnyFlatSpec with Matchers with Day${day}:
     |
     |  lazy val input: IndexedSeq[String] = Loader(this, "day${day}.txt").toIndexedSeq
     |  lazy val testInput: IndexedSeq[String] = Loader(this, "day${day}.test.txt").toIndexedSeq
     |  lazy val testInput2: IndexedSeq[String] = Loader(this, "day${day}.test2.txt").toIndexedSeq
     |
     |  it should "solve part 1 test" in:
     |    solvePart1(testInput) shouldBe 0
     |
     |  it should "solve part 1" in:
     |    solvePart1(input) shouldBe 0
     |
     |  it should "solve part 2 test" in:
     |    solvePart2(testInput) shouldBe 0
     |
     |  it should "solve part 2" in:
     |    solvePart2(input) shouldBe 0
     |""".stripMargin


def downloadInput(year: String, day: String): String =
  import scala.sys.process.*
  val url = s"https://adventofcode.com/$year/day/${day.toInt}/input"
  val cookies = "cat .session".!!.trim
  val conn = new java.net.URI(url).toURL.openConnection().asInstanceOf[java.net.HttpURLConnection]
  conn.setRequestMethod("GET")
  conn.setRequestProperty("Cookie", s"session=$cookies")
  if (conn.getResponseCode != 200) {
    throw new RuntimeException(s"Failed to connect, response code: ${conn.getResponseCode}")
  }
  val in = new BufferedReader(new java.io.InputStreamReader(conn.getInputStream))
  val dest = new StringWriter()
  val out = new PrintWriter(dest)
  var input = in.readLine()
  while input != null do
    out.println(input)
    input = in.readLine()
  in.close()
  out.close()
  dest.toString

def exists(path: String): Boolean = Files.exists(Paths.get(path))

def write(content: => String, path: String): Unit =
  if !exists(path) then new PrintWriter(path) {
    this.write(content)
    this.close()
  }

@main def template(args: String*): Unit =
  val year = args(0)
  val day = args(1)
  val srcPath = s"src/main/scala/y$year/Day$day.scala"
  val testPath = s"src/test/scala/y$year/Day${day}Spec.scala"
  val rezPath = s"src/main/resources/y$year/day$day.txt"
  val testRezPath = s"src/main/resources/y$year/day$day.test.txt"
  val testRezPath2 = s"src/main/resources/y$year/day$day.test2.txt"
  write(mainSrc(year, day), srcPath)
  write(mainTest(year, day), testPath)
  write("", testRezPath)
  write("", testRezPath2)
  write(downloadInput(year, day), rezPath)
