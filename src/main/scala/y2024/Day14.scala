package y2024

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO
import scala.annotation.targetName

// see https://adventofcode.com/2024/day/14
class Day14 extends util.Day(14):

  private def parseInput(input: Seq[String]): Config =
    val pattern = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".r
    val robots = input.map:
      case pattern(px, py, vx, vy) =>
        Robot(
          Point(px.toInt, py.toInt),
          Point(vx.toInt, vy.toInt)
        )

    val maxX = robots.map(_.position.x).max + 1
    val maxY = robots.map(_.position.y).max + 1
    Config(robots, maxX, maxY)
  end parseInput

  def solvePart1(input: IndexedSeq[String]): Long =
    val config = parseInput(input)
    config.solve(100)

  def solvePart2(input: IndexedSeq[String]): Int =
    val config = parseInput(input)

    val frameScores = (0 until 10000).map: step =>
      val movedRobots = config.moveRobots(step)
      val compression = movedRobots.compressionScores
      (step, compression)

    frameScores.minBy(_._2)._1
  end solvePart2

  case class Config(robots: Seq[Robot], width: Int, height: Int):

    def solve(steps: Int): Long =
      val counts = moveRobots(steps).countQuadrants
      counts.map(_.toLong).product

    def moveRobots(steps: Int): Config =
      copy(
        robots = robots.map: robot =>
          val newX = robot.position.x + (robot.velocity.x * steps)
          val newY = robot.position.y + (robot.velocity.y * steps)
          robot.copy(position = Point(newX, newY).wrapAround(width, height))
      )

    private def countQuadrants: Seq[Int] =
      val midX = width / 2
      val midY = height / 2

      val quadrants = robots.filterNot: r =>
        r.position.x == midX || r.position.y == midY
      .groupBy: robot =>
        (robot.position.x < midX, robot.position.y < midY) match
          case (true, true) => 0 // top-left
          case (false, true) => 1 // top-right
          case (true, false) => 2 // bottom-left
          case (false, false) => 3 // bottom-right

      quadrants.values.map(_.size).toSeq
    end countQuadrants

    def compressionScores: Int =
      val img = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY)

      robots.foreach: robot =>
        val x = robot.position.wrapAround(width, height).x
        val y = robot.position.wrapAround(width, height).y
        img.setRGB(x, y, 0xFFFFFF)

      val baos = new ByteArrayOutputStream()
      ImageIO.write(img, "png", baos)
      baos.size()
    end compressionScores

  end Config

  case class Point(x: Int, y: Int):
    @targetName("addPoint")
    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def wrapAround(width: Int, height: Int): Point =
      Point(
        ((x % width) + width) % width,
        ((y % height) + height) % height
      )

  case class Robot(position: Point, velocity: Point)
end Day14
