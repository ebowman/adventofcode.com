package y2023

import scala.util.Try

// see https://adventofcode.com/2023/day/5
trait Day05 {
  val categories: Seq[String] = Seq("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

  def solvePart1(input: Seq[String]): Long = {
    val (seeds, converterCollection) = parseInput(input, true)
    findMinimumMappedLocation(seeds, converterCollection)
  }

  def solvePart2(input: Seq[String]): Long = {
    val (seeds, converterCollection) = parseInput(input, false)
    findMinimumMappedLocation(seeds, converterCollection)
  }

  private def parseInput(lines: Seq[String], part1: Boolean): (Seq[Range], ConverterCollection) = {
    val seeds = parseSeeds(lines, part1)
    val converters = parseConverters(lines.drop(2))
    (seeds, converters)
  }

  private def parseSeeds(lines: Seq[String], part1: Boolean): Seq[Range] = {
    if (part1) {
      lines.find(_.startsWith("seeds:"))
        .map(_.replace("seeds: ", "").trim.split(" ").flatMap(i => Try(i.toLong).toOption).toSeq)
        .getOrElse(Seq.empty).map(f => Range(f, 1))
    } else {
      lines.find(_.startsWith("seeds:"))
        .map(_.replace("seeds: ", "").trim.split(" ").flatMap(i => Try(i.toLong).toOption).toSeq)
        .getOrElse(Seq.empty).grouped(2).map { a => Range(a.head, a(1)) }.toSeq
    }
  }

  private def parseConverters(lines: Seq[String]): ConverterCollection = {
    var remainingLines = lines
    var converters = Seq.empty[(String, String, SpanMap)]

    while (remainingLines.nonEmpty) {
      val Array(source, destination) = remainingLines.head.replaceAll(" map:", "").split("-to-")
      remainingLines = remainingLines.tail
      val spanMap = parseSpanMap(remainingLines)
      converters :+= (source, destination, spanMap)

      remainingLines = remainingLines.dropWhile(_.trim.nonEmpty).drop(1)
    }

    ConverterCollection(converters)
  }

  private def parseSpanMap(lines: Seq[String]): SpanMap = {
    val spans = lines.takeWhile(_.trim.nonEmpty).map { line =>
      val Array(destStart, srcStart, length) = line.split(" ").map(_.toLong)
      Span(destStart, srcStart, length)
    }
    SpanMap(spans)
  }

  private def findMinimumMappedLocation(seeds: Seq[Range], converterCollection: ConverterCollection): Long = {
    seeds.iterator.flatMap(range => range.map(seed => mapSeed(seed, converterCollection))).min
  }

  private def mapSeed(seed: Long, converterCollection: ConverterCollection): Long = {
    categories.foldLeft(seed) { (current, category) =>
      converterCollection.getMappedSpanMap(category).map(_.map(current)).getOrElse(current)
    }
  }

  case class Span(destStart: Long, srcStart: Long, length: Long) {
    def contains(src: Long): Boolean = src >= srcStart && src < srcStart + length
    def map(src: Long): Long = destStart + src - srcStart
  }

  case class SpanMap(spans: Seq[Span]) {
    def map(src: Long): Long = spans.find(_.contains(src)).map(_.map(src)).getOrElse(src)
  }

  case class ConverterCollection(converters: Seq[(String, String, SpanMap)]) {
    lazy val destinationSpanMaps: Map[String, SpanMap] = converters.map { case (_, dest, spanMap) => dest -> spanMap }.toMap
    def getMappedSpanMap(category: String): Option[SpanMap] = destinationSpanMaps.get(category)
  }

  case class Range(start: Long, count: Long) {
    def contains(value: Long): Boolean = value >= start && value <= start + count - 1
    def map(f: Long => Long): Iterable[Long] = (start until start + count).map(f)
  }
}