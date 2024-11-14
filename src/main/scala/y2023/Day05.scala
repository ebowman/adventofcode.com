package y2023

import scala.util.Try

// see https://adventofcode.com/2023/day/5
trait Day05 {
  val categories: Seq[String] = Seq("seed", "soil", "fertilizer", "water", "light", "temperature", "humidity", "location")

  def solvePart1(input: Seq[String]): Long = {
    val (seeds, converterCollection) = parseInput(input, true)
    findMinimumMappedLocation(seeds, converterCollection)
  }

  private def parseInput(lines: Seq[String], part1: Boolean): (Seq[Range], ConverterCollection) = {
    val seeds = parseSeeds(lines, part1)
    val converters = parseConverters(lines.drop(2))
    (seeds, converters)
  }

  private def parseSeeds(lines: Seq[String], part1: Boolean): Seq[Range] = {
    val numbers = lines.find(_.startsWith("seeds:"))
      .map(_.replace("seeds: ", "").trim.split(" ").flatMap(i => scala.util.Try(i.toLong).toOption).toSeq)
      .getOrElse(Seq.empty)

    if (part1) {
      numbers.map(n => Range(n, 1))
    } else {
      numbers.grouped(2).map { case Seq(start, length) => Range(start, length) }.toSeq
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
    // Process each category transformation sequentially
    val finalRanges = categories.foldLeft(seeds) { (currentRanges, category) =>
      val spanMap = converterCollection.getMappedSpanMap(category).getOrElse(SpanMap(Seq.empty))
      currentRanges.flatMap(range => transformRange(range, spanMap))
    }

    finalRanges.map(_.start).min
  }

  private def transformRange(inputRange: Range, spanMap: SpanMap): Seq[Range] = {
    if (spanMap.spans.isEmpty) return Seq(inputRange)

    // Sort spans and find all relevant transformations
    val sortedSpans = spanMap.spans.sortBy(_.srcStart)
    var result = Vector.empty[Range]
    var current = inputRange.start
    val end = inputRange.start + inputRange.length

    // Process each span that might intersect with our range
    for (span <- sortedSpans if span.srcStart + span.length > current && span.srcStart < end) {
      // Add gap before span if needed
      if (current < span.srcStart) {
        result :+= Range(current, span.srcStart - current)
      }

      // Calculate intersection
      val intersectStart = math.max(current, span.srcStart)
      val intersectEnd = math.min(end, span.srcStart + span.length)
      val intersectLength = intersectEnd - intersectStart

      // Map the intersection
      if (intersectLength > 0) {
        val mappedStart = span.destStart + (intersectStart - span.srcStart)
        result :+= Range(mappedStart, intersectLength)
      }

      current = intersectEnd
    }

    // Add remaining gap if needed
    if (current < end) {
      result :+= Range(current, end - current)
    }

    result
  }

  def solvePart2(input: Seq[String]): Long = {
    val (seeds, converterCollection) = parseInput(input, false)
    findMinimumMappedLocation(seeds, converterCollection)
  }

  case class Span(destStart: Long, srcStart: Long, length: Long)

  case class SpanMap(spans: Seq[Span])

  case class ConverterCollection(converters: Seq[(String, String, SpanMap)]) {
    lazy val destinationSpanMaps: Map[String, SpanMap] = converters.map { case (_, dest, spanMap) => dest -> spanMap }.toMap
    def getMappedSpanMap(category: String): Option[SpanMap] = destinationSpanMaps.get(category)
  }

  case class Range(start: Long, length: Long)
}