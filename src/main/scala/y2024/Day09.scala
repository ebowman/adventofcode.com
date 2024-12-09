package y2024

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*

class Day09 extends util.Day(9):

  private case class FileInfo(fileId: Int, start: Int, length: Int)

  def solvePart1(input: IndexedSeq[String]): Long =
    val initialDisk = parseInput(input)
    val finalDisk = simulateCompaction(initialDisk)
    computeChecksum(finalDisk)

  def solvePart2(input: IndexedSeq[String]): Long =
    val initialDisk = parseInput(input)
    val finalDisk = compactWholeFiles(initialDisk)
    computeChecksum(finalDisk)

  private def parseInput(input: IndexedSeq[String]): Vector[Option[Int]] =
    val line = input.head
    val digits = line.map(_.asDigit).toVector

    val (fileSegments, freeSegments) = extractSegments(digits)
    combineSegments(fileSegments, freeSegments)
  end parseInput

  private def extractSegments(digits: Vector[Int]): (Vector[(Int, Int)], Vector[Int]) =
    val fileSegments = digits.indices.collect:
      case i if i % 2 == 0 => (i / 2, digits(i))
    .toVector

    val freeSegments = digits.indices.collect:
      case i if i % 2 == 1 => digits(i)
    .toVector

    (fileSegments, freeSegments)
  end extractSegments

  private def combineSegments(fileSegments: Vector[(Int, Int)], freeSegments: Vector[Int]): Vector[Option[Int]] =
    val paddedFreeSegments =
      if freeSegments.length < fileSegments.length then freeSegments :+ 0 else freeSegments

    fileSegments.zip(paddedFreeSegments).flatMap:
      case ((fileId, fileLen), freeLen) =>
        List.fill(fileLen)(Some(fileId)) ++ List.fill(freeLen)(None)
    ++ (
      if fileSegments.length > paddedFreeSegments.length then
        val (lastFileId, lastFileLen) = fileSegments.last
        List.fill(lastFileLen)(Some(lastFileId))
      else Nil
    )
  end combineSegments

  private def simulateCompaction(disk: Vector[Option[Int]]): Vector[Option[Int]] =
    @tailrec
    def compact(left: Int,
                right: Int,
                current: Vector[Option[Int]]): Vector[Option[Int]] =
      if left >= right then current
      else
        val nextLeft = current.indexWhere(_.isEmpty, left)
        val nextRight = current.lastIndexWhere(_.isDefined, right)

        if nextLeft == -1 || nextRight == -1 || nextLeft >= nextRight then current
        else
          val swapped = current
            .updated(nextLeft, current(nextRight))
            .updated(nextRight, current(nextLeft))

          compact(nextLeft + 1, nextRight - 1, swapped)
    end compact

    compact(0, disk.length - 1, disk)
  end simulateCompaction

  private def compactWholeFiles(disk: Vector[Option[Int]]): Vector[Option[Int]] =
    val files = findFiles(disk)
    val filesDesc = files.sortBy(_.fileId)(Ordering[Int].reverse)

    filesDesc.par.foldLeft(disk): (curDisk, fileInfo) =>
      val currentPositions = curDisk.par.zipWithIndex.collect:
        case (Some(fileId), idx) if fileId == fileInfo.fileId => idx

      if currentPositions.isEmpty then curDisk
      else
        val fileStart = currentPositions.head
        val fileEnd = currentPositions.last
        val fileLength = fileEnd - fileStart + 1

        val freeSegment = findFreeSegment(curDisk, fileLength, fileStart)
        freeSegment match
          case None => curDisk
          case Some((segStart, _)) =>
            val newDiskAfterPlacement =
              currentPositions.zipWithIndex.foldLeft(curDisk):
                (d, elem) =>
                  val (oldPos, blockIndex) = elem
                  d.updated(segStart + blockIndex, Some(fileInfo.fileId))
                    .updated(oldPos, None)
            newDiskAfterPlacement
  end compactWholeFiles

  private def findFiles(disk: Vector[Option[Int]]): List[FileInfo] =
    @tailrec def recurse(i: Int = 0, acc: List[FileInfo] = Nil): List[FileInfo] =
      if i >= disk.length then acc
      else disk(i) match
        case Some(fileId) =>
          val start = i
          val (length, nextIndex) = getFileLength(disk, start, fileId)
          recurse(nextIndex, FileInfo(fileId, start, length) :: acc)
        case None =>
          recurse(i + 1, acc)

    recurse().reverse
  end findFiles

  private def getFileLength(disk: Vector[Option[Int]], start: Int, fileId: Int): (Int, Int) =
    val remaining = disk.drop(start + 1)
    val length = remaining.takeWhile(_.contains(fileId)).length
    (length, start + 1 + length)

  private def findFreeSegment(disk: Vector[Option[Int]], length: Int, limitIndex: Int): Option[(Int, Int)] =
    @tailrec
    def recurse(start: Int = 0): Option[(Int, Int)] =
      if start >= limitIndex then None
      else if disk(start).isEmpty then
        val end = disk
          .slice(start, limitIndex.min(disk.length))
          .takeWhile(_.isEmpty)
          .length + start

        val segLength = end - start
        if (segLength >= length) Some((start, segLength))
        else recurse(end)
      else recurse(start + 1)

    recurse()
  end findFreeSegment

  private def computeChecksum(disk: Vector[Option[Int]]): Long =
    disk.zipWithIndex.foldLeft(0L):
      (acc, elem) =>
        val (maybeFileId, i) = elem
        maybeFileId.fold(acc)(fileId => acc + i.toLong * fileId)
  end computeChecksum

end Day09
