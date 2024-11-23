package y2022

import scala.annotation.tailrec

trait Day07:
  private enum FSEntry(val name: String):
    case File(override val name: String, size: Int) extends FSEntry(name)
    case Dir(override val name: String, contents: Map[String, FSEntry] = Map()) extends FSEntry(name)

  import FSEntry.*

  private def parseCommands(input: Seq[String]): Dir =
    def updateFileSystem(fs: Dir,
                         path: List[String],
                         entry: FSEntry): Dir =
      path match
        case Nil =>
          fs.copy(contents = fs.contents + (entry.name -> entry))
        case dir :: rest =>
          val currentDir = fs.contents(dir).asInstanceOf[Dir]
          val updatedDir = updateFileSystem(currentDir, rest, entry)
          fs.copy(contents = fs.contents + (dir -> updatedDir))

    @tailrec
    def processCommands(lines: List[String],
                        currentPath: List[String],
                        fs: Dir): Dir =
      lines match
        case Nil => fs
        case cmd :: rest =>
          cmd match
            case "$ cd /" =>
              processCommands(rest, List(), fs)
            case "$ cd .." =>
              processCommands(rest, currentPath.tail, fs)
            case s"$$ cd $dir" =>
              processCommands(rest, dir :: currentPath, fs)
            case "$ ls" =>
              val (entries, remaining) = rest.span(!_.startsWith("$"))
              val updatedFS = entries.foldLeft(fs): (acc, entry) =>
                entry match
                  case s"dir $name" =>
                    updateFileSystem(acc, currentPath.reverse, Dir(name))
                  case s"$size $name" =>
                    updateFileSystem(acc, currentPath.reverse, File(name, size.toInt))
              processCommands(remaining, currentPath, updatedFS)

    processCommands(input.toList, List(), Dir("/"))

  private def calculateSizes(entry: FSEntry): Map[List[String], Int] =
    def go(entry: FSEntry, path: List[String]): (Int, Map[List[String], Int]) =
      entry match
        case File(_, size) => (size, Map())
        case Dir(name, contents) =>
          val currentPath = name :: path
          val (sizes, maps) = contents.values.map(e => go(e, currentPath)).unzip
          val totalSize = sizes.sum
          (totalSize, maps.fold(Map())(_ ++ _) + (currentPath -> totalSize))

    go(entry, Nil)._2

  def solvePart1(input: Seq[String]): Int =
    val fs = parseCommands(input)
    val sizes = calculateSizes(fs)
    sizes.values.filter(_ <= 100000).sum

  def solvePart2(input: Seq[String]): Int =
    val totalSpace = 70000000
    val requiredSpace = 30000000
    val fs = parseCommands(input)
    val sizes = calculateSizes(fs)
    val usedSpace = sizes(List("/"))
    val freeSpace = totalSpace - usedSpace
    val needToFree = requiredSpace - freeSpace
    sizes.values.filter(_ >= needToFree).min

end Day07
