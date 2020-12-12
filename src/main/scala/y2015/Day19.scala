package y2015

import scala.annotation.tailrec
import scala.util.Random
import scala.util.parsing.combinator.RegexParsers

trait Day19 extends RegexParsers {
  def rule: Parser[(String, String)] = ("""[a-zA-Z]+""".r <~ "=>") ~ """[a-zA-Z]+""".r ^^ { case a ~ b => (a, b) }

  def mkSystem(input: Iterable[String]): System = {
    val rules = input.init.init.map(line => parseAll(rule, line).get)
    val dict = rules.foldLeft(Map[String, Set[String]]()) {
      case (map, rule) =>
        if (map.contains(rule._1)) (map - rule._1) + (rule._1 -> (map(rule._1) + rule._2))
        else map + (rule._1 -> Set(rule._2))
    }
    System(input.last, dict)
  }

  case class System(molecule: String, rules: Map[String, Set[String]]) {
    def clear: System = System("e", rules)

    def next: Seq[System] = {
      rules.flatMap {
        case (mol, repls) =>
          repls.flatMap { repl =>
            mol.r.findAllMatchIn(molecule).map { m =>
              molecule.substring(0, m.start) + repl + molecule.substring(m.end)
            }
          }
      }.toSeq.distinct.map(m => System(m, rules))
    }
  }

  // fails on big input (OOM)
  def solveWide(system: System, targetMolecule: String): Int = {
    @tailrec
    def recurse(systems: Seq[System], count: Int = 0): Int = {
      val nextLayer: Seq[System] = systems.flatMap(_.next)
      if (nextLayer.exists(_.molecule == targetMolecule)) count
      else recurse(nextLayer, count + 1)
    }

    recurse(Seq(system)) + 1
  }

  // also too slow
  def solveDeep(system: System, targetMolecule: String): Int = {
    def recurse(system: System, count: Int = 0): Int = {
      if (system.molecule == targetMolecule) count
      else if (system.molecule.length > targetMolecule.length) {
        Int.MaxValue
      }
      else {
        system.next.map(s => recurse(s, count + 1)).min
      }
    }

    recurse(system)
  }

  // stolen from the brilliant solution: https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cu5b/
  def solveSneaky(system: System, targetMolecule: String): Int = {
    var rules = system.rules.toSeq.flatMap { case (a, bs) => bs.map(b => (a, b)) }
    var target = targetMolecule
    var count = 0
    while (target != "e") {
      val tmp = target
      for ((a, b) <- rules if target.contains(b)) {
        target = target.replaceFirst(b, a)
        count += 1
      }
    }
    count
  }
}