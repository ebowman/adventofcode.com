package y2015

trait Day13 {

  private val Gain = """(.*?) would gain (\d+) happiness units by sitting next to (.*?)\.""".r
  private val Lose = """(.*?) would lose (\d+) happiness units by sitting next to (.*?)\.""".r

  def buildWorld(defs: Iterable[String]): System =
    defs.foldLeft(System()) {
      case (sys: System, Gain(n1, num, n2)) => sys.add(n1, n2, num.toInt)
      case (sys: System, Lose(n1, num, n2)) => sys.add(n1, n2, -num.toInt)
    }

  case class Person(name: String, effects: Map[String, Int] = Map()) {
    def add(person: String, effect: Int): Person = this.copy(effects = effects + (person -> effect))
  }

  case class System(people: Set[Person] = Set()) {
    def add(person: String, other: String, effect: Int): System =
      people.find(_.name == person) match {
        case Some(p) => this.copy(people = (people - p) + p.add(other, effect))
        case None => this.copy(people = people + Person(person).add(other, effect))
      }

    def netHappiness: Int = happinessSeq(people.toSeq.permutations.maxBy(happinessSeq))

    def happinessSeq(s: Seq[Person]): Int =
      val tmp = s.last +: s :+ s.head
      (1 to s.size).map(i => tmp(i).effects(tmp(i - 1).name) + tmp(i).effects(tmp(i + 1).name)).sum

    def netAmbivalentMe: Int =
      val me = Person("me", people.map(_.name).map(_ -> 0).toMap)
      val newSys = people.foldLeft(this.copy(people = people + me)) {
        case (sys, person) => sys.add(person.name, me.name, 0)
      }
      newSys.netHappiness
  }


  def solve1(input: Iterable[String]): Int =
    buildWorld(input).netHappiness

  def solve2(input: Iterable[String]): Int =
    buildWorld(input).netAmbivalentMe
}
