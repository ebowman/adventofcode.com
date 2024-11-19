package y2017


trait Day07:
  def solvePart1(input: Seq[String]): String = findRoot(input.map(parseLine))

  private def parseLine(line: String): Program =
    val parts = line.split(" -> ")
    val NameWeight = """(\w+) \((\d+)\)""".r
    val NameWeight(name, weight) = parts.head: @unchecked
    val children = if parts.length > 1 then
      parts(1).split(", ").toList
    else List()
    Program(name, weight.toInt, children)

  private def findRoot(programs: Iterable[Program]): String =
    val allChildren = programs.flatMap(_.children).toSet
    programs.map(_.name).find(!allChildren.contains(_)).get

  def solvePart2(input: Seq[String]): Int =
    val programs = input.map(parseLine).map(p => p.name -> p).toMap
    val root = findRoot(programs.values)
    val tree = buildTree(programs)(root)
    findUnbalanced(tree).map(_._2).getOrElse(0)

  private def buildTree(programs: Map[String, Program])(root: String): WeightedProgram =
    val program = programs(root)
    val childTrees = program.children.map(buildTree(programs))
    val totalWeight = program.weight + childTrees.map(_.totalWeight).sum
    WeightedProgram(root, program.weight, totalWeight, childTrees)

  private def findUnbalanced(tree: WeightedProgram): Option[(String, Int)] =
    def groupChildren(node: WeightedProgram): Map[Int, List[WeightedProgram]] =
      node.children.groupBy(_.totalWeight)

    def findCorrectWeight(groups: Map[Int, List[WeightedProgram]]): Int =
      val (commonWeight, _) = groups.maxBy(_._2.length)
      commonWeight

    def traverse(node: WeightedProgram): Option[(String, Int)] =
      val groups = groupChildren(node)
      if groups.size > 1 then
        val correctWeight = findCorrectWeight(groups)
        val (wrongWeight, List(wrongNode)) = groups.find(_._2.length == 1).get
        val childrenUnbalanced = wrongNode.children.flatMap(traverse).headOption
        if childrenUnbalanced.nonEmpty then childrenUnbalanced
        else Some((wrongNode.name, wrongNode.weight + (correctWeight - wrongWeight)))
      else
        node.children.flatMap(traverse).headOption
    end traverse

    traverse(tree)
  end findUnbalanced

  private case class Program(name: String, weight: Int, children: List[String] = List())

  private case class WeightedProgram(name: String, weight: Int, totalWeight: Int, children: List[WeightedProgram] = List())
end Day07