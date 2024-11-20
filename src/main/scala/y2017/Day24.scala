package y2017

trait Day24:
  case class Component(port1: Int, port2: Int):
    def strength: Int = port1 + port2
    def hasPort(p: Int): Boolean = port1 == p || port2 == p
    def otherPort(p: Int): Int = if p == port1 then port2 else port1

  def parseComponent(s: String): Component =
    s.split("/").map(_.toInt) match
      case Array(a, b) => Component(a, b)
      case _ => throw IllegalArgumentException(s"Invalid component: $s")

  def findBridges(components: Set[Component], port: Int): Seq[Seq[Component]] =
    val validComponents = components.filter(_.hasPort(port))
    if validComponents.isEmpty then
      Seq(Seq.empty)
    else
      for
        component <- validComponents.toSeq
        nextPort = component.otherPort(port)
        remainingComponents = components - component
        bridge <- findBridges(remainingComponents, nextPort)
      yield component +: bridge

  def bridgeStrength(bridge: Seq[Component]): Int =
    bridge.map(_.strength).sum

  def solvePart1(input: Seq[String]): Int =
    val components = input.map(parseComponent).toSet
    val bridges = findBridges(components, 0)
    bridges.map(bridgeStrength).max

  def solvePart2(input: Seq[String]): Int =
    val components = input.map(parseComponent).toSet
    val bridges = findBridges(components, 0)
    val maxLength = bridges.map(_.length).max
    bridges
      .filter(_.length == maxLength)
      .map(bridgeStrength)
      .max
end Day24
