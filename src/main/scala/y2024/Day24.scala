package y2024

import scala.annotation.tailrec

class Day24 extends util.Day(24):

  def solvePart1(input: IndexedSeq[String]): Any =
    val circuit = parseInput(input)
    val finalWires = evaluateCircuit(circuit.operations, circuit.wires)

    val bits = finalWires.keys
      .filter(_.isZ)
      .toSeq
      .sortBy(_.zIndex)(Ordering.Int.reverse)
      .map(w => finalWires(w).toString)
      .mkString

    java.lang.Long.parseLong(bits, 2)
  end solvePart1

  def solvePart2(input: IndexedSeq[String]): Any =
    val circuit = parseInput(input)
    findWrongOperations(circuit)
      .toSeq
      .sortBy(_.name)
      .map(_.name)
      .mkString(",")
  end solvePart2

  private def parseInput(input: IndexedSeq[String]): Circuit =
    input.foldLeft(Circuit(Map.empty, List.empty, Wire("z00"))):
      case (circuit, line) => line match
        case s if s.contains(":") =>
          val Array(wire, value) = s.split(":", 2).map(_.trim)
          circuit.copy(wires = circuit.wires + (Wire(wire) -> value.toInt))

        case s if s.contains("->") =>
          val Array(op1, opStr, op2, _, res) = s.split("\\s+")
          val operation = Operation(Wire(op1), Op.fromString(opStr), Wire(op2), Wire(res))
          val newHighestZ =
            if operation.result.isZ && operation.result.zIndex > circuit.highestZ.zIndex then
              operation.result
            else
              circuit.highestZ

          circuit.copy(operations = operation :: circuit.operations,
                       highestZ = newHighestZ)

        case _ => circuit

  end parseInput

  private def evaluateCircuit(operations: List[Operation],
                              initialWires: Map[Wire, Int]): Map[Wire, Int] =
    @tailrec
    def recurse(ops: List[Operation], acc: Map[Wire, Int]): Map[Wire, Int] =
      val (ready, pending) = ops.partition: op =>
        acc.contains(op.op1) && acc.contains(op.op2)

      if ready.isEmpty then acc
      else
        val newValues = ready.map: op =>
          op.result -> op.process(acc(op.op1), acc(op.op2))
        .toMap
        recurse(pending, acc ++ newValues)
    end recurse

    recurse(operations, initialWires)

  end evaluateCircuit

  private def findWrongOperations(circuit: Circuit): Set[Wire] =
    val nonXorZ = for
      op <- circuit.operations
      if op.result.isZ && op.op != Op.XOR && op.result != circuit.highestZ
    yield op.result

    val invalidXor = for
      op <- circuit.operations
      if op.op == Op.XOR && !List(op.result, op.op1, op.op2).exists(_.isXYZ)
    yield op.result

    val invalidAnd = for
      case Operation(op1, Op.AND, op2, res) <- circuit.operations
      if op1.name != "x00" && op2.name != "x00"
      subop <- circuit.operations
      if (res == subop.op1 || res == subop.op2) && subop.op != Op.OR
    yield res

    val invalidXorOr = for
      case Operation(_, Op.XOR, _, res) <- circuit.operations
      subop <- circuit.operations
      if (res == subop.op1 || res == subop.op2) && subop.op == Op.OR
    yield res

    nonXorZ.toSet ++ invalidXor.toSet ++ invalidAnd.toSet ++ invalidXorOr.toSet
  end findWrongOperations

  private case class Circuit(wires: Map[Wire, Int],
                             operations: List[Operation],
                             highestZ: Wire)

  private case class Wire(name: String):
    def isXYZ: Boolean = Set("x", "y", "z").exists(name.startsWith)

    def zIndex: Int = name.drop(1).toInt

    def isZ: Boolean = name.startsWith("z")
  end Wire

  private case class Operation(op1: Wire, op: Op, op2: Wire, result: Wire):
    def process(v1: Int, v2: Int): Int = op match
      case Op.AND => v1 & v2
      case Op.OR => v1 | v2
      case Op.XOR => v1 ^ v2
  end Operation

  sealed trait Op

  object Op:
    def fromString(s: String): Op = s match
      case "AND" => AND
      case "OR" => OR
      case "XOR" => XOR
      case other => throw IllegalArgumentException(s"Unknown operation: $other")


    case object AND extends Op

    case object OR extends Op

    case object XOR extends Op
  end Op

end Day24
