package y2017

import scala.collection.immutable.TreeMap

trait Day25:
  enum Direction:
    case Left, Right

  case class Rule(writeValue: Int, move: Direction, nextState: Char)
  case class StateRules(zeroRule: Rule, oneRule: Rule)
  case class Blueprint(startState: Char,
                       steps: Int,
                       rules: Map[Char, StateRules])

  def parseInput(input: Seq[String]): Blueprint =
    val startState = input.head.charAt(15)
    val steps = input(1).split(" ")(5).toInt

    def parseRule(lines: Seq[String], offset: Int): Rule =
      val writeValue = lines(offset).charAt(22) - '0'
      val direction = if lines(offset + 1).contains("right") then Direction.Right else Direction.Left
      val nextState = lines(offset + 2).charAt(26)
      Rule(writeValue, direction, nextState)

    val stateRules = input.drop(3).grouped(10).map: stateBlock =>
      val state = stateBlock(0).charAt(9)
      val zeroRule = parseRule(stateBlock, 2)
      val oneRule = parseRule(stateBlock, 6)
      state -> StateRules(zeroRule, oneRule)
    .toMap

    Blueprint(startState, steps, stateRules)

  def runTuringMachine(blueprint: Blueprint): Int =
    var tape = TreeMap[Int, Int]().withDefaultValue(0)
    var currentPos = 0
    var currentState = blueprint.startState

    for _ <- 1 to blueprint.steps do
      val currentValue = tape(currentPos)
      val rule = if currentValue == 0 then
        blueprint.rules(currentState).zeroRule
      else
        blueprint.rules(currentState).oneRule

      tape = tape.updated(currentPos, rule.writeValue)
      currentPos += (if rule.move == Direction.Right then 1 else -1)
      currentState = rule.nextState

    tape.values.count(_ == 1)

  def solvePart1(input: Seq[String]): Int =
    val blueprint = parseInput(input)
    runTuringMachine(blueprint)

end Day25