package y2016

import scala.annotation.tailrec

trait Day10 {
  object State {
    def apply(input: Seq[String], seekLow: Int, seekHigh: Int): State = {
      class RuleParser extends scala.util.parsing.combinator.JavaTokenParsers {
        def botToken: Parser[Int] = "bot" ~> wholeNumber ^^ { i => i.toInt }

        def outputToken: Parser[Int] = "output" ~> wholeNumber ^^ { i => -i.toInt - 1 }

        def valueAssignment: Parser[State => State] = (("value" ~> wholeNumber) <~ ("goes" ~ "to")) ~ botToken ^^ {
          case value ~ bot => _.store(bot, value.toInt)
        }

        def botAssignment: Parser[State => State] =
          (((botToken <~ "gives low to") ~ (botToken | outputToken)) <~ "and high to") ~ (botToken | outputToken) ^^ {
            case src ~ low ~ high => _.setDestinations(src, low, high)
          }

        def assignment: Parser[State => State] = valueAssignment | botAssignment

        def parse(input: Seq[String], seekLow: Int, seekHigh: Int): State =
          input.foldLeft(State(seek = Seq(seekLow, seekHigh))) {
            case (state, line) => parseAll(assignment, line).get(state)
          }.checkSeek
      }

      new RuleParser().parse(input, seekLow, seekHigh)
    }
  }

  case class Bot(values: Seq[Int] = Seq.empty, destLow: Int = Int.MinValue, destHigh: Int = Int.MinValue)

  case class State(bots: Map[Int, Bot] = Map().withDefault(_ => Bot()),
                   outputs: Map[Int, Int] = Map(), seek: Seq[Int], found: Option[Int] = None) {
    def store(id: Int, value: Int): State =
      copy(bots = bots + (id -> bots(id).copy(values = (bots(id).values :+ value).sorted)))

    def setDestinations(id: Int, low: Int, high: Int): State =
      copy(bots = bots + (id -> bots(id).copy(destLow = low, destHigh = high)))

    def checkSeek: State =
      bots.find(b => b._2.values == seek).map(b => copy(found = Some(b._1))).getOrElse(this)

    @tailrec final def evaluate: State = {
      bots.find(_._2.values.size == 2).map { case (id, bot) =>
        val (low, high) = (bot.destLow, bot.destHigh)
        val (lowBot, highBot) = (bots(low), bots(high))
        (Seq(
          Seq(id -> bot.copy(values = Seq.empty)),
          if low >= 0 then Seq(low -> lowBot.copy(values = (lowBot.values :+ bot.values.head).sorted)) else Seq.empty,
          if high >= 0 then Seq(high -> highBot.copy(values = (highBot.values :+ bot.values.last).sorted)) else Seq.empty
        ).flatten, Seq(
          if low < 0 then Seq(((-low - 1) -> bot.values.head)) else Seq.empty,
          if high < 0 then Seq(((-high - 1) -> bot.values.last)) else Seq.empty
        ).flatten)
      } match {
        case Some((changedBots, changedOutputs)) =>
          copy(bots = bots ++ changedBots, outputs = outputs ++ changedOutputs).checkSeek.evaluate
        case None => this
      }
    }
  }

  def solve(input: Seq[String], lowSeek: Int, highSeek: Int): (Int, Int) = {
    val State(_, outputs, _, Some(found)) = State(input, lowSeek, highSeek).evaluate
    (found, (0 to 2).map(outputs).product)
  }
}