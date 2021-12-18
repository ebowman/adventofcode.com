package y2021

import scodec.bits._
import scala.annotation.tailrec

trait Day16 {
  object ops {
    implicit class cv(s: String) {
      def h2b: String = BitVector.fromHex(s).get.toBin

      def b2i: Int = BitVector.fromBin(s).get.toInt(signed = false)

      def b2l: Long = BitVector.fromBin(s).get.toLong(signed = false)
    }
  }

  import ops._
  import collection.mutable

  object StackString {
    def apply(str: String): StackString = new StackString(mutable.Stack.from(str.toCharArray.toSeq))
  }

  case class StackString(stack: mutable.Stack[Char]) {
    def take(n: Int): String =
      (1 to n).foldLeft(new StringBuilder) { case (sb, _) => sb.append(stack.pop()) }.toString()

    def takeUntil(n: Int, f: (String => Boolean)): Seq[String] = {
      @tailrec def recurse(i: Int = 0, accum: List[String] = Nil): Seq[String] = {
        if (accum.nonEmpty && f(accum.head)) accum.reverse
        else recurse(i + 1, take(n) :: accum)
      }

      recurse()
    }
  }

  case class State(bits: StackString, versions: List[Int] = Nil, values: List[Long] = Nil) {
    def pushVersion(version: Int): State = copy(versions = version :: versions)

    def pushVersions(version: List[Int]): State = copy(versions = version ::: versions)

    def pushValue(value: Long): State = copy(values = value :: values)

    def pushValues(value: List[Long]): State = copy(values = value ::: values)

    def absorb(that: State): State = pushValues(that.values.reverse).pushVersions(that.versions)

    def amnesia: State = copy(versions = Nil, values = Nil)

    def isEmpty: Boolean = bits.stack.size < 8

    def exhaust: State = if (isEmpty) this else parse(this).exhaust

    def mutate1(f: List[Long] => Long): State = copy(values = List(f(values)))

    def mutateR(f: (Long, Long) => Long): State = copy(values = List(values.reduce(f)))

    def mutateOp(f: (Long, Long) => Boolean): State = copy(values = List(if (f(values(1), values(0))) 1 else 0))
  }

  def handleState(op: Int, state: State): State = {
    if (op == 4) state.pushValue(state.bits.takeUntil(5, _ (0) == '0').map(_.tail).mkString.b2l)
    else {
      var subState = state.bits.take(1).b2i match {
        case 0 => State(bits = StackString(state.bits.take(state.bits.take(15).b2i))).exhaust
        case 1 => Iterator.iterate(state.amnesia, state.bits.take(11).b2i + 1)(parse).toSeq.last
      }
      subState = op match {
        case 0 => subState.mutate1(_.sum)
        case 1 => subState.mutate1(_.product)
        case 2 => subState.mutateR(math.min)
        case 3 => subState.mutateR(math.max)
        case 5 => subState.mutateOp(_ > _)
        case 6 => subState.mutateOp(_ < _)
        case 7 => subState.mutateOp(_ == _)
      }
      state.absorb(subState)
    }
  }

  def parse(state: State): State = {
    if (state.isEmpty) state
    else {
      val version = state.bits.take(3).b2i
      val op = state.bits.take(3).b2i
      handleState(op, state.pushVersion(version))
    }
  }

  def solve1(input: String): Int = parse(State(StackString(input.h2b))).versions.sum

  def solve2(input: String): Long = parse(State(StackString(input.h2b))).values.head
}
