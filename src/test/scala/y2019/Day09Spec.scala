package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {

  lazy val input: String = Loader(this, "day09.txt").head

  it should "solve part 1 tests" in {
    {
      val code = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
      val machine = Intcode.compiler(code).compile()
      machine.execute()
      machine.sink.asInstanceOf[LBQSink].queue.asScala.mkString(",") shouldBe code
    }

    {
      val code = "1102,34915192,34915192,7,4,7,99,0"
      val machine = Intcode.compiler(code).compile()
      machine.execute()
      machine.sink.asInstanceOf[LBQSink].queue.asScala.toSeq shouldBe Seq(34915192L * 34915192)
    }

    {
      val code = "104,1125899906842624,99"
      val machine = Intcode.compiler(code).compile()
      machine.execute()
      machine.sink.asInstanceOf[LBQSink].queue.asScala.toSeq shouldBe Seq(1125899906842624L)
    }
  }

  it should "solve part 1" in {
    val machine = Intcode.compiler(input).compile(Seq(1L))
    machine.execute()
    machine.sink.asInstanceOf[LBQSink].queue.asScala.toSeq shouldBe Seq(2662308295L)
  }

  it should "solve part 2" in {
    val machine = Intcode.compiler(input).compile(Seq(2L))
    machine.execute()
    machine.sink.asInstanceOf[LBQSink].queue.asScala.toSeq shouldBe Seq(63441L)
  }
}
