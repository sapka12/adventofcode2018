import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day06Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day06-0-input.txt").getLines().toList

  behavior of "Day06"

  it should "test of task1" in {
    Day06.task1("""1, 1
                  |1, 6
                  |8, 3
                  |3, 4
                  |5, 5
                  |8, 9""".stripMargin.split("\n").toList) shouldBe 17
  }

  it should "task1" in {
    Day06.task1(lines) shouldBe 6047
  }

}
