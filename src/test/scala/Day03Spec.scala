import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day03Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day03-0-input.txt").getLines().toList

  behavior of "Day03"

  it should "test of task1" in {
    Day03.task1(List(
      "#1 @ 1,3: 4x4",
      "#1 @ 3,1: 4x4",
      "#1 @ 5,5: 2x2"
    )) shouldBe 4
  }

  it should "task1" in {
    Day03.task1(lines) shouldBe 120408
  }
}
