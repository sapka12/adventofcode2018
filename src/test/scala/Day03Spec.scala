import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day03Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day03-0-input.txt").getLines().toList
  private val testData = List(
    "#1 @ 1,3: 4x4",
    "#2 @ 3,1: 4x4",
    "#3 @ 5,5: 2x2"
  )

  behavior of "Day03"

  it should "test of task1" in {
    Day03.task1(testData) shouldBe 4
  }

  it should "task1" in {
    Day03.task1(lines) shouldBe 120408
  }

  it should "test of task2" in {
    Day03.task2(testData) shouldBe 3
  }

  it should "task2" in {
    Day03.task2(lines) shouldBe 1276
  }
}
