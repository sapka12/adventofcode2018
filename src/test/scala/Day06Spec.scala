import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day06Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day06-0-input.txt").getLines().toList
  private val testInput = """1, 1
                    |1, 6
                    |8, 3
                    |3, 4
                    |5, 5
                    |8, 9"""
    .stripMargin.split("\n").toList

  behavior of "Day06"

  it should "test of task1" in {
    Day06.task1(testInput) shouldBe 17
  }

  it should "task1" in {
    Day06.task1(lines) shouldBe 6047
  }

  it should "test of task2" in {
    Day06.task2(testInput, 32) shouldBe 16
  }

  it should "task2" in {
    Day06.task2(lines, 10000) shouldBe 46320
  }

}
