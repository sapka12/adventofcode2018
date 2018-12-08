import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day07Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day07-0-input.txt").getLines().toList
  private val testInput = """Step C must be finished before step A can begin.
                            |Step C must be finished before step F can begin.
                            |Step A must be finished before step B can begin.
                            |Step A must be finished before step D can begin.
                            |Step B must be finished before step E can begin.
                            |Step D must be finished before step E can begin.
                            |Step F must be finished before step E can begin."""
    .stripMargin.split("\n").toList

  behavior of "Day07"

  it should "test of task1" in {
    Day07.task1(testInput) shouldBe "CABDFE"
  }

  it should "task1" in {
    Day07.task1(lines) shouldBe "LAPFCRGHVZOTKWENBXIMSUDJQY"
  }

  it should "test of task2" in {
    Day07.task2(testInput, 2, 0) shouldBe 15
  }

  it should "task2" in {
    Day07.task2(testInput, 5, 60) shouldBe 0
  }

}
