import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day05Spec extends FlatSpec with Matchers {

  private val line = Source.fromFile("day05-0-input.txt").getLines().next()

  behavior of "Day05"

  it should "test of task1 example 1" in {
    Day05.task1("aA") shouldBe "".length
  }

  it should "test of task1 example 2" in {
    Day05.task1("abBA") shouldBe "".length
  }

  it should "test of task1 example 3" in {
    Day05.task1("abAB") shouldBe "abAB".length
  }

  it should "test of task1 example 4" in {
    Day05.task1("aabAAB") shouldBe "aabAAB".length
  }

  it should "test of task1 example 5" in {
    Day05.task1("dabAcCaCBAcCcaDA") shouldBe "dabCBAcaDA".length
  }

  it should "test of task1 example 6" in {
    Day05.task1("CaABbc") shouldBe "".length
  }

  it should "task1" in {
    Day05.task1(line) shouldBe 11590
  }

  it should "test of task2" in {
    Day05.task2("dabAcCaCBAcCcaDA") shouldBe 4
  }

  it should "task2" in {
    Day05.task2(line) shouldBe 4504
  }

}
