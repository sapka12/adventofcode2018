import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day01Spec extends FlatSpec with Matchers {

  def toInts(text: String): List[Int] = text.split(",").toList.map(_.trim.toInt)

  val numbers = Source.fromFile("day01-0-input.txt").getLines().map(_.toInt).toList

  behavior of "Day01"

  it should "task1" in {
    Day01.task1(numbers) shouldBe 576
  }

  it should "task2" in {
    Day01.task2(toInts("+1, -1")) shouldBe 0
    Day01.task2(toInts("+3, +3, +4, -2, -4")) shouldBe 10
    Day01.task2(toInts("-6, +3, +8, +5, -6")) shouldBe 5
    Day01.task2(toInts("+7, +7, -2, -7, -4")) shouldBe 14
    Day01.task2(numbers) shouldBe 77674
  }
}
