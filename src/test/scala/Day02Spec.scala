import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day02Spec extends FlatSpec with Matchers {

  def toInts(text: String): List[String] = text.split(",").map(_.trim).toList

  val lines = Source.fromFile("day02-0-input.txt").getLines().toList

  behavior of "countOfx2Andx3"

  it should "abcdef" in {
    Day02.countOfx2Andx3("abcdef") shouldBe (0, 0)
  }

  it should "bababc" in {
    Day02.countOfx2Andx3("bababc") shouldBe (1, 1)
  }

  it should "abbcde" in {
    Day02.countOfx2Andx3("abbcde") shouldBe (1, 0)
  }

  it should "abcccd" in {
    Day02.countOfx2Andx3("abcccd") shouldBe (0, 1)
  }

  it should "aabcdd" in {
    Day02.countOfx2Andx3("aabcdd") shouldBe (1, 0)
  }

  it should "abcdee" in {
    Day02.countOfx2Andx3("abcdee") shouldBe (1, 0)
  }

  it should "ababab" in {
    Day02.countOfx2Andx3("ababab") shouldBe (0, 1)
  }

  behavior of "Day02"

  it should "test of task1" in {
    Day02.task1(List(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )) shouldBe 12
  }

  it should "task1" in {
    Day02.task1(lines) shouldBe 6944
  }

  //  it should "task2" in {
//    Day02.task2(numbers) shouldBe 77674
//  }
}
