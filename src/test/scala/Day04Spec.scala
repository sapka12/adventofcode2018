import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Day04Spec extends FlatSpec with Matchers {

  private val lines = Source.fromFile("day04-0-input.txt").getLines().toList.sorted

  lines.foreach(println)

  private val testData = """[1518-11-01 00:00] Guard #10 begins shift
                           |[1518-11-01 00:05] falls asleep
                           |[1518-11-01 00:25] wakes up
                           |[1518-11-01 00:30] falls asleep
                           |[1518-11-01 00:55] wakes up
                           |[1518-11-01 23:58] Guard #99 begins shift
                           |[1518-11-02 00:40] falls asleep
                           |[1518-11-02 00:50] wakes up
                           |[1518-11-03 00:05] Guard #10 begins shift
                           |[1518-11-03 00:24] falls asleep
                           |[1518-11-03 00:29] wakes up
                           |[1518-11-04 00:02] Guard #99 begins shift
                           |[1518-11-04 00:36] falls asleep
                           |[1518-11-04 00:46] wakes up
                           |[1518-11-05 00:03] Guard #99 begins shift
                           |[1518-11-05 00:45] falls asleep
                           |[1518-11-05 00:55] wakes up""".stripMargin.split("\n").toList

  behavior of "Day04"

  it should "test of task1" in {
    Day04.task1(testData) shouldBe 240
  }

  it should "task1" in {
    Day04.task1(lines) shouldBe 98680
  }

  it should "test of task2" in {
    Day04.task2(testData) shouldBe 4455
  }

  it should "task2" in {
    Day04.task2(lines) shouldBe 9763
  }

}
