import java.text.SimpleDateFormat

import scala.util.Try

case class Log(minute: Long, msg: String)

object Log{
  private val FORMAT = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  def parseMinute(time: String): Long = FORMAT.parse(time).getTime / 60000

  //[1518-09-13 00:12] falls asleep
  def apply(line: String): Log = {
    val s = line.split("] ")

    new Log(
      parseMinute(s(0).replace("[", "").trim),
      s(1).trim
    )
  }
}

case class Asleep(from: Long, to: Option[Long] = None){
  lazy val slept: Option[Long] = to.map(_ - from)

  //TODO: remove get
  lazy val rangeOf: List[Long] = to.map(from until _ ).get.toList
    .filter{minute =>
      val minOfDay = Math.floorMod(minute + 60, 24 * 60)
      minOfDay < 60
    }
}
case class Shift(id: Int, asleeps: List[Asleep])

object Day04 {

  private val minutes = (0 until 60).toList

  private def newShiftStarts(id: Int)(implicit shifts: List[Shift]): List[Shift] = Shift(id, List()) :: shifts

  private def guardFallsAsleep(time: Long)(implicit shifts: List[Shift]): List[Shift] = {
    val currentShift = shifts.head
    val withNewAsleep = currentShift.copy(asleeps = Asleep(time) :: currentShift.asleeps)
    withNewAsleep :: shifts.tail
  }

  private def guardWakesUp(time: Long)(implicit shifts: List[Shift]): List[Shift] = {
    val currentShift = shifts.head
    val currentSleep = currentShift.asleeps.head
    val closedSleep = currentSleep.copy(to = Some(time))
    currentShift.copy(asleeps = closedSleep :: currentShift.asleeps.tail) :: shifts.tail
  }

  def byAsleeps(lines: List[String]): Map[Int, List[Asleep]] = lines.map(Log(_))
    .sortBy(_.minute).foldLeft[List[Shift]](List())((shifts, log) => {
    implicit val _shifts: List[Shift] = shifts
    val msgParts = log.msg.split(" ")
    msgParts(0) match {
      case "Guard" => newShiftStarts(msgParts(1).replace("#", "").toInt)
      case "falls" => guardFallsAsleep(log.minute)
      case "wakes" => guardWakesUp(log.minute)
      case _ => throw new RuntimeException
    }
  }).groupBy(_.id).mapValues(_.flatMap(_.asleeps))

  def task1(lines: List[String]): Int = {

    val asleeps = byAsleeps(lines)

    val mostMinutesAsleepGuard =  asleeps.mapValues(_.flatMap(_.slept).sum).maxBy(_._2)._1
    val guardsAsleeps = asleeps(mostMinutesAsleepGuard)

    val countMins = for {
      minute <- minutes
      guardsAsleep <- guardsAsleeps
      asleepMin <- guardsAsleep.rangeOf
      if Math.floorMod(asleepMin, 60) == minute
    } yield (minute, 1)

//    val frequentMinute = countMins.groupBy(_._1).mapValues(_.map(_._2).sum).maxBy(_._2)._1
    val asdf = countMins.groupBy(_._1).mapValues(_.map(_._2).sum)
    val frequentMinute = Try{
      asdf.maxBy(_._2)._1
    }.getOrElse(0)

    mostMinutesAsleepGuard * frequentMinute
  }

  def task2(lines: List[String]): Int = {
    val allShifts = byAsleeps(lines)

    val guardAsleepMinutes = for {
      (guardId, _asleeps) <- allShifts.toList
      _asleep <- _asleeps
      minuteWhenGuardSleeps <- _asleep.rangeOf
      minuteOfHour <- minutes
      if Math.floorMod(minuteWhenGuardSleeps, 60) == minuteOfHour
    } yield (guardId, minuteOfHour)

    def mostFrequentMinute(m: Map[Int, Int]) = m.maxBy(_._2)._1

    val maxMinutePerGuards = guardAsleepMinutes.groupBy(_._1)
      .mapValues(_.map(_._2))
      .mapValues(_.groupBy(identity).mapValues(_.size))
      .mapValues(mostFrequentMinute)

    val maxMinutePerGuard = maxMinutePerGuards.maxBy(_._2)
    maxMinutePerGuard._1 * maxMinutePerGuard._2
  }
}