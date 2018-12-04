import java.text.SimpleDateFormat

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
  lazy val slept = to.map(_ - from)
  lazy val rangeOf = to.map(from until _ ).get
}
case class Shift(id: Int, asleeps: List[Asleep])

object Day04 {

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

  def task1(lines: List[String]): Int = {

    val allShifts = lines.map(Log(_)).sortBy(_.minute).foldLeft[List[Shift]](List())((shifts, log) => {
      implicit val _shifts = shifts
      val msgParts = log.msg.split(" ")
      msgParts(0) match {
        case "Guard" => newShiftStarts(msgParts(1).replace("#", "").toInt)
        case "falls" => guardFallsAsleep(log.minute)
        case "wakes" => guardWakesUp(log.minute)
        case _ => throw new RuntimeException
      }
    })

    val mostMinutesAsleepGuard =  allShifts.groupBy(_.id)
      .mapValues(_shifts => _shifts.flatMap(_.asleeps.map(_.slept.getOrElse(0L))))
      .mapValues(_.sum).maxBy(_._2)._1

    val guardsAsleeps = allShifts.filter(_.id == mostMinutesAsleepGuard).flatMap(_.asleeps).flatMap(_.rangeOf)

    val countMins = for {
      minute <- (0 until 60).toList
      asleepMin <- guardsAsleeps
      if Math.floorMod(asleepMin, 60) == minute
    } yield (minute, 1)

    val frequentMinute = countMins.groupBy(_._1).mapValues(_.map(_._2).sum).maxBy(_._2)._1

    mostMinutesAsleepGuard * frequentMinute
  }
}