import scala.annotation.tailrec

object Day07 {
  private def getInstructions(lines: List[String]) = {
    lines.map(line => (line(5), line(36)))
  }

  def task1(lines: List[String]): String = {
    val allInstructions = getInstructions(lines)

    def hasDependency(step: Char, instructions: List[(Char, Char)]) =
      instructions.map{case (_, followUpStep) => followUpStep}.contains(step)

    @tailrec
    def go(steps: String, orderedSteps: String): String =
      if (steps isEmpty) orderedSteps
      else {
        val instructions = allInstructions.filter{case (dependencyStep, _) => steps.contains(dependencyStep)}
        val nextElement = steps
          .filterNot(hasDependency(_, instructions))
          .sorted.headOption.getOrElse(steps.sorted.head)

        val remainingSteps = steps.filterNot(_ == nextElement)
        val nextOrderedSteps = orderedSteps + nextElement

        go(remainingSteps, nextOrderedSteps)
      }

    val allSteps = allInstructions.flatMap{case (x, y) => s"$x$y"}.toSet.mkString
    go(allSteps, "")
  }


  def task2(lines: List[String], workerCount: Int, extraTime: Int): Int = {
    val initWorkers: List[(Option[Char], Int)] = (0 until workerCount).toList.map(_ => (None, 0))
    Stream.iterate((0, initWorkers, getInstructions(lines))){
      case (sec, workers, instructions) =>
        val nextWorkers = ???
        val remainingInstructions = ???

        (sec + 1, nextWorkers, remainingInstructions)
    }
    ???
  }



}
