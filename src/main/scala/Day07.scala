import scala.annotation.tailrec

object Day07 {

  private def getInstructions(lines: List[String]) = {
    lines.map(line => (line(5), line(36)))
  }

  private def allStepsFromInstructions(instructions: List[(Char, Char)]) =
    instructions.flatMap { case (x, y) => s"$x$y" }.toSet.mkString

  def hasDependency(step: Char, instructions: List[(Char, Char)]) =
    instructions.map{case (_, followUpStep) => followUpStep}.contains(step)

  def task1(lines: List[String]): String = {
    val allInstructions = getInstructions(lines)

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

    val allSteps = allStepsFromInstructions(allInstructions)
    go(allSteps, "")
  }

  type Instruction = (Char, Char)
  case class Worker(name: Char, worked: Int){
    def workTimeWith(extraTime: Int): Int = extraTime + 1 + name - 'A'
  }
  case class WorkState(lastSec: Int, workers: List[Worker], instructions: List[Instruction], maxWorkers: Int)

  private def nextStepsToWorkOn(instructions: List[Instruction], numOfStepsNeeded: Int): String = {

    @tailrec
    def go(moreSteps: Int, aggr: String, instructions: List[Instruction]): String = {
      if (moreSteps == 0) aggr
      else {
        val remainingSteps = allStepsFromInstructions(instructions)
        val freeSteps = remainingSteps.filterNot(hasDependency(_, instructions)).sorted
        val stepNeeds = freeSteps.length min moreSteps
        val usedFreeSteps = freeSteps.take(stepNeeds)

        go(
          moreSteps - stepNeeds,
          aggr + usedFreeSteps,
          instructions.filterNot{case (s, _) => usedFreeSteps.contains(s)}
        )
      }
    }


    go(numOfStepsNeeded, "", instructions)
  }

  def task2(lines: List[String], workerCount: Int, extraTime: Int): Int = {

    val initState = WorkState(0, List(), getInstructions(lines), workerCount)

    val solution = Stream.iterate(initState){
      case WorkState(sec, workers, instructions, maxWorkers) =>

        val stillWorkingWorkers = workers
          .map(worker => worker.copy(worked = worker.worked + 1))
          .filter(worker => worker.worked < worker.workTimeWith(extraTime))

        val newStepsToWorkOn = nextStepsToWorkOn(instructions, maxWorkers - stillWorkingWorkers.size)

        val nextWorkers = stillWorkingWorkers ::: newStepsToWorkOn.map(step => Worker(step, 0)).toList

        val remainingInstructions = instructions.filterNot{case (s, _) => newStepsToWorkOn.contains(s)}

        WorkState(sec + 1, nextWorkers, remainingInstructions, maxWorkers)
    }.filter{case WorkState(sec, workers, instructions, maxWorkers) => workers.isEmpty && instructions.isEmpty}.head

    solution.lastSec
  }



}
