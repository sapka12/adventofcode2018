import scala.annotation.tailrec

object Day07 {

  def dependencies(step: Step, instructions: List[Instruction]): Set[Step] = {

    def go(steps: Set[Step]): Set[Step] = {
      val deps = for {
        _step <- steps
        (a, b) <- instructions
        if _step == b
      } yield a

      val aggr = deps ++ steps

      if (aggr.size > steps.size) go(aggr)
      else steps
    }

    go(Set(step)) -- Set(step)
  }

  private def getInstructions(lines: List[String]) = {
    lines.map(line => (line(5), line(36)))
  }

  private def allStepsFromInstructions(instructions: List[Instruction]) =
    (for ((x, y) <- instructions) yield s"$x$y").toSet.mkString

  def hasDependency(step: Char, instructions: List[(Char, Char)]) = !dependencies(step, instructions).isEmpty

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

  type Step = Char
  type Instruction = (Step, Step)
  case class Worker(name: Char, worked: Int){
    def workTimeWith(extraTime: Int): Int = extraTime + 1 + name - 'A'
  }

  def working(worker: Worker, extraTime: Int) = worker.worked < worker.workTimeWith(extraTime)

  case class WorkState(lastSec: Int, workers: List[Worker], maxWorkers: Int, doneSteps: Set[Step]){
    def next(extraTime: Int, allSteps: String): WorkState = {

      val (stillWorkingWorkers, doneWorkers) = workers
        .map(worker => worker.copy(worked = worker.worked + 1))
        .partition(working(_, extraTime))

      val aggrDoneSteps = doneSteps ++ doneWorkers.map(_.name)

      val newWorkers: List[Worker] = {
        val needs = maxWorkers - stillWorkingWorkers.size
        if (needs == 0) List()
        else {
       val remainingSteps =  allSteps.filterNot(aggrDoneSteps.contains(_))
  re

          ???
        }
      }
      val aggrWorkers = workers ++ newWorkers

      WorkState(lastSec + 1, aggrWorkers, maxWorkers, aggrDoneSteps)
    }
  }

  def task2(lines: List[String], workerCount: Int, extraTime: Int): Int = {


    val allInstructions = getInstructions(lines)
    val allSteps = allStepsFromInstructions(allInstructions)

    val initState = WorkState(0, List(), workerCount, Set())

    val solution = Stream.iterate(initState)(_.next(extraTime, allSteps))
      .filter{case WorkState(_, _, _, doneSteps) => allSteps.length == doneSteps.size}
      .head

    solution.lastSec
  }
}
