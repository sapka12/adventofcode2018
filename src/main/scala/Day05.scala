import scala.annotation.tailrec
import scala.util.Try

object Day05 {

  @tailrec
  def fullyReact(polymer: String): String = {

    val foundMatch = Try{
      (polymer.init zip polymer.tail)
        .find{case (a, b) => a != b && a.toLower == b.toLower}
        .map{case (a, b) => s"$a$b"}
    }.toOption.flatten

    val nextPolymer = foundMatch.map(polymer.replace(_, ""))

    nextPolymer match {
      case Some(next) => fullyReact(next)
      case _ => polymer
    }
  }

  def task1(line: String): Int = fullyReact(line).length

  def task2(polymer: String): Int = ('a' to 'z')
    .map{c =>
      val cleaned = polymer
        .replaceAll(c.toString, "")
        .replaceAll(c.toUpper.toString, "")
      val reacted = fullyReact(cleaned)
      reacted.length
    }.min
}
