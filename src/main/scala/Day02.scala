object Day02 {

  def countOfx2Andx3(id:String): (Int, Int) = {
    val counted = id.groupBy(identity).mapValues(_.length)
    def countOf(count: Int) =
      if (counted.exists{case (_, c) => count == c}) 1
      else 0
    val x2 = countOf(2)
    val x3 = countOf(3)
    (x2, x3)
  }

  def task1(ids: List[String]): Int = {
    val (x2s, x3s) = ids.map(countOfx2Andx3).unzip
    x2s.sum * x3s.sum
  }

  def similar(a: String, b: String): Boolean =
    (a zip b).filterNot(e => e._1 == e._2).size == 1

  def theMatch(a: String, b: String): String = (a zip b)
    .flatMap{
      case (_a, _b) => if (_a == _b) Some(_a) else None
    }.mkString

  def task2(ids: Stream[String]): String = (
    for {
      a <- ids
      b <- ids
      if a != b
      if similar(a, b)
    } yield theMatch(a, b)
    ).head
}
