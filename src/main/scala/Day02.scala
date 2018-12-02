object Day02 {

  def countOfx2Andx3(id:String): (Int, Int) = {
    val counted = id.groupBy(identity).mapValues(_.size)
    def countOf(count: Int) =
      if (counted.filter{case (_, c) => count == c}.size > 0) 1
      else 0
    val x2 = countOf(2)
    val x3 = countOf(3)
    (x2, x3)
  }

  def task1(ids: List[String]): Int = {
    val (x2s, x3s) = ids.map(countOfx2Andx3).unzip
    x2s.sum * x3s.sum
  }

}
