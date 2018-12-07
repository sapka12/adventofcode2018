object Day06 {

  def manhattanDistance(a: (Int, Int), b: (Int, Int)): Int = (a._1 - b._1).abs + (a._2 - b._2).abs


  def task1(lines: List[String]): Int = {

    val coords = lines
      .map(_.split(",").map(_.trim.toInt))
      .map(c => (c(0), c(1)))

    def closest(x: Int, y: Int): Option[(Int, Int)] = {
      val minDistance = coords.map(manhattanDistance(_, (x, y))).min
      val minimals = coords.filter(manhattanDistance(_, (x, y)) == minDistance)
      if (minimals.size == 1) Some(minimals.head) else None
    }

    val (minX, minY, maxX, maxY) = coords
      .foldLeft((Int.MaxValue, Int.MaxValue, Int.MinValue, Int.MinValue)){case (aggr, coord) =>
        val (x, y) = coord
        val (_minX, _minY, _maxX, _maxY) = aggr
        (
          _minX min x,
          _minY min y,
          _maxX max x,
          _maxY max x
        )
      }

    val closestPoints = for {
      x <- minX to maxX
      y <- minY to maxY
    } yield {
      ((x, y), closest(x, y))
    }

    def edgePoint(p: (Int, Int)): Boolean = p match {case (x, y) =>
      x == minX || x == maxX || y == minY || y == maxY
    }

    val insidePoints = coords.filterNot(edgePoint)

    val (_, result) = insidePoints.map{case (x, y) =>
      (
        (x, y),
        closestPoints.count{
          case (_, Some((closeX, closeY))) => closeX == x && closeY == y
          case _ => false
        }
      )
    }.maxBy{case (_, count) => count}

    result
  }
}
