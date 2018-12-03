
case class Coord(x: Int, y: Int)
case class Claim(id: Int, offset: Coord, size: Coord){
  lazy val coords: Set[Coord] = (for {
    x <- offset.x until (offset.x + size.x)
    y <- offset.y until (offset.y + size.y)
  } yield Coord(x, y)).toSet

  val hasOverlap: Claim => Boolean = _.coords.intersect(coords).nonEmpty
}

object Claim {
  //  row example: "#1 @ 1,3: 4x4"
  def apply(row: String): Claim = {
    val parts = row.split(" ")
    val id = parts(0).replaceAll("#", "").toInt

    val offsets = parts(2).replaceAll(":", "").split(",")
    val sizes = parts(3).split("x")

    Claim(id, Coord(offsets(0).toInt, offsets(1).toInt), Coord(sizes(0).toInt, sizes(1).toInt))
  }
}

object Day03 {

  def task1(claims: List[String]): Int = {
    val all = claims.flatMap(Claim(_).coords)
    val remaining = (all diff all.distinct).toSet
    remaining.size
  }

  def task2(rows: List[String]): Int = {
    val claims = rows.map(Claim(_))
    claims
      .filterNot(claim =>
        claims.diff(List(claim)).exists(_.hasOverlap(claim))
      ).head.id
  }

}
