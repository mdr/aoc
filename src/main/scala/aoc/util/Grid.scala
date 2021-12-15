package aoc.util

object Grid:

  def parseNumberGrid(s: String): Grid[Int] =
    val rows   = getLines(s.trim).map(_.trim.map(_.toString.toInt))
    val width  = rows(0).size
    val height = rows.size
    val coords =
      for
        x <- 0 until width
        y <- 0 until height
        point = Point(x, y)
        value = rows(point.y.toInt)(point.x.toInt)
      yield point -> value
    Grid(coords.toMap, width, height)

case class Grid[T](values: Map[Point, T], width: Int, height: Int):
  def points: Set[Point] = values.keySet

  def orthogonalNeighbours(point: Point): Seq[Point] =
    Direction.values.map(point + _).filter(values.contains)

  def valueAt(point: Point): T = values(point)
