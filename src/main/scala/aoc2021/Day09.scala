package aoc2021

import aoc.util.*

val heightMap = HeightMap.parse(loadString("aoc2021/input-2021-9.txt"))

def day09Part1: Int = heightMap.solvePart1

def day09Part2: Long = heightMap.solvePart2

object HeightMap:
  def parse(s: String) = HeightMap(getLines(s.trim).map(_.trim.map(_.toString.toInt)))

case class HeightMap(heights: Seq[Seq[Int]]):

  def solvePart1 = lowPoints.sumBy(riskLevel)

  def solvePart2 = basins.map(_.size).sorted.takeRight(3).product

  def width  = heights(0).size
  def height = heights.size

  def getHeight(point: Point): Int = heights(point.y.toInt)(point.x.toInt)

  def isInBounds(point: Point): Boolean =
    0 <= point.x && point.x < width && 0 <= point.y && point.y < height

  def neighbours(point: Point): Seq[Point] =
    Direction.values.map(point + _).filter(isInBounds)

  def isLowPoint(point: Point): Boolean =
    neighbours(point).map(getHeight).forall(getHeight(point) < _)

  def riskLevel(point: Point) = 1 + getHeight(point)

  def lowPoints: Seq[Point] =
    for
      x <- 0 until width
      y <- 0 until height
      point = Point(x, y)
      if isLowPoint(point)
    yield point

  def basins: Seq[Basin] = lowPoints.map(discoverBasin)

  def discoverBasin(lowPoint: Point): Basin = iterateUntilSteadyState(Basin.initial(lowPoint))(_.grow)

  object Basin:
    def initial(lowPoint: Point) = Basin(Set(lowPoint), Set(lowPoint))

  case class Basin(points: Set[Point], boundary: Set[Point]):
    def size = points.size

    def grow: Basin =
      val newBoundary =
        for
          boundaryPoint <- boundary
          boundaryPointHeight = getHeight(boundaryPoint)
          neighbour <- neighbours(boundaryPoint)
          if !points.contains(neighbour)
          if getHeight(neighbour) >= getHeight(boundaryPoint)
          if getHeight(neighbour) != 9
        yield neighbour
      copy(points = points ++ newBoundary, boundary = newBoundary)

  override def toString =
    val whiteCircled = "⓪①②③④⑤⑥⑦⑧⑨"
    val blackCircled = "⓿❶❷❸❹❺❻❼❽❾❿"
    // val toHighlight  = lowPoints.toSet
    val toHighlight = basins.flatMap(_.points).toSet
    def renderRow(y: Int): String =
      heights(y).zipWithIndex.map { (height, x) =>
        val point       = Point(x, y)
        val pointHeight = getHeight(point)
        if toHighlight contains point then whiteCircled(pointHeight) else blackCircled(pointHeight)
      }.mkString
    (0 until height).map(renderRow).mkString("\n")
