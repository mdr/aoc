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

  def heightAt(point: Point): Int = heights(point.y.toInt)(point.x.toInt)

  def isInBounds(point: Point): Boolean =
    0 <= point.x && point.x < mapWidth && 0 <= point.y && point.y < mapHeight

  def neighbours(point: Point): Seq[Point] =
    Direction.values.map(point + _).filter(isInBounds)

  def isLowPoint(point: Point): Boolean =
    neighbours(point).map(heightAt).forall(heightAt(point) < _)

  def lowPoints: Seq[Point] = allPoints.filter(isLowPoint)

  def riskLevel(point: Point) = 1 + heightAt(point)

  private val mapWidth  = heights(0).size
  private val mapHeight = heights.size

  def allPoints: Seq[Point] =
    for
      x <- 0 until mapWidth
      y <- 0 until mapHeight
    yield Point(x, y)

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
          neighbour <- neighbours(boundaryPoint)
          if !points.contains(neighbour)
          if heightAt(neighbour) > heightAt(boundaryPoint)
          if heightAt(neighbour) != 9
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
        val pointHeight = heightAt(point)
        if toHighlight contains point then whiteCircled(pointHeight) else blackCircled(pointHeight)
      }.mkString
    (0 until mapHeight).map(renderRow).mkString("\n")
