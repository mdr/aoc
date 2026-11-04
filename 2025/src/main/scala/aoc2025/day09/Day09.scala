package aoc2025.day09

import aoc2025.utils.{readInput, maxOf}

case class Point(x: Long, y: Long)

object Point:
  given Ordering[Point] = Ordering.by(p => (p.x, p.y))

  def parsePoint(s: String): Point =
    val Seq(x, y) = s.split(",").map(_.trim.toLong).toSeq
    Point(x, y)

  def parsePoints(input: String): Seq[Point] =
    input.linesIterator.map(Point.parsePoint).toSeq

case class Line(start: Point, end: Point):

  def isHorizontal: Boolean = start.y == end.y

  def isVertical: Boolean = start.x == end.x

  def contains(point: Point): Boolean =
    if isHorizontal then
      point.y == start.y && point.x >= math.min(start.x, end.x) && point.x <= math.max(start.x, end.x)
    else if isVertical then
      point.x == start.x && point.y >= math.min(start.y, end.y) && point.y <= math.max(start.y, end.y)
    else
      false

  def allPoints: Seq[Point] =
    if isHorizontal then
      val y = start.y
      val xRange = if start.x <= end.x then start.x to end.x else end.x to start.x
      xRange.map(x => Point(x, y))
    else
      val x = start.x
      val yRange = if start.y <= end.y then start.y to end.y else end.y to start.y
      yRange.map(y => Point(x, y))

case class Rectangle(bottomLeft: Point, topRight: Point):
  def area: Long =
    val width = topRight.x - bottomLeft.x + 1
    val height = topRight.y - bottomLeft.y + 1
    width * height

  def bottomRight = Point(topRight.x, bottomLeft.y)

  def topLeft = Point(bottomLeft.x, topRight.y)

  def edges: Seq[Line] =
    Seq(
      Line(bottomLeft, bottomRight),
      Line(bottomRight, topRight),
      Line(topRight, topLeft),
      Line(topLeft, bottomLeft)
    )

case class RectilinearPolygon(points: Seq[Point]):

  def edges: Seq[Line] =
    points.zip(points.tail :+ points.head).map { case (p1, p2) => Line(p1, p2) }

  def contains(point: Point): Boolean =
    def intersects(edge: Line): Boolean =
      val yMin = math.min(edge.start.y, edge.end.y)
      val yMax = math.max(edge.start.y, edge.end.y)
      edge.start.x > point.x && point.y >= yMin && point.y < yMax
    edges.exists(_.contains(point)) ||
      edges.filter(_.isVertical).count(intersects) % 2 == 1

  def contains(line: Line): Boolean = line.allPoints.forall(contains)

def solvePart1(points: Seq[Point]): Long =
  val distinctPairs = points.toSet.subsets(2).map { pair =>
    val Seq(p1, p2) = pair.toSeq.sorted
    (p1, p2)
  }
  val rectangles = distinctPairs.map { case (p1, p2) =>
      val bottomLeft = Point(math.min(p1.x, p2.x), math.min(p1.y, p2.y))
      val topRight = Point(math.max(p1.x, p2.x), math.max(p1.y, p2.y))
      Rectangle(bottomLeft, topRight)
    }
  rectangles.maxOf(_.area)

// filter the rectangles from part 1 but working out which are entirely contained within
// the polygon?
def solvePart2(points: Seq[Point]): Long =
  val polygon = RectilinearPolygon(points)
  val distinctPairs = points.toSet.subsets(2).map { pair =>
    val Seq(p1, p2) = pair.toSeq.sorted
    (p1, p2)
  }
  val rectangles = distinctPairs.map { case (p1, p2) =>
    val bottomLeft = Point(math.min(p1.x, p2.x), math.min(p1.y, p2.y))
    val topRight = Point(math.max(p1.x, p2.x), math.max(p1.y, p2.y))
    Rectangle(bottomLeft, topRight)
  }
  val validRectangles = rectangles.filter { rectangle =>
    rectangle.edges.forall(edge => polygon.contains(edge))
  }
  validRectangles.maxOf(_.area)

@main def day09(): Unit =
  val input = readInput("day09/input.txt")
  val points = Point.parsePoints(input)

  val part1 = solvePart1(points)
  println(s"Part 1: $part1")

  val part2 = solvePart2(points)
  println(s"Part 2: $part2")
