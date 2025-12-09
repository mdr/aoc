package aoc2025.day09

import aoc2025.utils.readInput

case class Point(x: Long, y: Long)

object Point:
  given Ordering[Point] = Ordering.by(p => (p.x, p.y))

  def parsePoint(s: String): Point =
    val Seq(x, y) = s.split(",").map(_.trim.toLong).toSeq
    Point(x, y)

  def parsePoints(input: String): Seq[Point] =
    input.linesIterator.map(Point.parsePoint).toSeq

case class Rectangle(bottomLeft: Point, topRight: Point):
  def area: Long =
    val width = topRight.x - bottomLeft.x + 1
    val height = topRight.y - bottomLeft.y + 1
    width * height

def solvePart1(points: Seq[Point]): Long =
  points.toSet.subsets(2).map { pair =>
    val Seq(p1, p2) = pair.toSeq.sorted
    (p1, p2)
  }.map { case (p1, p2) =>
    val bottomLeft = Point(math.min(p1.x, p2.x), math.min(p1.y, p2.y))
    val topRight = Point(math.max(p1.x, p2.x), math.max(p1.y, p2.y))
    Rectangle(bottomLeft, topRight)
  }.map(_.area).max

def solvePart2(input: String): Int = ???

@main def day09(): Unit =
  val input = readInput("day09/input.txt")
  val points = Point.parsePoints(input)

  val part1 = solvePart1(points)
  println(s"Part 1: $part1")

  val part2 = solvePart2(input)
  println(s"Part 2: $part2")
