package aoc2021

import scala.util.chaining.scalaUtilChainingOps
import aoc.util.*

val lineSegments: Seq[LineSegment] =
  loadLines("aoc2021/input-2021-5.txt").map(LineSegment.parse)

def day05Part1: Int = calculateDay05Part1(lineSegments)

def day05Part2: Int = calculateDay05Part2(lineSegments)

def calculateDay05Part1(lines: Seq[LineSegment]): Int =
  lines
    .filter(line => line.isHorizontal || line.isVertical)
    .pipe(countOverlappingPoints)

def calculateDay05Part2(lines: Seq[LineSegment]): Int =
  countOverlappingPoints(lines)

def countOverlappingPoints(lines: Seq[LineSegment]): Int =
  lines.flatMap(_.points).counts.count(_._2 >= 2)

case class LineSegment(end1: Point, end2: Point):

  def isHorizontal = end1.y == end2.y
  def isVertical   = end1.x == end2.x

  def points: Seq[Point] =
    def range(start: Long, finish: Long): Seq[Long] =
      if start > finish then start.to(finish, -1) else start to finish
    if (isHorizontal)
      for x <- range(end1.x, end2.x) yield Point(x, end1.y)
    else if (isVertical)
      for y <- range(end1.y, end2.y) yield Point(end1.x, y)
    else
      range(end1.x, end2.x) zip range(end1.y, end2.y) map Point.apply.tupled

object LineSegment:

  def parse(s: String): LineSegment =
    val points = s.split(" -> ").map(parsePoint)
    LineSegment(points(0), points(1))

  private def parsePoint(s: String): Point =
    val numbers = s.split(",")
    Point(numbers(0).toLong, numbers(1).toLong)
