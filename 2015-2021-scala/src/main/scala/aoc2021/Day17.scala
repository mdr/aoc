package aoc2021

import aoc.util.*
import scala.annotation.tailrec

val targetArea = TargetArea.parse(loadString("aoc2021/input-2021-17.txt"))

def day17Part1: Long = solveDay1(targetArea)

def day17Part2: Long = solveDay2(targetArea)

def candidateInitialVelocities(targetArea: TargetArea): Seq[Point] =
  for
    xVelocity <- 1 to targetArea.xTo + 1
    yVelocity <- -1000 to 1000
    velocity = Point(xVelocity, yVelocity)
  yield velocity

def solveDay1(targetArea: TargetArea): Long =
  candidateInitialVelocities(targetArea)
    .map(Probe.withVelocity)
    .filter(_ reaches targetArea)
    .map(_.highestY(targetArea))
    .max

def solveDay2(targetArea: TargetArea): Long =
  candidateInitialVelocities(targetArea)
    .map(Probe.withVelocity)
    .filter(_ reaches targetArea)
    .size

case class TargetArea(xFrom: Int, xTo: Int, yFrom: Int, yTo: Int):

  def contains(point: Point): Boolean =
    val Point(x, y) = point
    xFrom <= x && x <= xTo && yFrom <= y && y <= yTo

object TargetArea:
  def parse(s: String) =
    val Seq(xRange, yRange) = s.trim.drop("target area: ".length).split(", ").toSeq
    val Seq(xFrom, xTo)     = xRange.drop("x=".length).split("\\.\\.").toSeq.map(_.toInt)
    val Seq(yFrom, yTo)     = yRange.drop("y=".length).split("\\.\\.").toSeq.map(_.toInt)
    TargetArea(xFrom, xTo, yFrom, yTo)

def moveTowardsZero(n: Long): Long =
  n match
    case _ if n > 0 => n - 1
    case 0          => 0
    case _ if n < 0 => n + 1

object Probe:
  def withVelocity(velocity: Point): Probe = Probe(velocity = velocity)

case class Probe(position: Point = Point.Origin, velocity: Point):
  def step: Probe = copy(position = position + velocity, velocity = Point(moveTowardsZero(velocity.x), velocity.y - 1))

  def isBeyond(targetArea: TargetArea): Boolean =
    position.x > targetArea.xTo || position.y < targetArea.yFrom

  def reaches(targetArea: TargetArea): Boolean =
    val finalProbe =
      iterateUntil(this, probe => probe.isBeyond(targetArea) || targetArea.contains(probe.position))(_.step)
    targetArea contains finalProbe.position

  @tailrec
  final def highestY(targetArea: TargetArea, maxY: Long = Long.MinValue): Long =
    if targetArea contains position then maxY else step.highestY(targetArea, maxY max position.y)
