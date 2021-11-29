package aoc2015

import aoc.util.*

val directions: Seq[Direction] = parseDirections(loadString("aoc2015/input-2015-3.txt"))

def day3Part1: Int = countLocationsVisited(directions)

def parseDirection(c: Char): Direction =
  c match
    case '^' => Direction.North
    case '>' => Direction.East
    case 'v' => Direction.South
    case '<' => Direction.West
    case c   => throw RuntimeException(s"Unexpected move character $c")

def parseDirections(s: String): Seq[Direction] = s.map(parseDirection)

def locationsVisited(directions: Seq[Direction]): Set[Point] =
  directions.scanLeft(OriginPoint)(_ + _).toSet

def countLocationsVisited(directions: Seq[Direction]): Int = locationsVisited(directions).size

def countLocationsVisitedBySantaAndRoboSanta(directions: Seq[Direction]): Int =
  val (directions1, directions2) = uninterleave(directions)
  val allVisitedPoints = locationsVisited(directions1) ++ locationsVisited(directions2)
  allVisitedPoints.size

def day3Part2: Int = countLocationsBySantaAndRoboSanta(directions)
