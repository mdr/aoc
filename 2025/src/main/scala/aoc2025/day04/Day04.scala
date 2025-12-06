package aoc2025.day04

import aoc2025.utils.{iterateUntilStable, readInput}

case class Point(row: Int, column: Int):
  def adjacent8: Set[Point] =
    for
      dr <- (-1 to 1).toSet
      dc <- -1 to 1
      if dr != 0 || dc != 0
    yield Point(row + dr, column + dc)

case class PaperGrid(paperRolls: Set[Point]):

  def forkliftCanAccess(point: Point): Boolean = point.adjacent8.intersect(paperRolls).size < 4

  def removeAccessibleRolls(): PaperGrid = PaperGrid(paperRolls.filterNot(forkliftCanAccess))

object PaperGrid:
  def parse(input: String): PaperGrid =
    val points = input.linesIterator.zipWithIndex.flatMap { case (line, row) =>
      line.zipWithIndex.collect {
        case ('@', column) => Point(row, column)
      }
    }.toSet
    PaperGrid(points)


def solvePart1(grid: PaperGrid) = grid.paperRolls.count(grid.forkliftCanAccess)

def solvePart2(grid: PaperGrid) =
  val stableGrid = iterateUntilStable(grid)(_.removeAccessibleRolls())
  grid.paperRolls.size - stableGrid.paperRolls.size

@main def day04(): Unit =
  val input = readInput("day04/input.txt")
  val grid = PaperGrid.parse(input)
  val part1 = solvePart1(grid)
  println(s"Part 1: $part1")
  val part2 = solvePart2(grid)
  println(s"Part 2: $part2")
