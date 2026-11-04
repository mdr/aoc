package aoc2025.day07

import aoc2025.utils.{cached, everyNth, headAndTail, indexesOf, readInput}

case class BeamTrace(positions: Set[Int], splitCount: Int = 0)

case class TachyonManifold(start: Int, splitterRows: Seq[Set[Int]]):
  private def propagate(trace: BeamTrace, splitters: Set[Int]): BeamTrace =
    val newPositions = trace.positions.flatMap { pos =>
      if splitters contains pos then Set(pos - 1, pos + 1) else Set(pos)
    }
    val newSplitCount = trace.splitCount + trace.positions.intersect(splitters).size
    BeamTrace(newPositions, newSplitCount)

  def traceBeam: BeamTrace =
    val initialBeamTrace = BeamTrace(Set(start))
    splitterRows.foldLeft(initialBeamTrace)(propagate)

  @cached
  private def countPathsFrom(position: Int, rowIndex: Int): Long =
    if rowIndex >= splitterRows.length then
      1
    else if splitterRows(rowIndex) contains position then
      countPathsFrom(position - 1, rowIndex + 1) + countPathsFrom(position + 1, rowIndex + 1)
    else
      countPathsFrom(position, rowIndex + 1)

  def countPaths: Long = countPathsFrom(start, 0)

object TachyonManifold:
  def parse(input: String): TachyonManifold =
    val (startLine, splitterLines) = input.linesIterator.toSeq.everyNth(2).headAndTail
    val start = startLine.indexOf('S')
    val splitterRows = splitterLines.map(_.indexesOf('^').toSet)
    TachyonManifold(start, splitterRows)

def solvePart1(manifold: TachyonManifold): Int = manifold.traceBeam.splitCount

def solvePart2(manifold: TachyonManifold): Long = manifold.countPaths

@main def day07(): Unit =
  val input = readInput("day07/input.txt")
  val manifold = TachyonManifold.parse(input)

  val part1 = solvePart1(manifold)
  println(s"Part 1: $part1")

  val part2 = solvePart2(manifold)
  println(s"Part 2: $part2")
