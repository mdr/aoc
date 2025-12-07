package aoc2025.day07

import aoc2025.utils.{everyNth, headAndTail, indexesOf, readInput}

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

object TachyonManifold:
  def parse(input: String): TachyonManifold =
    val (startLine, splitterLines) = input.linesIterator.toSeq.everyNth(2).headAndTail
    val start = startLine.indexOf('S')
    val splitterRows = splitterLines.map(_.indexesOf('^').toSet)
    TachyonManifold(start, splitterRows)

def solvePart1(input: TachyonManifold): Int = input.traceBeam.splitCount

def solvePart2(input: TachyonManifold): Long = ???

@main def day07(): Unit =
  val input = readInput("day07/input.txt")
  val manifold = TachyonManifold.parse(input)

  val part1 = solvePart1(manifold)
  println(s"Part 1: $part1")

  val part2 = solvePart2(manifold)
  println(s"Part 2: $part2")
