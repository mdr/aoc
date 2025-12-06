package aoc2025.day06

import aoc2025.utils.{readInput, sumBy}

enum Operator:
  case Add, Multiply

object Operator:
  def parse(s: String): Operator = s match
    case "+" => Add
    case "*" => Multiply

case class PuzzleInput(numberRows: Seq[Seq[Long]], operators: Seq[Operator])

object PuzzleInput:
  def parse(input: String): PuzzleInput =
    val lines = input.linesIterator.toSeq
    val numberLines = lines.init
    val operatorLine = lines.last
    val numberRows = numberLines.map(_.trim.split("\\s+").map(_.toLong).toSeq)
    val operators = operatorLine.trim.split("\\s+").map(Operator.parse).toSeq
    PuzzleInput(numberRows, operators)

def solvePart1(input: PuzzleInput): Long =
  val columns = input.numberRows.transpose
  columns.zip(input.operators).sumBy {
    case (numbers, Operator.Add) => numbers.sum
    case (numbers, Operator.Multiply) => numbers.product
  }

def solvePart2(input: PuzzleInput): Long = ???

@main def day06(): Unit =
  val input = readInput("day06/input.txt")
  val puzzleInput = PuzzleInput.parse(input)
  val part1 = solvePart1(puzzleInput)
  println(s"Part 1: $part1")
  val part2 = solvePart2(puzzleInput)
  println(s"Part 2: $part2")
