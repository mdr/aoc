package aoc2025.day06

import aoc2025.utils.{readInput, sumBy}

enum Operator:
  case Add, Multiply

object Operator:
  def parse(s: String): Operator = s match
    case "+" => Add
    case "*" => Multiply

case class PuzzleInput1(numberRows: Seq[Seq[Long]], operators: Seq[Operator])

object PuzzleInput1:
  def parse(input: String): PuzzleInput1 =
    val lines = input.linesIterator.toSeq
    val numberLines = lines.init
    val operatorLine = lines.last
    val numberRows = numberLines.map(_.trim.split("\\s+").map(_.toLong).toSeq)
    val operators = parseOperatorLine(operatorLine)
    PuzzleInput1(numberRows, operators)

  def parseOperatorLine(input: String): Seq[Operator] =
    input.trim.split("\\s+").map(Operator.parse).toSeq

def solvePart1(input: PuzzleInput1): Long =
  val columns = input.numberRows.transpose
  columns.zip(input.operators).sumBy {
    case (numbers, Operator.Add) => numbers.sum
    case (numbers, Operator.Multiply) => numbers.product
  }

case class Problem(numbers: Seq[Long], operator: Operator):
  def solve: Long = operator match
    case Operator.Add => numbers.sum
    case Operator.Multiply => numbers.product

object Problem:
  def parse(input: String): Problem =
    val trimmed = input.trim
    val operator = Operator.parse(trimmed.last.toString)
    val numberStrings = trimmed.init.split("\n").map(_.trim.toLong)
    Problem(numberStrings, operator)

case class PuzzleInput2(problems: Seq[Problem]):
  def sumOfAllProblemAnswers: Long = problems.sumBy(_.solve)

object PuzzleInput2:
  def parse(input: String): PuzzleInput2 =
    val lines = input.linesIterator.toSeq
    val maxLen = lines.map(_.length).max
    val paddedLines = lines.map(_.padTo(maxLen, ' '))
    val problems = paddedLines.transpose.reverse.map(_.mkString.trim).mkString("\n").split("\n\n").map(Problem.parse)
    PuzzleInput2(problems)

def solvePart2(input: PuzzleInput2): Long = input.sumOfAllProblemAnswers

@main def day06(): Unit =
  val input = readInput("day06/input.txt")

  val puzzleInput1 = PuzzleInput1.parse(input)
  val part1 = solvePart1(puzzleInput1)
  println(s"Part 1: $part1")

  val puzzleInput2 = PuzzleInput2.parse(input)
  val part2 = solvePart2(puzzleInput2)
  println(s"Part 2: $part2")
