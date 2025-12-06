package aoc2025.day06

import aoc2025.utils.{readInput, sumBy}

enum Operator:
  case Add, Multiply

object Operator:
  def parse(s: String): Operator = s match
    case "+" => Add
    case "*" => Multiply

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

case class Worksheet(problems: Seq[Problem]):
  def solve: Long = problems.sumBy(_.solve)

object Worksheet:
  def parseForPart1(input: String): Worksheet =
    val lines = input.linesIterator.toSeq
    val numberLines = lines.init
    val operatorLine = lines.last
    val numberRows = numberLines.map(_.trim.split("\\s+").map(_.toLong).toSeq)
    val operators = operatorLine.trim.split("\\s+").map(Operator.parse).toSeq
    val columns = numberRows.transpose
    val problems = columns.zip(operators).map { case (numbers, operator) => Problem(numbers, operator) }
    Worksheet(problems)

  def parseForPart2(input: String): Worksheet =
    val lines = input.linesIterator.toSeq
    val maxLen = lines.map(_.length).max
    val paddedLines = lines.map(_.padTo(maxLen, ' '))
    val problems = paddedLines.transpose.reverse.map(_.mkString.trim).mkString("\n").split("\n\n").map(Problem.parse)
    Worksheet(problems)

@main def day06(): Unit =
  val input = readInput("day06/input.txt")

  val part1 = Worksheet.parseForPart1(input).solve
  println(s"Part 1: $part1")

  val part2 = Worksheet.parseForPart2(input).solve
  println(s"Part 2: $part2")
