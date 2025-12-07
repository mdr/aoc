package aoc2025.day06

import aoc2025.utils.{initAndLast, readInput, splitOnBlankLines, splitOnWhitespace, sumBy, transposeBlock}

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
    val (init, last) = input.trim.initAndLast
    val operator = Operator.parse(last.toString)
    val numbers = init.mkString.split("\n").toSeq.map(_.trim.toLong)
    Problem(numbers, operator)

case class Worksheet(problems: Seq[Problem]):
  def solve: Long = problems.sumBy(_.solve)

object Worksheet:
  def parseForPart1(input: String): Worksheet =
    val (numberLines, operatorLine) = input.linesIterator.toSeq.initAndLast
    val numberRows = numberLines.map(_.splitOnWhitespace.map(_.toLong))
    val operators = operatorLine.splitOnWhitespace.map(Operator.parse)
    val columns = numberRows.transpose
    val problems = columns.zip(operators).map { case (numbers, operator) => Problem(numbers, operator) }
    Worksheet(problems)

  def parseForPart2(input: String): Worksheet =
    val problems = input.transposeBlock.reverse.map(_.trim).mkString("\n").splitOnBlankLines.map(Problem.parse)
    Worksheet(problems)

@main def day06(): Unit =
  val input = readInput("day06/input.txt")

  val part1 = Worksheet.parseForPart1(input).solve
  println(s"Part 1: $part1")

  val part2 = Worksheet.parseForPart2(input).solve
  println(s"Part 2: $part2")
