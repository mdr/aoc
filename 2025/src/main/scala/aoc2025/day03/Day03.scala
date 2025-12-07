package aoc2025.day03

import aoc2025.utils.{cached, readInput, sumBy}

case class Bank(batteries: Seq[Long]):
  def largestPossibleJoltagePart1: Joltage =
    largestPossibleJoltage(length = 2, fromIndex = 0).getOrElse(Joltage.Zero)

  def largestPossibleJoltagePart2: Joltage =
    largestPossibleJoltage(length = 12, fromIndex = 0).getOrElse(Joltage.Zero)

  @cached
  private def largestPossibleJoltage(length: Int, fromIndex: Int): Option[Joltage] =
    if length == 0 then
      Some(Joltage.Zero)
    else
      fromIndex.until(batteries.length).flatMap { i =>
        val currentDigit = Joltage(batteries(i)) * math.pow(10, length - 1).toLong
        largestPossibleJoltage(length - 1, i + 1).map(_ + currentDigit)
      }.maxOption


object Bank:
  def parseBank(input: String): Bank = Bank(input.map(_.asDigit.toLong))

  def parseBanks(input: String): Seq[Bank] = input.linesIterator.map(Bank.parseBank).toSeq

def solvePart1(banks: Seq[Bank]): Joltage = banks.sumBy(_.largestPossibleJoltagePart1)

def solvePart2(banks: Seq[Bank]): Joltage = banks.sumBy(_.largestPossibleJoltagePart2)

@main def day03(): Unit =
  val banks = Bank.parseBanks(readInput("day03/input.txt"))
  val part1 = solvePart1(banks)
  println(s"Part 1: $part1")
  val part2 = solvePart2(banks)
  println(s"Part 2: $part2")