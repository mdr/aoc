package aoc2025.day03

import aoc2025.utils.{readInput, sumBy}

def orderedItemPairs[A](items: Seq[A]): Seq[(A, A)] =
  for
    i <- items.indices
    j <- items.indices
    if j > i
  yield (items(i), items(j))

def allSubsequencesOfLength[A](items: Seq[A], length: Int): Seq[Seq[A]] =
  val indices = items.indices

  def getSubsequences(length: Int, onlyIndicesAfter: Int): Seq[List[A]] =
    if length == 0 then
      Seq(List.empty)
    else
      for
        i <- indices
        if i > onlyIndicesAfter
        j <- getSubsequences(length - 1, i)
      yield items(i) :: j

  getSubsequences(length, -1).map(_.toSeq)

case class Bank(batteries: Seq[Long]):
  def largestPossibleJoltagePart1: Long =
    new JoltageSearch().largestPossibleJoltage(SubproblemKey(length = 2, fromIndex = 0)).getOrElse(0L)

  def largestPossibleJoltagePart2: Long =
    new JoltageSearch().largestPossibleJoltage(SubproblemKey(length = 12, fromIndex = 0)).getOrElse(0L)

  private case class SubproblemKey(length: Int, fromIndex: Int)

  private class JoltageSearch:
    private var subproblemResults: Map[SubproblemKey, Option[Long]] = Map.empty

    def largestPossibleJoltage(key: SubproblemKey): Option[Long] =
      subproblemResults.getOrElse(key, {
        val result = computeLargestPossibleJoltage(key)
        subproblemResults = subproblemResults.updated(key, result)
        result
      })

    private def computeLargestPossibleJoltage(key: SubproblemKey): Option[Long] =
      val SubproblemKey(length, fromIndex) = key
      if length == 0 then
        Some(0)
      else if batteries.length - fromIndex < length then
        None
      else
        fromIndex.until(batteries.length).flatMap { i =>
          val batteryJoltage = batteries(i)
          val currentDigit = batteryJoltage * math.pow(10, length - 1).toLong
          largestPossibleJoltage(SubproblemKey(length - 1, i + 1)).map { _ + currentDigit}
        }.maxOption


object Bank:
  def parseBank(input: String): Bank = Bank(input.map(_.asDigit.toLong))

  def parseBanks(input: String): Seq[Bank] = input.linesIterator.map(Bank.parseBank).toSeq

def solvePart1(banks: Seq[Bank]): Long = banks.sumBy(_.largestPossibleJoltagePart1)

def solvePart2(banks: Seq[Bank]): Long = banks.sumBy(_.largestPossibleJoltagePart2)

@main def day03(): Unit =
  val banks = Bank.parseBanks(readInput("day03/input.txt"))
  val part1 = solvePart1(banks)
  println(s"Part 1: $part1")
  val part2 = solvePart2(banks)
  println(s"Part 2: $part2")