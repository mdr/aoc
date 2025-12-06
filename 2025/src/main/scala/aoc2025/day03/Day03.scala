package aoc2025.day03

import aoc2025.utils.{readInput, sumBy}

class Memoised[Key, Result](compute: (Key, Key => Result) => Result):
  private var cache: Map[Key, Result] = Map.empty

  def apply(key: Key): Result =
    cache.getOrElse(key, {
      val result = compute(key, apply)
      cache = cache.updated(key, result)
      result
    })

case class Bank(batteries: Seq[Long]):
  def largestPossibleJoltagePart1: Long =
    largestPossibleJoltage(SubproblemKey(length = 2)).getOrElse(0L)

  def largestPossibleJoltagePart2: Long =
    largestPossibleJoltage(SubproblemKey(length = 12)).getOrElse(0L)

  private case class SubproblemKey(length: Int, fromIndex: Int = 0)

  private val largestPossibleJoltage: Memoised[SubproblemKey, Option[Long]] =
    Memoised { (key, recurse) =>
      val SubproblemKey(length, fromIndex) = key
      if length == 0 then
        Some(0)
      else
        fromIndex.until(batteries.length).flatMap { i =>
          val currentDigit = batteries(i) * math.pow(10, length - 1).toLong
          recurse(SubproblemKey(length - 1, i + 1)).map(_ + currentDigit)
        }.maxOption
    }


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