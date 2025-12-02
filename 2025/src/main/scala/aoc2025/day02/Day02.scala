package aoc2025.day02

import aoc2025.readInput
import aoc2025.utils.sumBy

import scala.util.matching.Regex

enum Invalidity(val regex: Regex):
  case Duplicated extends Invalidity("""^(\d+)\1$""".r)
  case Replicated extends Invalidity("""^(\d+)\1+$""".r)

  def isInvalidId(n: Long): Boolean = regex.matches(n.toString)

case class Range(start: Long, end: Long):
  def sumInvalid(invalidity: Invalidity): Long = (start to end).filter(invalidity.isInvalidId).sum

object Range:
  private def parseRange(input: String): Range =
    input.split("-") match
      case Array(a, b) => Range(a.toLong, b.toLong)

  def parseRanges(input: String): Seq[Range] =
    input.split(",").map(Range.parseRange).toSeq

def sumOfInvalidIds(ranges: Seq[Range], invalidity: Invalidity): Long =
  ranges.sumBy(_.sumInvalid(invalidity))

@main def day02(): Unit =
  val ranges = Range.parseRanges(readInput("day02/input.txt"))
  val part1 = sumOfInvalidIds(ranges, Invalidity.Duplicated)
  println(s"Part 1: $part1")
  val part2 = sumOfInvalidIds(ranges, Invalidity.Replicated)
  println(s"Part 2: $part2")